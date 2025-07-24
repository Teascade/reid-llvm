use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    convert::Infallible,
    fs::{self},
    path::PathBuf,
    rc::Rc,
};

use crate::{
    compile_module,
    error_raporting::{ErrorModules, ReidError},
    mir::{CustomTypeKey, SourceModuleId, TypeDefinition, TypeKind},
    parse_module,
};

use super::{
    implement::EqualsIssue,
    pass::{Pass, PassResult, PassState},
    Context, FunctionDefinition, Import, Metadata, Module,
};

pub static STD_SOURCE: &str = include_str!("../../lib/std.reid");
pub static STD_NAME: &str = "std";

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorKind {
    #[error("Unable to import inner modules, not yet supported: {0}")]
    InnerModulesNotYetSupported(Import),
    #[error("No such module: {0}")]
    ModuleNotFound(String),
    #[error("Error while compiling module {0}: {1}")]
    ModuleCompilationError(String, String),
    #[error("No such function {0} found in module {1}")]
    NoSuchFunctionInModule(String, String),
    #[error("Importing function {0}::{1} not possible: {2}")]
    FunctionImportIssue(String, String, EqualsIssue),
    #[error("Tried linking another main module: {0}")]
    TriedLinkingMain(String),
    #[error("Multiple Mains at the start!")]
    MultipleMainsAtStart,
    #[error("No Main-module found!")]
    NoMainDefined,
    #[error("Main module has no main-function!")]
    NoMainFunction,
    #[error("Function {1} in module {0} is private!")]
    FunctionIsPrivate(String, String),
}

pub fn compile_std(module_map: &mut ErrorModules) -> Result<Module, ReidError> {
    let (id, tokens) = parse_module(STD_SOURCE, STD_NAME, module_map)?;
    let module = compile_module(id, tokens, module_map, None, false)?;

    let module_id = module.module_id;
    let mut mir_context = super::Context::from(vec![module], Default::default());

    let std_compiled = mir_context.modules.remove(&module_id).unwrap();
    Ok(std_compiled)
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct LinkerPass<'map> {
    pub module_map: &'map mut ErrorModules,
    pub is_lib: bool,
}

type LinkerPassState<'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl<'map> Pass for LinkerPass<'map> {
    type Data = ();
    type TError = ErrorKind;
    fn context(&mut self, context: &mut Context, mut state: LinkerPassState) -> PassResult {
        let mains = context
            .modules
            .iter_mut()
            .filter(|(_, module)| module.is_main)
            .collect::<Vec<_>>();
        if mains.len() > 1 {
            state.note_errors(&vec![ErrorKind::MultipleMainsAtStart], Metadata::default());
            return Ok(());
        }
        if let Some((_, main)) = mains.first() {
            if let None = main.functions.iter().find(|f| f.name == "main") {
                if !self.is_lib {
                    state.note_errors(&vec![ErrorKind::NoMainFunction], Metadata::default());
                    return Ok(());
                }
            };
        } else {
            if !self.is_lib {
                state.note_errors(&vec![ErrorKind::NoMainDefined], Metadata::default());
                return Ok(());
            }
        };

        let mut modules = HashMap::<SourceModuleId, Rc<RefCell<_>>>::new();
        let mut module_ids = HashMap::<String, SourceModuleId>::new();

        for (mod_id, module) in context.modules.drain() {
            modules.insert(mod_id, Rc::new(RefCell::new(module)));
        }

        let mut modules_to_process: Vec<Rc<RefCell<_>>> = modules.values().cloned().collect();

        let mut already_imported_types = HashSet::<CustomTypeKey>::new();

        while let Some(module) = modules_to_process.pop() {
            let mut importer_module = module.borrow_mut();

            for import in importer_module.imports.clone() {
                let Import(path, _) = &import;
                if path.len() != 2 {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::InnerModulesNotYetSupported(import.clone())),
                        import.1,
                    );
                }

                let module_name = unsafe { path.get_unchecked(0) };

                let mut imported = if let Some(mod_id) = module_ids.get(module_name) {
                    modules.get(mod_id).unwrap()
                } else if module_name == STD_NAME {
                    let std = compile_std(&mut self.module_map)?;
                    modules.insert(
                        std.module_id,
                        Rc::new(RefCell::new(compile_std(&mut self.module_map)?)),
                    );
                    module_ids.insert(std.name, std.module_id);
                    modules.get(&std.module_id).unwrap()
                } else {
                    let file_path =
                        PathBuf::from(&context.base.clone()).join(module_name.to_owned() + ".reid");

                    let Ok(source) = fs::read_to_string(&file_path) else {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::ModuleNotFound(module_name.clone())),
                            import.1,
                        );
                        continue;
                    };

                    let (id, tokens) =
                        match parse_module(&source, module_name.clone(), &mut self.module_map) {
                            Ok(val) => val,
                            Err(err) => {
                                state.ok::<_, Infallible>(
                                    Err(ErrorKind::ModuleCompilationError(
                                        module_name.clone(),
                                        format!("{}", err),
                                    )),
                                    import.1,
                                );
                                continue;
                            }
                        };

                    match compile_module(id, tokens, &mut self.module_map, Some(file_path), false) {
                        Ok(imported_module) => {
                            if imported_module.is_main {
                                state.ok::<_, Infallible>(
                                    Err(ErrorKind::TriedLinkingMain(module_name.clone())),
                                    import.1,
                                );
                                continue;
                            }
                            let module_id = imported_module.module_id;
                            module_ids
                                .insert(imported_module.name.clone(), imported_module.module_id);
                            modules.insert(module_id, Rc::new(RefCell::new(imported_module)));
                            let imported = modules.get_mut(&module_id).unwrap();
                            modules_to_process.push(imported.clone());
                            imported
                        }
                        Err(err) => {
                            state.ok::<_, Infallible>(
                                Err(ErrorKind::ModuleCompilationError(
                                    module_name.clone(),
                                    format!("{}", err),
                                )),
                                import.1,
                            );
                            continue;
                        }
                    }
                }
                .borrow_mut();

                let func_name = unsafe { path.get_unchecked(1) };

                let Some(func) = imported.functions.iter_mut().find(|f| f.name == *func_name)
                else {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::NoSuchFunctionInModule(
                            module_name.clone(),
                            func_name.clone(),
                        )),
                        import.1,
                    );
                    continue;
                };

                let func_name = func.name.clone();

                if !func.is_pub {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::FunctionIsPrivate(
                            module_name.clone(),
                            func_name.clone(),
                        )),
                        import.1,
                    );
                    continue;
                }

                func.is_imported = true;

                if let Some(existing) = importer_module
                    .functions
                    .iter()
                    .find(|f| f.name == *func_name)
                {
                    if let Err(e) = existing.equals_as_imported(func) {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::FunctionImportIssue(
                                module_name.clone(),
                                func_name.clone(),
                                e,
                            )),
                            import.1,
                        );
                    }
                }

                fn import_type(ty: &TypeKind) -> Vec<CustomTypeKey> {
                    let mut imported_types = Vec::new();
                    match &ty {
                        TypeKind::CustomType(key) => imported_types.push(key.clone()),
                        TypeKind::Borrow(ty, _) => imported_types.extend(import_type(ty)),
                        TypeKind::Array(ty, _) => imported_types.extend(import_type(ty)),
                        TypeKind::UserPtr(ty) => imported_types.extend(import_type(ty)),
                        TypeKind::CodegenPtr(ty) => imported_types.extend(import_type(ty)),
                        _ => {}
                    };
                    imported_types
                }

                let mut imported_types = Vec::new();

                let types = import_type(&func.return_type);
                let return_type = func.return_type.clone();
                imported_types.extend(types);

                let mut param_tys = Vec::new();
                for (param_name, param_ty) in &func.parameters {
                    let types = import_type(&param_ty);
                    imported_types.extend(types);
                    param_tys.push((param_name.clone(), param_ty.clone()));
                }

                fn find_inner_types(
                    typedef: &TypeDefinition,
                    mut seen: HashSet<CustomTypeKey>,
                    mod_id: SourceModuleId,
                ) -> Vec<CustomTypeKey> {
                    match &typedef.kind {
                        crate::mir::TypeDefinitionKind::Struct(struct_type) => {
                            let typenames = struct_type
                                .0
                                .iter()
                                .filter(|t| matches!(t.1, TypeKind::CustomType(..)))
                                .map(|t| match &t.1 {
                                    TypeKind::CustomType(CustomTypeKey(t, _)) => t,
                                    _ => panic!(),
                                })
                                .cloned()
                                .collect::<Vec<_>>();

                            for typename in typenames {
                                if seen.contains(&CustomTypeKey(typename.clone(), mod_id)) {
                                    continue;
                                }
                                let inner = find_inner_types(typedef, seen.clone(), mod_id);
                                seen.insert(CustomTypeKey(typename, mod_id));
                                seen.extend(inner);
                            }

                            seen.into_iter().collect()
                        }
                    }
                }

                let mut seen = HashSet::new();
                seen.extend(imported_types.clone());

                let imported_mod_id = imported.module_id;
                let imported_mod_typedefs = &mut imported.typedefs;

                for typekey in imported_types.clone() {
                    let typedef = imported_mod_typedefs
                        .iter()
                        .find(|ty| CustomTypeKey(ty.name.clone(), imported_mod_id) == typekey)
                        .unwrap();
                    let inner = find_inner_types(typedef, seen.clone(), imported_mod_id);
                    seen.extend(inner);
                }

                // TODO: Unable to import same-named type from multiple places..
                let seen = seen
                    .difference(&already_imported_types)
                    .cloned()
                    .collect::<HashSet<_>>();

                already_imported_types.extend(seen.clone());

                for typekey in seen.into_iter() {
                    let typedef = imported_mod_typedefs
                        .iter()
                        .find(|ty| CustomTypeKey(ty.name.clone(), imported_mod_id) == typekey)
                        .unwrap()
                        .clone();

                    importer_module.typedefs.push(typedef.clone());
                }

                importer_module.functions.push(FunctionDefinition {
                    name: func_name,
                    is_pub: false,
                    is_imported: false,
                    return_type,
                    parameters: param_tys,
                    kind: super::FunctionDefinitionKind::Extern(true),
                });
            }
        }

        let mut modules: Vec<Module> = modules
            .into_values()
            .map(|v| Rc::into_inner(v).unwrap().into_inner())
            .collect();

        for module in modules.drain(..) {
            context.modules.insert(module.module_id, module);
        }

        Ok(())
    }
}
