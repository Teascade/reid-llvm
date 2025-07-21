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
    error_raporting::{ModuleMap, ReidError},
    lexer::FullToken,
    mir::{TypeDefinition, TypeKind},
    parse_module,
};

use super::{
    implement::EqualsIssue,
    pass::{Pass, PassResult, PassState},
    Context, FunctionDefinition, Import, Metadata, Module,
};

pub static STD_SOURCE: &str = include_str!("../../lib/std.reid");

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

pub fn compile_std(
    module_map: &mut ModuleMap,
) -> Result<(super::Module, Vec<FullToken>), ReidError> {
    let (id, tokens) = parse_module(STD_SOURCE, "standard_library", module_map)?;
    let module = compile_module(id, &tokens, module_map, None, false)?;

    let mut mir_context = super::Context::from(vec![module], Default::default());

    let std_compiled = mir_context.modules.remove(0);
    Ok((std_compiled, tokens))
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct LinkerPass<'map> {
    pub module_map: &'map mut ModuleMap,
}

type LinkerPassState<'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl<'map> Pass for LinkerPass<'map> {
    type Data = ();
    type TError = ErrorKind;
    fn context(&mut self, context: &mut Context, mut state: LinkerPassState) -> PassResult {
        let mains = context
            .modules
            .iter()
            .filter(|m| m.is_main)
            .collect::<Vec<_>>();
        if mains.len() > 1 {
            state.note_errors(&vec![ErrorKind::MultipleMainsAtStart], Metadata::default());
            return Ok(());
        }
        let Some(main) = mains.first() else {
            state.note_errors(&vec![ErrorKind::NoMainDefined], Metadata::default());
            return Ok(());
        };

        let Some(_) = main.functions.iter().find(|f| f.name == "main") else {
            state.note_errors(&vec![ErrorKind::NoMainFunction], Metadata::default());
            return Ok(());
        };

        let mut modules = HashMap::<String, Rc<RefCell<_>>>::new();

        for module in context.modules.drain(..) {
            let tokens = self
                .module_map
                .module(&module.module_id)
                .unwrap()
                .tokens
                .clone()
                .unwrap();
            modules.insert(module.name.clone(), Rc::new(RefCell::new((module, tokens))));
        }

        let mut modules_to_process: Vec<Rc<RefCell<(Module, Vec<FullToken>)>>> =
            modules.values().cloned().collect();

        while let Some(module) = modules_to_process.pop() {
            let mut importer_module = module.borrow_mut();

            for import in importer_module.0.imports.clone() {
                let Import(path, _) = &import;
                if path.len() != 2 {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::InnerModulesNotYetSupported(import.clone())),
                        import.1,
                    );
                }

                let module_name = unsafe { path.get_unchecked(0) };

                let mut imported = if let Some(module) = modules.get_mut(module_name) {
                    module
                } else if module_name == "std" {
                    modules.insert(
                        "std".to_owned(),
                        Rc::new(RefCell::new(compile_std(&mut self.module_map)?)),
                    );
                    modules.get("std").unwrap()
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

                    match compile_module(id, &tokens, &mut self.module_map, Some(file_path), false)
                    {
                        Ok(imported_module) => {
                            if imported_module.is_main {
                                state.ok::<_, Infallible>(
                                    Err(ErrorKind::TriedLinkingMain(module_name.clone())),
                                    import.1,
                                );
                                continue;
                            }
                            let module_name = imported_module.name.clone();
                            modules.insert(
                                module_name.clone(),
                                Rc::new(RefCell::new((imported_module, tokens))),
                            );
                            let imported = modules.get_mut(&module_name).unwrap();
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
                let imported_mod_name = imported.0.name.clone();
                let imported_mod_typedefs = imported.0.typedefs.clone();

                let Some(func) = imported
                    .0
                    .functions
                    .iter_mut()
                    .find(|f| f.name == *func_name)
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
                    .0
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

                fn import_type(base: &String, ty: &TypeKind) -> (TypeKind, Vec<String>) {
                    let mut imported_types = Vec::new();
                    let ty = match &ty {
                        TypeKind::CustomType(name) => {
                            imported_types.push(name.clone());

                            let name = format!("{}::{}", base, name);
                            TypeKind::CustomType(name)
                        }
                        _ => ty.clone(),
                    };
                    (ty, imported_types)
                }

                let mut imported_types = Vec::new();

                let (return_type, types) = import_type(&imported_mod_name, &func.return_type);
                imported_types.extend(types);

                let mut param_tys = Vec::new();
                for (param_name, param_ty) in &func.parameters {
                    let (param_type, types) = import_type(&imported_mod_name, &param_ty);
                    imported_types.extend(types);
                    param_tys.push((param_name.clone(), param_type));
                }

                fn find_inner_types(
                    typedef: &TypeDefinition,
                    mut seen: HashSet<String>,
                ) -> Vec<String> {
                    match &typedef.kind {
                        crate::mir::TypeDefinitionKind::Struct(struct_type) => {
                            let typenames = struct_type
                                .0
                                .iter()
                                .filter(|t| matches!(t.1, TypeKind::CustomType(_)))
                                .map(|t| match &t.1 {
                                    TypeKind::CustomType(t) => t,
                                    _ => panic!(),
                                })
                                .cloned()
                                .collect::<Vec<_>>();

                            for typename in typenames {
                                if seen.contains(&typename) {
                                    continue;
                                }
                                let inner = find_inner_types(typedef, seen.clone());
                                seen.insert(typename);
                                seen.extend(inner);
                            }

                            seen.into_iter().collect()
                        }
                    }
                }

                let mut seen = HashSet::new();
                seen.extend(imported_types.clone());

                for typename in imported_types.clone() {
                    let typedef = imported_mod_typedefs
                        .iter()
                        .find(|ty| ty.name == typename)
                        .unwrap();
                    let inner = find_inner_types(typedef, seen.clone());
                    seen.extend(inner);
                }

                for typename in seen.into_iter() {
                    let mut typedef = imported_mod_typedefs
                        .iter()
                        .find(|ty| ty.name == typename)
                        .unwrap()
                        .clone();

                    typedef.name = format!("{}::{}", imported_mod_name, typedef.name);
                    importer_module.0.typedefs.push(typedef);
                }

                importer_module.0.functions.push(FunctionDefinition {
                    name: func.name.clone(),
                    is_pub: false,
                    is_imported: false,
                    return_type: return_type,
                    parameters: param_tys,
                    kind: super::FunctionDefinitionKind::Extern(true),
                });
            }
        }

        context.modules = modules
            .into_values()
            .map(|v| Rc::into_inner(v).unwrap().into_inner().0)
            .collect();

        Ok(())
    }
}
