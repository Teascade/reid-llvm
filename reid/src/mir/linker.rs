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
    mir::{
        pass::BinopKey, BinopDefinition, CustomTypeKey, FunctionDefinitionKind, FunctionParam, SourceModuleId,
        TypeDefinition, TypeKind,
    },
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
    #[error("No such value {0} found in module {1}")]
    ImportDoesNotExist(String, String),
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
    let module = compile_module(id, tokens, module_map, None, false)?.map_err(|(_, e)| e)?;

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

#[derive(Default, Clone)]
pub struct LinkerState {
    extern_imported_types: HashMap<SourceModuleId, HashMap<String, SourceModuleId>>,
}

type LinkerPassState<'st, 'sc> = PassState<'st, 'sc, LinkerState, ErrorKind>;

impl<'map> Pass for LinkerPass<'map> {
    type Data = LinkerState;
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
        let mut already_imported_binops = HashSet::<BinopKey>::new();

        while let Some(module) = modules_to_process.pop() {
            let mut extern_types = HashMap::new();
            let mut importer_module = module.borrow_mut();

            for import in importer_module.imports.clone() {
                let Import(path, _) = &import;
                if path.len() != 2 {
                    state.ok::<_, Infallible>(Err(ErrorKind::InnerModulesNotYetSupported(import.clone())), import.1);
                }

                let Some((module_name, _)) = path.get(0) else {
                    continue;
                };

                let mut imported = if let Some(mod_id) = module_ids.get(module_name) {
                    modules.get(mod_id).unwrap()
                } else if module_name == STD_NAME {
                    let std = compile_std(&mut self.module_map)?;
                    modules.insert(std.module_id, Rc::new(RefCell::new(compile_std(&mut self.module_map)?)));
                    module_ids.insert(std.name, std.module_id);
                    modules.get(&std.module_id).unwrap()
                } else {
                    let file_path = PathBuf::from(&context.base.clone()).join(module_name.to_owned() + ".reid");

                    let Ok(source) = fs::read_to_string(&file_path) else {
                        state.ok::<_, Infallible>(Err(ErrorKind::ModuleNotFound(module_name.clone())), import.1);
                        continue;
                    };

                    let (id, tokens) = match parse_module(&source, module_name.clone(), &mut self.module_map) {
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
                        Ok(res) => match res {
                            Ok(imported_module) => {
                                if imported_module.is_main {
                                    state.ok::<_, Infallible>(
                                        Err(ErrorKind::TriedLinkingMain(module_name.clone())),
                                        import.1,
                                    );
                                    continue;
                                }
                                let module_id = imported_module.module_id;
                                module_ids.insert(imported_module.name.clone(), imported_module.module_id);
                                modules.insert(module_id, Rc::new(RefCell::new(imported_module)));
                                let imported = modules.get_mut(&module_id).unwrap();
                                modules_to_process.push(imported.clone());
                                imported
                            }
                            Err((_, err)) => {
                                state.ok::<_, Infallible>(
                                    Err(ErrorKind::ModuleCompilationError(
                                        module_name.clone(),
                                        format!("{}", err),
                                    )),
                                    import.1,
                                );
                                continue;
                            }
                        },
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

                let Some((import_name, _)) = path.get(1) else {
                    continue;
                };
                let import_id = imported.module_id;

                let mut imported_types = Vec::new();

                if let Some(func) = imported.functions.iter_mut().find(|f| f.name == *import_name) {
                    let func_name = func.name.clone();
                    let func_signature = func.signature();

                    if !func.is_pub {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::FunctionIsPrivate(module_name.clone(), func_name.clone())),
                            import.1,
                        );
                        continue;
                    }

                    func.is_imported = true;

                    if let Some(existing) = importer_module.functions.iter().find(|f| f.name == *func_name) {
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

                    let types = import_type(&func.return_type, false);
                    let return_type = func.return_type.clone();
                    imported_types.extend(types);

                    let mut param_tys = Vec::new();
                    for param in &func.parameters {
                        let types = import_type(&param.ty, false);
                        imported_types.extend(types);
                        param_tys.push(param.clone());
                    }

                    importer_module.functions.push(FunctionDefinition {
                        name: func_name.clone(),
                        linkage_name: None,
                        is_pub: false,
                        is_imported: false,
                        return_type,
                        parameters: param_tys,
                        kind: super::FunctionDefinitionKind::Extern(true),
                        source: Some(imported.module_id),
                        signature_meta: func_signature,
                    });
                } else if let Some(ty) = imported.typedefs.iter_mut().find(|f| f.name == *import_name) {
                    let external_key = CustomTypeKey(ty.name.clone(), ty.source_module);
                    let imported_ty = TypeKind::CustomType(external_key.clone());
                    imported_types.push((external_key, true));

                    for binop in &mut imported.binop_defs {
                        if binop.lhs.ty != imported_ty && binop.rhs.ty != imported_ty {
                            continue;
                        }
                        let binop_key = BinopKey {
                            params: (binop.lhs.ty.clone(), binop.rhs.ty.clone()),
                            operator: binop.op,
                        };
                        if already_imported_binops.contains(&binop_key) {
                            continue;
                        }
                        binop.exported = true;
                        already_imported_binops.insert(binop_key);
                        match &binop.fn_kind {
                            FunctionDefinitionKind::Local(..) => {
                                importer_module.binop_defs.push(BinopDefinition {
                                    lhs: binop.lhs.clone(),
                                    op: binop.op,
                                    rhs: binop.rhs.clone(),
                                    return_type: binop.return_type.clone(),
                                    fn_kind: FunctionDefinitionKind::Extern(true),
                                    meta: binop.meta.clone(),
                                    exported: false,
                                });
                            }
                            FunctionDefinitionKind::Extern(_) => {}
                            FunctionDefinitionKind::Intrinsic(_) => {}
                        }
                    }

                    for (ty, func) in &mut imported.associated_functions {
                        if *ty != imported_ty {
                            continue;
                        }

                        let func_name = func.name.clone();

                        if !func.is_pub {
                            state.ok::<_, Infallible>(
                                Err(ErrorKind::FunctionIsPrivate(module_name.clone(), func_name.clone())),
                                import.1,
                            );
                            continue;
                        }

                        func.is_imported = true;

                        if let Some((_, existing)) = importer_module
                            .associated_functions
                            .iter()
                            .find(|(ty, f)| *ty == imported_ty && f.name == *func_name)
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

                        let types = import_type(&func.return_type, false);
                        let return_type = func.return_type.clone();
                        imported_types.extend(types);

                        let mut param_tys = Vec::new();
                        for param in &func.parameters {
                            let types = import_type(&param.ty, false);
                            imported_types.extend(types);
                            param_tys.push(param.clone());
                        }

                        importer_module.associated_functions.push((
                            ty.clone(),
                            FunctionDefinition {
                                name: func_name.clone(),
                                linkage_name: Some(format!("{}::{}", ty, func_name)),
                                is_pub: false,
                                is_imported: false,
                                return_type,
                                parameters: param_tys,
                                kind: super::FunctionDefinitionKind::Extern(true),
                                source: Some(import_id),
                                signature_meta: func.signature_meta,
                            },
                        ));
                    }
                } else {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::ImportDoesNotExist(module_name.clone(), import_name.clone())),
                        import.1,
                    );
                    continue;
                }

                let mut seen = HashSet::new();
                let mut current_extern_types = HashSet::new();
                seen.extend(imported_types.clone().iter().map(|t| t.0.clone()));
                current_extern_types.extend(imported_types.clone().iter().filter(|t| t.1).map(|t| t.0.clone()));
                for extern_type in &current_extern_types {
                    extern_types.insert(extern_type.0.clone(), extern_type.1);
                }

                let imported_mod_id = imported.module_id;
                let imported_mod_typedefs = &mut imported.typedefs;

                for typekey in imported_types.clone() {
                    let typedef = imported_mod_typedefs
                        .iter()
                        .find(|ty| CustomTypeKey(ty.name.clone(), imported_mod_id) == typekey.0)
                        .unwrap();
                    let inner = find_inner_types(typedef, seen.clone(), imported_mod_id);
                    seen.extend(inner.iter().cloned());
                }

                // TODO: Unable to import same-named type from multiple places..
                let seen = seen
                    .difference(&already_imported_types)
                    .cloned()
                    .collect::<HashSet<_>>();

                already_imported_types.extend(seen.clone());

                for typekey in &already_imported_types {
                    if current_extern_types.contains(typekey) {
                        let module_id = importer_module.module_id;
                        let typedef = importer_module
                            .typedefs
                            .iter_mut()
                            .find(|t| t.name == typekey.0 && t.source_module == typekey.1);
                        if let Some(typedef) = typedef {
                            typedef.importer = Some(module_id);
                        }
                    }
                }

                for typekey in seen.into_iter() {
                    let mut typedef = imported_mod_typedefs
                        .iter()
                        .find(|ty| CustomTypeKey(ty.name.clone(), imported_mod_id) == typekey)
                        .unwrap()
                        .clone();

                    if current_extern_types.contains(&typekey) {
                        typedef = TypeDefinition {
                            importer: Some(importer_module.module_id),
                            ..typedef
                        };
                    }

                    importer_module.typedefs.push(typedef);
                }
            }
            state
                .scope
                .data
                .extern_imported_types
                .insert(importer_module.module_id, extern_types);
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

    fn function(
        &mut self,
        function: &mut FunctionDefinition,
        state: PassState<Self::Data, Self::TError>,
    ) -> PassResult {
        if matches!(function.kind, FunctionDefinitionKind::Local(_, _)) {
            let mod_id = state.scope.module_id.unwrap();
            let extern_types = &state.scope.data.extern_imported_types.get(&mod_id);
            if let Some(extern_types) = extern_types {
                function.return_type = function.return_type.update_imported(*extern_types, mod_id);
                for param in function.parameters.iter_mut() {
                    param.ty = param.ty.update_imported(extern_types, mod_id);
                }
            }
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &mut super::Statement, state: PassState<Self::Data, Self::TError>) -> PassResult {
        let mod_id = state.scope.module_id.unwrap();
        let extern_types = &state.scope.data.extern_imported_types.get(&mod_id);
        if let Some(extern_types) = extern_types {
            match &mut stmt.0 {
                super::StmtKind::Let(var_ref, _, _) => {
                    var_ref.0 = var_ref.0.update_imported(extern_types, mod_id);
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn expr(&mut self, expr: &mut super::Expression, state: PassState<Self::Data, Self::TError>) -> PassResult {
        let mod_id = state.scope.module_id.unwrap();
        let extern_types = &state.scope.data.extern_imported_types.get(&mod_id);
        if let Some(extern_types) = extern_types {
            match &mut expr.0 {
                super::ExprKind::Variable(var_ref) => {
                    var_ref.0 = var_ref.0.update_imported(extern_types, mod_id);
                }
                super::ExprKind::Indexed(.., type_kind, _) => {
                    *type_kind = type_kind.update_imported(extern_types, mod_id)
                }
                super::ExprKind::Accessed(.., type_kind, _, _) => {
                    *type_kind = type_kind.update_imported(extern_types, mod_id)
                }
                super::ExprKind::BinOp(.., type_kind) => *type_kind = type_kind.update_imported(extern_types, mod_id),

                super::ExprKind::Borrow(..) => {}
                super::ExprKind::Deref(..) => {}
                super::ExprKind::CastTo(_, type_kind) => *type_kind = type_kind.update_imported(extern_types, mod_id),
                super::ExprKind::AssociatedFunctionCall(type_kind, _) => {
                    *type_kind = type_kind.update_imported(extern_types, mod_id)
                }
                _ => {}
            }
        }
        Ok(())
    }
}

impl TypeKind {
    fn update_imported(
        &self,
        extern_types: &HashMap<String, SourceModuleId>,
        importer_mod_id: SourceModuleId,
    ) -> TypeKind {
        match &self {
            TypeKind::Array(type_kind, len) => {
                TypeKind::Array(Box::new(type_kind.update_imported(extern_types, importer_mod_id)), *len)
            }
            TypeKind::CustomType(custom_type_key) => {
                if let Some(mod_id) = extern_types.get(&custom_type_key.0) {
                    TypeKind::CustomType(CustomTypeKey(custom_type_key.0.clone(), *mod_id))
                } else {
                    self.clone()
                }
            }
            TypeKind::Borrow(type_kind, mutable) => TypeKind::Borrow(
                Box::new(type_kind.update_imported(extern_types, importer_mod_id)),
                *mutable,
            ),
            TypeKind::UserPtr(type_kind) => {
                TypeKind::UserPtr(Box::new(type_kind.update_imported(extern_types, importer_mod_id)))
            }
            TypeKind::CodegenPtr(type_kind) => {
                TypeKind::CodegenPtr(Box::new(type_kind.update_imported(extern_types, importer_mod_id)))
            }
            _ => self.clone(),
        }
    }
}

fn import_type(ty: &TypeKind, usable_import: bool) -> Vec<(CustomTypeKey, bool)> {
    let mut imported_types = Vec::new();
    match &ty {
        TypeKind::CustomType(key) => imported_types.push((key.clone(), usable_import)),
        TypeKind::Borrow(ty, _) => imported_types.extend(import_type(ty, usable_import)),
        TypeKind::Array(ty, _) => imported_types.extend(import_type(ty, usable_import)),
        TypeKind::UserPtr(ty) => imported_types.extend(import_type(ty, usable_import)),
        TypeKind::CodegenPtr(ty) => imported_types.extend(import_type(ty, usable_import)),
        _ => {}
    };
    imported_types
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
