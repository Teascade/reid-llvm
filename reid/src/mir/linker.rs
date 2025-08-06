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
        pass::BinopKey, BinopDefinition, CustomTypeKey, FunctionDefinitionKind, SourceModuleId, StructType,
        TypeDefinition, TypeDefinitionKind, TypeKind,
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
    #[error("Type {0} has cyclical fields!")]
    CyclicalType(String),
    #[error("Type {0} is imported cyclically!")]
    RecursiveTypeImport(String),
    #[error("Type {} does not exist in module {}", 0.0, 0.1)]
    NoSuchTypeInModule(CustomTypeKey),
    #[error("Function {1} in module {0} is private!")]
    FunctionIsPrivate(String, String),
}

pub fn compile_std(module_map: &mut ErrorModules) -> Result<Module, ReidError> {
    let (id, tokens) = parse_module(STD_SOURCE, STD_NAME, None, module_map, None)?;
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
    foreign_types: HashMap<SourceModuleId, HashMap<CustomTypeKey, SourceModuleId>>,
}

type LinkerPassState<'st, 'sc> = PassState<'st, 'sc, LinkerState, ErrorKind>;

#[derive(Clone, Debug)]
struct LinkerModule {
    module: Rc<RefCell<Module>>,
    // Functions imported directly from a module
    function_imports: HashMap<String, (SourceModuleId, Metadata)>,
    // Types imported either directly by the user or indirectly via functions.
    // May contain type-imports that are again recursively imported elsewhere.
    type_imports: HashMap<String, (SourceModuleId, Metadata)>,
}

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

        let mut modules = HashMap::<SourceModuleId, LinkerModule>::new();
        let mut module_ids = HashMap::<String, SourceModuleId>::new();

        for (mod_id, module) in context.modules.drain() {
            modules.insert(
                mod_id,
                LinkerModule {
                    module: Rc::new(RefCell::new(module)),
                    function_imports: HashMap::new(),
                    type_imports: HashMap::new(),
                },
            );
        }

        let mut module_queue: Vec<LinkerModule> = modules.values().cloned().collect();

        while let Some(mut importer) = module_queue.pop() {
            let importer_mod = importer.module.borrow_mut();

            // Gp go through all imports in this specific modulee
            for import in importer_mod.imports.clone() {
                let Import(path, _) = &import;
                if path.len() != 2 {
                    state.ok::<_, Infallible>(Err(ErrorKind::InnerModulesNotYetSupported(import.clone())), import.1);
                }

                // Cut the import statement into parts
                let Some((module_name, _)) = path.get(0) else {
                    continue;
                };
                let Some((import_name, _)) = path.get(1) else {
                    continue;
                };

                // Actually compile or fetch the imported module
                let imported = if let Some(mod_id) = module_ids.get(module_name) {
                    modules.get(mod_id).unwrap()
                } else if module_name == STD_NAME {
                    let std = compile_std(&mut self.module_map)?;
                    let module_id = std.module_id;
                    modules.insert(
                        std.module_id,
                        LinkerModule {
                            module: Rc::new(RefCell::new(std)),
                            function_imports: HashMap::new(),
                            type_imports: HashMap::new(),
                        },
                    );
                    module_ids.insert(module_name.clone(), module_id);
                    modules.get(&module_id).unwrap()
                } else {
                    let file_path = PathBuf::from(&context.base.clone()).join(module_name.to_owned() + ".reid");

                    let Ok(source) = fs::read_to_string(&file_path) else {
                        state.ok::<_, Infallible>(Err(ErrorKind::ModuleNotFound(module_name.clone())), import.1);
                        continue;
                    };

                    let (id, tokens) = match parse_module(
                        &source,
                        module_name.clone(),
                        Some(file_path.clone()),
                        &mut self.module_map,
                        None,
                    ) {
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
                                modules.insert(
                                    module_id,
                                    LinkerModule {
                                        module: Rc::new(RefCell::new(imported_module)),
                                        function_imports: HashMap::new(),
                                        type_imports: HashMap::new(),
                                    },
                                );
                                let imported = modules.get_mut(&module_id).unwrap();
                                module_queue.push(imported.clone());
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
                };

                let imported_module = imported.module.borrow();

                if let Some(func) = imported_module.functions.iter().find(|f| f.name == *import_name) {
                    // If the imported item is a function, add it to the list of imported functions
                    importer
                        .function_imports
                        .insert(func.name.clone(), (imported_module.module_id, import.1));
                } else if let Some(ty) = imported_module.typedefs.iter().find(|t| t.name == *import_name) {
                    // If the imported item is a type, add it to the list of imported types
                    // imported_types.insert((CustomTypeKey(ty.name.clone(), ty.source_module), true));
                    importer
                        .type_imports
                        .insert(ty.name.clone(), (imported_module.module_id, import.1));
                }
            }

            let module_id = importer_mod.module_id;
            drop(importer_mod);
            modules.insert(module_id, importer);
        }

        for (_, linker_module) in &modules {
            let mut importer_module = linker_module.module.borrow_mut();

            let mut unresolved_types = HashMap::new();

            // 1. Import functions and find all types that are dependencies of
            //    functions
            for (name, (function_source, import_meta)) in &linker_module.function_imports {
                let mut function_module = modules.get(&function_source).unwrap().module.borrow_mut();
                let func_module_name = function_module.name.clone();
                let func_module_id = function_module.module_id;

                let function = function_module.functions.iter_mut().find(|f| f.name == *name).unwrap();

                // If function is not pub, error
                if !function.is_pub {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::FunctionIsPrivate(func_module_name, function.name.clone())),
                        import_meta.clone(),
                    );
                    continue;
                }

                // If function already exists, error
                if let Some(existing) = importer_module.functions.iter().find(|f| f.name == *name) {
                    if let Err(e) = existing.equals_as_imported(&function) {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::FunctionImportIssue(func_module_name, name.clone(), e)),
                            import_meta.clone(),
                        );
                    }
                }

                function.is_imported = true;

                for ty in import_type(&function.return_type) {
                    unresolved_types.insert(ty, (import_meta.clone(), true));
                }
                for param in &function.parameters {
                    for ty in import_type(&param.ty) {
                        unresolved_types.insert(ty, (import_meta.clone(), true));
                    }
                }

                importer_module.functions.push(FunctionDefinition {
                    name: function.name.clone(),
                    linkage_name: None,
                    is_pub: false,
                    is_imported: false,
                    return_type: function.return_type.clone(),
                    parameters: function.parameters.clone(),
                    kind: super::FunctionDefinitionKind::Extern(true),
                    source: Some(func_module_id),
                    signature_meta: function.signature(),
                });
            }

            // 2. Add all manually imported types to the list of types that need
            //    to be resolved and recursed
            for (name, (source_module, meta)) in &linker_module.type_imports {
                let imported_ty_key = CustomTypeKey(name.clone(), source_module.clone());

                let imported_ty = TypeKind::CustomType(imported_ty_key.clone());
                let imported = modules.get(&imported_ty_key.1).unwrap().module.borrow();

                for (ty, func) in &imported.associated_functions {
                    if *ty != imported_ty {
                        continue;
                    }

                    for ty in import_type(&func.return_type) {
                        unresolved_types.insert(ty, (meta.clone(), true));
                    }
                    for param in &func.parameters {
                        for ty in import_type(&param.ty) {
                            unresolved_types.insert(ty, (meta.clone(), true));
                        }
                    }
                }

                unresolved_types.insert(imported_ty_key.clone(), (meta.clone(), false));
            }

            dbg!(&importer_module.module_id, &unresolved_types);

            // 3. Recurse these types to find their true sources, find their
            //    dependencies, and list them all. Store manually imported types
            //    in a separate mapping for later.
            let mut imported_types = HashMap::new();
            let mut foreign_keys = HashSet::new();

            let mut already_imported_binops = HashSet::new();

            for (ty, (meta, is_dependency)) in unresolved_types {
                // First deal with manually imported types
                if !is_dependency {
                    // Add them to the list of foreign types (types that are
                    // later replaced in-source by name)
                    let imported_ty_key = match resolve_type(&ty, &modules) {
                        Ok(ty) => {
                            foreign_keys.insert(ty.clone());
                            ty
                        }
                        Err(e) => {
                            state.note_errors(&vec![e], meta);
                            return Ok(());
                        }
                    };

                    match resolve_types_recursively(
                        &TypeKind::CustomType(CustomTypeKey(ty.0.clone(), importer_module.module_id)),
                        &modules,
                        HashSet::new(),
                    ) {
                        Ok(resolved) => {
                            imported_types.extend(resolved);
                        }
                        Err(e) => {
                            state.note_errors(&vec![e], meta);
                            return Ok(());
                        }
                    }

                    let mut imported = modules.get(&imported_ty_key.1).unwrap().module.borrow_mut();
                    let imported_module_name = imported.name.clone();
                    let imported_module_id = imported.module_id.clone();
                    let imported_ty = TypeKind::CustomType(imported_ty_key);

                    // Add all binary operators that are defined for this type
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

                    // Import all functions that are associated with this type
                    for (ty, func) in &mut imported.associated_functions {
                        if *ty != imported_ty {
                            continue;
                        }

                        let func_name = func.name.clone();

                        if !func.is_pub {
                            state.ok::<_, Infallible>(
                                Err(ErrorKind::FunctionIsPrivate(
                                    imported_module_name.clone(),
                                    func_name.clone(),
                                )),
                                meta.clone(),
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
                                        imported_module_name.clone(),
                                        func_name.clone(),
                                        e,
                                    )),
                                    meta.clone(),
                                );
                            }
                        }

                        importer_module.associated_functions.push((
                            ty.clone(),
                            FunctionDefinition {
                                name: func_name.clone(),
                                linkage_name: Some(format!("{}::{}", ty, func_name)),
                                is_pub: false,
                                is_imported: false,
                                return_type: func.return_type.clone(),
                                parameters: func.parameters.clone(),
                                kind: super::FunctionDefinitionKind::Extern(true),
                                source: Some(imported_module_id),
                                signature_meta: func.signature_meta,
                            },
                        ));
                    }
                }

                match resolve_types_recursively(&TypeKind::CustomType(ty.clone()), &modules, HashSet::new()) {
                    Ok(resolved) => {
                        imported_types.extend(resolved);
                    }
                    Err(e) => {
                        state.note_errors(&vec![e], meta);
                        return Ok(());
                    }
                }
                // let resolved = match resolve_type(&ty, &modules) {
                //     Ok(ty) => ty.clone(),
                //     Err(e) => {
                //         state.note_errors(&vec![e], meta);
                //         return Ok(());
                //     }
                // };
            }

            dbg!(&imported_types);

            // 4. Import all listed types.
            for (importer_typekey, imported_module_id) in &imported_types {
                let imported_ty_module = modules.get(&imported_module_id).unwrap().module.borrow();
                let importee_typekey = CustomTypeKey(importer_typekey.0.clone(), *imported_module_id);
                if let Some(typedef) = imported_ty_module
                    .typedefs
                    .iter()
                    .find(|ty| CustomTypeKey(ty.name.clone(), ty.source_module) == importee_typekey)
                    .cloned()
                {
                    importer_module.typedefs.push(TypeDefinition {
                        importer: Some(importer_typekey.1),
                        ..typedef
                    });
                }
            }

            state
                .scope
                .data
                .foreign_types
                .insert(importer_module.module_id, imported_types);
        }

        let mut modules: Vec<Module> = modules
            .into_values()
            .map(|v| Rc::into_inner(v.module).unwrap().into_inner())
            .collect();

        for module in modules.drain(..) {
            context.modules.insert(module.module_id, module);
        }

        Ok(())
    }

    fn module(&mut self, module: &mut Module, state: PassState<Self::Data, Self::TError>) -> PassResult {
        let foreign_types = &state.scope.data.foreign_types.get(&module.module_id);
        if let Some(foreign_types) = foreign_types {
            for ty in &mut module.typedefs {
                match &mut ty.kind {
                    TypeDefinitionKind::Struct(StructType(fields)) => {
                        for field in fields {
                            field.1 = field.1.update_imported(foreign_types);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn function(
        &mut self,
        function: &mut FunctionDefinition,
        state: PassState<Self::Data, Self::TError>,
    ) -> PassResult {
        let mod_id = state.scope.module_id.unwrap();
        let foreign_types = &state.scope.data.foreign_types.get(&mod_id);
        if let Some(foreign_types) = foreign_types {
            function.return_type = function.return_type.update_imported(*foreign_types);
            for param in function.parameters.iter_mut() {
                param.ty = param.ty.update_imported(foreign_types);
            }
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &mut super::Statement, state: PassState<Self::Data, Self::TError>) -> PassResult {
        let mod_id = state.scope.module_id.unwrap();
        let foreign_types = &state.scope.data.foreign_types.get(&mod_id);
        if let Some(foreign_types) = foreign_types {
            match &mut stmt.0 {
                super::StmtKind::Let(var_ref, _, _) => {
                    var_ref.0 = var_ref.0.update_imported(foreign_types);
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn expr(&mut self, expr: &mut super::Expression, state: PassState<Self::Data, Self::TError>) -> PassResult {
        let mod_id = state.scope.module_id.unwrap();
        let foreign_types = &state.scope.data.foreign_types.get(&mod_id);
        if let Some(foreign_types) = foreign_types {
            match &mut expr.0 {
                super::ExprKind::Variable(var_ref) => {
                    var_ref.0 = var_ref.0.update_imported(foreign_types);
                }
                super::ExprKind::Indexed(.., type_kind, _) => *type_kind = type_kind.update_imported(foreign_types),
                super::ExprKind::Accessed(.., type_kind, _, _) => *type_kind = type_kind.update_imported(foreign_types),
                super::ExprKind::BinOp(.., type_kind) => *type_kind = type_kind.update_imported(foreign_types),

                super::ExprKind::Borrow(..) => {}
                super::ExprKind::Deref(..) => {}
                super::ExprKind::CastTo(_, type_kind) => *type_kind = type_kind.update_imported(foreign_types),
                super::ExprKind::AssociatedFunctionCall(type_kind, fn_call) => {
                    *type_kind = type_kind.update_imported(foreign_types);
                    fn_call.return_type = fn_call.return_type.update_imported(foreign_types);
                }
                super::ExprKind::Struct(key, _) => {
                    *key = if let Some(mod_id) = foreign_types.get(&key) {
                        CustomTypeKey(key.0.clone(), *mod_id)
                    } else {
                        key.clone()
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}

impl TypeKind {
    fn update_imported(&self, foreign_types: &HashMap<CustomTypeKey, SourceModuleId>) -> TypeKind {
        match &self {
            TypeKind::Array(type_kind, len) => {
                TypeKind::Array(Box::new(type_kind.update_imported(foreign_types)), *len)
            }
            TypeKind::CustomType(custom_type_key) => {
                if let Some(mod_id) = foreign_types.get(&custom_type_key) {
                    TypeKind::CustomType(CustomTypeKey(custom_type_key.0.clone(), *mod_id))
                } else {
                    self.clone()
                }
            }
            TypeKind::Borrow(type_kind, mutable) => {
                TypeKind::Borrow(Box::new(type_kind.update_imported(foreign_types)), *mutable)
            }
            TypeKind::UserPtr(type_kind) => TypeKind::UserPtr(Box::new(type_kind.update_imported(foreign_types))),
            TypeKind::CodegenPtr(type_kind) => TypeKind::CodegenPtr(Box::new(type_kind.update_imported(foreign_types))),
            _ => self.clone(),
        }
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

fn resolve_type(
    ty: &CustomTypeKey,
    modules: &HashMap<SourceModuleId, LinkerModule>,
) -> Result<CustomTypeKey, ErrorKind> {
    let mut source_module_id = ty.1;
    let mut seen = HashSet::new();
    loop {
        seen.insert(source_module_id);
        let source_module = modules.get(&source_module_id).unwrap();
        if let Some((new_module_id, _)) = source_module.type_imports.get(&ty.0) {
            if seen.contains(new_module_id) {
                return Err(ErrorKind::RecursiveTypeImport(ty.0.clone()));
            }
            source_module_id = *new_module_id;
        } else {
            break;
        }
    }

    Ok(CustomTypeKey(ty.0.clone(), source_module_id))
}

fn resolve_types_recursively(
    ty: &TypeKind,
    modules: &HashMap<SourceModuleId, LinkerModule>,
    mut seen: HashSet<CustomTypeKey>,
) -> Result<HashMap<CustomTypeKey, SourceModuleId>, ErrorKind> {
    let mut types = HashMap::new();
    match ty {
        TypeKind::CustomType(type_key) => {
            let resolved_ty = resolve_type(type_key, modules)?;

            if seen.contains(&resolved_ty) {
                return Err(ErrorKind::CyclicalType(type_key.0.clone()));
            }

            if type_key.1 != resolved_ty.1 {
                types.insert(type_key.clone(), resolved_ty.1);
            }
            seen.insert(resolved_ty.clone());

            let resolved = modules
                .get(&resolved_ty.1)
                .unwrap()
                .module
                .borrow()
                .typedefs
                .iter()
                .find(|t| t.name == resolved_ty.0)
                .ok_or(ErrorKind::NoSuchTypeInModule(type_key.clone()))
                .cloned()?;
            match resolved.kind {
                TypeDefinitionKind::Struct(StructType(fields)) => {
                    for field in fields {
                        types.extend(resolve_types_recursively(&field.1, modules, seen.clone())?);
                    }
                }
            }
        }
        TypeKind::Array(type_kind, _) => types.extend(resolve_types_recursively(&type_kind, modules, seen.clone())?),
        TypeKind::Borrow(type_kind, _) => types.extend(resolve_types_recursively(&type_kind, modules, seen.clone())?),
        TypeKind::UserPtr(type_kind) => types.extend(resolve_types_recursively(&type_kind, modules, seen.clone())?),
        TypeKind::CodegenPtr(type_kind) => types.extend(resolve_types_recursively(&type_kind, modules, seen.clone())?),
        _ => {}
    }
    Ok(types)
}
