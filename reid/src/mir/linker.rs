use std::{
    cell::RefCell,
    collections::HashMap,
    convert::Infallible,
    fs::{self},
    path::PathBuf,
    rc::Rc,
};

use crate::{compile_module, error_raporting::ModuleMap, lexer::FullToken, parse_module};

use super::{
    pass::{Pass, PassState},
    r#impl::EqualsIssue,
    Context, FunctionDefinition, Import, Metadata, Module, SourceModuleId,
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

pub fn compile_std(module_map: &mut ModuleMap) -> (super::Module, Vec<FullToken>) {
    let (id, tokens) = parse_module(STD_SOURCE, "standard_library", module_map).unwrap();
    let module = compile_module(id, &tokens, module_map, None, false).unwrap();

    let mut mir_context = super::Context::from(vec![module], Default::default());

    let std_compiled = mir_context.modules.remove(0);
    (std_compiled, tokens)
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
    fn context(&mut self, context: &mut Context, mut state: LinkerPassState) {
        let mains = context
            .modules
            .iter()
            .filter(|m| m.is_main)
            .collect::<Vec<_>>();
        if mains.len() > 1 {
            state.note_errors(&vec![ErrorKind::MultipleMainsAtStart], Metadata::default());
            return;
        }
        let Some(main) = mains.first() else {
            state.note_errors(&vec![ErrorKind::NoMainDefined], Metadata::default());
            return;
        };

        let Some(_) = main.functions.iter().find(|f| f.name == "main") else {
            state.note_errors(&vec![ErrorKind::NoMainFunction], Metadata::default());
            return;
        };

        let mut modules = HashMap::<String, Rc<RefCell<_>>>::new();

        for module in context.modules.drain(..) {
            let tokens = self
                .module_map
                .get_module(&module.module_id)
                .unwrap()
                .tokens
                .clone()
                .unwrap();
            modules.insert(module.name.clone(), Rc::new(RefCell::new((module, tokens))));
        }

        modules.insert(
            "std".to_owned(),
            Rc::new(RefCell::new(compile_std(&mut self.module_map))),
        );

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

                importer_module.0.functions.push(FunctionDefinition {
                    name: func.name.clone(),
                    is_pub: false,
                    is_imported: false,
                    return_type: func.return_type.clone(),
                    parameters: func.parameters.clone(),
                    kind: super::FunctionDefinitionKind::Extern(true),
                });
            }
        }

        context.modules = modules
            .into_values()
            .map(|v| Rc::into_inner(v).unwrap().into_inner().0)
            .collect();
    }
}
