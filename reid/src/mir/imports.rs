use std::{collections::HashMap, convert::Infallible, fs, path::PathBuf};

use crate::{compile_module, ReidError};

use super::{
    pass::{Pass, PassState},
    types::EqualsIssue,
    Context, FunctionDefinition, Import, Module,
};

#[derive(thiserror::Error, Debug, Clone)]
pub enum ErrorKind {
    #[error("Unable to import inner modules, not yet supported: {0}")]
    InnerModulesNotYetSupported(Import),
    #[error("No such module: {0}")]
    ModuleNotFound(String),
    #[error("Error while compiling module {0}: {1}")]
    ModuleCompilationError(String, ReidError),
    #[error("No such function {0} found in module {1}")]
    NoSuchFunctionInModule(String, String),
    #[error("Importing function {0}::{1} not possible: {2}")]
    FunctionImportIssue(String, String, EqualsIssue),
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct ImportsPass;

impl Pass for ImportsPass {
    type TError = ErrorKind;
    fn context(&mut self, context: &mut Context, mut state: PassState<Self::TError>) {
        let mut modules = HashMap::<String, Module>::new();

        for module in context.modules.clone() {
            modules.insert(module.name.clone(), module);
        }

        let mut modules_to_process: Vec<Module> = modules.values().cloned().collect();

        while let Some(mut module) = modules_to_process.pop() {
            for import in &module.imports {
                let Import(path, _) = import;
                if path.len() != 2 {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::InnerModulesNotYetSupported(import.clone())),
                        import.1,
                    );
                }

                let module_name = unsafe { path.get_unchecked(0) };

                let imported = if let Some(module) = modules.get(module_name) {
                    module
                } else {
                    let file_path =
                        PathBuf::from(&context.base.clone()).join(module_name.to_owned() + ".reid");

                    dbg!(&file_path);
                    let Ok(source) = fs::read_to_string(&file_path) else {
                        state.ok::<_, Infallible>(
                            Err(ErrorKind::ModuleNotFound(module_name.clone())),
                            import.1,
                        );
                        continue;
                    };

                    match compile_module(&source, module_name.clone(), Some(file_path)) {
                        Ok(m) => {
                            let module_name = module.name.clone();
                            modules.insert(module_name.clone(), m);
                            modules_to_process.push(modules.get_mut(&module_name).unwrap().clone());
                            modules.get(&module_name).unwrap()
                        }
                        Err(err) => {
                            state.ok::<_, Infallible>(
                                Err(ErrorKind::ModuleCompilationError(module_name.clone(), err)),
                                import.1,
                            );
                            continue;
                        }
                    }
                };

                let func_name = unsafe { path.get_unchecked(1) };

                let Some(func) = imported.functions.iter().find(|f| f.name == *func_name) else {
                    state.ok::<_, Infallible>(
                        Err(ErrorKind::NoSuchFunctionInModule(
                            module_name.clone(),
                            func_name.clone(),
                        )),
                        import.1,
                    );
                    continue;
                };

                if let Some(existing) = module.functions.iter().find(|f| f.name == *func_name) {
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

                module.functions.push(FunctionDefinition {
                    name: func.name.clone(),
                    is_pub: false,
                    return_type: func.return_type.clone(),
                    parameters: func.parameters.clone(),
                    kind: super::FunctionDefinitionKind::Extern,
                });
            }

            modules.insert(module.name.clone(), module);
        }

        context.modules = modules.into_values().collect();
    }
}
