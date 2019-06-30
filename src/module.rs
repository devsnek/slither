use crate::interpreter::{Context, Interpreter, Scope};
use crate::parser::{Node, Parser};
use crate::{Agent, IntoValue, Value};
use gc::{Gc, GcCell};
use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone, Copy)]
enum ModuleStatus {
    Uninstantiated,
    Instantiating,
    Instantiated,
    Evaluating,
    Evaluated,
}

#[derive(Debug, Finalize)]
pub(crate) struct Module {
    pub(crate) filename: String,
    imports: HashSet<String>,
    pub(crate) context: Gc<GcCell<Context>>,
    status: ModuleStatus,
    dfs_index: u32,
    dfs_ancestor_index: u32,
    pub(crate) bytecode_position: usize,
}

unsafe impl gc::Trace for Module {
    custom_trace!(this, {
        mark(&this.context);
    });
}

impl Module {
    pub(crate) fn new(filename: &str, source: &str, agent: &mut Agent) -> Result<Module, Value> {
        let ast = match Parser::parse(&source) {
            Ok(v) => v,
            Err(e) => return Err(e.into_value(agent)),
        };

        let mut module = Module {
            filename: filename.to_string(),
            context: Context::new(Scope::new(Some(agent.root_scope.clone()))),
            imports: HashSet::new(),
            status: ModuleStatus::Uninstantiated,
            dfs_index: 0,
            dfs_ancestor_index: 0,
            bytecode_position: agent.assembler.assemble(&ast),
        };

        if let Node::Block(_scope, stmts, ..) = ast {
            for stmt in stmts {
                match stmt {
                    Node::ImportDefaultDeclaration(specifier, name, ..) => {
                        let mr = agent.load(&specifier, filename)?;
                        module
                            .context
                            .borrow()
                            .scope
                            .borrow_mut()
                            .create_import(&name, mr);
                        module.imports.insert(specifier);
                    }
                    Node::ImportNamedDeclaration(specifier, names, ..) => {
                        let mr = agent.load(&specifier, filename)?;
                        for name in names {
                            module
                                .context
                                .borrow()
                                .scope
                                .borrow_mut()
                                .create_import(&name, mr.clone());
                        }
                        module.imports.insert(specifier);
                    }
                    Node::ImportStandardDeclaration(specifier, names, ..) => {
                        match agent.builtins.get(&specifier) {
                            Some(s) => {
                                for name in names {
                                    match s.get(&name) {
                                        Some(v) => {
                                            let ctx = module.context.borrow();
                                            let mut scope = ctx.scope.borrow_mut();
                                            scope.create(agent, &name, false)?;
                                            scope.initialize(&name, v.clone());
                                        }
                                        None => {
                                            return Err(Value::new_error(agent, "unknown export"));
                                        }
                                    }
                                }
                            }
                            None => return Err(Value::new_error(agent, "unknown standard module")),
                        }
                    }
                    Node::ExportDeclaration(..) => {}
                    _ => {}
                }
            }
        } else {
            unreachable!();
        }

        Ok(module)
    }

    pub(crate) fn instantiate(agent: &mut Agent, module: Gc<GcCell<Module>>) -> Result<(), Value> {
        inner_module_instantiation(agent, module, &mut Vec::new(), 0)?;
        Ok(())
    }

    pub(crate) fn evaluate(agent: &mut Agent, module: Gc<GcCell<Module>>) -> Result<(), Value> {
        inner_module_evaluation(agent, module, &mut Vec::new(), 0)?;
        Ok(())
    }
}

fn inner_module_instantiation(
    agent: &mut Agent,
    module: Gc<GcCell<Module>>,
    stack: &mut Vec<Gc<GcCell<Module>>>,
    mut index: u32,
) -> Result<u32, Value> {
    let status = module.borrow().status;
    match status {
        ModuleStatus::Instantiating | ModuleStatus::Instantiated | ModuleStatus::Evaluated => {
            Ok(index)
        }
        ModuleStatus::Uninstantiated => {
            {
                let mut module = module.borrow_mut();
                module.status = ModuleStatus::Instantiating;
                module.dfs_index = index;
                module.dfs_ancestor_index = index;
            }
            index += 1;
            stack.push(module.clone());
            for import in &module.borrow().imports {
                let m = agent.load(import.as_str(), module.borrow().filename.as_str())?;
                index = inner_module_instantiation(agent, m.clone(), stack, index)?;
                if m.borrow().status == ModuleStatus::Instantiating {
                    let mut module = module.borrow_mut();
                    module.dfs_ancestor_index =
                        std::cmp::min(module.dfs_ancestor_index, m.borrow().dfs_ancestor_index);
                }
            }
            if module.borrow().dfs_ancestor_index == module.borrow().dfs_index {
                loop {
                    let m = stack.pop().unwrap();
                    m.borrow_mut().status = ModuleStatus::Instantiated;
                    if m.borrow().filename == module.borrow().filename {
                        break;
                    }
                }
            }
            Ok(index)
        }
        ModuleStatus::Evaluating => unreachable!(),
    }
}

fn inner_module_evaluation(
    agent: &mut Agent,
    module: Gc<GcCell<Module>>,
    stack: &mut Vec<Gc<GcCell<Module>>>,
    mut index: u32,
) -> Result<u32, Value> {
    let status = module.borrow().status;
    match status {
        ModuleStatus::Evaluated | ModuleStatus::Evaluating => Ok(index),
        ModuleStatus::Instantiated => {
            {
                let mut module = module.borrow_mut();
                module.status = ModuleStatus::Evaluating;
                module.dfs_index = index;
                module.dfs_ancestor_index = index;
            }
            index += 1;
            stack.push(module.clone());
            for import in &module.borrow().imports {
                let m = agent.load(import.as_str(), module.borrow().filename.as_str())?;
                index = inner_module_evaluation(agent, m.clone(), stack, index)?;
                if m.borrow().status == ModuleStatus::Evaluating {
                    let mut module = module.borrow_mut();
                    module.dfs_ancestor_index =
                        std::cmp::min(module.dfs_ancestor_index, m.borrow().dfs_ancestor_index);
                }
            }
            {
                let module = module.borrow();
                let mut interpreter =
                    Interpreter::new(module.bytecode_position, module.context.clone());
                interpreter.run(agent).unwrap()?;
            }
            if module.borrow().dfs_ancestor_index == module.borrow().dfs_index {
                loop {
                    let m = stack.pop().unwrap();
                    m.borrow_mut().status = ModuleStatus::Evaluated;
                    if m.borrow().filename == module.borrow().filename {
                        break;
                    }
                }
            }
            Ok(index)
        }
        ModuleStatus::Uninstantiated | ModuleStatus::Instantiating => unreachable!(),
    }
}
