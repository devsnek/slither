use crate::intrinsics::{
    create_array_prototype, create_boolean_prototype, create_float_prototype,
    create_function_prototype, create_object_prototype, create_promise, create_promise_prototype,
    create_string_prototype, create_symbol, create_symbol_prototype,
};
use crate::parser::{Node, Parser};
use crate::value::{new_builtin_function, new_error, Symbol, Value};
use crate::vm::{evaluate_at, Compiled, Compiler, ExecutionContext, LexicalEnvironment};
use gc::{Gc, GcCell};
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(PartialEq, Clone, Trace, Finalize, Debug)]
enum ModuleStatus {
    Uninstantiated,
    Instantiating,
    Instantiated,
    Evaluating,
    Evaluated,
}

#[derive(Finalize, Debug)]
pub struct ModuleX {
    filename: String,
    pub context: Gc<GcCell<ExecutionContext>>,
    imports: HashSet<String>,
    compiled: Compiled,
    status: ModuleStatus,
    dfs_index: u32,
    dfs_ancestor_index: u32,
}

impl PartialEq for ModuleX {
    fn eq(&self, other: &Self) -> bool {
        self.filename == other.filename
    }
}

unsafe impl gc::Trace for ModuleX {
    #[inline]
    unsafe fn trace(&self) {
        gc::Trace::trace(&self.context);
    }
    #[inline]
    unsafe fn root(&self) {
        gc::Trace::root(&self.context);
    }
    #[inline]
    unsafe fn unroot(&self) {
        gc::Trace::unroot(&self.context);
    }
    #[inline]
    fn finalize_glue(&self) {
        gc::Finalize::finalize(self);
        gc::Trace::finalize_glue(&self.context);
    }
}

pub type Module = Gc<GcCell<ModuleX>>;

impl ModuleX {
    fn new(filename: &str, agent: &Agent) -> Result<ModuleX, Value> {
        let source = std::fs::read_to_string(filename).expect("no such file");
        let ast = Parser::parse(&source)?;

        let mut module = ModuleX {
            filename: filename.to_string(),
            context: ExecutionContext::new(LexicalEnvironment::new(Some(agent.root_env.clone()))),
            compiled: Compiler::go(&ast).unwrap(),
            imports: HashSet::new(),
            status: ModuleStatus::Uninstantiated,
            dfs_index: 0,
            dfs_ancestor_index: 0,
        };

        if let Node::BlockStatement(nodes, declarations) = ast {
            for node in nodes {
                match &node {
                    Node::ImportDefaultDeclaration(specifier, name) => {
                        let mr = agent.load(specifier, name)?;
                        module
                            .context
                            .borrow()
                            .environment
                            .borrow_mut()
                            .create_import(name, mr)?;
                        module.imports.insert(specifier.to_string());
                    }
                    Node::ImportNamedDeclaration(specifier, names) => {
                        let mr = agent.load(specifier, filename)?;
                        for name in names {
                            module
                                .context
                                .borrow()
                                .environment
                                .borrow_mut()
                                .create_import(name, mr.clone())?;
                            module.imports.insert(specifier.to_string());
                        }
                    }
                    Node::ImportStandardDeclaration(specifier, names) => match specifier.as_str() {
                        "debug" => {
                            if names.len() != 1 || names[0] != "print" {
                                return Err(new_error("unknown item from debug"));
                            }
                            module
                                .context
                                .borrow()
                                .environment
                                .borrow_mut()
                                .create("print", false)?;
                            module
                                .context
                                .borrow()
                                .environment
                                .borrow_mut()
                                .initialize("print", new_builtin_function(agent, print))?;
                        }
                        _ => return Err(new_error("unknown standard module")),
                    },
                    Node::ExportDeclaration(decl) => match *decl.clone() {
                        Node::LexicalInitialization(name, ..)
                        | Node::FunctionDeclaration(name, ..) => {
                            module
                                .context
                                .borrow()
                                .environment
                                .borrow_mut()
                                .create_export(name.as_str(), *declarations.get(&name).unwrap())?;
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }

        Ok(module)
    }
}

fn inner_module_instantiation(
    agent: &Agent,
    module: Module,
    stack: &mut Vec<Module>,
    mut index: u32,
) -> Result<u32, Value> {
    let status = module.borrow().status.clone();
    match status {
        ModuleStatus::Instantiating | ModuleStatus::Instantiated | ModuleStatus::Evaluated => {
            Ok(index)
        }
        _ => {
            assert!(module.borrow().status == ModuleStatus::Uninstantiated);
            module.borrow_mut().status = ModuleStatus::Instantiating;
            module.borrow_mut().dfs_index = index;
            module.borrow_mut().dfs_ancestor_index = index;
            index += 1;
            stack.push(module.clone());
            for import in module.borrow().imports.clone() {
                let m = agent.load(import.as_str(), module.borrow().filename.as_str())?;
                index = inner_module_instantiation(agent, m.clone(), stack, index)?;
                if m.borrow().status == ModuleStatus::Instantiating {
                    module.borrow_mut().dfs_ancestor_index = std::cmp::min(
                        module.borrow().dfs_ancestor_index,
                        m.borrow().dfs_ancestor_index,
                    );
                }
            }
            if module.borrow().dfs_ancestor_index == module.borrow().dfs_index {
                let mut done = false;
                while !done {
                    let m = stack.pop().unwrap();
                    m.borrow_mut().status = ModuleStatus::Instantiated;
                    if m.borrow().filename == module.borrow().filename {
                        done = true;
                    }
                }
            }
            Ok(index)
        }
    }
}

fn inner_module_evaluation(
    agent: &Agent,
    module: Module,
    stack: &mut Vec<Module>,
    mut index: u32,
) -> Result<u32, Value> {
    let status = module.borrow().status.clone();
    if status == ModuleStatus::Evaluated || status == ModuleStatus::Evaluating {
        Ok(index)
    } else {
        assert!(module.borrow().status == ModuleStatus::Instantiated);
        module.borrow_mut().status = ModuleStatus::Evaluating;
        module.borrow_mut().dfs_index = index;
        module.borrow_mut().dfs_ancestor_index = index;
        index += 1;
        stack.push(module.clone());
        for import in module.borrow().imports.clone() {
            let m = agent.load(import.as_str(), module.borrow().filename.as_str())?;
            index = inner_module_evaluation(agent, m.clone(), stack, index)?;
            if m.borrow().status == ModuleStatus::Evaluating {
                m.borrow_mut().dfs_ancestor_index = std::cmp::min(
                    module.borrow().dfs_ancestor_index,
                    m.borrow().dfs_ancestor_index,
                );
            }
        }
        {
            let mut stack = Vec::new();
            let mut scope = vec![module.borrow_mut().context.clone()];
            evaluate_at(agent, &module.borrow().compiled, 0, &mut stack, &mut scope, &mut vec![])?;
        }
        if module.borrow().dfs_ancestor_index == module.borrow().dfs_index {
            let mut done = false;
            while !done {
                let m = stack.pop().unwrap();
                m.borrow_mut().status = ModuleStatus::Evaluated;
                if m.borrow().filename == module.borrow().filename {
                    done = true;
                }
            }
        }
        Ok(index)
    }
}

type JobFn = fn(&Agent, Vec<Value>);

#[derive(Finalize)]
struct Job(JobFn, Vec<Value>);

unsafe impl gc::Trace for Job {
    #[inline]
    unsafe fn trace(&self) {}
    #[inline]
    unsafe fn root(&self) {}
    #[inline]
    unsafe fn unroot(&self) {}
    #[inline]
    fn finalize_glue(&self) {}
}

fn print(_: &Agent, _: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let mut output = String::new();
    for arg in args {
        match &arg {
            Value::Null => output += " null",
            Value::True => output += " true",
            Value::False => output += " false",
            Value::Float(n) => output += &format!(" {}f", n),
            Value::Integer(n) => output += &format!(" {}i", n),
            Value::String(s) => output += &format!(" '{}'", s),
            Value::Symbol(Symbol(_, _, d)) => {
                if let Some(s) = d {
                    output += &format!(" Symbol({})", s);
                } else {
                    output += " Symbol()";
                }
            }
            Value::Object(_) => output += " {...}",
            _ => unreachable!(),
        }
    }
    println!("{}", output.trim());
    Ok(Value::Null)
}

pub struct Intrinsics {
    pub object_prototype: Value,
    pub array_prototype: Value,
    pub function_prototype: Value,
    pub boolean_prototype: Value,
    pub string_prototype: Value,
    pub float_prototype: Value,
    pub promise_prototype: Value,
    pub promise: Value,
    pub symbol_prototype: Value,
    pub symbol: Value,
}

pub struct Agent {
    pub intrinsics: Intrinsics,
    modules: GcCell<HashMap<String, Module>>,
    pub root_env: Gc<GcCell<LexicalEnvironment>>,
    job_queue: GcCell<VecDeque<Job>>,
}

impl Agent {
    pub fn new() -> Agent {
        let object_prototype = create_object_prototype();
        let array_prototype = create_array_prototype(object_prototype.clone());
        let function_prototype = create_function_prototype(object_prototype.clone());
        let boolean_prototype = create_boolean_prototype(object_prototype.clone());
        let float_prototype = create_float_prototype(object_prototype.clone());
        let string_prototype = create_string_prototype(object_prototype.clone());
        let symbol_prototype = create_symbol_prototype(object_prototype.clone());
        let mut agent = Agent {
            intrinsics: Intrinsics {
                object_prototype: object_prototype.clone(),
                array_prototype,
                function_prototype,
                boolean_prototype,
                float_prototype,
                string_prototype,
                promise_prototype: Value::Null,
                promise: Value::Null,
                symbol_prototype,
                symbol: Value::Null,
            },
            root_env: LexicalEnvironment::new(None),
            modules: GcCell::new(HashMap::new()),
            job_queue: GcCell::new(VecDeque::new()),
        };

        agent.intrinsics.promise_prototype =
            create_promise_prototype(&agent, object_prototype.clone());
        agent.intrinsics.promise =
            create_promise(&agent, agent.intrinsics.promise_prototype.clone());

        agent.intrinsics.symbol = create_symbol(&agent, agent.intrinsics.symbol_prototype.clone());

        {
            let mut env = agent.root_env.borrow_mut();
            env.create("Promise", true).unwrap();
            env.initialize("Promise", agent.intrinsics.promise.clone())
                .unwrap();

            env.create("Symbol", true).unwrap();
            env.initialize("Symbol", agent.intrinsics.symbol.clone())
                .unwrap();
        }

        agent
    }

    fn load(&self, specifier: &str, referrer: &str) -> Result<Module, Value> {
        let filename = std::path::Path::new(referrer)
            .parent()
            .unwrap()
            .join(specifier);
        let filename = filename.to_str().unwrap();
        if !self.modules.borrow().contains_key(filename) {
            let module = Gc::new(GcCell::new(ModuleX::new(filename, self)?));
            // let imports = module.imports.clone();
            self.modules
                .borrow_mut()
                .insert(filename.to_string(), module.clone());
            Ok(module)
        } else {
            let map = self.modules.borrow();
            let module = map.get(filename).unwrap().clone();
            Ok(module)
        }
    }

    pub fn import(&self, specifier: &str, referrer: &str) -> Result<(), Value> {
        let module = self.load(specifier, referrer)?;
        inner_module_instantiation(self, module.clone(), &mut Vec::new(), 0)?;
        inner_module_evaluation(self, module.clone(), &mut Vec::new(), 0)?;
        Ok(())
    }

    pub fn enqueue_job(&self, f: JobFn, args: Vec<Value>) {
        self.job_queue.borrow_mut().push_back(Job(f, args));
    }

    pub fn run_jobs(&self) {
        loop {
            let mut job = self.job_queue.borrow_mut().pop_front();
            match &mut job {
                Some(Job(f, args)) => {
                    f(self, std::mem::replace(args, Vec::new()));
                }
                None => break,
            }
        }
    }
}
