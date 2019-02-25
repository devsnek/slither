use crate::intrinsics::{
    create_array_prototype, create_boolean_prototype, create_float_prototype,
    create_function_prototype, create_object_prototype, create_promise, create_promise_prototype,
    create_string_prototype, create_symbol, create_symbol_prototype,
};
use crate::parser::{Node, Parser};
use crate::value::{new_error, Value};
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
    fn new(filename: &str, source: &str, agent: &Agent) -> Result<ModuleX, Value> {
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
                    Node::ImportStandardDeclaration(specifier, names) => {
                        match agent.builtins.get(specifier) {
                            Some(s) => {
                                for name in names {
                                    match s.get(name) {
                                        Some(v) => {
                                            module
                                                .context
                                                .borrow()
                                                .environment
                                                .borrow_mut()
                                                .create(name, false)?;
                                            module
                                                .context
                                                .borrow()
                                                .environment
                                                .borrow_mut()
                                                .initialize(name, v.clone())?;
                                        }
                                        None => return Err(new_error("unknown export")),
                                    }
                                }
                            }
                            None => return Err(new_error("unknown standard module")),
                        }
                    }
                    Node::ExportDeclaration(decl) => match *decl.clone() {
                        Node::LexicalInitialization(name, ..)
                        | Node::FunctionDeclaration(name, ..) => {
                            module
                                .context
                                .borrow()
                                .environment
                                .borrow_mut()
                                .create_export(name.as_str(), declarations[&name])?;
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
            evaluate_at(
                agent,
                &module.borrow().compiled,
                0,
                &mut stack,
                &mut scope,
                &mut vec![],
            )?;
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

type JobFn = fn(&Agent, Vec<Value>) -> Result<(), Value>;

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

pub enum MioMapType {
    Timer(mio::Registration, Value),
}

fn call_timer_job(agent: &Agent, args: Vec<Value>) -> Result<(), Value> {
    args[0].call(agent, Value::Null, Vec::new())?;
    Ok(())
}

pub struct Agent {
    pub intrinsics: Intrinsics,
    builtins: HashMap<String, HashMap<String, Value>>,
    modules: GcCell<HashMap<String, Module>>,
    pub root_env: Gc<GcCell<LexicalEnvironment>>,
    job_queue: GcCell<VecDeque<Job>>,
    pub mio: mio::Poll,
    pub mio_map: std::cell::RefCell<HashMap<mio::Token, MioMapType>>,
}

impl Default for Agent {
    fn default() -> Self {
        Self::new()
    }
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
            builtins: HashMap::new(),
            root_env: LexicalEnvironment::new(None),
            modules: GcCell::new(HashMap::new()),
            job_queue: GcCell::new(VecDeque::new()),
            mio: mio::Poll::new().expect("create mio poll failed"),
            mio_map: std::cell::RefCell::new(HashMap::new()),
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

        agent.builtins = crate::builtins::create(&agent);

        agent
    }

    fn load(&self, specifier: &str, referrer: &str) -> Result<Module, Value> {
        let filename = std::path::Path::new(referrer)
            .parent()
            .unwrap()
            .join(specifier);
        let filename = filename.to_str().unwrap();
        if !self.modules.borrow().contains_key(filename) {
            let source = std::fs::read_to_string(filename).expect("no such file");
            let module = Gc::new(GcCell::new(ModuleX::new(filename, source.as_str(), self)?));
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

    fn run_mio(&self, block: bool) -> bool {
        let mut events = mio::Events::with_capacity(1024);
        self.mio
            .poll(
                &mut events,
                if block {
                    None
                } else {
                    Some(std::time::Duration::from_millis(0))
                },
            )
            .expect("mio poll failed");
        for event in events.iter() {
            let data = self
                .mio_map
                .borrow_mut()
                .remove(&event.token())
                .expect("mio_map did not have event");
            match data {
                MioMapType::Timer(_r, callback) => {
                    self.enqueue_job(call_timer_job, vec![callback]);
                }
            }
        }
        true
    }

    pub fn run_jobs(&self) {
        self.run_mio(false);
        loop {
            loop {
                let job = self.job_queue.borrow_mut().pop_front();
                match job {
                    Some(Job(f, args)) => {
                        f(self, args).unwrap_or_else(|e: Value| {
                            eprintln!("Uncaught Exception: {}", e);
                            std::process::exit(1);
                        });
                    }
                    None => break,
                }
            }

            // job queue is empty

            if !self.run_mio(true) {
                break;
            }
        }
    }
}

macro_rules! test {
    ( $name:ident, $source:expr, $result:expr ) => {
        #[test]
        fn $name() {
            use num::BigInt;
            let agent = Agent::new();
            match ModuleX::new(stringify!(test_$name.sl), $source, &agent) {
                Err(e) => assert_eq!(Err::<Value, Value>(e), $result),
                Ok(module) => {
                    let mut stack = Vec::new();
                    let mut scope = vec![module.context.clone()];
                    let result = evaluate_at(
                        &agent,
                        &module.compiled,
                        0,
                        &mut stack,
                        &mut scope,
                        &mut vec![],
                    );
                    assert_eq!(result, $result);
                }
            }
        }
    };
}

test!(test_decl_return, "const a = 1;", Ok(Value::Null));
test!(
    test_decl_assign,
    "let a = 1.0; a += 1; a;",
    Ok(Value::Float(2.0))
);
test!(test_throw, "throw 5.0;", Err(Value::Float(5.0)));

test!(
    test_paren_expr,
    "const a = 1.0; (a);",
    Ok(Value::Float(1.0))
);
test!(
    test_arrow_expr,
    "const a = 1.0; ((a) => { return a; })(2.0);",
    Ok(Value::Float(2.0))
);

// TODO: figure out matching objects
// test!(test_arrow_expr_invalid_arg, "(1) => {};", Err(Value::Null));

test!(
    test_object_literal,
    r#"
    const obj = {
      a: 1.0,
    };
    const arr = [2.0];
    const f = {
      a: obj.a,
      b: arr[0],
    };
    f.a + f.b;
    "#,
    Ok(Value::Float(3.0))
);

test!(
    test_while_break,
    r#"
    let i = 0;
    while true {
      i += 1;
      if i > 5 {
        break;
      }
    }
    i;
    "#,
    Ok(Value::Integer(BigInt::from(6)))
);
