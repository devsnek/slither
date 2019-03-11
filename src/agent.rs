use crate::intrinsics::{
    create_array_prototype, create_async_iterator_prototype, create_boolean_prototype,
    create_function_prototype, create_generator_prototype, create_iterator_prototype,
    create_net_client_prototype, create_number_prototype, create_object_prototype, create_promise,
    create_promise_prototype, create_regex_prototype, create_string_prototype, create_symbol,
    create_symbol_prototype,
};
use crate::parser::{Node, Parser};
use crate::value::{new_error, Value};
use crate::vm::{compile, Evaluator, ExecutionContext, LexicalEnvironment};
use gc::{Gc, GcCell};
use num_cpus;
use rust_decimal::Decimal;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use threadpool::ThreadPool;

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
    status: ModuleStatus,
    dfs_index: u32,
    dfs_ancestor_index: u32,
    compiled: (usize, usize),
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
    fn new(filename: &str, source: &str, agent: &mut Agent) -> Result<ModuleX, Value> {
        let ast = Parser::parse(&source)?;

        let mut module = ModuleX {
            filename: filename.to_string(),
            context: ExecutionContext::new(LexicalEnvironment::new(Some(agent.root_env.clone()))),
            imports: HashSet::new(),
            status: ModuleStatus::Uninstantiated,
            dfs_index: 0,
            dfs_ancestor_index: 0,
            compiled: compile(agent, &ast).unwrap(),
        };

        if let Node::BlockStatement(nodes, declarations, ..) = ast {
            for node in nodes {
                match &node {
                    Node::ImportDefaultDeclaration(specifier, name) => {
                        let mr = agent.load(specifier, filename)?;
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
                                            let ctx = module.context.borrow();
                                            let mut env = ctx.environment.borrow_mut();
                                            env.create(name, false)?;
                                            env.initialize(name, v.clone());
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
    agent: &mut Agent,
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
    agent: &mut Agent,
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
            let mut evaluator = Evaluator::new(module.borrow().compiled);
            evaluator.scope.push(module.borrow_mut().context.clone());
            evaluator.run(agent).unwrap()?;
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
    pub number_prototype: Value,
    pub promise_prototype: Value,
    pub promise: Value,
    pub symbol_prototype: Value,
    pub symbol: Value,
    pub regex_prototype: Value,
    pub iterator_prototype: Value,
    pub generator_prototype: Value,
    pub async_iterator_prototype: Value,
    pub net_client_prototype: Value,
}

#[derive(Debug)]
pub enum MioMapType {
    Timer(mio::Registration, Value),
    FS(mio::Registration, Value),
    Net(crate::builtins::net::Net),
}

fn call_timer_job(agent: &Agent, args: Vec<Value>) -> Result<(), Value> {
    args[0].call(agent, Value::Null, Vec::new())?;
    Ok(())
}

pub struct Agent {
    pub intrinsics: Intrinsics,
    well_known_symbols: RefCell<HashMap<String, Value>>,
    builtins: HashMap<String, HashMap<String, Value>>,
    modules: GcCell<HashMap<String, Module>>,
    pub root_env: Gc<GcCell<LexicalEnvironment>>,
    job_queue: GcCell<VecDeque<Job>>,
    pub mio: mio::Poll,
    pub mio_map: RefCell<HashMap<mio::Token, MioMapType>>,
    pub pool: ThreadPool,
    pub code: Vec<u8>,
    pub string_table: Vec<String>,
    pub number_table: Vec<Decimal>,
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
        let string_prototype = create_string_prototype(object_prototype.clone());
        let symbol_prototype = create_symbol_prototype(object_prototype.clone());
        let mut agent = Agent {
            intrinsics: Intrinsics {
                object_prototype: object_prototype.clone(),
                array_prototype,
                function_prototype,
                boolean_prototype: Value::Null,
                number_prototype: Value::Null,
                string_prototype,
                promise_prototype: Value::Null,
                promise: Value::Null,
                symbol_prototype,
                symbol: Value::Null,
                regex_prototype: Value::Null,
                iterator_prototype: Value::Null,
                generator_prototype: Value::Null,
                async_iterator_prototype: Value::Null,
                net_client_prototype: Value::Null,
            },
            well_known_symbols: RefCell::new(HashMap::new()),
            builtins: HashMap::new(),
            root_env: LexicalEnvironment::new(None),
            modules: GcCell::new(HashMap::new()),
            job_queue: GcCell::new(VecDeque::new()),
            mio: mio::Poll::new().expect("create mio poll failed"),
            mio_map: RefCell::new(HashMap::new()),
            pool: ThreadPool::new(num_cpus::get()),
            code: Vec::new(),
            string_table: Vec::new(),
            number_table: Vec::new(),
        };

        agent.intrinsics.boolean_prototype = create_boolean_prototype(&agent);
        agent.intrinsics.number_prototype = create_number_prototype(&agent);
        agent.intrinsics.regex_prototype = create_regex_prototype(&agent);
        agent.intrinsics.symbol = create_symbol(&agent);
        agent.intrinsics.iterator_prototype = create_iterator_prototype(&agent);
        agent.intrinsics.async_iterator_prototype = create_async_iterator_prototype(&agent);
        agent.intrinsics.generator_prototype = create_generator_prototype(&agent);

        agent.intrinsics.promise_prototype = create_promise_prototype(&agent);
        agent.intrinsics.promise = create_promise(&agent);

        agent.intrinsics.net_client_prototype = create_net_client_prototype(&agent);

        {
            let mut env = agent.root_env.borrow_mut();
            env.create("Promise", true).unwrap();
            env.initialize("Promise", agent.intrinsics.promise.clone());

            env.create("Symbol", true).unwrap();
            env.initialize("Symbol", agent.intrinsics.symbol.clone());
        }

        agent.builtins = crate::builtins::create(&agent);

        agent
    }

    fn resolve(&self, specifier: &str, referrer: &str) -> std::io::Result<String> {
        let filename = std::path::Path::new(referrer)
            .parent()
            .unwrap()
            .join(specifier)
            .to_str()
            .unwrap()
            .to_string();
        match std::fs::metadata(&filename) {
            Ok(ref r) if r.is_file() => Ok(filename),
            Ok(_) => {
                let r = filename + "/module.sl";
                match std::fs::metadata(&r) {
                    Ok(_) => Ok(r),
                    Err(e) => Err(e),
                }
            }
            Err(_) => {
                let r = filename + ".sl";
                match std::fs::metadata(&r) {
                    Ok(_) => Ok(r),
                    Err(e) => Err(e),
                }
            }
        }
    }

    fn load(&mut self, specifier: &str, referrer: &str) -> Result<Module, Value> {
        let filename = self.resolve(specifier, referrer).unwrap();
        if !self.modules.borrow().contains_key(&filename) {
            let source = std::fs::read_to_string(&filename).expect("no such file");
            let module = Gc::new(GcCell::new(ModuleX::new(filename.as_str(), source.as_str(), self)?));
            self.modules
                .borrow_mut()
                .insert(filename.to_string(), module.clone());
            Ok(module)
        } else {
            let map = self.modules.borrow();
            let module = map.get(&filename).unwrap().clone();
            Ok(module)
        }
    }

    pub fn import(&mut self, specifier: &str, referrer: &str) -> Result<(), Value> {
        let module = self.load(specifier, referrer)?;
        inner_module_instantiation(self, module.clone(), &mut Vec::new(), 0)?;
        inner_module_evaluation(self, module.clone(), &mut Vec::new(), 0)?;
        Ok(())
    }

    pub fn well_known_symbol(&self, name: &str) -> Value {
        let mut wks = self.well_known_symbols.borrow_mut();
        match wks.get(name) {
            Some(v) => v.clone(),
            None => {
                let v = Value::new_symbol(Some(name.to_string()));
                wks.insert(name.to_string(), v.clone());
                v
            }
        }
    }

    pub fn enqueue_job(&self, f: JobFn, args: Vec<Value>) {
        self.job_queue.borrow_mut().push_back(Job(f, args));
    }

    pub fn run_jobs(&self) {
        let mut events = mio::Events::with_capacity(128);
        loop {
            self.mio
                .poll(&mut events, Some(std::time::Duration::from_millis(0)))
                .expect("mio poll failed");
            for event in events.iter() {
                let entry = self
                    .mio_map
                    .borrow_mut()
                    .remove(&event.token())
                    .expect("mio map was missing entry for event");
                match entry {
                    MioMapType::Timer(_, callback) => {
                        self.enqueue_job(call_timer_job, vec![callback]);
                    }
                    MioMapType::FS(_, promise) => {
                        crate::builtins::fs::handle(self, event.token(), promise);
                    }
                    MioMapType::Net(n) => {
                        crate::builtins::net::handle(self, event.token(), n);
                    }
                }
            }

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

            if self.mio_map.borrow().is_empty() {
                break;
            }
        }
    }
}

macro_rules! test {
    ( $name:ident, $source:expr, $result:expr ) => {
        #[test]
        fn $name() {
            let mut agent = Agent::new();
            match ModuleX::new(stringify!(test_$name.sl), $source, &mut agent) {
                Err(e) => assert_eq!(Err::<Value, Value>(e), $result),
                Ok(module) => {
                    let mut evaluator = Evaluator::new(module.compiled);
                    evaluator.scope.push(module.context.clone());
                    let mut result = evaluator.run(&agent).unwrap();
                    if let Ok(value) = &result {
                        agent.run_jobs();
                        if value.has_slot("promise state") {
                            if value.get_slot("promise state")
                                == Value::String("fulfilled".to_string())
                            {
                                result = Ok(value.get_slot("result"));
                            } else {
                                result = Err(value.get_slot("result"));
                            }
                        }
                    }
                    assert_eq!(result, $result);
                }
            }
        }
    };
}

test!(test_decl_return, "const a = 1;", Ok(Value::Null));
test!(
    test_decl_assign,
    "let a = 1; a += 1; a;",
    Ok(Value::Number(2.into()))
);
test!(test_throw, "throw 5.0;", Err(Value::Number(5.into())));

test!(
    test_paren_expr,
    "const a = 1; (a);",
    Ok(Value::Number(1.into()))
);
test!(
    test_arrow_expr,
    "const a = 1; ((a) => { return a; })(2);",
    Ok(Value::Number(2.into()))
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
    Ok(Value::Number(3.into()))
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
    Ok(Value::Number(6.into()))
);

test!(
    test_well_known_symbols,
    r#"
    const x = :wk;
    const y = { [x]: 5 };
    y[:wk];
    "#,
    Ok(Value::Number(5.into()))
);

test!(
    test_regex,
    r#"
    const re = /ab/;
    const t1 = re.test('ab');
    const t2 = re.test('ba');
    t1 && !t2;
    "#,
    Ok(Value::True)
);

test!(
    test_async,
    r#"
    const a = async () => {
      return 5;
    };
    async function b() {
      const av = await a();
      return av + 5;
    }
    b();
    "#,
    Ok(Value::Number(10.into()))
);

test!(
    test_for_loop,
    r#"
    gen function numbers() {
      let i = 0;
      while i < 10 {
        yield i;
        i += 1;
      }
    }

    let i = 0;
    for item in numbers() {
      i += item;
    }
    i;
    "#,
    Ok(Value::Number(45.into()))
);
