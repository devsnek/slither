use crate::interpreter::{Assembler, Interpreter, Scope};
use crate::intrinsics::{
    create_array_iterator_prototype, create_array_prototype, create_async_iterator_prototype,
    create_boolean_prototype, create_error_prototype, create_function_prototype,
    create_generator_prototype, create_iterator_map_prototype, create_iterator_prototype,
    create_net_client_prototype, create_net_server_prototype, create_number_prototype,
    create_object_prototype, create_promise, create_promise_prototype, create_regex_prototype,
    create_string_prototype, create_symbol, create_symbol_prototype,
    create_timer_iterator_prototype,
};
use crate::module::Module;
use crate::Value;
use gc::{Gc, GcCell};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, VecDeque};
use threadpool::ThreadPool;

#[derive(Trace, Finalize)]
pub(crate) struct Intrinsics {
    pub(crate) object_prototype: Value,
    pub(crate) array_prototype: Value,
    pub(crate) array_iterator_prototype: Value,
    pub(crate) function_prototype: Value,
    pub(crate) boolean_prototype: Value,
    pub(crate) string_prototype: Value,
    pub(crate) number_prototype: Value,
    pub(crate) promise_prototype: Value,
    pub(crate) promise: Value,
    pub(crate) symbol_prototype: Value,
    pub(crate) symbol: Value,
    pub(crate) regex_prototype: Value,
    pub(crate) iterator_prototype: Value,
    pub(crate) iterator_map_prototype: Value,
    pub(crate) generator_prototype: Value,
    pub(crate) async_iterator_prototype: Value,
    pub(crate) net_client_prototype: Value,
    pub(crate) net_server_prototype: Value,
    pub(crate) error_prototype: Value,
    pub(crate) timer_iterator_prototype: Value,
}

type JobFn = fn(&Agent, Vec<Value>) -> Result<(), Value>;
#[derive(Finalize)]
struct Job(JobFn, Vec<Value>);

unsafe impl gc::Trace for Job {
    custom_trace!(this, {
        mark(&this.1);
    });
}

#[derive(Debug, Finalize)]
pub(crate) enum MioMapType {
    Timer(crate::builtins::timers::Timer),
    FS(mio::Registration, Value),
    Net(crate::builtins::net::Net),
}

unsafe impl gc::Trace for MioMapType {
    custom_trace!(this, {
        match this {
            MioMapType::Timer(v) => mark(v),
            MioMapType::FS(_, v) => mark(v),
            MioMapType::Net(v) => mark(v),
        }
    });
}

#[derive(Finalize)]
pub struct Agent {
    pub assembler: Assembler,
    pub(crate) intrinsics: Intrinsics,
    pub(crate) builtins: HashMap<String, HashMap<String, Value>>,
    pub root_scope: Gc<GcCell<Scope>>,
    job_queue: GcCell<VecDeque<Job>>,
    pub(crate) mio: mio::Poll,
    pub(crate) mio_map: RefCell<HashMap<mio::Token, MioMapType>>,
    mio_token: Cell<usize>,
    pub(crate) pool: ThreadPool,
    uncaught_exception_handler: Option<Box<Fn(&Agent, Value) -> ()>>,
    modules: GcCell<HashMap<String, Gc<GcCell<Module>>>>,
}

unsafe impl gc::Trace for Agent {
    custom_trace!(this, {
        mark(&this.intrinsics);
        mark(&this.builtins);
        mark(&this.root_scope);
        mark(&this.job_queue);
        for v in this.mio_map.borrow().values() {
            mark(v);
        }
        mark(&this.modules);
    });
}

impl Agent {
    pub fn new() -> Agent {
        let object_prototype = create_object_prototype();
        let symbol_prototype = create_symbol_prototype(object_prototype.clone());

        let mut agent = Agent {
            assembler: Assembler::new(),
            intrinsics: Intrinsics {
                object_prototype: object_prototype.clone(),
                array_prototype: Value::Null,
                array_iterator_prototype: Value::Null,
                function_prototype: Value::Null,
                boolean_prototype: Value::Null,
                number_prototype: Value::Null,
                string_prototype: Value::Null,
                promise_prototype: Value::Null,
                promise: Value::Null,
                symbol_prototype,
                symbol: Value::Null,
                regex_prototype: Value::Null,
                iterator_prototype: Value::Null,
                iterator_map_prototype: Value::Null,
                generator_prototype: Value::Null,
                async_iterator_prototype: Value::Null,
                net_client_prototype: Value::Null,
                net_server_prototype: Value::Null,
                error_prototype: Value::Null,
                timer_iterator_prototype: Value::Null,
            },
            builtins: HashMap::new(),
            root_scope: Scope::new(None),
            job_queue: GcCell::new(VecDeque::new()),
            mio: mio::Poll::new().expect("create mio poll failed"),
            mio_map: RefCell::new(HashMap::new()),
            mio_token: Cell::new(0),
            pool: ThreadPool::new(num_cpus::get()),
            uncaught_exception_handler: None,
            modules: GcCell::new(HashMap::new()),
        };

        create_function_prototype(&mut agent);
        agent.intrinsics.boolean_prototype = create_boolean_prototype(&agent);
        agent.intrinsics.number_prototype = create_number_prototype(&agent);
        agent.intrinsics.string_prototype = create_string_prototype(&agent);
        agent.intrinsics.regex_prototype = create_regex_prototype(&agent);
        agent.intrinsics.symbol = create_symbol(&agent);
        agent.intrinsics.error_prototype = create_error_prototype(&agent);
        agent.intrinsics.iterator_prototype = create_iterator_prototype(&agent);
        agent.intrinsics.iterator_map_prototype = create_iterator_map_prototype(&agent);
        agent.intrinsics.async_iterator_prototype = create_async_iterator_prototype(&agent);
        agent.intrinsics.generator_prototype = create_generator_prototype(&agent);

        agent.intrinsics.array_prototype = create_array_prototype(&agent);
        agent.intrinsics.array_iterator_prototype = create_array_iterator_prototype(&agent);

        agent.intrinsics.promise_prototype = create_promise_prototype(&agent);
        agent.intrinsics.promise = create_promise(&agent);

        agent.intrinsics.net_client_prototype = create_net_client_prototype(&agent);
        agent.intrinsics.net_server_prototype = create_net_server_prototype(&agent);

        agent.intrinsics.timer_iterator_prototype = create_timer_iterator_prototype(&agent);

        agent.builtins = crate::builtins::create(&agent);

        {
            let mut scope = agent.root_scope.borrow_mut();

            scope.create(&agent, "Symbol", true).unwrap();
            scope.initialize("Symbol", agent.intrinsics.symbol.clone());
        }

        agent
    }

    pub fn import(&mut self, specifier: &str, referrer: &str) -> Result<Value, Value> {
        let module = self.load(specifier, referrer)?;
        Module::instantiate(self, module.clone())?;
        Module::evaluate(self, module)?;
        Ok(Value::Null)
    }

    pub(crate) fn load(
        &mut self,
        specifier: &str,
        referrer: &str,
    ) -> Result<Gc<GcCell<Module>>, Value> {
        let filename = self.resolve(specifier, referrer).unwrap();
        if !self.modules.borrow().contains_key(&filename) {
            let source = std::fs::read_to_string(&filename).expect("no such file");
            let module = Gc::new(GcCell::new(Module::new(
                filename.as_str(),
                source.as_str(),
                self,
            )?));
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

    fn resolve(&self, specifier: &str, referrer: &str) -> std::io::Result<String> {
        let filename = std::path::Path::new(referrer)
            .parent()
            .unwrap()
            .join(specifier);
        match std::fs::metadata(&filename) {
            Ok(ref r) if r.is_file() => Ok(filename
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()),
            Ok(_) => {
                let r = filename.with_file_name("module.sl");
                match std::fs::metadata(&r) {
                    Ok(_) => Ok(r.canonicalize().unwrap().to_str().unwrap().to_string()),
                    Err(e) => Err(e),
                }
            }
            Err(_) => {
                let r = filename.with_extension("sl");
                match std::fs::metadata(&r) {
                    Ok(_) => Ok(r.canonicalize().unwrap().to_str().unwrap().to_string()),
                    Err(e) => Err(e),
                }
            }
        }
    }

    pub(crate) fn enqueue_job(&self, f: JobFn, args: Vec<Value>) {
        self.job_queue.borrow_mut().push_back(Job(f, args));
    }

    pub fn run_jobs(&self) {
        let mut events = mio::Events::with_capacity(16);
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
                    MioMapType::Timer(t) => {
                        crate::builtins::timers::handle(self, event.token(), t);
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
                            self.uncaught_exception(e);
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

    pub(crate) fn mio_token(&self) -> mio::Token {
        let old = self.mio_token.get();
        mio::Token(self.mio_token.replace(old + 1))
    }

    pub fn set_uncaught_exception_handler<F: 'static>(&mut self, f: F)
    where
        F: Fn(&Agent, Value) -> (),
    {
        self.uncaught_exception_handler = Some(Box::new(f));
    }

    pub(crate) fn uncaught_exception(&self, e: Value) {
        // TODO: add way to handle this from sl
        match &self.uncaught_exception_handler {
            Some(f) => f(self, e),
            None => {
                eprintln!("Uncaught Exception: {}", Value::inspect(self, &e));
                std::process::exit(1);
            }
        }
    }

    pub fn run(&mut self, specifier: &str, source: &str) -> Result<Value, Value> {
        match Module::new(specifier, source, self) {
            Err(e) => Err(e),
            Ok(module) => {
                let mut evaluator = Interpreter::new(module.bytecode_position, module.context);
                evaluator.run(self).unwrap()
            }
        }
    }
}

impl Default for Agent {
    fn default() -> Self {
        Agent::new()
    }
}

macro_rules! test {
    ( $name:ident, $source:expr, $result:expr ) => {
        #[test]
        fn $name() {
            let mut agent = Agent::new();
            let mut result = agent.run(stringify!(test_$name.sl), $source);
            if let Ok(value) = &result {
                agent.run_jobs();
                if value.has_slot("promise state") {
                    if value.get_slot("promise state") == Value::from("fulfilled") {
                        result = Ok(value.get_slot("result"));
                    } else {
                        result = Err(value.get_slot("result"));
                    }
                }
            }
            assert_eq!(result, $result);
        }
    };
}

test!(test_decl_return, "const a = 1;", Ok(Value::Null));

test!(
    test_decl_assign,
    "let a = 1; a += 1; a;",
    Ok(Value::from(2))
);

test!(test_throw, "throw 5.0;", Err(Value::from(5.0)));

test!(test_paren_expr, "const a = 1; (a);", Ok(Value::from(1)));
test!(
    test_arrow_expr,
    "const a = 1; ((a) => { return a; })(2);",
    Ok(Value::from(2))
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
    Ok(Value::from(3))
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
    Ok(Value::from(6))
);

test!(
    test_symbols,
    r#"
    const a = Symbol('a');
    const b = Symbol('a');

    a == a && b != a;
    "#,
    Ok(Value::from(true))
);

test!(
    test_well_known_symbols,
    r#"
    const x = :wk;
    const y = { [x]: 5 };
    y[:wk];
    "#,
    Ok(Value::from(5))
);

test!(
    test_regex,
    r#"
    const re = /ab/;
    const t1 = re.test('ab');
    const t2 = re.test('ba');
    t1 && !t2;
    "#,
    Ok(Value::from(true))
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
    Ok(Value::from(10))
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
    for item in [1, 2, 3] {
        i += item;
    }
    i;
    "#,
    Ok(Value::from(51))
);

test!(
    test_tuple,
    r#"
    const a = (1, 2, 3);
    const b = (1,);
    a == a
      && a != b
      && a[0] == b[0]
      && b.length == 1
      && typeof a == 'tuple'
      && typeof b == 'tuple';
    "#,
    Ok(Value::from(true))
);

test!(
    test_decorator,
    r#"
    function x(v) {
      return (func) =>
        () => v + func();
    }
    @x('1')
    @x('2')
    function owo() {
      return '3';
    }
    owo();
    "#,
    Ok(Value::from("123"))
);

/*
test!(
    test_rest,
    r#"
    function x(a, ...args) {
      return a + (args[0] || '8') + (args[1] || '9');
    }
    const a = x('1', '2', '3');
    const b = x('1');
    a == '123' && b == '189';
    "#,
    Ok(Value::from(true))
);
*/

test!(
    test_class,
    r#"
    class X {
      constructor(x, y) {
        this.x = x;
        this.y = y;
      }
      z() {
        return this.x + this.y;
      }
    }
    const x = new X(1, 2);
    x.z() == 3 && X.name == 'X';
    "#,
    Ok(Value::from(true))
);

test!(
    test_default_args,
    r#"
    function add(a, b = 1) {
      return a + b;
    }
    add(2);
    "#,
    Ok(Value::from(3))
);

test!(
    test_string_unicode,
    r#"
    const a = 'hi \u{2764}';
    const b = '\U{HEAVY_RIGHT_POINTING_ANGLE_QUOTATION_MARK_ORNAMENT}';
    b + a;
    "#,
    Ok(Value::from("\u{276F}hi \u{2764}"))
);

test!(
    test_array_sort,
    r#"
    const a = [3, 1, 2];
    a.sort();
    const c1 = a[0] == 1 && a[1] == 2 && a[2] == 3;

    const b = [3, 1, 2];
    b.sort((a, b) => b - a);
    const c2 = b[0] == 3 && b[1] == 2 && b[2] == 1;

    c1 && c2;
    "#,
    Ok(Value::from(true))
);

test!(
    test_has_operator,
    r#"
    [1] has 0
      && !([1] has 1)
      && { a: 1 } has 'a'
      && [] has 'sort';
    "#,
    Ok(Value::from(true))
);

test!(
    test_number_parsing,
    r#"
    1e3 == 1000
      && 0b10 == 2
      && 0o10 == 8
      && 0x10 == 16
      && 0o7654321 == 2054353
      && 0xAbCdEf19 == 2882400025;
    "#,
    Ok(Value::from(true))
);
