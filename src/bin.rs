use clap::App;
use rustyline::{error::ReadlineError, Editor};
use slither::{disassemble, Agent, Context, Interpreter, Parser, Scope, Value};

fn main() {
    let matches = App::new("slither")
        .version("0.1")
        .args_from_usage(
            r#"
        [FILENAME]           'File to run'
        -d, --disassemble    'Print disassembly instead of running'
        -e, --eval=[code]    'Code to eval inline'
        "#,
        )
        .get_matches();

    let source = if matches.is_present("FILENAME") {
        let filename = matches.value_of("FILENAME").unwrap();
        std::fs::read_to_string(filename).unwrap()
    } else if matches.is_present("eval") {
        matches.value_of("eval").unwrap().to_string()
    } else {
        start_repl();
        return;
    };

    if matches.is_present("disassemble") {
        disassemble(source.as_str());
    } else if matches.is_present("eval") {
        let mut agent = Agent::new();
        let value = agent.run("eval", source.as_str());
        agent.run_jobs();
        match value {
            Ok(v) => println!("{}", Value::inspect(&agent, &v)),
            Err(e) => println!("Uncaught Exception: {}", Value::inspect(&agent, &e)),
        };
    } else {
        let filename = matches.value_of("FILENAME").unwrap();
        let referrer = std::env::current_dir().unwrap().join("slither");
        let referrer = referrer.to_str().unwrap();

        let mut agent = Agent::new();
        agent.import(filename, referrer).unwrap();
        agent.run_jobs();
    }
}

fn start_repl() {
    let mut agent = Agent::new();

    agent.set_uncaught_exception_handler(|agent: &Agent, v: Value| {
        println!("Uncaught Exception: {}", Value::inspect(agent, &v));
    });

    let context = Context::new(Scope::new(Some(agent.root_scope.clone())));

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                let ast = match Parser::parse(&line) {
                    Ok(a) => a,
                    Err(e) => match Parser::parse((line + ";").as_str()) {
                        Ok(a) => a,
                        Err(_) => {
                            println!("Uncaught Exception: {:?}", e);
                            continue;
                        }
                    },
                };
                let index = agent.assembler.assemble(&ast);
                let mut interpreter = Interpreter::new(index, context.clone());
                let value = interpreter.run(&agent).unwrap();
                agent.run_jobs();
                match value {
                    Ok(v) => println!("{}", Value::inspect(&agent, &v)),
                    Err(e) => println!("Uncaught Exception: {}", Value::inspect(&agent, &e)),
                }
            }
            Err(ReadlineError::Interrupted) => {
                // println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                // println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Readline error: {:?}", err);
                break;
            }
        }
    }
}
