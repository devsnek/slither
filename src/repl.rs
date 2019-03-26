extern crate rustyline;

use clap::App;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use slither::{Agent, Context, Interpreter, Parser, Scope, Value};

fn main() {
    let _matches = App::new("slither repl").version("0.1").get_matches();

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
                    Err(e) => {
                        println!("Uncaught Exception: {:?}", e);
                        continue;
                    }
                };
                let index = agent.assembler.assemble(&ast);
                let mut interpreter = Interpreter::new(index, context.clone());
                let value = interpreter.run(&agent).unwrap();
                agent.run_jobs();
                match value {
                    Ok(v) => println!("{}", Value::inspect(&agent, &v)),
                    Err(e) => println!("Uncaught Exception: {}", Value::inspect(&agent, &e),),
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
