extern crate rustyline;

use clap::App;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use slither::{Agent, Value};

fn main() {
    let _matches = App::new("slither repl").version("0.1").get_matches();

    let mut agent = Agent::new();

    agent.set_uncaught_exception_handler(|agent: &Agent, v: Value| {
        println!("Uncaught Exception: {}", Value::inspect(agent, &v).unwrap());
    });

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let value = agent.run("repl", line.as_str());
                rl.add_history_entry(line.as_ref());
                agent.run_jobs();
                match value {
                    Ok(v) => println!("{}", Value::inspect(&agent, &v).unwrap()),
                    Err(e) => println!(
                        "Uncaught Exception: {}",
                        Value::inspect(&agent, &e).unwrap()
                    ),
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
