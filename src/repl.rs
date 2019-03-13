extern crate rustyline;

use clap::App;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use slither::Agent;

fn main() {
    let _matches = App::new("slither repl").version("0.1").get_matches();

    let mut agent = Agent::new();

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let value = agent.run("repl", line.as_str());
                rl.add_history_entry(line.as_ref());
                match value {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("Error: {}", e),
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
