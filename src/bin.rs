use clap::{App, Arg};
use slither::Agent;

fn main() {
    let matches = App::new("slither")
        .version("0.1")
        .arg(Arg::with_name("filename").required(true))
        .get_matches();

    let filename = matches.value_of("filename").unwrap();
    let referrer = std::env::current_dir().unwrap().join("slither");
    let referrer = referrer.to_str().unwrap();

    let mut agent = Agent::new();
    agent.import(filename, referrer).unwrap();
    agent.run_jobs();
}
