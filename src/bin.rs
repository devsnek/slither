use slither::run;
use clap::{App, Arg};

fn main() {
    let matches = App::new("slither")
        .version("0.1")
        .arg(Arg::with_name("filename").required(true))
        .get_matches();

    let filename = matches.value_of("filename").unwrap();
    let referrer = std::env::current_dir().unwrap().join("slither");
    let referrer = referrer.to_str().unwrap();

    match run(filename, referrer) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("Uncaught Exception: {}", e);
            std::process::exit(1);
        }
    }
}
