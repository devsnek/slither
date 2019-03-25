use clap::App;
use slither::{disassemble, Agent};

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
    } else {
        matches.value_of("eval").unwrap().to_string()
    };

    if matches.is_present("disassemble") {
        disassemble(source.as_str());
    } else {
        let filename = matches.value_of("FILENAME").unwrap();
        let referrer = std::env::current_dir().unwrap().join("slither");
        let referrer = referrer.to_str().unwrap();

        let mut agent = Agent::new();
        agent.import(filename, referrer).unwrap();
        agent.run_jobs();
    }
}
