extern crate phf_codegen;

#[path = "./src/unicode_name_list.rs"]
mod unicode_name_list;

use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    for (name, value) in env::vars() {
        println!("{} = {}", name, value);
    }

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("unicode_name_map_gen.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    write!(
        &mut file,
        "#[allow(clippy::unreadable_literal)]\nstatic UNICODE_NAME_MAP: phf::Map<&'static str, char> =\n"
    )
    .unwrap();
    let mut map = phf_codegen::Map::new();
    for (name, c) in unicode_name_list::UNICODE_NAME_LIST {
        match *c {
            '\'' => map.entry(*name, "'\\''"),
            '\\' => map.entry(*name, "'\\\\'"),
            _ => map.entry(*name, format!("'{}'", c).as_str()),
        };
    }
    map.build(&mut file).unwrap();
    write!(&mut file, ";\n").unwrap();
}
