use crate::interpreter::{Assembler, Op, OpArg};
use byteorder::{LittleEndian, ReadBytesExt};

const ANSI_RED: &str = "\x1b[31m";
const ANSI_GREEN: &str = "\x1b[32m";
const ANSI_YELLOW: &str = "\x1b[33m";
const ANSI_RESET: &str = "\x1b[39m";
const ANSI_GREY: &str = "\x1b[90m";
const ANSI_BLUE: &str = "\x1b[94m";

pub fn disassemble(assembler: &Assembler, mut pc: usize, n_instructions: usize) {
    let format_args = |pc: &mut usize, args: Vec<OpArg>| -> String {
        args.iter()
            .map(|arg: &OpArg| -> String {
                match arg {
                    OpArg::U8 => {
                        *pc += 1;
                        let n = assembler.code[*pc - 1];
                        format!("[{}{}{}]", ANSI_BLUE, n, ANSI_RESET)
                    }
                    OpArg::U32 => {
                        *pc += 4;
                        let n = (&assembler.code[(*pc - 4)..])
                            .read_u32::<LittleEndian>()
                            .unwrap();
                        format!("[{}{}{}]", ANSI_BLUE, n, ANSI_RESET)
                    }
                    OpArg::F64 => {
                        *pc += 8;
                        let n = (&assembler.code[(*pc - 8)..])
                            .read_f64::<LittleEndian>()
                            .unwrap();
                        format!("[{}{}{}]", ANSI_BLUE, n, ANSI_RESET)
                    }
                    OpArg::String => {
                        *pc += 4;
                        let sid = (&assembler.code[(*pc - 4)..])
                            .read_u32::<LittleEndian>()
                            .unwrap() as usize;
                        format!(
                            "[{}\"{}\"{}]",
                            ANSI_GREEN, assembler.string_table[sid], ANSI_RESET
                        )
                    }
                    OpArg::Boolean => {
                        *pc += 1;
                        let b = assembler.code[*pc] == 1;
                        format!("[{}{}{}]", ANSI_YELLOW, b, ANSI_RESET)
                    }
                    OpArg::Position => {
                        *pc += 4;
                        let position = (&assembler.code[(*pc - 4)..])
                            .read_u32::<LittleEndian>()
                            .unwrap();
                        format!("[{}@{}{}]", ANSI_YELLOW, position, ANSI_RESET)
                    }
                    OpArg::Register => {
                        *pc += 4;
                        let r = (&assembler.code[(*pc - 4)..])
                            .read_u32::<LittleEndian>()
                            .unwrap();
                        format!("{}r{}{}", ANSI_RED, r, ANSI_RESET)
                    }
                    OpArg::FunctionInfo => {
                        *pc += 4;
                        let r = (&assembler.code[(*pc - 4)..])
                            .read_u32::<LittleEndian>()
                            .unwrap() as usize;
                        let f = &assembler.function_info[r];
                        format!(
                            "<FunctionInfo {} {}@{}{} {:?}>",
                            r, ANSI_YELLOW, f.position, ANSI_RESET, f.parameters
                        )
                    }
                }
            })
            .collect::<Vec<String>>()
            .join(", ")
    };

    for _ in 0..n_instructions {
        if pc >= assembler.code.len() {
            break;
        }
        let op = assembler.code[pc].into();
        pc += 1;

        macro_rules! define_matcher {
            ( $( ( $name:ident, $acu:expr $( , $arg:expr )* ), )* ) => (

                match op {
                    $(
                    Op::$name => {
                        println!("{}{:03}{} {} {}",
                            ANSI_GREY,
                            pc - 1,
                            ANSI_RESET,
                            stringify!($name),
                            format_args(&mut pc, vec![ $( $arg, )* ]),
                        );
                    }
                    )*
                }

            );
        }

        OPS!(define_matcher);
    }
}
