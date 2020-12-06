mod value;
mod chunk;
mod compiler;
mod debug;
mod error;
mod scanner;
mod vm;

use std::io::{self, BufRead, Write};

fn interpret(source: &str) {
    let mut chunk = chunk::Chunk::new();
    let result = compiler::compile(&source, &mut chunk);
    if result.is_ok() {
	let mut vm = vm::Vm::new(&chunk);
	vm.run();
    }
}

fn run_repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut input).unwrap();
        println!("");
        interpret(&input);
    }
}

fn run_file(_: &str) {}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.len() {
        0 => {
            run_repl();
        }
        1 => {
            run_file(&args[1]);
        }
        _ => {
            eprintln!("Usage: lox_vm [path]");
            std::process::exit(64);
        }
    }
}
