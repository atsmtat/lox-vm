mod chunk;
mod compiler;
mod debug;
mod error;
mod memory;
mod scanner;
mod value;
mod vm;

use std::io::{self, BufRead, Write};

fn interpret(source: &str) {
    let mut chunk = chunk::Chunk::new();
    let mut heap = memory::Heap::new();
    if let Some(()) = compiler::compile(&source, &mut chunk, &mut heap) {
        let mut vm = vm::Vm::new(&chunk, &mut heap);
        vm.run().unwrap_or_else(|err| {
            eprintln!("{}", err);
        });
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
