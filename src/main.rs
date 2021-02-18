use std::fs;
use std::io::{self, BufRead, Write};

fn run_repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut input).unwrap();
        println!("");
        lox_vm::interpret(&input);
    }
}

fn run_file(fname: &str) {
    let content = fs::read_to_string(fname).expect("error reading the file");
    lox_vm::interpret(&content);
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.len() {
        0 => {
            run_repl();
        }
        1 => {
            run_file(&args[0]);
        }
        _ => {
            eprintln!("Usage: lox_vm [path]");
            std::process::exit(64);
        }
    }
}
