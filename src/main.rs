use clap::{App, Arg};
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

        let stdout = io::stdout();
        let mut handle = stdout.lock();
        lox_vm::interpret(&input, &mut handle);
    }
}

fn run_file(fname: &str) {
    let content = fs::read_to_string(fname).expect("error reading the file");

    let stdout = io::stdout();
    let mut handle = stdout.lock();
    lox_vm::interpret(&content, &mut handle);
}

fn main() {
    let matches = App::new("loxi")
        .version("0.1.0")
        .about("Interpreter for Lox scripts")
        .arg(
            Arg::with_name("file")
                .help("Input script")
                .required(false)
                .index(1),
        )
        .get_matches();

    if let Some(f) = matches.value_of("file") {
        run_file(f);
    } else {
        run_repl();
    }
}
