mod chunk;
mod compiler;
#[cfg(feature = "disassemble")]
mod debug;
mod error;
mod memory;
mod object;
mod scanner;
mod value;
mod vm;

use std::io;

pub fn interpret<Out: io::Write>(source: &str, stdout: &mut Out) {
    let mut heap = memory::Heap::new();
    if let Some(script_fn) = compiler::compile(&source, &mut heap) {
        let mut vm = vm::Vm::new(script_fn, &mut heap, stdout);
        vm.run().unwrap_or_else(|err| {
            eprintln!("{}", err);
        });
    }
}
