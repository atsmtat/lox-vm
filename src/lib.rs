mod chunk;
mod compiler;
mod debug;
mod error;
mod memory;
mod object;
mod scanner;
mod value;
mod vm;

pub fn interpret(source: &str) {
    let mut heap = memory::Heap::new();
    if let Some(script_fn) = compiler::compile(&source, &mut heap) {
        let mut vm = vm::Vm::new(script_fn, &mut heap);
        vm.run().unwrap_or_else(|err| {
            eprintln!("{}", err);
        });
    }
}
