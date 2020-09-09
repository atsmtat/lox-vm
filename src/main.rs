mod chunk;
mod debug;
mod vm;

fn main() {
    let mut chunk = chunk::Chunk::new();
    let val_offset = chunk.add_constant(chunk::Value::Double(4.2));
    chunk.add_instruction(chunk::Instruction::OpConstant(val_offset), 41);

    let val_offset = chunk.add_constant(chunk::Value::Double(1.2));
    chunk.add_instruction(chunk::Instruction::OpConstant(val_offset), 42);

    chunk.add_instruction(chunk::Instruction::OpReturn, 42);
    debug::disassemble_chunk(&chunk, "Test chunk");
    let mut vm = vm::Vm::new(&chunk);
    vm.run();
}
