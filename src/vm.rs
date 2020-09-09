use crate::chunk::{Chunk, Instruction, Value};

const STACK_MAX: usize = 256;

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

pub struct Vm<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Vm {
            chunk,
            stack: Vec::with_capacity(STACK_MAX),
        }
    }

    pub fn run(&mut self) -> InterpretResult {
        for instr in self.chunk.iter() {
            match instr {
                Instruction::OpReturn => {
                    if let Some(val) = self.stack.pop() {
                        println!("'{:?}'", val);
                        return InterpretResult::Ok;
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }

                Instruction::OpConstant(val_offset) => {
                    let val = self.chunk.get_constant(val_offset);
                    self.stack.push(val);
                }

                Instruction::OpInvalid => return InterpretResult::RuntimeError,
            }
        }
        InterpretResult::Ok
    }
}
