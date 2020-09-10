use crate::chunk::{Chunk, Instruction, Value};

const STACK_MAX: usize = 256;

pub enum InterpretError {
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

    pub fn run(&mut self) -> Result<(), InterpretError> {
        for instr in self.chunk.iter() {
            match instr {
                Instruction::OpReturn => {
		    let val = self.pop()?;
                    println!("'{:?}'", val);
		    return Ok(())
                }

		Instruction::OpNegate => {
		    let val = self.pop()?;
		    let new_val = match val {
			Value::Double(v) => Value::Double(-v)
		    };
		    self.push(new_val);
		}

                Instruction::OpConstant(val_offset) => {
                    let val = self.chunk.get_constant(val_offset);
                    self.push(val);
                }

                Instruction::OpInvalid => return Err(InterpretError::RuntimeError),
            }
        }
        Ok(())
    }

    fn push( &mut self, val: Value ) {
	self.stack.push(val);
    }

    fn pop(&mut self) -> Result<Value, InterpretError> {
	if let Some(val) = self.stack.pop() {
	    Ok(val)
	} else {
	    Err(InterpretError::RuntimeError)
	}
    }
}
