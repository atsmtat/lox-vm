use crate::chunk::{Chunk, Instruction, Value};
use crate::error::InterpretError;

const STACK_MAX: usize = 256;

pub struct Vm<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
    ip : usize,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Vm {
            chunk,
            stack: Vec::with_capacity(STACK_MAX),
	    ip: 0,
        }
    }

    fn get_number(&mut self) -> Result<f64, InterpretError> {
	let val = self.pop()?;
	match val {
	    Value::Double(v) => Ok(v),
	    _ => {
		self.report_error( "operand must be a number" );
		return Err(InterpretError::RuntimeError);
	    }
	}
    }

    fn report_error(&self, msg: &str) {
	eprintln!( "[line {}] Error: {}", self.chunk.get_line(self.ip), msg);
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
        for (instr_index, instr) in self.chunk.iter().enumerate() {
	    self.ip = instr_index;
            match instr {
                Instruction::OpReturn => {
                    let val = self.pop()?;
                    println!("'{:?}'", val);
                    return Ok(());
                }

                Instruction::OpNegate => {
                    let val = self.get_number()?;
                    let result = Value::Double(-val);
                    self.push(result);
                }

		Instruction::OpAdd |
		Instruction::OpSubtract |
		Instruction::OpMultiply |
		Instruction::OpDivide => {
		    self.exec_binary_op(instr)?;
		}

		Instruction::OpTrue => self.push(Value::Boolean(true)),
		Instruction::OpFalse => self.push(Value::Boolean(false)),
		Instruction::OpNil => self.push(Value::Nil),

                Instruction::OpConstant(val_offset) => {
                    let val = self.chunk.get_constant(val_offset);
                    self.push(val);
                }

                Instruction::OpInvalid => return Err(InterpretError::InternalError),
            }
        }
        Ok(())
    }

    fn exec_binary_op(&mut self, instr: Instruction) -> Result<(), InterpretError> {
	let rhs = self.get_number()?;
	let lhs = self.get_number()?;

	let result = match instr {
	    Instruction::OpAdd => lhs + rhs,
	    Instruction::OpSubtract => lhs - rhs,
	    Instruction::OpMultiply => lhs * rhs,
	    Instruction::OpDivide => lhs / rhs,
	    _ => { return Err(InterpretError::InternalError); }
	};

	let result = Value::Double(result);
	self.push(result);
	Ok(())
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Result<Value, InterpretError> {
        if let Some(val) = self.stack.pop() {
            Ok(val)
        } else {
            Err(InterpretError::InternalError)
        }
    }
}
