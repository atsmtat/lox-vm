use crate::chunk::{Chunk, Instruction};
use crate::error::InterpretError;
use crate::memory::{Gc, Heap, StrObj};
use crate::value::Value;

const STACK_MAX: usize = 256;

pub struct Vm<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
    ip: usize,
    heap: &'a mut Heap,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk, heap: &'a mut Heap) -> Self {
        Vm {
            chunk,
            stack: Vec::with_capacity(STACK_MAX),
            ip: 0,
            heap: heap,
        }
    }

    fn get_number(&mut self) -> Result<f64, InterpretError> {
        let val = self.pop()?;
        match val {
            Value::Double(v) => Ok(v),
            _ => {
                self.report_error("operand must be a number");
                return Err(InterpretError::RuntimeError);
            }
        }
    }

    fn report_error(&self, msg: &str) {
        eprintln!("[line {}] Error: {}", self.chunk.get_line(self.ip), msg);
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
        for (instr_index, instr) in self.chunk.iter().enumerate() {
            self.ip = instr_index;
            match instr {
                Instruction::OpReturn => {
                    return Ok(());
                }

                Instruction::OpPop => {
                    self.pop()?;
                }

                Instruction::OpPrint => {
                    println!("{}", self.pop()?);
                }

                Instruction::OpNegate => {
                    let val = self.get_number()?;
                    let result = Value::Double(-val);
                    self.push(result);
                }

                Instruction::OpNot => {
                    let val = self.pop()?;
                    let result = Value::Boolean(val.is_falsey());
                    self.push(result);
                }

                Instruction::OpAdd => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = match lhs {
                        Value::String(lstr) => match rhs {
                            Value::String(rstr) => Ok(self.concatenate(lstr, rstr)),
                            _ => lhs + rhs,
                        },
                        _ => lhs + rhs,
                    };
                    self.push(result?);
                }

                Instruction::OpSubtract => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = lhs - rhs;
                    self.push(result?);
                }

                Instruction::OpMultiply => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = lhs * rhs;
                    self.push(result?);
                }

                Instruction::OpDivide => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = lhs / rhs;
                    self.push(result?);
                }

                Instruction::OpEqual => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = Value::Boolean(lhs == rhs);
                    self.push(result);
                }

                Instruction::OpGreater => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = lhs.gt(&rhs);
                    self.push(result?);
                }

                Instruction::OpLess => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    let result = lhs.lt(&rhs);
                    self.push(result?);
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

    fn concatenate(&mut self, lstr: Gc<StrObj>, rstr: Gc<StrObj>) -> Value {
        let mut new_str = String::from(&lstr.0);
        new_str.push_str(&rstr.0);
        Value::String(self.heap.allocate_string(new_str))
    }
}
