use crate::chunk::{Chunk, Instruction};
use crate::error::{ErrorKind, RuntimeError, VmError};
use crate::memory::{Gc, Heap};
use crate::object::StrObj;
use crate::value::Value;
use fnv::FnvHashMap;

const STACK_MAX: usize = 256;

pub struct Vm<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
    ip: usize,
    heap: &'a mut Heap,
    globals: FnvHashMap<Gc<StrObj>, Value>,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk, heap: &'a mut Heap) -> Self {
        Vm {
            chunk,
            stack: Vec::with_capacity(STACK_MAX),
            ip: 0,
            heap: heap,
            globals: FnvHashMap::default(),
        }
    }

    fn runtime_error(&self, kind: ErrorKind) -> RuntimeError {
        RuntimeError::new(self.chunk.get_line(self.ip), kind)
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        while self.ip < self.chunk.code_len() {
            let (instr_size, instr) = self.chunk.read_instruction(self.ip);
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
                    let val = self.pop_number()?;
                    self.push(Value::Double(-val));
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
                            _ => Err(self.runtime_error(ErrorKind::InvalidOperand(rhs))),
                        },
                        Value::Double(lnum) => match rhs {
                            Value::Double(rnum) => Ok(Value::Double(lnum + rnum)),
                            _ => Err(self.runtime_error(ErrorKind::InvalidOperand(rhs))),
                        },
                        _ => Err(self.runtime_error(ErrorKind::InvalidOperand(lhs))),
                    };
                    self.push(result?);
                }

                Instruction::OpSubtract => {
                    let rhs = self.pop_number()?;
                    let lhs = self.pop_number()?;
                    self.push(Value::Double(lhs - rhs));
                }

                Instruction::OpMultiply => {
                    let rhs = self.pop_number()?;
                    let lhs = self.pop_number()?;
                    self.push(Value::Double(lhs * rhs));
                }

                Instruction::OpDivide => {
                    let rhs = self.pop_number()?;
                    let lhs = self.pop_number()?;
                    self.push(Value::Double(lhs / rhs));
                }

                Instruction::OpEqual => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    self.push(Value::Boolean(lhs == rhs));
                }

                Instruction::OpGreater => {
                    let rhs = self.pop_number()?;
                    let lhs = self.pop_number()?;
                    self.push(Value::Boolean(lhs > rhs));
                }

                Instruction::OpLess => {
                    let rhs = self.pop_number()?;
                    let lhs = self.pop_number()?;
                    self.push(Value::Boolean(lhs < rhs));
                }

                Instruction::OpTrue => self.push(Value::Boolean(true)),
                Instruction::OpFalse => self.push(Value::Boolean(false)),
                Instruction::OpNil => self.push(Value::Nil),

                Instruction::OpConstant(val_offset) => {
                    let val = self.chunk.get_constant(val_offset);
                    self.push(val);
                }

                Instruction::OpDefineGlobal(val_offset) => {
                    let var_name = self.get_variable_at(val_offset)?;
                    let init_val = self.pop()?;
                    self.globals.insert(var_name, init_val);
                }

                Instruction::OpGetGlobal(val_offset) => {
                    let var_name = self.get_variable_at(val_offset)?;
                    match self.globals.get(&var_name) {
                        Some(val) => {
                            let result = *val;
                            self.push(result);
                        }
                        None => {
                            let err_kind = ErrorKind::UndefinedVariable(Value::String(var_name));
                            return Err(self.runtime_error(err_kind));
                        }
                    }
                }

                Instruction::OpSetGlobal(val_offset) => {
                    let var_name = self.get_variable_at(val_offset)?;
                    let new_val = self.peek()?;
                    match self.globals.get_mut(&var_name) {
                        Some(val) => {
                            *val = new_val;
                        }
                        None => {
                            let err_kind = ErrorKind::UndefinedVariable(Value::String(var_name));
                            return Err(self.runtime_error(err_kind));
                        }
                    }
                }

                Instruction::OpGetLocal(stack_ix) => match self.stack.get(stack_ix as usize) {
                    Some(val) => {
                        let result = *val;
                        self.push(result);
                    }
                    None => {
                        let err_kind = ErrorKind::InternalError(VmError::EmptyStackPop);
                        return Err(self.runtime_error(err_kind));
                    }
                },

                Instruction::OpSetLocal(stack_ix) => {
                    let new_val = self.peek()?;
                    match self.stack.get_mut(stack_ix as usize) {
                        Some(val) => {
                            *val = new_val;
                        }
                        None => {
                            let err_kind = ErrorKind::InternalError(VmError::EmptyStackPop);
                            return Err(self.runtime_error(err_kind));
                        }
                    }
                }

                Instruction::OpJumpIfFalse(offset) => {
                    let cond_val = self.peek()?;
                    if cond_val.is_falsey() {
                        self.ip += offset as usize;
                    }
                }

                Instruction::OpJump(offset) => {
                    self.ip += offset as usize;
                }

                Instruction::OpLoop(offset) => {
                    self.ip -= offset as usize;
                }

                Instruction::OpInvalid => {
                    let err_kind = ErrorKind::InternalError(VmError::InvalidOpCode);
                    return Err(self.runtime_error(err_kind));
                }
            }
            self.ip += instr_size as usize;
        }
        Ok(())
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        if let Some(val) = self.stack.pop() {
            Ok(val)
        } else {
            Err(self.runtime_error(ErrorKind::InternalError(VmError::EmptyStackPop)))
        }
    }

    fn peek(&mut self) -> Result<Value, RuntimeError> {
        if let Some(val) = self.stack.last() {
            Ok(*val)
        } else {
            Err(self.runtime_error(ErrorKind::InternalError(VmError::EmptyStackPop)))
        }
    }

    fn pop_number(&mut self) -> Result<f64, RuntimeError> {
        let val = self.pop()?;
        match val {
            Value::Double(v) => Ok(v),
            _ => Err(self.runtime_error(ErrorKind::InvalidOperand(val))),
        }
    }

    fn get_variable_at(&self, offset: u8) -> Result<Gc<StrObj>, RuntimeError> {
        let ident = self.chunk.get_constant(offset);
        match ident {
            Value::String(ident_str) => Ok(ident_str),
            _ => {
                let err_kind = ErrorKind::InternalError(VmError::UnexpectedValue(ident));
                Err(self.runtime_error(err_kind))
            }
        }
    }

    fn concatenate(&mut self, lstr: Gc<StrObj>, rstr: Gc<StrObj>) -> Value {
        let mut new_str = String::from(&lstr.0);
        new_str.push_str(&rstr.0);
        Value::String(self.heap.allocate_string(new_str))
    }
}
