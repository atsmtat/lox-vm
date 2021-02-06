use crate::chunk::Instruction;
use crate::error::{ErrorKind, RuntimeError, StackFrame, VmError};
use crate::memory::{Gc, Heap};
use crate::object::{ClosureObj, FnObj, NativeFn, NativeObj, StrObj};
use crate::value::Value;
use fnv::FnvHashMap;
use std::time::SystemTime;

// native function
fn clock() -> Value {
    let duration = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    Value::Double(duration.as_secs() as f64)
}

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * u8::MAX as usize;

struct CallFrame {
    closure: Gc<ClosureObj>,
    frame_ptr: usize,
    ip: usize,
}

pub struct Vm<'a> {
    call_frames: Vec<CallFrame>,
    stack: Vec<Value>,
    heap: &'a mut Heap,
    globals: FnvHashMap<Gc<StrObj>, Value>,
}

impl<'a> Vm<'a> {
    pub fn new(script_fn: Gc<FnObj>, heap: &'a mut Heap) -> Self {
        let mut vm = Vm {
            call_frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::with_capacity(STACK_MAX),
            heap: heap,
            globals: FnvHashMap::default(),
        };

        vm.push(Value::Function(script_fn));

        let closure = vm.heap.allocate(ClosureObj::new(script_fn));
        vm.call_frames.push(CallFrame {
            closure,
            frame_ptr: 0,
            ip: 0,
        });

        vm.pop().unwrap();
        vm.push(Value::Closure(closure));

        vm.define_native("clock".to_string(), clock);
        vm
    }

    fn call_frame(&self) -> &CallFrame {
        self.call_frames.last().expect("empty call frames stack")
    }

    fn call_frame_mut(&mut self) -> &mut CallFrame {
        self.call_frames
            .last_mut()
            .expect("empty call frames stack")
    }

    fn stack_frame(&self) -> &[Value] {
        let fp = self.call_frame().frame_ptr;
        &self.stack[fp..]
    }

    fn stack_frame_mut(&mut self) -> &mut [Value] {
        let fp = self.call_frame().frame_ptr;
        &mut self.stack[fp..]
    }

    fn move_ip_fwd(&mut self, steps: usize) {
        let ip = &mut self.call_frame_mut().ip;
        *ip += steps;
    }

    fn move_ip_back(&mut self, steps: usize) {
        let ip = &mut self.call_frame_mut().ip;
        *ip -= steps;
    }

    fn next_instruction(&self) -> (u8, Instruction) {
        let ip = self.call_frame().ip;
        self.call_frame()
            .closure
            .function
            .chunk
            .read_instruction(ip)
    }

    fn get_chunk_constant(&self, offset: u8) -> Value {
        self.call_frame()
            .closure
            .function
            .chunk
            .get_constant(offset)
    }

    fn get_chunk_variable(&self, offset: u8) -> Result<Gc<StrObj>, RuntimeError> {
        let ident = self.get_chunk_constant(offset);
        match ident {
            Value::String(ident_str) => Ok(ident_str),
            _ => {
                let err_kind = ErrorKind::InternalError(VmError::UnexpectedValue(ident));
                Err(self.runtime_error(err_kind))
            }
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let (instr_size, instr) = self.next_instruction();
            match instr {
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
                    let val = self.get_chunk_constant(val_offset);
                    self.push(val);
                }

                Instruction::OpDefineGlobal(val_offset) => {
                    let var_name = self.get_chunk_variable(val_offset)?;
                    let init_val = self.pop()?;
                    self.globals.insert(var_name, init_val);
                }

                Instruction::OpGetGlobal(val_offset) => {
                    let var_name = self.get_chunk_variable(val_offset)?;
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
                    let var_name = self.get_chunk_variable(val_offset)?;
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

                Instruction::OpGetLocal(stack_ix) => {
                    match self.stack_frame().get(stack_ix as usize) {
                        Some(val) => {
                            let result = *val;
                            self.push(result);
                        }
                        None => {
                            let err_kind = ErrorKind::InternalError(VmError::EmptyStackPop);
                            return Err(self.runtime_error(err_kind));
                        }
                    }
                }

                Instruction::OpSetLocal(stack_ix) => {
                    let new_val = self.peek()?;
                    match self.stack_frame_mut().get_mut(stack_ix as usize) {
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
                        self.move_ip_fwd(offset as usize);
                    }
                }

                Instruction::OpJump(offset) => {
                    self.move_ip_fwd(offset as usize);
                }

                Instruction::OpLoop(offset) => {
                    self.move_ip_back(offset as usize);
                }

                Instruction::OpClosure(val_offset) => match self.get_chunk_constant(val_offset) {
                    Value::Function(fn_obj) => {
                        let closure = self.heap.allocate(ClosureObj::new(fn_obj));
                        self.push(Value::Closure(closure));
                    }
                    _ => {
                        let err_kind = ErrorKind::InternalError(VmError::InvalidOpCode);
                        return Err(self.runtime_error(err_kind));
                    }
                },

                Instruction::OpCall(args) => {
                    self.move_ip_fwd(instr_size as usize);
                    self.call_value(self.peek_nth(args as usize)?, args)?;
                    continue;
                }

                Instruction::OpReturn => {
                    let result = self.pop()?;
                    let frame = self.call_frames.pop().ok_or_else(|| {
                        self.runtime_error(ErrorKind::InternalError(VmError::EmptyStackPop))
                    })?;

                    if self.call_frames.is_empty() {
                        // returning from top-level script function.
                        // terminate the interpreter successfully.
                        return Ok(());
                    }

                    // drop the frame from the stack.
                    self.stack.truncate(frame.frame_ptr);

                    // push the return value on the stack, in the caller's frame.
                    self.stack.push(result);
                    continue;
                }

                Instruction::OpInvalid => {
                    let err_kind = ErrorKind::InternalError(VmError::InvalidOpCode);
                    return Err(self.runtime_error(err_kind));
                }
            }
            self.move_ip_fwd(instr_size as usize);
        }
    }

    fn call_value(&mut self, val: Value, arg_count: u8) -> Result<(), RuntimeError> {
        match val {
            Value::Closure(clos_obj) => self.call(clos_obj, arg_count),
            Value::Native(native_obj) => Ok(self.call_native(native_obj)),
            _ => Err(self.runtime_error(ErrorKind::NonCallable(val))),
        }
    }

    fn call(&mut self, closure: Gc<ClosureObj>, arg_count: u8) -> Result<(), RuntimeError> {
        let exp_args = closure.function.arity();
        if arg_count != exp_args {
            return Err(self.runtime_error(ErrorKind::MismatchArgCount(exp_args, arg_count)));
        }

        let fp = self.stack.len() - arg_count as usize - 1;
        self.call_frames.push(CallFrame {
            closure,
            frame_ptr: fp,
            ip: 0,
        });

        if self.call_frames.len() == FRAMES_MAX {
            return Err(self.runtime_error(ErrorKind::StackOverflow));
        }
        Ok(())
    }

    fn call_native(&mut self, native: Gc<NativeObj>) {
        let result = (native.function)();
        self.stack.push(result);
    }

    // === Native function FFI ===
    fn define_native(&mut self, name: String, native: NativeFn) {
        let native_name = self.heap.allocate_string(name);
        let native_obj = self.heap.allocate(NativeObj::new(native));
        self.globals.insert(native_name, Value::Native(native_obj));
    }

    // === Error reporting ===
    fn runtime_error(&self, kind: ErrorKind) -> RuntimeError {
        let ip = self.call_frame().ip;
        let chunk = &self.call_frame().closure.function.chunk;
        RuntimeError::new(chunk.get_line(ip), kind, self.stack_trace())
    }

    fn stack_trace(&self) -> Vec<StackFrame> {
        let mut trace = Vec::new();
        for frame in self.call_frames.iter().rev() {
            let chunk = &frame.closure.function.chunk;
            let line = chunk.get_line(frame.ip);
            trace.push(StackFrame::new(line, frame.closure.function.name()));
        }
        trace
    }

    // === Stack APIs ===
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

    fn peek(&self) -> Result<Value, RuntimeError> {
        if let Some(val) = self.stack.last() {
            Ok(*val)
        } else {
            Err(self.runtime_error(ErrorKind::InternalError(VmError::EmptyStackPop)))
        }
    }

    fn peek_nth(&self, offset: usize) -> Result<Value, RuntimeError> {
        if let Some(val) = self.stack.iter().rev().nth(offset) {
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

    fn concatenate(&mut self, lstr: Gc<StrObj>, rstr: Gc<StrObj>) -> Value {
        let mut new_str = String::from(&lstr.0);
        new_str.push_str(&rstr.0);
        Value::String(self.heap.allocate_string(new_str))
    }
}
