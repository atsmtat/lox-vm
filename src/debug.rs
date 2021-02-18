use crate::chunk::{Chunk, Instruction};
use crate::memory::Gc;
use crate::object::FnObj;
use crate::value::Value;

pub struct InstructionIter<'a> {
    chunk: &'a Chunk,
    ip: usize,
}

impl<'a> InstructionIter<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        InstructionIter { chunk, ip: 0 }
    }
}

impl<'a> Iterator for InstructionIter<'a> {
    type Item = (usize, Instruction);

    fn next(&mut self) -> Option<Self::Item> {
        if self.ip >= self.chunk.code_len() {
            return None;
        }
        let (instr_size, instr) = self.chunk.read_instruction(self.ip);
        let result = (self.ip, instr);
        self.ip += instr_size as usize;
        Some(result)
    }
}

pub struct FunIter<'a> {
    const_iter: std::slice::Iter<'a, Value>,
}

impl<'a> FunIter<'a> {
    fn new(chunk: &'a Chunk) -> Self {
        FunIter {
            const_iter: chunk.constants.iter(),
        }
    }
}

impl<'a> Iterator for FunIter<'a> {
    type Item = Gc<FnObj>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(val) = self.const_iter.next() {
            match val {
                Value::Function(fn_obj) => {
                    return Some(*fn_obj);
                }
                _ => {
                    continue;
                }
            }
        }
        return None;
    }
}

struct Disassembler<'a> {
    chunk: &'a Chunk,
    iter: InstructionIter<'a>,
}

impl<'a> Disassembler<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Disassembler {
            iter: InstructionIter::new(chunk),
            chunk,
        }
    }

    fn get_line(&self, instr_index: usize) -> String {
        if instr_index > 0
            && self.chunk.get_line(instr_index) == self.chunk.get_line(instr_index - 1)
        {
            format!("{:>4} ", "|")
        } else {
            format!("{:04} ", self.chunk.get_line(instr_index))
        }
    }

    fn get_constant(&self, const_index: u8) -> String {
        if let Some(val) = self.chunk.get_constant_checked(const_index) {
            format!("'{:?}'", val)
        } else {
            "<out-of-bound>".to_string()
        }
    }
}

impl<'a> Iterator for Disassembler<'a> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((code_offset, instr)) = self.iter.next() {
            let mut result = format!("{:04} {} ", code_offset, self.get_line(code_offset));
            match instr {
                Instruction::OpConstant(val_offset) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} {}",
                            "OP_CONSTANT",
                            val_offset,
                            self.get_constant(val_offset)
                        )
                        .as_str(),
                    );
                }
                Instruction::OpDefineGlobal(val_offset) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} {}",
                            "OP_DEFINE_GLOBAL",
                            val_offset,
                            self.get_constant(val_offset)
                        )
                        .as_str(),
                    );
                }
                Instruction::OpGetGlobal(val_offset) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} {}",
                            "OP_GET_GLOBAL",
                            val_offset,
                            self.get_constant(val_offset)
                        )
                        .as_str(),
                    );
                }
                Instruction::OpSetGlobal(val_offset) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} {}",
                            "OP_SET_GLOBAL",
                            val_offset,
                            self.get_constant(val_offset)
                        )
                        .as_str(),
                    );
                }
                Instruction::OpGetLocal(stack_slot) => {
                    result.push_str(format!("{:<20} {:04}", "OP_GET_LOCAL", stack_slot).as_str());
                }
                Instruction::OpSetLocal(stack_slot) => {
                    result.push_str(format!("{:<20} {:04}", "OP_SET_LOCAL", stack_slot).as_str());
                }

                Instruction::OpGetUpval(offset) => {
                    result.push_str(format!("{:<20} {:04}", "OP_GET_UPVAL", offset).as_str());
                }
                Instruction::OpSetUpval(offset) => {
                    result.push_str(format!("{:<20} {:04}", "OP_SET_UPVAL", offset).as_str());
                }

                Instruction::OpJumpIfFalse(jump) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} -> {:04}",
                            "OP_JUMP_IF_FALSE",
                            code_offset,
                            code_offset + 3 + jump as usize
                        )
                        .as_str(),
                    );
                }
                Instruction::OpJump(jump) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} -> {:04}",
                            "OP_JUMP",
                            code_offset,
                            code_offset + 3 + jump as usize
                        )
                        .as_str(),
                    );
                }
                Instruction::OpLoop(jump) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} -> {:04}",
                            "OP_LOOP",
                            code_offset,
                            code_offset + 3 - jump as usize
                        )
                        .as_str(),
                    );
                }
                Instruction::OpCall(args) => {
                    result.push_str(format!("{:<20} {:04}", "OP_CALL", args).as_str());
                }
                Instruction::OpClosure(val_offset) => {
                    result.push_str(
                        format!(
                            "{:<20} {:04} {}",
                            "OP_CLOSURE",
                            val_offset,
                            self.get_constant(val_offset)
                        )
                        .as_str(),
                    );
                }
                Instruction::OpCaptureLocal(stack_slot) => {
                    result
                        .push_str(format!("{:<20} {:04}", "OP_CAPTURE_LOCAL", stack_slot).as_str());
                }
                Instruction::OpCaptureUpval(stack_slot) => {
                    result
                        .push_str(format!("{:<20} {:04}", "OP_CAPTURE_UPVAL", stack_slot).as_str());
                }
                Instruction::OpCloseUpval => result.push_str("OP_CLOSE_UPVAL"),
                Instruction::OpNegate => result.push_str("OP_NEGATE"),
                Instruction::OpAdd => result.push_str("OP_ADD"),
                Instruction::OpSubtract => result.push_str("OP_SUBTRACT"),
                Instruction::OpMultiply => result.push_str("OP_MULTIPLY"),
                Instruction::OpDivide => result.push_str("OP_DIVIDE"),
                Instruction::OpTrue => result.push_str("OP_TRUE"),
                Instruction::OpFalse => result.push_str("OP_FALSE"),
                Instruction::OpNil => result.push_str("OP_NIL"),
                Instruction::OpNot => result.push_str("OP_NOT"),
                Instruction::OpEqual => result.push_str("OP_EQUAL"),
                Instruction::OpGreater => result.push_str("OP_GREATER"),
                Instruction::OpLess => result.push_str("OP_LESS"),
                Instruction::OpPrint => result.push_str("OP_PRINT"),
                Instruction::OpPop => result.push_str("OP_POP"),
                Instruction::OpReturn => result.push_str("OP_RETURN"),
                Instruction::OpInvalid => result.push_str("OP_INVALID"),
            }
            Some(result)
        } else {
            None
        }
    }
}

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let disassembler = Disassembler::new(chunk);
    for instr_str in disassembler {
        println!("{}", instr_str);
    }

    for fun in FunIter::new(chunk) {
        println!("");
        disassemble_chunk(&fun.chunk, &fun.name().0)
    }
}
