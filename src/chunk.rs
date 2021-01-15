use crate::value::Value;
use std::convert::TryInto;

const OP_CONSTANT: u8 = 1;
const OP_RETURN: u8 = 2;
const OP_NEGATE: u8 = 3;
const OP_ADD: u8 = 4;
const OP_SUBTRACT: u8 = 5;
const OP_MULTIPLY: u8 = 6;
const OP_DIVIDE: u8 = 7;
const OP_TRUE: u8 = 8;
const OP_FALSE: u8 = 9;
const OP_NIL: u8 = 10;
const OP_NOT: u8 = 11;
const OP_EQUAL: u8 = 12;
const OP_GREATER: u8 = 13;
const OP_LESS: u8 = 14;
const OP_PRINT: u8 = 15;
const OP_POP: u8 = 16;
const OP_DEFINE_GLOBAL: u8 = 17;
const OP_GET_GLOBAL: u8 = 18;
const OP_SET_GLOBAL: u8 = 19;
const OP_GET_LOCAL: u8 = 20;
const OP_SET_LOCAL: u8 = 21;
const OP_JUMP_IF_FALSE: u8 = 22;

const OP_INVALID: u8 = u8::MAX;

pub enum Instruction {
    OpConstant(u8),
    OpNegate,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpTrue,
    OpFalse,
    OpNil,
    OpNot,
    OpEqual,
    OpGreater,
    OpLess,
    OpPrint,
    OpPop,
    OpReturn,
    OpDefineGlobal(u8),
    OpGetGlobal(u8),
    OpSetGlobal(u8),
    OpGetLocal(u8),
    OpSetLocal(u8),
    OpJumpIfFalse(u16),
    OpInvalid,
}

impl From<Instruction> for Vec<u8> {
    fn from(instr: Instruction) -> Self {
        match instr {
            Instruction::OpConstant(offset) => vec![OP_CONSTANT, offset],
            Instruction::OpReturn => vec![OP_RETURN],
            Instruction::OpNegate => vec![OP_NEGATE],
            Instruction::OpAdd => vec![OP_ADD],
            Instruction::OpSubtract => vec![OP_SUBTRACT],
            Instruction::OpMultiply => vec![OP_MULTIPLY],
            Instruction::OpDivide => vec![OP_DIVIDE],
            Instruction::OpTrue => vec![OP_TRUE],
            Instruction::OpFalse => vec![OP_FALSE],
            Instruction::OpNil => vec![OP_NIL],
            Instruction::OpNot => vec![OP_NOT],
            Instruction::OpEqual => vec![OP_EQUAL],
            Instruction::OpGreater => vec![OP_GREATER],
            Instruction::OpLess => vec![OP_LESS],
            Instruction::OpPrint => vec![OP_PRINT],
            Instruction::OpPop => vec![OP_POP],
            Instruction::OpDefineGlobal(offset) => vec![OP_DEFINE_GLOBAL, offset],
            Instruction::OpGetGlobal(offset) => vec![OP_GET_GLOBAL, offset],
            Instruction::OpSetGlobal(offset) => vec![OP_SET_GLOBAL, offset],
            Instruction::OpGetLocal(offset) => vec![OP_GET_LOCAL, offset],
            Instruction::OpSetLocal(offset) => vec![OP_SET_LOCAL, offset],
            Instruction::OpJumpIfFalse(offset) => vec![
                OP_JUMP_IF_FALSE,
                ((offset >> 8) & 0xff).try_into().unwrap(),
                (offset & 0xff).try_into().unwrap(),
            ],
            Instruction::OpInvalid => vec![OP_INVALID],
        }
    }
}

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn push_instruction(&mut self, instr: Instruction, line: u32) -> usize {
        let mut bytes: Vec<u8> = instr.into();
        let instr_size = bytes.len();
        self.code.append(&mut bytes);
        for _i in 0..instr_size {
            self.lines.push(line);
        }
        self.code.len() - instr_size
    }

    pub fn push_constant(&mut self, v: Value) -> u8 {
        let offset = self.constants.len();
        self.constants.push(v);
        offset as u8
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    fn read_u8(&self, index: usize) -> u8 {
        self.code[index]
    }

    fn read_u16(&self, index: usize) -> u16 {
        let upper8: u16 = self.read_u8(index) as u16;
        let lower8 = self.read_u8(index + 1);
        (upper8 << 8) | lower8 as u16
    }

    pub fn read_instruction(&self, index: usize) -> (u8, Instruction) {
        match self.read_u8(index) {
            OP_CONSTANT => (2, Instruction::OpConstant(self.read_u8(index + 1))),
            OP_DEFINE_GLOBAL => (2, Instruction::OpDefineGlobal(self.read_u8(index + 1))),
            OP_GET_GLOBAL => (2, Instruction::OpGetGlobal(self.read_u8(index + 1))),
            OP_SET_GLOBAL => (2, Instruction::OpSetGlobal(self.read_u8(index + 1))),
            OP_GET_LOCAL => (2, Instruction::OpGetLocal(self.read_u8(index + 1))),
            OP_SET_LOCAL => (2, Instruction::OpSetLocal(self.read_u8(index + 1))),
            OP_JUMP_IF_FALSE => (3, Instruction::OpJumpIfFalse(self.read_u16(index + 1))),
            OP_NEGATE => (1, Instruction::OpNegate),
            OP_ADD => (1, Instruction::OpAdd),
            OP_SUBTRACT => (1, Instruction::OpSubtract),
            OP_MULTIPLY => (1, Instruction::OpMultiply),
            OP_DIVIDE => (1, Instruction::OpDivide),
            OP_TRUE => (1, Instruction::OpTrue),
            OP_FALSE => (1, Instruction::OpFalse),
            OP_NIL => (1, Instruction::OpNil),
            OP_NOT => (1, Instruction::OpNot),
            OP_EQUAL => (1, Instruction::OpEqual),
            OP_GREATER => (1, Instruction::OpGreater),
            OP_LESS => (1, Instruction::OpLess),
            OP_PRINT => (1, Instruction::OpPrint),
            OP_POP => (1, Instruction::OpPop),
            OP_RETURN => (1, Instruction::OpReturn),
            _ => (1, Instruction::OpInvalid),
        }
    }

    pub fn patch_jump_offset(&mut self, instr_ix: usize, offset: u16) {
        // instr_ix points to op code
        self.code[instr_ix + 1] = ((offset >> 8) & 0xff).try_into().unwrap();
        self.code[instr_ix + 2] = (offset & 0xff).try_into().unwrap();
    }

    pub fn get_constant(&self, index: u8) -> Value {
        self.constants[index as usize]
    }

    pub fn get_constant_checked(&self, index: u8) -> Option<&Value> {
        self.constants.get(index as usize)
    }

    pub fn get_line(&self, index: usize) -> u32 {
        self.lines[index]
    }

    pub fn iter(&self) -> InstructionIter {
        InstructionIter::new(&self)
    }
}

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
