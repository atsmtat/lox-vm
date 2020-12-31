use crate::value::Value;

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

    pub fn add_instruction(&mut self, instr: Instruction, line: u32) {
        let mut bytes: Vec<u8> = instr.into();
        self.code.append(&mut bytes);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, v: Value) -> u8 {
        let offset = self.constants.len();
        self.constants.push(v);
        offset as u8
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
        InstructionIter::new(&self.code)
    }
}

pub struct InstructionIter<'a> {
    code_iter: std::slice::Iter<'a, u8>,
    code_size: usize,
}

impl<'a> InstructionIter<'a> {
    pub fn new(code: &'a [u8]) -> Self {
        InstructionIter {
            code_iter: code.iter(),
            code_size: code.len(),
        }
    }

    pub fn code_offset(&self) -> usize {
        self.code_size - self.code_iter.len()
    }

    pub fn with_code_offset(self) -> InstructionOffsetIter<'a> {
        InstructionOffsetIter::new(self)
    }
}

impl<'a> Iterator for InstructionIter<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        let opcode = match self.code_iter.next() {
            None => return None,
            Some(b) => *b,
        };

        let instr = match opcode {
            OP_CONSTANT => {
                if let Some(offset) = self.code_iter.next() {
                    Instruction::OpConstant(*offset)
                } else {
                    Instruction::OpInvalid
                }
            }
            OP_DEFINE_GLOBAL => {
                if let Some(offset) = self.code_iter.next() {
                    Instruction::OpDefineGlobal(*offset)
                } else {
                    Instruction::OpInvalid
                }
            }
            OP_GET_GLOBAL => {
                if let Some(offset) = self.code_iter.next() {
                    Instruction::OpGetGlobal(*offset)
                } else {
                    Instruction::OpInvalid
                }
            }
            OP_NEGATE => Instruction::OpNegate,
            OP_ADD => Instruction::OpAdd,
            OP_SUBTRACT => Instruction::OpSubtract,
            OP_MULTIPLY => Instruction::OpMultiply,
            OP_DIVIDE => Instruction::OpDivide,
            OP_TRUE => Instruction::OpTrue,
            OP_FALSE => Instruction::OpFalse,
            OP_NIL => Instruction::OpNil,
            OP_NOT => Instruction::OpNot,
            OP_EQUAL => Instruction::OpEqual,
            OP_GREATER => Instruction::OpGreater,
            OP_LESS => Instruction::OpLess,
            OP_PRINT => Instruction::OpPrint,
            OP_POP => Instruction::OpPop,
            OP_RETURN => Instruction::OpReturn,
            _ => Instruction::OpInvalid,
        };
        Some(instr)
    }
}

pub struct InstructionOffsetIter<'a> {
    instr_iter: InstructionIter<'a>,
}

impl<'a> InstructionOffsetIter<'a> {
    pub fn new(instr_iter: InstructionIter<'a>) -> Self {
        InstructionOffsetIter { instr_iter }
    }
}

impl<'a> Iterator for InstructionOffsetIter<'a> {
    type Item = (usize, Instruction);

    fn next(&mut self) -> Option<Self::Item> {
        let code_offset = self.instr_iter.code_offset();
        if let Some(instr) = self.instr_iter.next() {
            Some((code_offset, instr))
        } else {
            None
        }
    }
}
