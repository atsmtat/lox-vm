const OP_CONSTANT: u8 = 1;
const OP_RETURN: u8 = 2;
const OP_NEGATE: u8 = 3;
const OP_INVALID: u8 = u8::MAX;

pub enum Instruction {
    OpConstant(u8),
    OpNegate,
    OpReturn,
    OpInvalid,
}

impl From<Instruction> for Vec<u8> {
    fn from(instr: Instruction) -> Self {
        match instr {
            Instruction::OpConstant(offset) => vec![OP_CONSTANT, offset],
	    Instruction::OpNegate => vec![OP_NEGATE],
            Instruction::OpReturn => vec![OP_RETURN],
            Instruction::OpInvalid => vec![OP_INVALID],
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Double(f64),
}

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    pub lines: Vec<u32>,
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
	    OP_NEGATE => Instruction::OpNegate,
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
