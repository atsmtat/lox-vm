mod chunk {
    const OP_CONSTANT: u8 = 1;
    const OP_RETURN: u8 = 2;
    const OP_INVALID: u8 = u8::MAX;

    pub enum Instruction {
        OpConstant(u8),
        OpReturn,
        OpInvalid,
    }

    impl From<Instruction> for Vec<u8> {
        fn from(instr: Instruction) -> Self {
            match instr {
                Instruction::OpConstant(offset) => vec![OP_CONSTANT, offset],
                Instruction::OpReturn => vec![OP_RETURN],
                Instruction::OpInvalid => vec![OP_INVALID],
            }
        }
    }

    #[derive(Debug)]
    pub enum Value {
        Double(f64),
    }

    pub struct Chunk {
        code: Vec<u8>,
        pub constants: Vec<Value>,
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
}

mod debug {
    use crate::chunk::{Chunk, Instruction, InstructionOffsetIter};

    pub struct Disassembler<'a> {
        chunk: &'a Chunk,
        iter: std::iter::Enumerate<InstructionOffsetIter<'a>>,
    }

    impl<'a> Disassembler<'a> {
        pub fn new(chunk: &'a Chunk) -> Self {
            Disassembler {
                iter: chunk.iter().with_code_offset().enumerate(),
                chunk,
            }
        }

        fn get_line(&self, instr_index: usize) -> String {
            if instr_index > 0 && self.chunk.lines[instr_index] == self.chunk.lines[instr_index - 1]
            {
                format!("{:>4} ", "|")
            } else {
                format!("{:04} ", self.chunk.lines[instr_index])
            }
        }
    }

    impl<'a> Iterator for Disassembler<'a> {
        type Item = String;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some((instr_index, (code_offset, instr))) = self.iter.next() {
                let mut result = format!("{:04} {} ", code_offset, self.get_line(instr_index));
                match instr {
                    Instruction::OpConstant(val_offset) => {
                        result.push_str(
                            format!("OP_CONSTANT {:>16}", format!("{:04}", val_offset)).as_str(),
                        );
                        if let Some(val) = self.chunk.constants.get(val_offset as usize) {
                            result.push_str(format!(" '{:?}'", val).as_str());
                        } else {
                            result.push_str("<out-of-bound>");
                        }
                    }
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
    }
}

fn main() {
    let mut chunk = chunk::Chunk::new();
    let val_offset = chunk.add_constant(chunk::Value::Double(4.2));
    chunk.add_instruction(chunk::Instruction::OpConstant(val_offset), 41);

    let val_offset = chunk.add_constant(chunk::Value::Double(1.2));
    chunk.add_instruction(chunk::Instruction::OpConstant(val_offset), 42);

    chunk.add_instruction(chunk::Instruction::OpReturn, 42);
    debug::disassemble_chunk(&chunk, "Test chunk");
}
