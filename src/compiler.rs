use crate::chunk::{Chunk, Instruction};
use crate::debug;
use crate::memory::Heap;
use crate::scanner::{Scanner, ScannerError, Token, TokenKind};
use crate::value::Value;

pub fn compile(source: &str, chunk: &mut Chunk, heap: &mut Heap) -> Option<()> {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner, chunk, heap);
    if parser.parse() {
        if cfg!(debug_assertions) {
            debug::disassemble_chunk(chunk, "code");
        }
        Some(())
    } else {
        None
    }
}

struct Parser<'a> {
    scanner: std::iter::Peekable<Scanner<'a>>,
    chunk: &'a mut Chunk,
    heap: &'a mut Heap,
    had_error: bool,
    panic_mode: bool,
    curr_line: u32,
}

#[derive(Copy, Clone)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>, chunk: &'a mut Chunk, heap: &'a mut Heap) -> Self {
        Parser {
            scanner: scanner.peekable(),
            chunk,
            heap,
            had_error: false,
            panic_mode: false,
            curr_line: 0,
        }
    }

    pub fn parse(&mut self) -> bool {
        self.program();
        if !self.had_error {
            self.emit_instruction(Instruction::OpReturn, self.curr_line);
        }
        !self.had_error
    }

    // === code emitters ===
    fn emit_instruction(&mut self, instr: Instruction, line: u32) {
        self.chunk.add_instruction(instr, line);
    }

    fn emit_constant(&mut self, val: Value) -> u8 {
        let const_ix = self.chunk.add_constant(val);
        if const_ix == u8::MAX - 1 {
            self.report_error(self.curr_line, "too many constants in one chunk");
        }
        const_ix
    }

    fn emit_identifier(&mut self, ident: String) -> u8 {
        let val = self.heap.allocate_string(ident);
        self.emit_constant(Value::String(val))
    }

    // === parsing methods ===
    fn program(&mut self) {
        while !self.is_eof() {
            self.declaration();
        }
    }

    fn declaration(&mut self) {
        match self.peek() {
            Some(tok) => match tok.kind {
                TokenKind::Var => {
                    self.advance();
                    self.var_decl();
                }
                _ => {
                    self.statement();
                }
            },
            None => {
                self.unexpected_eof();
            }
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        match self.peek() {
            Some(tok) => match tok.kind {
                TokenKind::Print => {
                    self.advance();
                    self.print_statement();
                }
                _ => {
                    self.expr_statement();
                }
            },
            None => {}
        }
    }

    fn var_decl(&mut self) {
        let (ident, line) = self.consume_identifier();

        if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Equal => {
                    self.advance();
                    self.expression();
                }
                _ => {
                    // if initializer expression is missing, assign
                    // nil as the initial value.
                    self.emit_instruction(Instruction::OpNil, line);
                }
            }
        }
        self.consume(TokenKind::Semicolon);

        // store variable name in constant table
        let offset = self.emit_identifier(ident);
        self.emit_instruction(Instruction::OpDefineGlobal(offset), line);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon);
        self.emit_instruction(Instruction::OpPrint, self.curr_line);
    }

    fn expr_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon);
        self.emit_instruction(Instruction::OpPop, self.curr_line);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment as i32)
    }

    fn number(&mut self, tok: Token<'a>) {
        let val = tok.lexeme.parse::<f64>().unwrap();
        let offset = self.emit_constant(Value::Double(val));
        self.emit_instruction(Instruction::OpConstant(offset), tok.line);
    }

    fn string(&mut self, tok: Token<'a>) {
        // trim enclosing '"' from the input string
        let string_lit = tok.lexeme.trim_matches('"').to_string();

        // allocate gc string object on the heap
        let str_obj = self.heap.allocate_string(string_lit);

        let offset = self.emit_constant(Value::String(str_obj));
        self.emit_instruction(Instruction::OpConstant(offset), tok.line);
    }

    fn variable(&mut self, tok: Token<'a>) {
        let var_name = tok.lexeme.to_string();
        let offset = self.emit_identifier(var_name);
        self.emit_instruction(Instruction::OpGetGlobal(offset), tok.line);
    }

    fn literal(&mut self, tok: Token<'a>) {
        match tok.kind {
            TokenKind::True => {
                self.emit_instruction(Instruction::OpTrue, tok.line);
            }
            TokenKind::False => {
                self.emit_instruction(Instruction::OpFalse, tok.line);
            }
            TokenKind::Nil => {
                self.emit_instruction(Instruction::OpNil, tok.line);
            }
            _ => {
                self.report_error(tok.line, "invalid literal");
            }
        }
    }

    fn grouping(&mut self, _: Token<'a>) {
        self.expression();
        self.consume(TokenKind::RightParen);
    }

    fn unary(&mut self, tok: Token<'a>) {
        self.parse_precedence(Precedence::Unary as i32);
        match tok.kind {
            TokenKind::Minus => {
                self.emit_instruction(Instruction::OpNegate, tok.line);
            }
            TokenKind::Bang => {
                self.emit_instruction(Instruction::OpNot, tok.line);
            }
            _ => {
                self.report_error(tok.line, "invalid prefix operator");
            }
        }
    }

    fn binary(&mut self, tok: Token<'a>) {
        let my_prec = self.infix_prec(tok.kind);
        self.parse_precedence(my_prec as i32 + 1);

        match tok.kind {
            TokenKind::Plus => self.emit_instruction(Instruction::OpAdd, tok.line),
            TokenKind::Minus => self.emit_instruction(Instruction::OpSubtract, tok.line),
            TokenKind::Star => self.emit_instruction(Instruction::OpMultiply, tok.line),
            TokenKind::Slash => self.emit_instruction(Instruction::OpDivide, tok.line),
            TokenKind::BangEqual => {
                self.emit_instruction(Instruction::OpEqual, tok.line);
                self.emit_instruction(Instruction::OpNot, tok.line);
            }
            TokenKind::EqualEqual => self.emit_instruction(Instruction::OpEqual, tok.line),
            TokenKind::Greater => self.emit_instruction(Instruction::OpGreater, tok.line),
            TokenKind::GreaterEqual => {
                self.emit_instruction(Instruction::OpLess, tok.line);
                self.emit_instruction(Instruction::OpNot, tok.line);
            }
            TokenKind::Less => self.emit_instruction(Instruction::OpLess, tok.line),
            TokenKind::LessEqual => {
                self.emit_instruction(Instruction::OpGreater, tok.line);
                self.emit_instruction(Instruction::OpNot, tok.line);
            }
            _ => {
                self.report_error(tok.line, "invalid binary operator");
            }
        }
    }

    fn parse_precedence(&mut self, prec: i32) {
        let tok = self.advance();
        if tok.is_none() {
            self.report_error(self.curr_line, "expected expression; found EOF");
            return;
        }
        let tok = tok.unwrap();

        // parse prefix
        if let Some(parselet) = self.prefix_rule(tok.kind) {
            parselet(self, tok);
        } else {
            self.report_error(tok.line, "expected expression");
        }

        // parse infix
        while let Some(tok) = self.peek() {
            let next_prec = self.infix_prec(tok.kind);
            if prec <= (next_prec as i32) {
                let parselet = self.infix_rule(tok.kind).unwrap();
                self.advance();
                parselet(self, tok);
            } else {
                break;
            }
        }
    }

    fn prefix_rule(&self, tok_kind: TokenKind) -> Option<fn(&mut Self, Token<'a>)> {
        match tok_kind {
            TokenKind::LeftParen => Some(Self::grouping),
            TokenKind::Number => Some(Self::number),
            TokenKind::String => Some(Self::string),
            TokenKind::Identifier => Some(Self::variable),
            TokenKind::True => Some(Self::literal),
            TokenKind::False => Some(Self::literal),
            TokenKind::Nil => Some(Self::literal),
            TokenKind::Minus => Some(Self::unary),
            TokenKind::Bang => Some(Self::unary),
            _ => None,
        }
    }

    fn infix_prec(&self, tok_kind: TokenKind) -> Precedence {
        match tok_kind {
            TokenKind::Minus => Precedence::Term,
            TokenKind::Plus => Precedence::Term,
            TokenKind::Star => Precedence::Factor,
            TokenKind::Slash => Precedence::Factor,
            TokenKind::BangEqual => Precedence::Equality,
            TokenKind::EqualEqual => Precedence::Equality,
            TokenKind::Greater => Precedence::Comparison,
            TokenKind::GreaterEqual => Precedence::Comparison,
            TokenKind::Less => Precedence::Comparison,
            TokenKind::LessEqual => Precedence::Comparison,
            _ => Precedence::None,
        }
    }

    fn infix_rule(&self, tok_kind: TokenKind) -> Option<fn(&mut Self, Token<'a>)> {
        match tok_kind {
            TokenKind::Minus => Some(Self::binary),
            TokenKind::Plus => Some(Self::binary),
            TokenKind::Star => Some(Self::binary),
            TokenKind::Slash => Some(Self::binary),
            TokenKind::BangEqual => Some(Self::binary),
            TokenKind::EqualEqual => Some(Self::binary),
            TokenKind::Greater => Some(Self::binary),
            TokenKind::GreaterEqual => Some(Self::binary),
            TokenKind::Less => Some(Self::binary),
            TokenKind::LessEqual => Some(Self::binary),
            _ => None,
        }
    }

    // === parse utils ===
    fn advance(&mut self) -> Option<Token<'a>> {
        while let Some(tok_or_err) = self.scanner.next() {
            match tok_or_err {
                Ok(tok) => {
                    self.curr_line = tok.line;
                    return Some(tok);
                }
                Err(err) => {
                    self.error_in_scan(err);
                }
            }
        }
        None
    }

    fn peek(&mut self) -> Option<Token<'a>> {
        while let Some(tok_or_err) = self.scanner.peek() {
            match tok_or_err.clone() {
                Ok(tok) => {
                    return Some(tok);
                }
                Err(err) => {
                    self.error_in_scan(err);
                }
            }
        }
        None
    }

    fn consume(&mut self, tok_kind: TokenKind) {
        match self.advance() {
            Some(tok) => {
                if tok.kind != tok_kind {
                    let msg = format!(
                        "mismatched token; expected {:?}, found {:?}",
                        tok_kind, tok.kind
                    );
                    self.report_error(tok.line, &msg);
                }
            }
            None => {
                let msg = format!("mismatched token; expected {:?}, found EOF", tok_kind);
                self.report_error(self.curr_line, &msg);
            }
        }
    }

    fn consume_identifier(&mut self) -> (String, u32) {
        match self.advance() {
            Some(tok) => match tok.kind {
                TokenKind::Identifier => (tok.lexeme.to_string(), tok.line),
                _ => {
                    let msg = format!(
                        "mismatched token; expected {:?}, found {:?}",
                        TokenKind::Identifier,
                        tok.kind
                    );
                    self.report_error(tok.line, &msg);
                    ("".to_string(), 0)
                }
            },
            None => {
                let msg = format!(
                    "mismatched token; expected {:?}, found EOF",
                    TokenKind::Identifier
                );
                self.report_error(self.curr_line, &msg);
                ("".to_string(), 0)
            }
        }
    }

    fn error_in_scan(&mut self, err: ScannerError) {
        match err {
            ScannerError::UnterminatedString(line) => {
                self.report_error(line, "unterminated string literal");
            }
            ScannerError::UnrecognizedChar(line) => {
                self.report_error(line, "unrecognized character");
            }
        }
    }

    fn unexpected_eof(&mut self) {
        self.report_error(self.curr_line, "unexpected EOF");
    }

    fn report_error(&mut self, line: u32, msg: &str) {
        if self.panic_mode {
            return;
        }
        eprintln!("[line {}] Error: {}", line, msg);
        self.had_error = true;

        // to be reset at the next synchronization point
        self.panic_mode = true;
    }

    fn is_eof(&mut self) -> bool {
        self.scanner.peek().is_none()
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Fun
                | TokenKind::Class
                | TokenKind::If
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Var
                | TokenKind::Print
                | TokenKind::Return => {
                    return;
                }
                TokenKind::Semicolon => {
                    self.advance();
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }
}
