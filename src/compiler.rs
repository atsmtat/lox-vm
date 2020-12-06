use std::collections::HashMap;
use crate::value::Value;
use crate::chunk::{Chunk, Instruction};
use crate::debug;
use crate::scanner::{Scanner, Token, TokenKind, ScannerError};
use crate::error::InterpretError;

pub fn compile(source: &str, chunk: &mut Chunk) -> Result<(), InterpretError> {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner, chunk);
    parser.parse();
    if parser.had_error {
	Err(InterpretError::CompileError)
    } else {
	if cfg!(debug_assertions) {
	    debug::disassemble_chunk(chunk, "code");
	}
	Ok(())
    }
}

struct Parser<'a> {
    scanner: std::iter::Peekable<Scanner<'a>>,
    chunk: &'a mut Chunk,
    pub had_error: bool,
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
    Primary
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>, chunk: &'a mut Chunk) -> Self {
	Parser {
	    scanner: scanner.peekable(),
	    chunk,
	    had_error: false,
	    panic_mode: false,
	    curr_line: 0,
	}
    }

    pub fn parse(&mut self) {
	self.expression();
	if !self.had_error {
	    self.chunk.add_instruction(Instruction::OpReturn, self.curr_line);
	}
    }

    fn advance(&mut self) -> Option<Token<'a>> {
	while let Some(tok_or_err) = self.scanner.next() {
	    match tok_or_err {
		Ok(tok) => {
		    self.curr_line = tok.line;
		    return Some(tok);
		}
		Err(err) => { self.error_in_scan(err); }
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
		Err(err) => { self.error_in_scan(err); }
	    }
	}
	None
    }

    fn consume(&mut self, tok_kind: TokenKind) {
	match self.advance() {
	    Some(tok) => {
		if tok.kind != tok_kind {
		    let msg = format!( "mismatched token; expected {:?}, found {:?}",
					tok_kind, tok.kind );
		    self.report_error(tok.line, &msg);
		}
	    }
	    None => {
		let msg = format!( "mismatched token; expected {:?}, found EOF",
				    tok_kind );
		self.report_error(self.curr_line, &msg);
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

    fn report_error(&mut self, line: u32, msg: &str) {
	if self.panic_mode {
	    return;
	}
	eprintln!( "[line {}] Error: {}", line, msg );
	self.had_error = true;

	// to be reset at the next synchronization point
	self.panic_mode = true;
    }

    fn expression(&mut self) {
	self.parse_precedence(Precedence::Assignment as i32)
    }

    fn number(&mut self, tok: Token<'a>) {
	let val = tok.lexeme.parse::<f64>().unwrap();
	let const_ix = self.chunk.add_constant(Value::Double(val));
	let line = tok.line;
	if const_ix == u8::MAX - 1 {
	    self.report_error(line, "too many constants in one chunk");
	} else {
	    self.chunk.add_instruction(Instruction::OpConstant(const_ix), line);
	}
    }

    fn literal(&mut self, tok: Token<'a>) {
	match tok.kind {
	    TokenKind::True => { self.chunk.add_instruction(Instruction::OpTrue, tok.line); }
	    TokenKind::False => { self.chunk.add_instruction(Instruction::OpFalse, tok.line); }
	    TokenKind::Nil => { self.chunk.add_instruction(Instruction::OpNil, tok.line); }
	    _ => { self.report_error(tok.line, "invalid literal"); }
	    
	}
    }

    fn grouping(&mut self, _: Token<'a>) {
	self.expression();
	self.consume(TokenKind::RightParen);
    }

    fn unary(&mut self, tok: Token<'a>) {
	self.parse_precedence(Precedence::Unary as i32);
	match tok.kind {
	    TokenKind::Minus =>{ self.chunk.add_instruction(Instruction::OpNegate, tok.line); }
	    TokenKind::Bang =>{ self.chunk.add_instruction(Instruction::OpNot, tok.line); }
	    _ => { self.report_error(tok.line, "invalid prefix operator"); }
	}
    }

    fn binary(&mut self, tok: Token<'a>) {
	let my_prec = self.infix_prec(tok.kind);
	self.parse_precedence(my_prec as i32 + 1);

	match tok.kind {
	    TokenKind::Plus => self.chunk.add_instruction(Instruction::OpAdd, tok.line),
	    TokenKind::Minus => self.chunk.add_instruction(Instruction::OpSubtract, tok.line),
	    TokenKind::Star => self.chunk.add_instruction(Instruction::OpMultiply, tok.line),
	    TokenKind::Slash => self.chunk.add_instruction(Instruction::OpDivide, tok.line),
	    TokenKind::BangEqual => {
		self.chunk.add_instruction(Instruction::OpEqual, tok.line);
		self.chunk.add_instruction(Instruction::OpNot, tok.line);
	    }
	    TokenKind::EqualEqual => self.chunk.add_instruction(Instruction::OpEqual, tok.line),
	    TokenKind::Greater => self.chunk.add_instruction(Instruction::OpGreater, tok.line),
	    TokenKind::GreaterEqual => {
		self.chunk.add_instruction(Instruction::OpLess, tok.line);
		self.chunk.add_instruction(Instruction::OpNot, tok.line);
	    }
	    TokenKind::Less => self.chunk.add_instruction(Instruction::OpLess, tok.line),
	    TokenKind::LessEqual => {
		self.chunk.add_instruction(Instruction::OpGreater, tok.line);
		self.chunk.add_instruction(Instruction::OpNot, tok.line);
	    }
	    _ => { self.report_error(tok.line, "invalid binary operator"); }
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
	    TokenKind::True => Some(Self::literal),
	    TokenKind::False => Some(Self::literal),
	    TokenKind::Nil => Some(Self::literal),
	    TokenKind::Minus => Some(Self::unary),
	    TokenKind::Bang => Some(Self::unary),
	    _ => None,
	}
    }

    fn infix_prec(&self, tok_kind: TokenKind ) -> Precedence {
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

    fn infix_rule(&self, tok_kind: TokenKind) -> Option< fn(&mut Self, Token<'a>) > {
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
	    _ => None
	}
    }
}
