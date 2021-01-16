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

struct Local<'a> {
    name_token: Token<'a>,
    depth: usize,
    initialized: bool,
}

impl<'a> Local<'a> {
    pub fn new_uninit(name_token: Token<'a>, depth: usize) -> Self {
        Local {
            name_token,
            depth,
            initialized: false,
        }
    }

    pub fn var_name(&self) -> &str {
        self.name_token.lexeme
    }
}

struct CompilerState<'a> {
    locals: Vec<Local<'a>>,
    scope_depth: usize,
}

enum LookupError {
    Unresolved,
    ResolvedUninit,
}

impl<'a> CompilerState<'a> {
    fn new() -> Self {
        CompilerState {
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    fn in_global_scope(&self) -> bool {
        self.scope_depth == 0
    }

    fn add_local(&mut self, name_token: Token<'a>) {
        self.locals
            .push(Local::new_uninit(name_token, self.scope_depth));
    }

    fn init_last_local(&mut self) {
        if let Some(last) = self.locals.last_mut() {
            last.initialized = true;
        }
    }

    fn locals_count(&self) -> usize {
        self.locals.len()
    }

    fn resolve_local(&self, name: &str) -> Result<u8, LookupError> {
        if let Some((r_ix, found)) = self
            .locals
            .iter()
            .rev()
            .enumerate()
            .find(|item| item.1.var_name() == name)
        {
            if !found.initialized {
                return Err(LookupError::ResolvedUninit);
            }
            return Ok((self.locals.len() - r_ix - 1) as u8);
        }
        return Err(LookupError::Unresolved);
    }
}

struct Parser<'a> {
    scanner: std::iter::Peekable<Scanner<'a>>,
    chunk: &'a mut Chunk,
    heap: &'a mut Heap,
    had_error: bool,
    panic_mode: bool,
    curr_line: u32,
    compiler_state: CompilerState<'a>,
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

type Parselet<'a> = fn(&mut Parser<'a>, Token<'a>, bool);

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>, chunk: &'a mut Chunk, heap: &'a mut Heap) -> Self {
        Parser {
            scanner: scanner.peekable(),
            chunk,
            heap,
            had_error: false,
            panic_mode: false,
            curr_line: 0,
            compiler_state: CompilerState::new(),
        }
    }

    pub fn parse(&mut self) -> bool {
        self.program();
        if !self.had_error {
            self.emit_instruction(Instruction::OpReturn, self.curr_line);
        }
        !self.had_error
    }

    // === compiler state management ===
    fn begin_scope(&mut self) {
        self.compiler_state.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler_state.scope_depth -= 1;
        let curr_depth = self.compiler_state.scope_depth;
        let mut drop_count = self
            .compiler_state
            .locals
            .iter()
            .rev()
            .take_while(|loc| loc.depth > curr_depth)
            .count();

        while drop_count > 0 {
            self.compiler_state.locals.pop();
            self.emit_instruction(Instruction::OpPop, self.curr_line);
            drop_count -= 1;
        }
    }

    // === code emitters ===
    fn emit_instruction(&mut self, instr: Instruction, line: u32) {
        self.chunk.push_instruction(instr, line);
    }

    fn emit_jump(&mut self, instr: Instruction, line: u32) -> usize {
        self.chunk.push_instruction(instr, line)
    }

    fn emit_constant(&mut self, val: Value) -> u8 {
        let const_ix = self.chunk.push_constant(val);
        if const_ix == u8::MAX - 1 {
            self.report_error(self.curr_line, "too many constants in one chunk");
        }
        const_ix
    }

    fn emit_identifier(&mut self, ident: String) -> u8 {
        let val = self.heap.allocate_string(ident);
        self.emit_constant(Value::String(val))
    }

    fn patch_jump(&mut self, instr_index: usize) {
        // count the jump starting from end of the instruction, which
        // is assumed to be of 3 bytes in size
        let jump = self.chunk.code_len() - instr_index - 3;
        if jump > u16::MAX as usize {
            self.report_error(self.curr_line, "too much code to jump over");
        }
        self.chunk.patch_jump_offset(instr_index, jump as u16);
    }

    // === parsing methods ===
    fn program(&mut self) {
        while !self.is_eof() {
            self.declaration();
            if self.panic_mode {
                self.synchronize();
            }
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
    }

    fn statement(&mut self) {
        match self.peek() {
            Some(tok) => match tok.kind {
                TokenKind::Print => {
                    self.advance();
                    self.print_statement();
                }
                TokenKind::LeftBrace => {
                    self.advance();
                    self.begin_scope();
                    self.block();
                    self.end_scope();
                }
                TokenKind::If => {
                    self.advance();
                    self.if_statement();
                }
                _ => {
                    self.expr_statement();
                }
            },
            None => {}
        }
    }

    fn var_decl(&mut self) {
        let ident_tok = self.consume(TokenKind::Identifier);
        if ident_tok.is_none() {
            return;
        }
        let ident_tok = ident_tok.unwrap();

        if !self.compiler_state.in_global_scope() {
            // declare local variable
            self.declare_local_var(ident_tok.clone());
        }

        if let Some(tok) = self.peek() {
            match tok.kind {
                TokenKind::Equal => {
                    self.advance();
                    self.expression();
                }
                _ => {
                    // if initializer expression is missing, assign
                    // nil as the initial value.
                    self.emit_instruction(Instruction::OpNil, ident_tok.line);
                }
            }
        }
        self.consume(TokenKind::Semicolon);

        // define variable
        if self.compiler_state.in_global_scope() {
            // global variable
            let var_name = ident_tok.lexeme.to_string();
            let var_line = ident_tok.line;
            // store variable name in constant table
            let offset = self.emit_identifier(var_name);
            self.emit_instruction(Instruction::OpDefineGlobal(offset), var_line);
        } else {
            // local variable
            self.compiler_state.init_last_local();
        }
    }

    fn declare_local_var(&mut self, var_tok: Token<'a>) {
        let line = var_tok.line;

        if self.compiler_state.locals_count() == (u8::MAX - 1) as usize {
            self.report_error(line, "too many local variables in function");
        }

        let var_name = var_tok.lexeme;
        let curr_depth = self.compiler_state.scope_depth;
        let dup_var = self
            .compiler_state
            .locals
            .iter()
            .rev()
            .take_while(|loc| curr_depth == loc.depth)
            .any(|loc| var_name == loc.var_name());

        if dup_var {
            self.report_error(
                line,
                &format!(
                    "variable with name {} already exists in this scope.",
                    var_name
                ),
            );
        }

        self.compiler_state.add_local(var_tok.clone());
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

    fn if_statement(&mut self) {
        self.consume(TokenKind::LeftParen);
        self.expression();
        self.consume(TokenKind::RightParen);

        let then_jump = self.emit_jump(Instruction::OpJumpIfFalse(u16::MAX), self.curr_line);

        // pop the condition value first if we fall-through
        self.emit_instruction(Instruction::OpPop, self.curr_line);
        self.statement();

        let else_jump = self.emit_jump(Instruction::OpJump(u16::MAX), self.curr_line);
        self.patch_jump(then_jump);

        // pop the condition value right after then-jump and before
        // running else (this adds an implicit else doing just the pop
        // if user has skipped the else)
        self.emit_instruction(Instruction::OpPop, self.curr_line);

        if self.consume_if(TokenKind::Else).is_some() {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn block(&mut self) {
        while let Some(tok) = self.peek() {
            if tok.kind == TokenKind::RightBrace {
                break;
            }
            self.declaration();
        }
        self.consume(TokenKind::RightBrace);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment as i32)
    }

    fn number(&mut self, tok: Token<'a>, _: bool) {
        let val = tok.lexeme.parse::<f64>().unwrap();
        let offset = self.emit_constant(Value::Double(val));
        self.emit_instruction(Instruction::OpConstant(offset), tok.line);
    }

    fn string(&mut self, tok: Token<'a>, _: bool) {
        // trim enclosing '"' from the input string
        let string_lit = tok.lexeme.trim_matches('"').to_string();

        // allocate gc string object on the heap
        let str_obj = self.heap.allocate_string(string_lit);

        let offset = self.emit_constant(Value::String(str_obj));
        self.emit_instruction(Instruction::OpConstant(offset), tok.line);
    }

    fn variable(&mut self, tok: Token<'a>, can_assign: bool) {
        let var_name = tok.lexeme.to_string();
        let is_assign = can_assign && self.consume_if(TokenKind::Equal).is_some();

        match self.compiler_state.resolve_local(&var_name) {
            Ok(offset) => {
                // found a local variable
                if is_assign {
                    self.expression();
                    self.emit_instruction(Instruction::OpSetLocal(offset), tok.line);
                } else {
                    self.emit_instruction(Instruction::OpGetLocal(offset), tok.line);
                }
            }
            Err(err) => match err {
                LookupError::Unresolved => {
                    // no local with this name, assume it's a global
                    let offset = self.emit_identifier(var_name);
                    if is_assign {
                        self.expression();
                        self.emit_instruction(Instruction::OpSetGlobal(offset), tok.line);
                    } else {
                        self.emit_instruction(Instruction::OpGetGlobal(offset), tok.line);
                    }
                }
                LookupError::ResolvedUninit => {
                    self.report_error(
                        tok.line,
                        &format!(
                            "can't read local variable {} in its own initializer",
                            var_name
                        ),
                    );
                }
            },
        }
    }

    fn literal(&mut self, tok: Token<'a>, _: bool) {
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

    fn grouping(&mut self, _: Token<'a>, _: bool) {
        self.expression();
        self.consume(TokenKind::RightParen);
    }

    fn unary(&mut self, tok: Token<'a>, _: bool) {
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

    fn binary(&mut self, tok: Token<'a>, _: bool) {
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

    fn and(&mut self, tok: Token<'a>, _: bool) {
        let end_jump = self.emit_jump(Instruction::OpJumpIfFalse(u16::MAX), tok.line);

        self.emit_instruction(Instruction::OpPop, tok.line);
        let my_prec = self.infix_prec(tok.kind);
        self.parse_precedence(my_prec as i32 + 1);

        self.patch_jump(end_jump);
    }

    fn or(&mut self, tok: Token<'a>, _: bool) {
        let else_jump = self.emit_jump(Instruction::OpJumpIfFalse(u16::MAX), tok.line);
        let end_jump = self.emit_jump(Instruction::OpJump(u16::MAX), tok.line);

        self.patch_jump(else_jump);
        self.emit_instruction(Instruction::OpPop, tok.line);

        let my_prec = self.infix_prec(tok.kind);
        self.parse_precedence(my_prec as i32 + 1);

        self.patch_jump(end_jump);
    }

    fn parse_precedence(&mut self, prec: i32) {
        let tok = self.advance();
        if tok.is_none() {
            self.report_error(self.curr_line, "expected expression; found EOF");
            return;
        }
        let tok = tok.unwrap();

        // individual parselets look for next assignment token, only
        // if they're told from here.
        let can_assign = prec <= Precedence::Assignment as i32;

        // parse prefix
        if let Some(parselet) = self.prefix_rule(tok.kind) {
            parselet(self, tok, can_assign);
        } else {
            self.report_error(tok.line, "expected expression");
        }

        // parse infix
        while let Some(tok) = self.peek() {
            let next_prec = self.infix_prec(tok.kind);
            if prec <= (next_prec as i32) {
                let parselet = self.infix_rule(tok.kind).unwrap();
                self.advance();
                parselet(self, tok, can_assign);
            } else {
                break;
            }
        }

        if can_assign {
            if let Some(assign) = self.consume_if(TokenKind::Equal) {
                self.report_error(assign.line, "invalid assignment target");
            }
        }
    }

    fn prefix_rule(&self, tok_kind: TokenKind) -> Option<Parselet<'a>> {
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
            TokenKind::And => Precedence::And,
            TokenKind::Or => Precedence::Or,
            _ => Precedence::None,
        }
    }

    fn infix_rule(&self, tok_kind: TokenKind) -> Option<Parselet<'a>> {
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
            TokenKind::And => Some(Self::and),
            TokenKind::Or => Some(Self::or),
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

    fn consume(&mut self, tok_kind: TokenKind) -> Option<Token<'a>> {
        match self.advance() {
            Some(tok) => {
                if tok.kind != tok_kind {
                    let msg = format!(
                        "mismatched token; expected {:?}, found {:?}",
                        tok_kind, tok.kind
                    );
                    self.report_error(tok.line, &msg);
                    return None;
                }
                return Some(tok);
            }
            None => {
                let msg = format!("mismatched token; expected {:?}, found EOF", tok_kind);
                self.report_error(self.curr_line, &msg);
                return None;
            }
        }
    }

    fn consume_if(&mut self, tok_kind: TokenKind) -> Option<Token<'a>> {
        if let Some(tok) = self.peek() {
            if tok.kind == tok_kind {
                self.advance();
                return Some(tok);
            }
        }
        None
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
