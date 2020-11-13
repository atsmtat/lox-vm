use std::str::CharIndices;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // single character
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // operators
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Identifier,
    String,
    Number(f64),

    // keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme : &'a str,
    pub line: u32,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, line: u32) -> Self {
	Token{kind, lexeme, line}
    }
}

#[derive(Debug)]
pub enum ScannerError {
    UnterminatedString,
    UnrecognizedChar,
}

pub struct Scanner<'a> {
    text : &'a str,
    iter : CharIndices<'a>,
    line : u32,
    keywords : trie::Trie<TokenKind>,
}

impl<'a> Scanner<'a> {
    pub fn new(text:&'a str) -> Self {
	let mut scanner = Scanner{ text, iter: text.char_indices(), line: 0, keywords: trie::Trie::new() };
	for (k,v) in vec![
	    ( "and", TokenKind::And ),
	    ( "class", TokenKind::Class ),
	    ( "else", TokenKind::Else),
	    ( "false", TokenKind::False),
	    ( "fun", TokenKind::Fun ),
	    ( "for", TokenKind::For ),
	    ( "if", TokenKind::If ),
	    ("nil", TokenKind::Nil ),
	    ( "or", TokenKind::Or ),
	    ("print", TokenKind::Print ),
	    ( "return", TokenKind::Return ),
	    ( "super", TokenKind::Super ),
	    ( "this", TokenKind::This ),
	    ( "true", TokenKind::True ),
	    ( "var", TokenKind::Var ),
	    ( "while", TokenKind::While ) ] {
	    scanner.keywords.insert(k,v);
	}
	scanner
    }

    // consumes next char and returns it
    fn advance(&mut self) -> Option<char> {
        if let Some((_, c)) = self.iter.next() {
	    Some(c)
	} else {
	    None
	}
    }

    // peeks nth char without consuming it, and returns it.
    // count starts from 0 -- 0 peeks current char
    fn peek_nth(&mut self, n: usize) -> Option<char> {
        if let Some((_, c)) = self.iter.clone().nth(n) {
	    Some(c)
	} else {
	    None
	}
    }

    fn peek(&mut self) -> Option<char> {
	self.peek_nth(0)
    }

    // advance if next char matches with a given char c
    fn advance_if(&mut self, c: char) -> bool {
        if let Some(nc) = self.peek() {
            if nc == c {
                self.advance();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn get_lexeme(&self) -> &'a str {
	if let Some((index, _)) = self.iter.clone().next() {
	    self.text.split_at(index).0
	} else {
	    self.text
	}
    }

    fn make_token(&mut self, token_kind: TokenKind) -> Token<'a> {
	Token::new(token_kind, self.get_lexeme(), self.line)
    }

    // eat chars till newline '\n' or eof is hit
    fn eat_comment(&mut self) {
        while let Some(nc) = self.peek() {
	    match nc {
		'\n' => return,
		_ => { self.advance(); }
	    }
        }
    }

    fn eat_whitespace(&mut self) {
        while let Some(nc) = self.peek() {
	    match nc {
		' ' | '\r' | '\t' => { self.advance(); }
		'\n' => {
                    self.line += 1;
		    self.advance();
		}
		'/' => {
		    if let Some(nnc)  = self.peek_nth(1) {
			match nnc {
			    '/' => self.eat_comment(),
			    _ => return,
			}
		    } else {
			return;
		    }
		}
		_ => return,
	    }
        }
    }

    fn string_literal(&mut self) -> Result<Token<'a>, ScannerError> {
	while let Some(c) = self.advance() {
	    match c {
		'"' => return Ok(self.make_token(TokenKind::String)),
		'\n' => self.line += 1,
		_ => {}
	    }
	}
	Err(ScannerError::UnterminatedString)
    }

    fn number_literal(&mut self) -> Token<'a> {
	while let Some('0'..='9') = self.peek() {
	    self.advance();
	}

	if Some('.') == self.peek() {
	    if let Some('0'..='9') = self.peek_nth(1) {
		self.advance();
		while let Some('0'..='9') = self.peek() {
		    self.advance();
		}
	    }
	}

	let lexeme = self.get_lexeme();
	let token_kind = TokenKind::Number(lexeme.parse::<f64>().unwrap());
	Token::new(token_kind, lexeme, self.line)
    }

    fn identifier(&mut self) -> Token<'a> {
	while let Some(c) = self.peek() {
	    match c {
		'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => { self.advance(); }
		_ => break,
	    }
	}

	let lexeme = self.get_lexeme();
	let mut token_kind = TokenKind::Identifier;
	if let Some(tr) = self.keywords.get(lexeme) {
	    token_kind = tr.clone();
	}

	Token::new(token_kind, lexeme, self.line)
    }

    fn scan_token(&mut self) -> Option<Result<Token<'a>, ScannerError>> {
	self.eat_whitespace();

	self.text = self.iter.as_str();
	self.iter = self.text.char_indices();
        let curr_char;
	match self.advance() {
	    Some(c) => curr_char = c,
	    None => return None,
	}

        let token = match curr_char {
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            ',' => self.make_token(TokenKind::Comma),
            '.' => self.make_token(TokenKind::Dot),
            '-' => self.make_token(TokenKind::Minus),
            '+' => self.make_token(TokenKind::Plus),
            ';' => self.make_token(TokenKind::Semicolon),
            '*' => self.make_token(TokenKind::Star),

            '!' => {
                if self.advance_if('=') {
                    self.make_token(TokenKind::BangEqual)
                } else {
                    self.make_token(TokenKind::Bang)
                }
            }
            '=' => {
                if self.advance_if('=') {
                    self.make_token(TokenKind::EqualEqual)
                } else {
                    self.make_token(TokenKind::Equal)
                }
            }
            '>' => {
                if self.advance_if('=') {
                    self.make_token(TokenKind::GreaterEqual)
                } else {
                    self.make_token(TokenKind::Greater)
                }
            }
            '<' => {
                if self.advance_if('=') {
                    self.make_token(TokenKind::LessEqual)
                } else {
                    self.make_token(TokenKind::Less)
                }
            }

            '/' => self.make_token(TokenKind::Slash),
	    '"' => return Some(self.string_literal()),
	    '0'..='9' => self.number_literal(),
	    'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
	    
	    _ => return Some(Err(ScannerError::UnrecognizedChar)),
	};

	Some(Ok(token))
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
	self.scan_token()
    }
}

mod trie {
    use std::collections::HashMap;

    struct TrieNode<V> {
	value : Option<V>,
	edges : HashMap<char, TrieNode<V>>,
    }

    impl<V> TrieNode<V> {
	fn new(value: Option<V>) -> Self {
	    TrieNode{ edges: HashMap::new(), value }
	}
    }

    pub struct Trie<V> {
	root : TrieNode<V>,
    }

    impl<V> Trie<V> {
	pub fn new() -> Self {
	    Trie{ root : TrieNode::new(None) }
	}

	pub fn get(&self, key: &str) -> Option<&V> {
	    let mut node = &(self.root);
	    for c in key.chars() {
		match node.edges.get(&c) {
		    Some(vr) => node = vr,
		    None => return None,
		}
	    }
	    node.value.as_ref()
	}

	pub fn insert(&mut self, key: &str, val:V) {
	    let mut node = &mut(self.root);

	    for c in key.chars() {
		if !node.edges.contains_key(&c) {
		    node.edges.insert(c, TrieNode::new(None));
		}
		node = node.edges.get_mut(&c).unwrap();
	    }
	    node.value = Some(val);
	}
    }
}

