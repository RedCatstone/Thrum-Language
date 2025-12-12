use core::fmt;
use std::{str::Chars, iter::Peekable};
// use memmap2::Mmap;

use crate::lexing::tokens::{LexerToken, TokenType, get_keyword};

pub mod tokens;



// pub fn tokenize_file(path: &str) -> Result<(Vec<LexerToken>, Vec<LexerError>), Box<dyn std::error::Error>> {
//     let file = File::open(path)?;
//     // memory-map the file. (unsafe because it could be modified by another process)
//     let mmap = unsafe { Mmap::map(&file)? };
//     // treat the memory-mapped bytes as a UTF-8 string slice. (zero-copy)
//     let source_code = std::str::from_utf8(&mmap)?;

//     Ok(tokenize_code(source_code))
// }


pub struct Lexer<'a> {
    source_iter: Peekable<Chars<'a>>,
    tokens: Vec<LexerToken>,
    byte_offset: usize,
    line: usize,
    errors: Vec<LexerError>
}

impl<'a> Lexer<'a> {
    fn new(source_code: &'a str) -> Self {
        Lexer {
            source_iter: source_code.chars().peekable(),
            tokens: Vec::new(),
            byte_offset: 0,
            line: 1,
            errors: Vec::new()
        }
    }

    pub fn tokenize_code(source_code: &str) -> (Vec<LexerToken>, Vec<LexerError>) {
        let mut lexer = Lexer::new(source_code);
        lexer.tokenize(None);
        (lexer.tokens, lexer.errors)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.source_iter.next();
        if let Some(ch) = c {
            self.byte_offset += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
            }
        }
        c
    }

    fn match_next(&mut self, expected: char) -> bool {
        if let Some(&next_char) = self.source_iter.peek()
            && next_char == expected {
                self.advance();
                return true;
            }
        false
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(LexerToken {
            token_type,
            line: self.line,
            byte_start: self.byte_offset,
            length: 0,
        });
    }

    fn add_error(&mut self, error_msg: String) {
        self.errors.push(LexerError { line: self.line, message: error_msg });
    }


    pub fn tokenize(&mut self, mut string_brace_level: Option<usize>) {
        while let Some(c) = self.advance() {
            match c {
                // Basic
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '[' => self.add_token(TokenType::LeftBracket),
                ']' => self.add_token(TokenType::RightBracket),
                '{' => {
                    self.add_token(TokenType::LeftBrace);
                    if let Some(s) = &mut string_brace_level {
                        *s += 1;
                    }
                }
                '}' => {
                    self.add_token(TokenType::RightBrace);
                    if let Some(s) = &mut string_brace_level {
                        *s -= 1;
                        if *s == 0 { return; }
                    }
                }

                ',' => self.add_token(TokenType::Comma),
                '.' => self.process_dot_token(),
                ';' => self.add_token(TokenType::Semicolon),
                ':' => {
                    let token = if self.match_next(':') { TokenType::ColonColon } else { TokenType::Colon };
                    self.add_token(token);
                }
                

                // Operators
                '+' => {
                    let token = if self.match_next('=') { TokenType::PlusEqual } else { TokenType::Plus };
                    self.add_token(token);
                }
                '-' => {
                    let token = if self.match_next('>') { TokenType::RightArrow }
                    else if self.match_next('=') { TokenType::MinusEqual } else { TokenType::Minus };
                    self.add_token(token);
                }
                '*' => {
                    let token = if self.match_next('*') {
                        if self.match_next('=') { TokenType::StarStarEqual } else { TokenType::StarStar }
                    } else if self.match_next('=') { TokenType::StarEqual } else { TokenType::Star };
                    self.add_token(token);
                }
                '/' => {
                    if self.match_next('/') {
                        // comment!!! consume until newline
                        while let Some(comment_char) = self.advance() {
                            if comment_char == '\n' { break; }
                        }
                    }
                    else if self.match_next('\\') {
                        // multi line comment, consume until '\/'
                        while let Some('\\') = self.advance() {
                            if self.match_next('/') { break; }
                        }
                    }
                    else {
                        let token = if self.match_next('=') { TokenType::SlashEqual } else { TokenType::Slash };
                        self.add_token(token);
                    }
                }
                '%' => {
                    let token = if self.match_next('=') { TokenType::PercentEqual } else { TokenType::Percent };
                    self.add_token(token);
                }
                '?' => {
                    let token = if self.match_next('=') { TokenType::QuestEqual }
                    else if self.match_next('.') { TokenType::QuestDot } else { TokenType::Quest };
                    self.add_token(token);
                }

                // Bitwise
                '~' => {
                    let token = if self.match_next('!') {
                        if self.match_next('=') { TokenType::BitNotEqual } else { TokenType::BitNot }
                    } else if self.match_next('&') {
                        if self.match_next('=') { TokenType::BitAndEqual } else { TokenType::BitAnd }
                    } else if self.match_next('|') {
                        if self.match_next('=') { TokenType::BitOrEqual } else { TokenType::BitOr }
                    } else if self.match_next('^') {
                        if self.match_next('=') { TokenType::BitXorEqual } else { TokenType::BitXor }
                    } else if self.match_next('>') {
                        if self.match_next('=') { TokenType::RightShiftEqual } else { TokenType::RightShift }
                    } else if self.match_next('<') {
                        if self.match_next('=') { TokenType::LeftShiftEqual } else { TokenType::LeftShift }
                    } else {
                        self.add_error("Unexpected character '~'. Did you mean '~!' (Bitwise Not)?".to_string());
                        continue;
                    };
                    self.add_token(token);
                },


                // Logical
                '&' => self.add_token(TokenType::Ampersand),
                '|' => {
                    let token = if self.match_next('>') { TokenType::PipeGreater } else { TokenType::Pipe };
                    self.add_token(token);
                }
                '^' => self.add_token(TokenType::Caret),
                '=' => {
                    let token = if self.match_next('=') { TokenType::EqualEqual } else { TokenType::Equal };
                    self.add_token(token);
                }
                '!' => {
                    let token = if self.match_next('=') { TokenType::NotEqual } else { TokenType::Exclamation };
                    self.add_token(token);
                }
                '<' => {
                    let token = if self.match_next('<') {
                        if self.match_next('=') { TokenType::LeftShiftEqual } else { TokenType::LeftShift }
                    } else if self.match_next('=') { TokenType::LessEqual } else { TokenType::Less };
                    self.add_token(token);
                },
                '>' => {
                    let token = if self.match_next('=') { TokenType::GreaterEqual } else { TokenType::Greater };
                    self.add_token(token);
                },

                '#' => self.add_token(TokenType::Hashtag),

                
                // Ignore whitespace/new lines
                ' ' | '\r' | '\t' | '\n' => (),

                // Handle string literals
                '"' => self.lex_string('"'),
                '\'' => self.lex_string('\''),

                // numbers
                _ if c.is_ascii_digit() => self.lex_number(c),

                // identifiers and keywords
                _ if c == '_' || c.is_alphabetic() => { self.lex_identifier(c); }

                _ => self.add_error(format!("Unexpected character '{}'.", c)),
            }
        }
    }

    fn lex_string(&mut self, quote: char) {
        self.add_token(TokenType::StringStart);
        let mut string = String::new();
        while let Some(c) = self.advance() {
            match c {
                _ if c == quote => {
                    // quote ends
                    if !string.is_empty() { self.add_token(TokenType::StringFrag(string)); }
                    self.add_token(TokenType::StringEnd);
                    return;
                }
                '{' => {
                    // start template string sttuff
                    if !string.is_empty() { self.add_token(TokenType::StringFrag(string)); }
                    string = String::new();
                    self.add_token(TokenType::LeftBrace);
                    self.tokenize(Some(1));

                    // back from recursion, meaning that it hit the correct '}'
                }
                '\\' => {
                    // backslashed chars
                    if let Some(backslash_c) = self.advance() {
                        match backslash_c {
                            'n' => string.push('\n'),
                            't' => string.push('\t'),
                            'r' => string.push('\r'),
                            '0' => string.push('\0'),
                            '\n' => {
                                // skip whitespaces on new line
                                string.push('\n');
                                while let Some(' ') = self.source_iter.peek() { self.advance(); }
                            }
                            any => string.push(any),
                        }
                    }
                }
                // normal char, just add it!
                any => string.push(any),
            }
        }
        self.add_error("Unterminated string.".to_string());
    }

    fn lex_number(&mut self, first_char: char) {
        let mut text = String::from(first_char);
        let mut already_has_dot = false;
        let mut process_dot_later = false;

        while let Some(&c) = self.source_iter.peek() {
            if c.is_ascii_digit() {
                text.push(c);
                self.advance();
            }
            else if c == '.' {
                if already_has_dot { break; }
                already_has_dot = true;
                self.advance();
                // '.' is only part of the number if the next character is a digit
                if let Some(after_dot) = self.source_iter.peek() {
                    if after_dot.is_ascii_digit() {
                        text.push('.');
                        text.push(*after_dot);
                        self.advance();
                    }
                    else {
                        process_dot_later = true;
                        break;
                    }
                }
            }
            else if c == '_' {
                self.advance();
            }
            else { break; }
        }
        match text.parse::<f64>() {
            Ok(num) => self.add_token(TokenType::Number(num)),
            Err(_) => self.add_error( format!("Could not parse number '{}'.", text)),
        }
        if process_dot_later { self.process_dot_token(); }
    }

    fn process_dot_token(&mut self) {
        // '.' already consumed.
        let token = if self.match_next('.') {
            if self.match_next('.') { TokenType::DotDotDot }
            else if self.match_next('<') { TokenType::DotDotLess }
            else { TokenType::DotDot }
        } else { TokenType::Dot };
        self.add_token(token);
    }
    
    fn lex_identifier(&mut self, first_char: char) {
        let mut text = String::from(first_char);
        
        while let Some(&c) = self.source_iter.peek() && (c == '_' || c.is_alphanumeric()) {
            text.push(c);
            self.advance();
        }
        
        // Check if the identifier is a reserved keyword
        if let Some(keyword_token) = get_keyword(&text) {
            self.add_token(keyword_token);
        }
        else { self.add_token(TokenType::Identifier(text)); }
    }
}



pub struct LexerError {
    pub line: usize,
    pub message: String,
}
impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.line, self.message)
    }
}