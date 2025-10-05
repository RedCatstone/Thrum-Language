use core::fmt;
use std::{fs::File, str::Chars, iter::Peekable};
use memmap2::Mmap;

use crate::tokens::{LexerToken, TokenType, get_keyword};



pub fn tokenize_file(path: &str) -> Result<(Vec<LexerToken>, Vec<LexerError>), Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    // memory-map the file. (unsafe because it could be modified by another process)
    let mmap = unsafe { Mmap::map(&file)? };
    // treat the memory-mapped bytes as a UTF-8 string slice. (zero-copy)
    let source_code = std::str::from_utf8(&mmap)?;

    Ok(tokenize_code(source_code))
}

fn tokenize_code(source_code: &str) -> (Vec<LexerToken>, Vec<LexerError>) {
    let mut lexer = Lexer::new(source_code);
    lexer.tokenize(None);
    (lexer.tokens, lexer.errors)
}


pub struct Lexer<'a> {
    source_iter: Peekable<Chars<'a>>,
    pub tokens: Vec<LexerToken>,
    line: usize,
    pub errors: Vec<LexerError>
}

#[derive(Debug)]
pub struct LexerError {
    pub line: usize,
    pub message: String,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.line, self.message)
    }
}

impl<'a> Lexer<'a> {
    fn new(source_code: &'a str) -> Self {
        Lexer {
            source_iter: source_code.chars().peekable(),
            tokens: Vec::new(),
            line: 0,
            errors: Vec::new()
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(LexerToken { token_type, line: self.line });
    }

    fn add_error(&mut self, error_msg: String) {
        self.errors.push(LexerError { line: self.line, message: error_msg });
    }

    fn match_next(&mut self, expected: char) -> bool {
        if let Some(&next_char) = self.source_iter.peek() {
            if next_char == expected {
                self.source_iter.next();
                return true;
            }
        }
        false
    }

    pub fn tokenize(&mut self, mut string_brace_level: Option<usize>) {
        while let Some(c) = self.source_iter.next() {
            match c {
                // Basic
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
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
                '[' => self.add_token(TokenType::LeftBracket),
                ']' => self.add_token(TokenType::RightBracket),

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
                    } else {
                        if self.match_next('=') { TokenType::StarEqual } else { TokenType::Star }
                    };
                    self.add_token(token);
                }
                '/' => {
                    if self.match_next('/') {
                        // It's a comment, consume until newline
                        while let Some(comment_char) = self.source_iter.next() {
                            if comment_char == '\n' {
                                self.line += 1;
                                break;
                            }
                        }
                    }
                    else if self.match_next('*') {
                        while let Some(comment_char) = self.source_iter.next() {
                            if comment_char == '*' {
                                if let Some('/') = self.source_iter.peek() {
                                    self.source_iter.next(); // Consume the '/'
                                    break;
                                }
                            }
                            else if comment_char == '\n' { self.line += 1; }
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

                
                // Ignore whitespace
                ' ' | '\r' | '\t' => (),

                '\n' => self.line += 1,

                // Handle string literals
                '"' => self.lex_string('"'),
                '\'' => self.lex_string('\''),

                _ => {
                    // numbers
                    if c.is_ascii_digit() { self.lex_number(c); }

                    // identifiers and keywords
                    else if c.is_alphabetic() || c == '_' { self.lex_identifier(c); }
                    else { self.add_error(format!("Unexpected character '{}'.", c)); }
                }
            }
        }
    }

    fn lex_string(&mut self, quote: char) {
        self.add_token(TokenType::StringStart);
        let mut string = String::new();
        while let Some(c) = self.source_iter.next() {
            match c {
                c if c == quote => {
                    if !string.is_empty() { self.add_token(TokenType::StringFrag(string)); }
                    self.add_token(TokenType::StringEnd);
                    return;
                }
                '{' => {
                    if !string.is_empty() { self.add_token(TokenType::StringFrag(string)); }
                    string = String::new();
                    self.add_token(TokenType::LeftBrace);
                    self.tokenize(Some(1));
                    continue;
                }
                '\n' => self.line += 1,
                '\\' => {
                    if let Some(backslash_c) = self.source_iter.next() {
                        match backslash_c {
                            'n' => string.push('\n'),
                            '\n' => {
                                string.push('\n');
                                while let Some(' ') = self.source_iter.peek() { self.source_iter.next(); }
                            }
                            any => string.push(any),
                        }
                    }
                    continue;
                }
                _ => { }
            }
            string.push(c);
        }
        self.add_error(format!("Unterminated string."));
    }

    fn lex_number(&mut self, first_char: char) {
        let mut text = String::from(first_char);
        let mut already_has_dot = false;
        let mut process_dot_later = false;

        while let Some(&c) = self.source_iter.peek() {
            if c.is_ascii_digit() {
                text.push(c);
                self.source_iter.next();
            }
            else if c == '.' {
                if already_has_dot { break; }
                already_has_dot = true;
                self.source_iter.next();
                // '.' is only part of the number if the next character is a digit
                if let Some(after_dot) = self.source_iter.peek() {
                    if after_dot.is_ascii_digit() {
                        text.push('.');
                        text.push(*after_dot);
                        self.source_iter.next();
                    }
                    else {
                        process_dot_later = true;
                        break;
                    }
                }
            }
            else if c == '_' {
                self.source_iter.next();
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
        
        while let Some(&c) = self.source_iter.peek() {
            if !c.is_alphanumeric() && c != '_' { break; }
            text.push(c);
            self.source_iter.next();
        }
        
        // Check if the identifier is a reserved keyword
        if let Some(token_type) = get_keyword(&text) {
            self.add_token(token_type);
        }
        else { self.add_token(TokenType::Identifier(text)); }
    }
}