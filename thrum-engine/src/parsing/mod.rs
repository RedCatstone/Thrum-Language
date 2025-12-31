use core::fmt;

use crate::{lexing::tokens::{LexerToken, TokenType}, parsing::ast_structure::{Expr, TypedExpr}};

mod parse_expressions;
mod parse_patterns_types;
pub mod ast_structure;
pub mod desugar;



#[derive(Default)]
pub struct Parser {
    tokens: Vec<LexerToken>,
    position: usize,
    errors: Vec<ParserError>,
    prev_token_line: usize,
    pipe_operators_active: usize,
}

impl Parser {
    fn new(tokens: Vec<LexerToken>) -> Self {
        Parser { tokens, ..Default::default() }
    }

    pub fn parse_program(tokens: Vec<LexerToken>) -> (Vec<TypedExpr>, Vec<ParserError>) {
        let mut parser = Self::new(tokens);

        let Expr::Block(body) = parser.parse_block_expression(TokenType::EndOfFile).expression
        else { unreachable!() };
        (body, parser.errors)
    }


    fn peek(&self) -> &LexerToken {
        self.tokens.get(self.position).unwrap_or(&LexerToken {
            token_type: TokenType::EndOfFile,
            line: usize::MAX,
            byte_start: usize::MAX,
            length: 0,
        })
    }

    fn advance(&mut self) {
        self.prev_token_line = self.peek().line;
        self.position += 1;
    }

    fn peek_is_on_same_line(&self) -> bool {
        self.peek().line == self.prev_token_line
    }

    fn peek_precedence(&self) -> Precedence {
        self.get_precedence(&self.peek().token_type)
    }

    fn peek_is_expression_start(&self) -> bool {
        match self.peek().token_type {
            // these should match parse_prefix function
            TokenType::Identifier(_) | TokenType::Number(_) | TokenType::StringFrag(_) | TokenType::StringStart | TokenType::Bool(_) | TokenType::Null
            | TokenType::Minus | TokenType::Exclamation | TokenType::BitNot
            | TokenType::LeftBrace | TokenType::LeftParen
            | TokenType::Let | TokenType::If | TokenType::Fn => true,
            _ => false
        }
    }

    fn expect_token(&mut self, expected: TokenType, error_msg: &str) -> Result<LexerToken, ParserError> {
        let token = self.peek();
        if std::mem::discriminant(&token.token_type) == std::mem::discriminant(&expected) {
            let cloned_token = token.clone();
            self.advance();
            Ok(cloned_token)
        } else {
            Err(self.error(&format!("Expected '{}' {}. Found '{}' instead.", expected, error_msg, self.peek())))
        }
    }
    fn expect_identifier(&mut self, error_msg: &str) -> Result<String, ParserError> {
        let TokenType::Identifier(name) = self.expect_token(TokenType::Identifier(String::new()), error_msg)?.token_type else { unreachable!() };
        Ok(name)
    }

    fn optional_dot_token(&mut self) -> Option<String> {
        if let TokenType::Dot(text) = &self.peek().token_type {
            let text = text.clone();
            self.advance();
            Some(text)
        }
        else { None }
    }

    fn parse_optional_label(&mut self) -> Option<String> {
        if self.optional_token(TokenType::Hashtag) {
            let label = 
                match &self.peek().token_type {
                    TokenType::Identifier(s) => s.to_string(),
                    x => x.to_string()
                };
            self.advance();
            Some(label)
        } else { None }
    }

    fn optional_token(&mut self, expected: TokenType) -> bool {
        if std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(&expected) {
            self.advance();
            return true;
        }
        false
    }

    pub(super) fn parse_comma_separated<T>(
        &mut self,
        end_token: TokenType,
        parse_element: impl Fn(&mut Self, i32) -> Result<T, ParserError>, 
        err_msg: &str
    ) -> Result<Vec<T>, ParserError>
    {
        let mut list = Vec::new();
        
        // handles empty lists immediately
        for i in 0.. {
            if self.peek().token_type == end_token { break }
            list.push(parse_element(self, i)?);
            if !self.optional_token(TokenType::Comma) { break; }
        }
        self.expect_token(end_token, err_msg)?;
        Ok(list)
    }

    pub(super) fn parse_line_seperated<T>(
        &mut self,
        end_token: TokenType,
        parse_element: impl Fn(&mut Self) -> Result<T, ParserError>,
        on_semicolon: impl Fn() -> Option<T>,
    ) -> Vec<T>
    {
        let mut list = Vec::new();

        // handles empty lists immediately
        while self.peek().token_type != end_token && self.peek().token_type != TokenType::EndOfFile {
            println!("parsing expression: {} - endtoken: {} - errors: {}", self.peek(), end_token, self.errors.len());
            match parse_element(self) {
                Ok(elem) => {
                    list.push(elem);
                    if self.optional_token(TokenType::Semicolon) {
                        if let Some(semicolon_elem) = on_semicolon() {
                            list.push(semicolon_elem);
                        }
                    }
                    else {
                        // no semicolon -> next expression can't be on the same line.
                        if self.peek_is_on_same_line() && self.peek_is_expression_start() {
                            self.errors.push(self.error(&format!("2 expressions cannot be on the same line without a ';'. Found '{}'.", self.peek())));
                        }
                    }
                }
                Err(parser_error) => {
                    self.errors.push(parser_error);
                    self.recover(&[end_token.clone(), TokenType::Semicolon]);
                }
            }
        }
        if let Err(msg) = self.expect_token(end_token.clone(), "to close the block") {
            self.errors.push(msg);
        }
        list
    }

    pub(super) fn error(&self, msg: &str) -> ParserError {
        ParserError {
            msg: msg.to_string(),
            token: self.peek().clone(),
        }
    }

    fn recover(&mut self, recover_tokens: &[TokenType]) {
        while self.peek().token_type != TokenType::EndOfFile {
            if recover_tokens.contains(&self.peek().token_type) {
                self.advance();
                return;
            }
            if self.peek_is_expression_start() {
                return;
            }
            else { self.advance(); }
        }
    }
}




pub struct ParserError {
    pub msg: String,
    pub token: LexerToken,
}
impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]:{}", self.msg, self.token)
    }
}







#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Colon,      // :
    Arrow,      // ->
    Assign,     // =, +=, -=, etc.
    Pipe,       // |>
    Range,      // 1..2
    Nullish,    // ??
    Or,         // |
    And,        // &
    BitwiseOr,  // ~|
    BitwiseXor, // ~^
    BitwiseAnd, // ~&
    Equals,     // ==, !=
    LessGreater,// <, >, <=, >=
    Shift,      // <<, >>
    Sum,        // +, -
    Product,    // *, /, %
    Power,      // **
    Postfix,    // ^
    Prefix,     // !, ~!, -
    CallIndex,  // square(X), array[i], dict {1, 2}
}