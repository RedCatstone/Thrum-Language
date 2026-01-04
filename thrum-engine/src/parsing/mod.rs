use core::fmt;
use std::{iter::Peekable, vec::IntoIter};

use crate::{ErrType, Program, ProgramError, lexing::tokens::{LexerToken, TokenType}, parsing::ast_structure::{Expr, Span}};

mod parse_expressions;
mod parse_pattern_types;
pub mod ast_structure;
pub mod desugar;



pub fn parse_program(program: &mut Program) {
    let mut parser = Parser::new(program);

    let Expr::Block(body) = parser.parse_block_expression(TokenType::EndOfFile, Span::default()).expression
    else { unreachable!() };

    program.ast = body;
}



pub struct Parser<'a> {
    errors: &'a mut Vec<ProgramError>,
    tokens: Peekable<IntoIter<LexerToken>>,
    prev_token_line: usize,
    pipe_operators_active: usize,
}

impl<'a> Parser<'a> {
    fn new(program: &'a mut Program) -> Self {
        Parser {
            errors: &mut program.errors,
            // lexer tokens will be gone after parsing!!!
            tokens: std::mem::take(&mut program.lexer_tokens).into_iter().peekable(),

            prev_token_line: 0,
            pipe_operators_active: 0,
        }
    }

    fn peek(&mut self) -> &LexerToken {
        self.tokens.peek().unwrap_or(&LexerToken::END_TOKEN)
    }

    fn next(&mut self) -> LexerToken {
        let next = self.tokens.next().unwrap_or(LexerToken::END_TOKEN);
        self.prev_token_line = next.span.line;
        next
    }

    fn peek_is_on_same_line(&mut self) -> bool {
        self.peek().span.line == self.prev_token_line
    }

    fn peek_is_expression_start(&mut self) -> bool {
        match self.peek().token_type {
            // these should match parse_prefix function
            TokenType::Identifier(_) | TokenType::Number(_) | TokenType::StringFrag(_) | TokenType::StringStart | TokenType::Bool(_) | TokenType::Null
            | TokenType::Minus | TokenType::Exclamation | TokenType::BitNot
            | TokenType::LeftBrace | TokenType::LeftParen
            | TokenType::Let | TokenType::If | TokenType::Fn => true,
            _ => false
        }
    }

    fn error(&mut self, err_type: ErrType) {
        let err = ProgramError {
            line: self.peek().span.line,
            byte_offset: self.peek().span.byte_offset,
            typ: err_type
        };
        self.errors.push(err);
    }

    fn expect_token(&mut self, expected: TokenType, error_msg: &str) -> Span {
        let token = self.peek();
        if std::mem::discriminant(&token.token_type) == std::mem::discriminant(&expected) {
            self.next().span
        }
        else {
            let found_instead = self.peek().clone();
            self.error(ErrType::ParserExpectToken(expected, error_msg.to_string(), found_instead.token_type));
            found_instead.span
        }
    }
    fn expect_identifier(&mut self, error_msg: &str) -> String {
        if let TokenType::Identifier(text) = &self.peek().token_type {
            let text = text.clone();
            self.next();
            text
        }
        else {
            let found_instead = self.peek().token_type.clone();
            self.error(ErrType::ParserExpectToken(TokenType::Identifier(String::new()), error_msg.to_string(), found_instead));
            String::new()
        }
    }

    fn optional_dot_token(&mut self) -> Option<(Span, String)> {
        if let TokenType::Dot(text) = &self.peek().token_type {
            let text = text.clone();
            let dot_token = self.next();
            Some((dot_token.span, text))
        }
        else { None }
    }

    fn parse_optional_label(&mut self) -> Option<(Span, String)> {
        if self.optional_token(TokenType::Hashtag).is_some() {
            let next_token = self.next();
            let label = 
                match next_token.token_type {
                    TokenType::Identifier(s) => s,
                    x => x.to_string()
                };
            Some((next_token.span, label))
        } else { None }
    }

    fn optional_token(&mut self, expected: TokenType) -> Option<Span> {
        if std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(&expected) {
            Some(self.next().span)
        }
        else { None }
    }

    pub(super) fn parse_comma_separated<T>(
        &mut self,
        end_token: TokenType,
        parse_element: impl Fn(&mut Self, i32) -> T,
        err_msg: &str
    ) -> (Span, Vec<T>)
    {
        let mut list = Vec::new();
        
        // handles empty lists immediately
        for i in 0.. {
            if self.peek().token_type == end_token { break }
            list.push(parse_element(self, i));
            if self.optional_token(TokenType::Comma).is_none() { break; }
        }
        let span = self.expect_token(end_token, err_msg);
        (span, list)
    }

    pub(super) fn parse_line_seperated<T>(
        &mut self,
        end_token: TokenType,
        parse_element: impl Fn(&mut Self) -> T,
        on_semicolon: impl Fn(Span) -> Option<T>,
    ) -> (Span, Vec<T>)
    {
        let mut list = Vec::new();

        // handles empty lists immediately
        while self.peek().token_type != end_token && self.peek().token_type != TokenType::EndOfFile {
            list.push(parse_element(self));
            if let Some(semi_span) = self.optional_token(TokenType::Semicolon) {
                if let Some(semicolon_elem) = on_semicolon(semi_span) {
                    list.push(semicolon_elem);
                }
            }
            else {
                // no semicolon -> next expression can't be on the same line.
                if self.peek_is_on_same_line() && self.peek_is_expression_start() {
                    self.error(ErrType::ParserUnexpectedExpression);
                }
            }
        }
        let span = self.expect_token(end_token.clone(), "to close the block");
        (span, list)
    }

    fn recover(&mut self, recover_tokens: &[TokenType]) {
        while self.peek().token_type != TokenType::EndOfFile {
            if recover_tokens.contains(&self.peek().token_type) {
                self.next();
                return;
            }
            if self.peek_is_expression_start() {
                return;
            }
            else { self.next(); }
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