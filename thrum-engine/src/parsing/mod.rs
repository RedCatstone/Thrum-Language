use std::{iter::Peekable, vec::IntoIter};

use crate::{ErrType, Program, ProgramError, lexing::tokens::{TokenSpan, TokenType}, parsing::ast_structure::Span};

mod parse_expressions;
mod parse_pattern_types;
pub mod ast_structure;
pub mod desugar;



pub fn parse_program(program: &mut Program) {
    let mut parser = Parser::new(program);

    program.ast = Some(parser.parse_block_expression(TokenType::EndOfFile, Span::default()));
}



pub struct Parser<'a> {
    errors: &'a mut Vec<ProgramError>,
    tokens: Peekable<IntoIter<TokenSpan>>,
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

    fn peek(&mut self) -> &TokenSpan {
        self.tokens.peek().unwrap_or(&TokenSpan::END_TOKEN)
    }

    fn next(&mut self) -> TokenSpan {
        let next = self.tokens.next().unwrap_or(TokenSpan::END_TOKEN);
        self.prev_token_line = next.span.line;
        next
    }

    fn peek_is_on_same_line(&mut self) -> bool {
        self.peek().span.line == self.prev_token_line
    }

    fn peek_is_expression_start(&mut self) -> bool {
        match self.peek().token {
            // these should match parse_prefix function
            TokenType::Identifier(_) | TokenType::Number(_) | TokenType::StringFrag(_) | TokenType::StringStart | TokenType::Bool(_) | TokenType::Null
            | TokenType::Minus | TokenType::Exclamation | TokenType::BitNot
            | TokenType::LeftBrace | TokenType::LeftParen
            | TokenType::Let | TokenType::If | TokenType::Fn => true,
            _ => false
        }
    }

    fn error(&mut self, err_type: ErrType) {
        let span = self.peek().span;
        self.error_with_span(err_type, span);
    }
    fn error_with_span(&mut self, err_type: ErrType, span: Span) {
        self.errors.push(ProgramError {
            line: span.line,
            byte_offset: span.byte_offset,
            length: span.length,
            typ: err_type
        });
    }

    fn expect_token(&mut self, expected: TokenType, error_msg: &str) -> Span {
        let token = self.peek();
        if std::mem::discriminant(&token.token) == std::mem::discriminant(&expected) {
            self.next().span
        }
        else {
            let found_instead = self.peek().clone();
            self.error(ErrType::ParserExpectToken(expected, error_msg.to_string(), found_instead.token));
            found_instead.span
        }
    }
    fn expect_identifier(&mut self, error_msg: &str) -> (Span, String) {
        if let TokenType::Identifier(text) = &self.peek().token {
            let text = text.clone();
            (self.next().span, text)
        }
        else {
            let found_instead = self.peek().clone();
            self.error(ErrType::ParserExpectToken(TokenType::Identifier(String::new()), error_msg.to_string(), found_instead.token));
            (found_instead.span, String::new())
        }
    }

    fn optional_dot_token(&mut self) -> Option<(Span, String)> {
        if let TokenType::Dot(text) = &self.peek().token {
            let text = text.clone();
            let dot_token = self.next();
            Some((dot_token.span, text))
        }
        else { None }
    }

    fn parse_optional_label(&mut self) -> Option<(Span, String)> {
        let is_on_same_line = self.peek_is_on_same_line();
        if self.optional_token(TokenType::Hashtag).is_some() {
            if !is_on_same_line {
                self.error(ErrType::ParserLabelsHaveToBeOnSameLine);
            }
            let next_token = self.next();
            let label = 
                match next_token.token {
                    TokenType::Identifier(s) => s,
                    x => x.to_string()
                };
            Some((next_token.span, label))
        } else {
            None
        }
    }

    fn optional_token(&mut self, expected: TokenType) -> Option<Span> {
        if std::mem::discriminant(&self.peek().token) == std::mem::discriminant(&expected) {
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
            if self.peek().token == end_token { break }
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
        while self.peek().token != end_token && self.peek().token != TokenType::EndOfFile {
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
        while self.peek().token != TokenType::EndOfFile {
            if recover_tokens.contains(&self.peek().token) {
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