use std::rc::Rc;

use crate::{ErrType, lexing::tokens::{TokenSpan, TokenType}, parsing::{Parser, Precedence, ast_structure::{EnumExpression, Expr, ExprInfo, MatchArm, Span, TupleElement, TypeKind, Value}}};

impl<'a> Parser<'a> {
    pub(super) fn parse_expression(&mut self, precedence: Precedence) -> ExprInfo {
        // examples of prefixes:
        // 1
        // !(1 + 2)
        // Vec::new(data = [1, 2, 3])
        // if x { 1 } else { 2 }
        let mut left_expr = self.parse_prefix();

        // Pratt parser loop
        loop {
            let op_token = self.peek().clone();
            let mut op_precedence = self.get_precedence(&op_token.token);
            
            // Not an infix operator.
            if op_precedence == Precedence::Lowest { break }

            // only includes the next operator if it binds stronger than the current one.
            // 1 + 2 * 3   -> this would consume until * and only afterwards process +
            // 1 + 2 * 3   -> this would stop before * and only afterwards process it
            if precedence >= op_precedence { break }
            
            // 1 level lower for right associativity
            // ** is special because 2**3**2 should get parsed as: 2**(3**2)
            if op_token.token == TokenType::StarStar { op_precedence = Precedence::Product }

            // operators that are not allowed to be line-split.
            // this is so semicolons are actually not needed in the language,
            // for example here the parser would normally want to keep consuming the ( as a function call
            // let (a, b) = (0, 1)
            // (a, b) = (b, a + b)
            if let TokenType::LeftParen | TokenType::LeftBracket | TokenType::ColonColon = op_token.token
                && !self.peek_is_on_same_line() {
                    break;
                }

            self.next(); // consume the operator

            // update the left expression with the new infix result.
            left_expr = self.parse_infix(left_expr, op_token, op_precedence);
        }
        left_expr
    }



    fn parse_infix(&mut self, left_expr: ExprInfo, op_token: TokenSpan, op_precedence: Precedence) -> ExprInfo {
        let s_span = left_expr.span;
        match op_token.token {
            TokenType::LeftParen => {
                let (end_span, arguments) = self.parse_comma_seperated_expressions(
                    TokenType::RightParen,
                    "to close the function call argument list"
                );
                Expr::Call { callee: Box::new(left_expr), arguments }
                .to_info(s_span.merge(end_span))
            },

            TokenType::LeftBracket => {
                let right_index_expr = self.parse_expression(Precedence::Lowest);
                let end_span = self.expect_token(TokenType::RightBracket, "to close the index expression");
                Expr::Index { left: Box::new(left_expr), index: Box::new(right_index_expr) }
                .to_info(s_span.merge(end_span))
            },

            TokenType::Dot(member) => {
                // support both expr.member and expr.3
                Expr::MemberAccess { left: Box::new(left_expr), member, resolved_index: None }
                .to_info(s_span.merge(op_token.span))
            },

            TokenType::ColonColon => self.parse_path_operator(left_expr),
            
            _ if op_precedence == Precedence::Assign => {
                let pattern = Box::new(self.convert_lhs_assign_into_pattern(left_expr));
                let extra_operator = TokenSpan {
                    token: self.convert_assign_operator_to_operator(&op_token.token).unwrap(),
                    span: op_token.span
                };

                // expression
                let value = self.parse_expression(Precedence::Lowest);
                let value_span = value.span;

                Expr::Assign { pattern, extra_operator, value: Some(Box::new(value)) }
                .to_info(s_span.merge(value_span))
            },

            _ => {  // other operators
                let right_expr = self.parse_expression(op_precedence);
                let right_expr_span = right_expr.span;
                Expr::Infix {
                    operator: op_token,
                    left: Box::new(left_expr),
                    right: Box::new(right_expr)
                }
                .to_info(s_span.merge(right_expr_span))
            }
        }
    }









    fn parse_prefix(&mut self) -> ExprInfo {
        let first_token = self.next();
        let s_span = first_token.span;
        // after advancing:
        match first_token.token {
            TokenType::Identifier(name) => Expr::Identifier { name, var_id: None }.to_info(s_span),
            TokenType::Number(val) => Expr::Literal(Value::Num(val)).to_info(s_span),
            TokenType::Bool(val) => Expr::Literal(Value::Bool(val)).to_info(s_span),

            // Prefix operators
            TokenType::Minus | TokenType::Exclamation | TokenType::BitNot | TokenType::DotDotDot => {
                let right = Box::new(self.parse_expression(Precedence::Prefix));
                let right_span = right.span;
                Expr::Prefix { operator: first_token.token, right }
                .to_info(s_span.merge(right_span))
            },

            TokenType::Star => {
                let expr = Box::new(self.parse_expression(Precedence::Prefix));
                let expr_span = expr.span;
                Expr::Deref { expr }
                .to_info(s_span.merge(expr_span))
            },

            TokenType::Caret => {
                Expr::Identifier { name: format!("_pipe_{}", self.pipe_operators_active), var_id: None }
                .to_info(s_span)
            },

            TokenType::LeftBrace => self.parse_block_expression(TokenType::RightBrace, s_span),

            TokenType::LeftParen => 'l: {
                // empty tuple case '()'
                if let Some(end_span) = self.optional_token(TokenType::RightParen) {
                    break 'l Expr::Tuple(vec![]).to_info(s_span.merge(end_span))
                }

                let first_elem_labeled = matches!(self.peek().token, TokenType::Dot(_));
                let first_elem = self.parse_tuple_element("0".to_string());

                if self.optional_token(TokenType::Comma).is_some() || first_elem_labeled {
                    // , means its a tuple!
                    // e.g. (1, 2) (1,) (.x = 1,)
                    let mut tuple_body = vec![first_elem];
                    let (end_span, other_tuple_elems) = self.parse_comma_separated(
                        TokenType::RightParen,
                        |p, i| {
                            p.parse_tuple_element((i+1).to_string())
                        },
                        "to close the tuple"
                    );
                    tuple_body.extend(other_tuple_elems);
                    tuple_body.sort_by(|a, b| a.label.cmp(&b.label));
                    Expr::Tuple(tuple_body)
                    .to_info(s_span.merge(end_span))
                }
                else {
                    // normal grouped expression
                    self.expect_token(TokenType::RightParen, "to close the grouped expression");
                    // if first_elem_labeled {
                    //     Err(self.error("If this is supposed to be a tuple, use a trailing comma."))
                    // }
                    first_elem.expr
                }
            },

            TokenType::LeftBracket => {
                let (end_span, elements) = self.parse_comma_seperated_expressions(
                    TokenType::RightBracket,
                    "to close the array"
                );
                Expr::Array(elements)
                .to_info(s_span.merge(end_span))
            },

            TokenType::StringStart => {
                let mut parts = Vec::new();

                loop {
                    let token = self.next();
                    match token.token {
                        TokenType::StringFrag(s) => parts.push(Expr::Literal(Value::Str(s)).to_info(token.span)),

                        TokenType::LeftBrace => {
                            parts.push(self.parse_expression(Precedence::Lowest));
                            self.expect_token(TokenType::RightBrace, "to close the template string block");
                        }

                        TokenType::StringEnd => break Expr::TemplateString(parts).to_info(s_span.merge(token.span)),

                        // the lexer should make sure that no other tokens end up here!
                        _ => unreachable!()
                    }
                }
            },

            TokenType::ColonColon => self.parse_path_operator(
                Expr::TypePath(vec!["".to_string()]).to_info(s_span)
            ),

            TokenType::Let => {
                let pattern = self.parse_binding_match_pattern(false);
                let mut end_span = pattern.span;

                // the value of let-expressions can be optional. for example:
                // let x;
                // x = ...
                let value = if let Some(span) = self.optional_token(TokenType::Equal) {
                    end_span = span.merge(end_span);
                    Some(Box::new(self.parse_expression(Precedence::Lowest)))
                }
                else { None };

                Expr::Assign { pattern: Box::new(pattern), extra_operator: TokenSpan { token: TokenType::Equal, span: s_span }, value }
                .to_info(s_span.merge(end_span))
            },

            TokenType::Case => {
                let pattern = self.parse_binding_match_pattern(false);
                self.expect_token(TokenType::Equal, "after the case-expression pattern");
                let value = self.parse_expression(Precedence::Lowest);
                let value_span = value.span;

                Expr::Case { pattern: Box::new(pattern), value: Box::new(value) }
                .to_info(s_span.merge(value_span))
            }

            TokenType::If => {
                let condition = Box::new(self.parse_expression(Precedence::Lowest));
                let (then, alt) = self.parse_if_and_else();
                let alt_span = alt.span;
                Expr::If { condition, then, alt }
                .to_info(s_span.merge(alt_span))
            },

            TokenType::Ensure => {
                let condition = Box::new(self.parse_expression(Precedence::Lowest));
                self.expect_token(TokenType::Else, "after the ensure condition");
                let alt = Box::new(self.parse_expression(Precedence::Lowest));
                let alt_span = alt.span;
                Expr::Ensure { condition, alt, then: Box::new(Expr::Void.to_info(alt_span.to_0_width_right())) }
                .to_info(s_span.merge(alt_span))
            },

            TokenType::While => {
                let label = self.parse_optional_label().map_or("while".to_string(), |(_, l)| l);

                let condition = Box::new(self.parse_expression(Precedence::Lowest));

                let block_span = self.expect_token( TokenType::LeftBrace, "to open the while block");
                let body = Box::new(self.parse_block_expression(TokenType::RightBrace, block_span));
                let body_span = body.span;

                Expr::While { condition, body, label }
                .to_info(s_span.merge(body_span))
            },

            TokenType::Loop => {
                let label = self.parse_optional_label().map_or("loop".to_string(), |(_, l)| l);

                let block_span = self.expect_token( TokenType::LeftBrace, "to open the loop block");
                let body = Box::new(self.parse_block_expression(TokenType::RightBrace, block_span));
                let body_span = body.span;

                Expr::Loop { body, label }
                .to_info(s_span.merge(body_span))
            },

            TokenType::Match => {
                let match_value = Box::new(self.parse_expression(Precedence::Lowest));

                self.expect_token( TokenType::LeftBrace, "to open the match block");

                let (end_span, arms) = self.parse_line_seperated(
                    TokenType::RightBrace,
                    |p| {
                        let pattern = p.parse_binding_match_pattern(false);
                        p.expect_token(TokenType::RightArrow, "after the match arm pattern.");
                        let body = p.parse_expression(Precedence::Lowest);
                        MatchArm { pattern, body }
                    },
                    |_| None,
                );

                Expr::Match { match_value, arms }
                .to_info(s_span.merge(end_span))
            },
            
            TokenType::Enum => {
                // enum Option { Some(T), None }
                let (_, name) = self.expect_identifier("to name the enum");
                self.expect_token( TokenType::LeftBrace, "to open the enum definition block");
                let (end_span, enums) = self.parse_comma_separated(
                    TokenType::RightBrace,
                    |p, _| p.parse_enum_definition_variant(),
                    "to close the enum definition block"
                );
                Expr::EnumDefinition { name, enums }
                .to_info(s_span.merge(end_span))
            },

            TokenType::Fn => {
                let (_, name) = self.expect_identifier("to name the function");
                self.expect_token(TokenType::LeftParen, "to open the fn definition paramter list");
                let (_, params) = self.parse_binding_pattern_list(
                    TokenType::RightParen,
                    true, "to close the fn definition parameter list"
                );

                let return_type = if self.optional_token(TokenType::RightArrow).is_some() {
                    self.parse_type_expression()
                }
                else { TypeKind::Void };

                let block_span = self.expect_token(TokenType::LeftBrace, "to open the fn definition block");
                let body = Rc::new(self.parse_block_expression(TokenType::RightBrace, block_span));
                let body_span = body.span;
    
                Expr::FnDefinition { name, params, return_type, body, var_id: None }
                .to_info(s_span.merge(body_span))
            },

            TokenType::Pipe => {
                // Closure!
                let (_, params) = self.parse_binding_pattern_list(
                    TokenType::RightArrow,
                    true, "to close the fn definition parameter list"
                );

                let return_type = if self.optional_token(TokenType::Colon).is_some() {
                    self.parse_type_expression()
                }
                else { TypeKind::ParserUnknown };
                
                let body = Rc::new(self.parse_expression(Precedence::Lowest));
                let body_span = body.span;

                Expr::Closure { params, return_type, body }
                .to_info(s_span.merge(body_span))
            }

            TokenType::Return => {
                let expr = Box::new(if self.peek_is_expression_start() && self.peek_is_on_same_line() {
                    self.parse_expression(Precedence::Lowest)
                }
                else { Expr::Void.to_info(s_span.to_0_width_right()) });
                let expr_span = expr.span;

                Expr::Return(expr)
                .to_info(s_span.merge(expr_span))
            },

            TokenType::Break => {
                let label = self.parse_optional_label().map(|(_, l)| l);

                let expr = Box::new(if self.peek_is_expression_start() && self.peek_is_on_same_line() {
                    self.parse_expression(Precedence::Lowest)
                } else { Expr::Void.to_info(s_span.to_0_width_right()) });
                let expr_span = expr.span;

                Expr::Break { expr, label }
                .to_info(s_span.merge(expr_span))
            },

            TokenType::Continue => {
                let mut end_span = s_span;
                let label = self.parse_optional_label().map(|(span, l)| {
                    end_span = span;
                    l
                });
                
                Expr::Continue { label }
                .to_info(s_span.merge(end_span))
            }

            TokenType::Mut => {
                let (end_span, name) = self.expect_identifier(&format!("after {}", TokenType::Mut));
                Expr::MutRef { expr: Box::new(Expr::Identifier { name, var_id: None }.to_info(end_span)) }
                .to_info(s_span.merge(end_span))
            }

            _ => {
                self.error(ErrType::ParserExpectedAnExpression);
                Expr::Void.to_info(s_span.to_0_width_right())
            }
        }
    }







    pub(super) fn get_precedence(&self, token_type: &TokenType) -> Precedence {
        match token_type {
            TokenType::Colon => Precedence::Colon,
            TokenType::RightArrow => Precedence::Arrow,
            _ if self.convert_assign_operator_to_operator(token_type).is_some() => Precedence::Assign,
            TokenType::DotDot | TokenType::DotDotLess => Precedence::Range,
            TokenType::Quest => Precedence::Nullish,
            TokenType::PipeGreater => Precedence::Pipe,
            TokenType::Pipe => Precedence::Or,
            TokenType::Ampersand => Precedence::And,
            TokenType::BitOr => Precedence::BitwiseOr,
            TokenType::BitXor => Precedence::BitwiseXor,
            TokenType::BitAnd => Precedence::BitwiseAnd,
            TokenType::EqualEqual | TokenType::NotEqual => Precedence::Equals,
            TokenType::Less | TokenType::Greater | TokenType::LessEqual | TokenType::GreaterEqual => Precedence::LessGreater,
            TokenType::LeftShift | TokenType::RightShift => Precedence::Shift,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Star | TokenType::Slash | TokenType::Percent => Precedence::Product,
            TokenType::StarStar => Precedence::Power,
            TokenType::Exclamation | TokenType::BitNot | TokenType::DotDotDot => Precedence::Prefix,
            TokenType::Caret => Precedence::Postfix,
            TokenType::LeftParen | TokenType::LeftBracket | TokenType::Dot(_) | TokenType::ColonColon | TokenType::QuestDot => Precedence::CallIndex,
            _ => Precedence::Lowest,
        }
    }

    fn convert_assign_operator_to_operator(&self, assign_operator: &TokenType) -> Option<TokenType> {
        match assign_operator {
            TokenType::Equal => Some(TokenType::Equal),
            TokenType::PlusEqual => Some(TokenType::Plus),
            TokenType::MinusEqual => Some(TokenType::Minus),
            TokenType::StarEqual => Some(TokenType::Star),
            TokenType::SlashEqual => Some(TokenType::Slash),
            TokenType::StarStarEqual => Some(TokenType::StarStar),
            TokenType::PercentEqual => Some(TokenType::Percent),
            TokenType::QuestEqual => Some(TokenType::Quest),
            TokenType::BitAndEqual => Some(TokenType::BitAnd),
            TokenType::BitOrEqual => Some(TokenType::BitOr),
            TokenType::BitXorEqual => Some(TokenType::BitXor),
            TokenType::LeftShiftEqual => Some(TokenType::LeftShift),
            TokenType::RightShiftEqual => Some(TokenType::RightShift),
            _ => None
        }
    }





    pub(super) fn parse_block_expression(&mut self, end_token: TokenType, start_span: Span) -> ExprInfo {
        // '{' already consumed.
        let (end_span, exprs) = self.parse_line_seperated(
            end_token,
            |p| p.parse_expression(Precedence::Lowest),
            |span| Some(Expr::Void.to_info(span))
        );

        Expr::Block { exprs, drops_vars: Vec::new() }.to_info(start_span.merge(end_span))
    }


    fn parse_if_and_else(&mut self) -> (Box<ExprInfo>, Box<ExprInfo>) {
        let if_span = self.expect_token( TokenType::LeftBrace, "to open the if block");

        let then = Box::new(self.parse_block_expression(TokenType::RightBrace, if_span));
        let alt = Box::new(if let Some(_else_span) = self.optional_token(TokenType::Else) {
            self.parse_expression(Precedence::Lowest)
        }
        else { Expr::Void.to_info(then.span.to_0_width_right()) });

        (then, alt)
    }

    fn parse_enum_definition_variant(&mut self) -> EnumExpression {
        // Some(T)
        let (_, enum_name) = self.expect_identifier("to name an enum variant");

        let inner_types = if self.optional_token(TokenType::LeftParen).is_some() {
            self.parse_comma_separated(
                TokenType::RightParen,
                |p, _| p.parse_binding_match_pattern(true),
                "to close the enum variant tuple"
            ).1
        }
        else { Vec::new() };

        EnumExpression { name: enum_name, inner_types }
    }





    fn parse_comma_seperated_expressions(&mut self, end_token: TokenType, error_msg: &str) -> (Span, Vec<ExprInfo>) {
        // '[1, 2, 3]',   '(1, 2)',   dict { 1, 2 }
        self.parse_comma_separated(
            end_token,
            |p, _| p.parse_expression(Precedence::Lowest),
            error_msg
        )
    }

    fn parse_tuple_element(&mut self, default_label: String) -> TupleElement {
        if let Some((dot_span, label)) = self.optional_dot_token() {
            // named tuple element
            let expr = if self.optional_token(TokenType::Equal).is_some() {
                    self.parse_expression(Precedence::Lowest)
                }
                else {
                    Expr::Identifier { name: label.clone(), var_id: None }.to_info(dot_span)
                };
            
            TupleElement { label, expr }
        }
        else {
            let expr = self.parse_expression(Precedence::Lowest);
            TupleElement { label: default_label, expr }
        }
    }



    fn parse_path_operator(&mut self, left_expr: ExprInfo) -> ExprInfo {
        match left_expr.expression {
            Expr::Identifier { name, var_id: _ } => {
                let (span, next_path_segment) = self.expect_identifier(&format!("after {}", TokenType::ColonColon));
                Expr::TypePath(vec![name, next_path_segment]).to_info(left_expr.span.merge(span))
            }
            Expr::TypePath(mut segments) => {
                let (span, next_path_segment) = self.expect_identifier(&format!("after {}", TokenType::ColonColon));
                segments.push(next_path_segment);
                Expr::TypePath(segments).to_info(left_expr.span.merge(span))
            }
            _ => {
                self.error(ErrType::ParserUnexpectedPathToken);
                Expr::Void.to_info(left_expr.span)
            }
        }
    }
}