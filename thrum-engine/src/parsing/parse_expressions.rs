use std::rc::Rc;

use crate::{ErrType, lexing::tokens::{LexerToken, TokenType}, parsing::{Parser, Precedence, ast_structure::{EnumExpression, Expr, ExprInfo, MatchArm, MatchPattern, Span, TupleElement, TypeKind, Value}}};

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
            let mut op_precedence = self.get_precedence(&op_token.token_type);
            
            // Not an infix operator.
            if op_precedence == Precedence::Lowest { break }

            // only includes the next operator if it binds stronger than the current one.
            // 1 + 2 * 3   -> this would consume until * and only afterwards process +
            // 1 + 2 * 3   -> this would stop before * and only afterwards process it
            if precedence < op_precedence { break }
            
            // 1 level lower for right associativity
            // ** is special because 2**3**2 should get parsed as: 2**(3**2)
            if op_token.token_type == TokenType::StarStar { op_precedence = Precedence::Product }

            // operators that are not allowed to be line-split.
            // this is so semicolons are actually not needed in the language,
            // for example here the parser would normally want to keep consuming the ( as a function call
            // let (a, b) = (0, 1)
            // (a, b) = (b, a + b)
            if let TokenType::LeftParen | TokenType::LeftBracket | TokenType::ColonColon = op_token.token_type
                && !self.peek_is_on_same_line() {
                    break;
                }

            self.next(); // consume the operator

            // update the left expression with the new infix result.
            left_expr = self.parse_infix(left_expr, op_token, op_precedence);
        }
        left_expr
    }



    fn parse_infix(&mut self, left_expr: ExprInfo, op_token: LexerToken, op_precedence: Precedence) -> ExprInfo {
        let left_expr_span = left_expr.span;
        let (end_span, expr) = match op_token.token_type {
            TokenType::LeftParen => {
                let (arguments, end_span) = self.parse_comma_seperated_expressions(
                    TokenType::RightParen,
                    "to close the function call argument list"
                );
                (end_span, Expr::Call { callee: Box::new(left_expr), arguments })
            },

            TokenType::LeftBracket => {
                let right_index_expr = self.parse_expression(Precedence::Lowest);
                let end_span = self.expect_token(TokenType::RightBracket, "to close the index expression");
                (end_span, Expr::Index { left: Box::new(left_expr), index: Box::new(right_index_expr) })
            },

            TokenType::Dot(member) => {
                // support both expr.member and expr.3
                (op_token.span, Expr::MemberAccess { left: Box::new(left_expr), member, resolved_index: None })
            },

            TokenType::ColonColon => self.parse_path_operator(left_expr),
            
            _ if op_precedence == Precedence::Assign => {
                let pattern = Box::new(self.convert_lhs_assign_into_pattern(left_expr));
                let extra_operator = self.convert_assign_operator_to_operator(&op_token.token_type).unwrap();

                // expression
                let value = self.parse_expression(Precedence::Lowest);

                (value.span, Expr::Assign { pattern, extra_operator, value: Some(Box::new(value)) })
            },

            _ => {  // other operators
                let right_expr = self.parse_expression(op_precedence);
                (right_expr.span, Expr::Infix {
                    operator: op_token.token_type,
                    left: Box::new(left_expr),
                    right: Box::new(right_expr)
                })
            }
        };
        expr.to_info(left_expr_span.merge(end_span))
    }









    fn parse_prefix(&mut self) -> ExprInfo {
        let first_token = self.next();
        let start_span = first_token.span;
        // after advancing:
        let (end_span, expr) = match first_token.token_type {
            TokenType::Identifier(name) => (start_span, Expr::Identifier { name }),
            TokenType::Number(val) => (start_span, Expr::Literal(Value::Num(val)).into()),
            TokenType::Bool(val) => (start_span, Expr::Literal(Value::Bool(val)).into()),

            // Prefix operators
            TokenType::Minus | TokenType::Exclamation | TokenType::BitNot | TokenType::DotDotDot => {
                let right = Box::new(self.parse_expression(Precedence::Prefix));
                (right.span, Expr::Prefix { operator: first_token.token_type, right })
            },

            TokenType::Star => {
                let expr = Box::new(self.parse_expression(Precedence::Prefix));
                (expr.span, Expr::Deref { expr })
            },

            TokenType::Caret => {
                (start_span, Expr::Identifier { name: format!("_pipe_{}", self.pipe_operators_active) })
            },

            TokenType::LeftBrace => {
                let block_expr = self.parse_block_expression(TokenType::RightBrace);
                (block_expr.span, block_expr.expression)
            },

            TokenType::LeftParen => 'l: {
                // empty tuple case '()'
                if let Some(span) = self.optional_token(TokenType::RightParen) {
                    break 'l Expr::Tuple(vec![]).to_info(span);
                }

                let first_elem_labeled = matches!(self.peek().token_type, TokenType::Dot(_));
                let first_elem = self.parse_tuple_element("0".to_string());

                if self.optional_token(TokenType::Comma).is_some() || first_elem_labeled {
                    // , means its a tuple!
                    // e.g. (1, 2) (1,) (.x = 1,)
                    let mut tuple_body = vec![first_elem];
                    let (span, other_tuple_elems) = self.parse_comma_separated(
                        TokenType::RightParen,
                        |p, i| {
                            p.parse_tuple_element((i+1).to_string())
                        },
                        "to close the tuple"
                    );
                    tuple_body.extend(other_tuple_elems);
                    tuple_body.sort_by(|a, b| a.label.cmp(&b.label));
                    (span, Expr::Tuple(tuple_body))
                }
                else {
                    // normal grouped expression
                    let span = self.expect_token(TokenType::RightParen, "to close the grouped expression");
                    // if first_elem_labeled {
                    //     Err(self.error("If this is supposed to be a tuple, use a trailing comma."))
                    // }
                    (span, first_elem.expr)
                }
            },

            TokenType::LeftBracket => {
                Expr::Array(
                    self.parse_comma_seperated_expressions(TokenType::RightBracket, "to close the array")
                ).into()
            },

            TokenType::StringStart => {
                let mut parts = Vec::new();

                loop {
                    match self.peek().token_type.clone() {
                        TokenType::StringFrag(s) => {
                            self.next();
                            parts.push(Expr::Literal(Value::Str(s)).into());
                        }
                        TokenType::LeftBrace => {
                            self.next();
                            parts.push(self.parse_expression(Precedence::Lowest));
                            self.expect_token(TokenType::RightBrace, "to close the template string block");
                        }
                        TokenType::StringEnd => {
                            self.next();
                            break Expr::TemplateString(parts).into();
                        }
                        _ => {
                            // the lexer should make sure that its impossible to reach this point
                            unreachable!()
                        }
                    }
                }
            },

            TokenType::ColonColon => self.parse_path_operator(Expr::TypePath(vec!["".to_string()]).into()),

            TokenType::Let => {
                let pattern = Box::new(self.parse_binding_match_pattern(false));

                // value can be optional. for example: `let x` and later `x = ...`
                let value = if self.optional_token(TokenType::Equal) {
                    Some(Box::new(self.parse_expression(Precedence::Lowest)))
                }
                else { None };

                Expr::Assign { pattern, extra_operator: TokenType::Equal, value }.into()
            },

            TokenType::Case => {
                let pattern = Box::new(self.parse_binding_match_pattern(false));
                self.expect_token(TokenType::Equal, "after the case-expression pattern");
                let value = Box::new(self.parse_expression(Precedence::Lowest));

                Expr::Case { pattern, value }.into()
            }

            TokenType::If => {
                let condition = Box::new(self.parse_expression(Precedence::Lowest));
                let (then, alt) = self.parse_if_and_else();
                Expr::If { condition, then, alt }.into()
            },

            TokenType::Ensure => {
                let condition = Box::new(self.parse_expression(Precedence::Lowest));
                self.expect_token(TokenType::Else, "after the ensure condition");
                let alt = Box::new(self.parse_expression(Precedence::Lowest));
                Expr::Ensure { condition, alt, then: Expr::Void.into() }.into()
            },

            TokenType::While => {
                let label = self.parse_optional_label().unwrap_or("while".to_string());

                let condition = Box::new(self.parse_expression(Precedence::Lowest));

                self.expect_token( TokenType::LeftBrace, "to open the while block");
                let body = Box::new(self.parse_block_expression(TokenType::RightBrace));

                Expr::While { condition, body, label }.into()
            },

            TokenType::Loop => {
                let label = self.parse_optional_label().unwrap_or("loop".to_string());

                self.expect_token( TokenType::LeftBrace, "to open the loop block");
                let body = Box::new(self.parse_block_expression(TokenType::RightBrace));

                Expr::Loop { body, label }.into()
            },

            TokenType::Match => {
                let match_value = Box::new(self.parse_expression(Precedence::Lowest));

                self.expect_token( TokenType::LeftBrace, "to open the match block");

                let arms = self.parse_line_seperated(
                    TokenType::RightBrace,
                    |p| {
                        let pattern = p.parse_binding_match_pattern(false);
                        p.expect_token(TokenType::RightArrow, "after the match arm pattern.");
                        let body = p.parse_expression(Precedence::Lowest);
                        MatchArm { pattern, body }
                    },
                    || None,
                );

                Expr::Match { match_value, arms }.into()
            },
            
            TokenType::Enum => {
                // enum Option { Some(T), None }
                let name = self.expect_identifier("to name the enum");
                self.expect_token( TokenType::LeftBrace, "to open the enum definition block");
                let enums = self.parse_comma_separated(
                    TokenType::RightBrace,
                    |p, _| p.parse_enum_definition_variant(),
                    "to close the enum definition block"
                );
                Expr::EnumDefinition { name, enums }.into()     
            },

            TokenType::Fn => {
                let name = self.expect_identifier("to name the function");
                self.expect_token(TokenType::LeftParen, "to open the fn definition paramter list");
                let params = self.parse_binding_pattern_list(
                    TokenType::RightParen,
                    true, "to close the fn definition parameter list"
                );

                let return_type = if self.optional_token(TokenType::RightArrow) {
                    self.parse_type_expression()
                }
                else { TypeKind::Void };

                self.expect_token(TokenType::LeftBrace, "to open the fn definition block");
                let body = Rc::new(self.parse_block_expression(TokenType::RightBrace));
    
                Expr::FnDefinition { name, params, return_type, body }.into()
            },

            TokenType::Pipe => {
                // Closure!
                let params = self.parse_binding_pattern_list(
                    TokenType::RightArrow,
                    true, "to close the fn definition parameter list"
                );

                let return_type = if self.optional_token(TokenType::Colon) {
                    self.parse_type_expression()
                }
                else { TypeKind::ParserUnknown };
                
                let body = Rc::new(self.parse_expression(Precedence::Lowest));

                Expr::Closure { params, return_type, body }.into()
            }

            TokenType::Return => {
                let return_expression = if self.peek_is_expression_start() && self.peek_is_on_same_line() {
                    Box::new(self.parse_expression(Precedence::Lowest))
                }
                else { Box::new(Expr::Void.into()) };
                Expr::Return(return_expression).into()
            },

            TokenType::Break => {
                let label = self.parse_optional_label();

                let break_expression = if self.peek_is_expression_start() && self.peek_is_on_same_line() {
                    Box::new(self.parse_expression(Precedence::Lowest))
                } else { Box::new(Expr::Void.into()) };

                Expr::Break { expr: break_expression, label }.into()
            },

            TokenType::Continue => {
                let label = if self.optional_token(TokenType::Hashtag) {
                    Some(self.expect_identifier("to name the break label."))
                } else { None };
                
                Expr::Continue { label }.into()
            }

            TokenType::Mut => {
                let name = self.expect_identifier(&format!("after {}", TokenType::Mut));
                Expr::MutRef { expr: Box::new(Expr::Identifier { name }.into()) }.into()
            }

            _ => {
                self.error(ErrType::ParserExpectedAnExpression);
                Expr::Void.to_info_with_type(TypeKind::TypeError)
            }
        };
        expr.to_info(start_span.merge(end_span))
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
            TokenType::EqualEqual | TokenType::NotEqual | TokenType::PipeGreater => Precedence::Equals,
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
        let (end_span, expression_body) = self.parse_line_seperated(
            end_token,
            |p| p.parse_expression(Precedence::Lowest),
            |span| Some(Expr::Void.to_info(span))
        );

        Expr::Block(expression_body).to_info(start_span.merge(end_span))
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
        let enum_name = self.expect_identifier("to name an enum variant");

        let inner_types = if self.optional_token(TokenType::LeftParen) {
            self.parse_comma_separated(
                TokenType::RightParen,
                |p, _| p.parse_binding_match_pattern(true),
                "to close the enum variant tuple"
            )
        }
        else { Vec::new() };

        EnumExpression { name: enum_name, inner_types }
    }





    fn parse_comma_seperated_expressions(&mut self, end_token: TokenType, error_msg: &str) -> (Vec<ExprInfo>, Span) {
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
                    Expr::Identifier { name: label.clone() }.to_info(dot_span)
                };
            
            TupleElement { label, expr }
        }
        else {
            let expr = self.parse_expression(Precedence::Lowest);
            TupleElement { label: default_label, expr }
        }
    }



    fn parse_path_operator(&mut self, left_expr: ExprInfo) -> (Span, Expr) {
        match left_expr.expression {
            Expr::Identifier { name } => {
                let next_path_segment = self.expect_identifier(&format!("after {}", TokenType::ColonColon));
                Expr::TypePath(vec![name, next_path_segment]).into()
            }
            Expr::TypePath(mut segments) => {
                let next_path_segment = self.expect_identifier(&format!("after {}", TokenType::ColonColon));
                segments.push(next_path_segment);
                Expr::TypePath(segments).into()
            }
            _ => {
                self.error(ErrType::ParserUnexpectedPathToken);
                Expr::Void.to_info_with_type(TypeKind::TypeError)
            }
        }
    }
}