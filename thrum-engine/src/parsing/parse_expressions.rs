use std::rc::Rc;

use crate::{lexing::tokens::TokenType, parsing::{Parser, ParserError, Precedence, ast_structure::{MatchPattern, EnumExpression, Expr, MatchArm, TypeKind, TypedExpr, Value}}};

impl Parser {
    pub(super) fn parse_expression(&mut self, precedence: Precedence) -> Result<TypedExpr, ParserError> {
        // could be number, identifier, '-', 'let', ...
        let mut left_expr = self.parse_prefix()?;

        // core Pratt parser loop
        // it continues as long as the next token is an infix operator with a higher precedence than the current level.
        while precedence < self.peek_precedence() {
            let operator = self.peek().token_type.clone();

            // operators that are not allowed to be line-split:
            if let TokenType::LeftParen | TokenType::LeftBracket | TokenType::ColonColon = operator {
                if !self.peek_is_on_same_line() {
                    break;
                }
            }

            let mut op_precedence = self.get_precedence(&operator);
            if op_precedence == Precedence::Lowest { break; } // Not an infix operator.
            if operator == TokenType::StarStar { op_precedence = Precedence::Product }  // 1 level lower for right associativity

            self.advance(); // consume the operator

            // update the left expression with the new infix
            left_expr = self.parse_infix(left_expr, operator, op_precedence)?;
        }
        Ok(left_expr)
    }



    fn parse_infix(&mut self, left_expr: TypedExpr, operator: TokenType, op_precedence: Precedence) -> Result<TypedExpr, ParserError> {
        match operator {
            TokenType::LeftParen => {
                let arguments = self.parse_comma_seperated_expressions(
                    TokenType::RightParen,
                    "to close the function call argument list"
                )?;
                Ok(Expr::Call { callee: Box::new(left_expr), arguments }.into())
            },

            TokenType::LeftBracket => {
                let right_index_expr = self.parse_expression(Precedence::Lowest)?;
                self.expect_token(TokenType::RightBracket, "to close the index expression")?;
                Ok(Expr::Index { left: Box::new(left_expr), index: Box::new(right_index_expr) }.into())
            },

            TokenType::RightArrow => {
                let params = self.convert_param_exprs_into_patterns(left_expr)?;
                let body = self.parse_expression(Precedence::Lowest)?.into();
                Ok(Expr::Closure { params, return_type: TypeKind::ParserUnknown, body }.into())
            },

            TokenType::Dot => {
                let member = self.expect_identifier(&format!("after '{}'", TokenType::Dot))?;
                Ok(Expr::MemberAccess { left: Box::new(left_expr), member }.into())
            },

            TokenType::Colon => {
                match left_expr.expression {
                    Expr::Identifier { name } => {
                        let right_type_expr = self.parse_type_expression()?;
                        Ok(Expr::ParserTempTypeAnnotation(MatchPattern::Binding { name, typ: right_type_expr }).into())
                    }
                    _ => return Err(self.error(&format!("Type annotations are only allowed after identifiers. Found after: {:?}", left_expr))),
                }
            },

            TokenType::ColonColon => self.parse_path_operator(left_expr),
            
            TokenType::PipeGreater => self.parse_pipe_greater_operator(left_expr),
            
            _ if op_precedence == Precedence::Assign => {
                let pattern = Box::new(self.convert_lhs_assign_into_pattern(left_expr)?);
                let extra_operator = self.convert_assign_operator_to_operator(&operator).unwrap();

                // expression
                let value = Some(Box::new(self.parse_expression(Precedence::Lowest)?));

                Ok(Expr::Assign { pattern, extra_operator, value }.into())
            },

            _ => {  // other operators
                let right_expr = self.parse_expression(op_precedence)?;
                Ok(Expr::Infix {
                    operator,
                    left: Box::new(left_expr),
                    right: Box::new(right_expr)
                }.into())
            }
        }
    }









    fn parse_prefix(&mut self) -> Result<TypedExpr, ParserError> {
        let token_type = self.peek().token_type.clone();
        self.advance();
        // after advancing:
        match token_type {
            TokenType::Identifier(name) => Ok(Expr::Identifier { name }.into()),
            TokenType::Number(val) => Ok(Expr::Literal(Value::Num(val)).into()),
            TokenType::StringFrag(val) => Ok(Expr::Literal(Value::Str(val)).into()),
            TokenType::Bool(val) => Ok(Expr::Literal(Value::Bool(val)).into()),

            // Prefix operators
            TokenType::Minus | TokenType::Exclamation | TokenType::BitNot | TokenType::DotDotDot => {
                Ok(Expr::Prefix {
                    operator: token_type,
                    right: self.parse_expression(Precedence::Prefix)?.into()
                }.into())
            },

            TokenType::Star => {
                Ok(Expr::Deref {
                    expr: self.parse_expression(Precedence::Prefix)?.into()
                }.into())
            },

            TokenType::Caret => {
                Ok(Expr::Identifier {
                    name: format!("_pipe_{}", self.pipe_operators_active)
                }.into())
            },

            TokenType::LeftBrace => Ok(self.parse_block_expression(TokenType::RightBrace)),

            TokenType::LeftParen => {
                // empty tuple case '()'
                if self.optional_token(TokenType::RightParen) { return Ok(Expr::Tuple(Vec::new()).into()); }

                let first_expr = self.parse_expression(Precedence::Lowest)?;
                if self.optional_token(TokenType::Comma) {
                    // , => Tuple!
                    let mut tuple_body = vec![first_expr];
                    tuple_body.extend(self.parse_comma_seperated_expressions(TokenType::RightParen, "to close the tuple")?);
                    Ok(Expr::Tuple(tuple_body).into())
                }
                else {
                    // normal grouped expression
                    self.expect_token(TokenType::RightParen, "to close the grouped expression")?;
                    Ok(first_expr)
                }
            },

            TokenType::LeftBracket => {
                Ok(Expr::Array(
                    self.parse_comma_seperated_expressions(TokenType::RightBracket, "to close the array")?
                ).into())
            },

            TokenType::StringStart => {
                let mut parts = Vec::new();

                loop {
                    match self.peek().token_type.clone() {
                        TokenType::StringFrag(s) => {
                            self.advance();
                            parts.push(Expr::Literal(Value::Str(s)).into());
                        }
                        TokenType::LeftBrace => {
                            self.advance();
                            parts.push(self.parse_expression(Precedence::Lowest)?);
                            self.expect_token(TokenType::RightBrace, "to close the template string block")?;
                        }
                        TokenType::StringEnd => {
                            self.advance();
                            break Ok(Expr::TemplateString(parts).into());
                        }
                        _ => {
                            break Err(self.error(&format!("Unexpected token inside string: '{}'", self.peek())));
                        }
                    }
                }
            },

            TokenType::ColonColon => self.parse_path_operator(Expr::TypePath(vec!["".to_string()]).into()),

            TokenType::Let => {
                let pattern = Box::new(self.parse_binding_match_pattern(false)?);

                // value can be optional. for example: `let x` and later `x = ...`
                let value = if self.optional_token(TokenType::Equal) {
                    Some(Box::new(self.parse_expression(Precedence::Lowest)?))
                }
                else { None };

                Ok(Expr::Assign { pattern, extra_operator: TokenType::Equal, value }.into())
            },

            TokenType::Case => {
                let pattern = Box::new(self.parse_binding_match_pattern(false)?);
                self.expect_token(TokenType::Equal, "after the case-expression pattern")?;
                let value = Box::new(self.parse_expression(Precedence::Lowest)?);

                Ok(Expr::Case { pattern, value }.into())
            }

            TokenType::If => {
                let condition = Box::new(self.parse_expression(Precedence::Lowest)?);
                let (consequence, alternative) = self.parse_if_and_else()?;
                Ok(Expr::If { condition, consequence, alternative }.into())
            },

            TokenType::While => {
                let condition = Box::new(self.parse_expression(Precedence::Lowest)?);
                self.expect_token( TokenType::LeftBrace, "to open the while block")?;
                let body = Box::new(self.parse_block_expression(TokenType::RightBrace));
                Ok(Expr::While { condition, body }.into())
            },

            TokenType::Loop => {
                self.expect_token( TokenType::LeftBrace, "to open the loop block")?;
                let body = Box::new(self.parse_block_expression(TokenType::RightBrace));
                Ok(Expr::Loop { body }.into())
            },

            TokenType::Match => {
                let match_value = Box::new(self.parse_expression(Precedence::Lowest)?);

                self.expect_token( TokenType::LeftBrace, "to open the match block")?;

                let arms = self.parse_line_seperated(
                    TokenType::RightBrace,
                    |p| {
                        let pattern = p.parse_binding_match_pattern(false)?;
                        p.expect_token(TokenType::RightArrow, "after the match arm pattern.")?;
                        let body = p.parse_expression(Precedence::Lowest)?;
                        Ok(MatchArm { pattern, body })
                    },
                    || None,
                );

                Ok(Expr::Match { match_value, arms }.into())
            },
            
            TokenType::Enum => {
                // enum Option { Some(T), None }
                let name = self.expect_identifier("to name the enum")?;
                self.expect_token( TokenType::LeftBrace, "to open the enum definition block")?;
                let enums = self.parse_comma_separated(
                    TokenType::RightBrace,
                    |p| p.parse_enum_definition_variant(),
                    "to close the enum definition block"
                )?;
                Ok(Expr::EnumDefinition { name, enums }.into())     
            },

            TokenType::Fn => {
                let name = self.expect_identifier("to name the function")?;
                self.expect_token(TokenType::LeftParen, "to open the fn definition paramter list")?;
                let params = self.parse_binding_pattern_list(
                    TokenType::RightParen,
                    true, "to close the fn definition parameter list"
                )?;

                let return_type = if self.optional_token(TokenType::RightArrow) {
                    self.parse_type_expression()?
                }
                else { TypeKind::Void };

                self.expect_token(TokenType::LeftBrace, "to open the fn definition block")?;
                let body = Rc::new(self.parse_block_expression(TokenType::RightBrace));
    
                Ok(Expr::FnDefinition { name, params, return_type, body }.into())
            },

            TokenType::Return => {
                let return_expression = if self.peek_is_expression_start() && self.peek_is_on_same_line() {
                    Box::new(self.parse_expression(Precedence::Lowest)?)
                }
                else { Box::new(Expr::Void.into()) };
                Ok(Expr::Return(return_expression).into())
            },

            TokenType::Break => {
                let break_expression = if self.peek_is_expression_start() && self.peek_is_on_same_line() {
                    Box::new(self.parse_expression(Precedence::Lowest)?)
                }
                else { Box::new(Expr::Void.into()) };
                Ok(Expr::Break { expr: break_expression }.into())
            },

            TokenType::Mut => {
                let name = self.expect_identifier(&format!("after {}", TokenType::Mut))?;
                Ok(Expr::MutRef { expr: Box::new(Expr::Identifier { name }.into()) }.into())
            }

            _ => Err(self.error(&format!("Expected an expression. Found '{}' instead.", token_type))),
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
            TokenType::LeftParen | TokenType::LeftBracket | TokenType::Dot | TokenType::ColonColon | TokenType::QuestDot => Precedence::CallIndex,
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





    pub(super) fn parse_block_expression(&mut self, end_token: TokenType) -> TypedExpr {
        // '{' already consumed.
        let expression_body = self.parse_line_seperated(
            end_token,
            |p| p.parse_expression(Precedence::Lowest),
            || Some(Expr::Void.into())
        );

        Expr::Block(expression_body).into()
    }


    fn parse_if_and_else(&mut self) -> Result<(Box<TypedExpr>, Box<TypedExpr>), ParserError> {
        self.expect_token( TokenType::LeftBrace, "to open the if block")?;

        let consequence = Box::new(self.parse_block_expression(TokenType::RightBrace));
        let alternative = if self.optional_token(TokenType::Else) {
            Box::new(self.parse_expression(Precedence::Lowest)?)
        }
        else { Expr::Void.into() };

        Ok((consequence, alternative))
    }

    fn parse_enum_definition_variant(&mut self) -> Result<EnumExpression, ParserError> {
        // Some(T)
        let enum_name = self.expect_identifier("to name an enum variant")?;

        let inner_types = if self.optional_token(TokenType::LeftParen) {
            self.parse_comma_separated(
                TokenType::RightParen,
                |p| p.parse_binding_match_pattern(true),
                "to close the enum variant tuple"
            )?
        }
        else { Vec::new() };

        Ok(EnumExpression { name: enum_name, inner_types })
    }





    fn parse_comma_seperated_expressions(&mut self, end_token: TokenType, error_msg: &str) -> Result<Vec<TypedExpr>, ParserError> {
        // '[1, 2, 3]',   '(1, 2)',   dict { 1, 2 }
        self.parse_comma_separated(
            end_token,
            |p| p.parse_expression(Precedence::Lowest),
            error_msg
        )
    }



    fn parse_path_operator(&mut self, left_expr: TypedExpr) -> Result<TypedExpr, ParserError> {
        match left_expr.expression {
            Expr::Identifier { name } => {
                let next_path_segment = self.expect_identifier(&format!("after {}", TokenType::ColonColon))?;
                Ok(Expr::TypePath(vec![name, next_path_segment]).into())
            }
            Expr::TypePath(mut segments) => {
                let next_path_segment = self.expect_identifier(&format!("after {}", TokenType::ColonColon))?;
                segments.push(next_path_segment);
                Ok(Expr::TypePath(segments).into())
            }
            _ => return Err(self.error(&format!("'::'-paths are only allowed after identifiers. Found after: {:?}", left_expr))),
        }
    }

    fn parse_pipe_greater_operator(&mut self, left_expr: TypedExpr) -> Result<TypedExpr, ParserError> {
        self.pipe_operators_active += 1;
        let right_expr_result = self.parse_expression(Precedence::Lowest);
        let pipe_identifier_name = format!("_pipe_{}", self.pipe_operators_active);
        self.pipe_operators_active -= 1;

        Ok(Expr::Block(vec![
            Expr::Assign {
                pattern: Box::new(MatchPattern::Binding { name: pipe_identifier_name, typ: TypeKind::ParserUnknown }),
                extra_operator: TokenType::Equal,
                value: Some(Box::new(left_expr)),
            }.into(),
            right_expr_result?
        ]).into())
    }
}