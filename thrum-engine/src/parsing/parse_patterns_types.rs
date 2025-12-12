use std::rc::Rc;

use crate::{lexing::tokens::TokenType, parsing::{Parser, ParserError, Precedence, ast_structure::{MatchPattern, Expr, PlaceExpr, TypeKind, TypedExpr, Value}}};

impl Parser {
    pub(super) fn parse_type_expression(&mut self) -> Result<TypeKind, ParserError> {
        // mut
        if self.optional_token(TokenType::Mut) {
            return Ok(TypeKind::MutPointer(Box::new(self.parse_type_expression()?)))
        }

        // '?' Option wrapper
        if self.optional_token(TokenType::Quest) {
            return Ok(TypeKind::Struct { name: "Option".to_string(), inner_types: vec![self.parse_type_expression()?] })
        }

        // 'str' or 'arr<T>'.
        let str_type = if self.optional_token(TokenType::Null) { String::from("null") }
        else if self.optional_token(TokenType::Minus) { String::from("-") }
        else if self.optional_token(TokenType::Exclamation) { String::from("!") }
        else { self.expect_identifier("to name a type")? };

        // '<' start of an inner type
        let inner_types = if self.optional_token(TokenType::Less) { self.parse_type_list()? }
        else { Vec::new() };

        self.type_from_str(&str_type, inner_types)
    }

    fn type_from_str(&mut self, str: &str, inner_types: Vec<TypeKind>) -> Result<TypeKind, ParserError> {
        match str {
            "num" => Ok(TypeKind::Num),
            "str" => Ok(TypeKind::Str),
            "bool" => Ok(TypeKind::Bool),
            "tup" => Ok(TypeKind::Tup(inner_types)),
            "arr" => {
                if inner_types.len() == 1 { Ok(TypeKind::Arr(Box::new(inner_types.into_iter().next().unwrap()))) }
                else { Err(self.error("Invalid types for arr. arr needs exactly 1 inner type.")) }
            }
            "-" => Ok(TypeKind::Void),
            "!" => Ok(TypeKind::Never),
            "_" => Ok(TypeKind::ParserUnknown),
            &_ => Ok(TypeKind::Struct { name: str.to_string(), inner_types })
        }
    }


    fn parse_type_list(&mut self) -> Result<Vec<TypeKind>, ParserError> {
        self.parse_comma_separated(
            TokenType::Greater,
            |p| p.parse_type_expression(),
            "to close the generic types list"
        )
    }




    pub(super) fn convert_param_exprs_into_patterns(&mut self, expr: TypedExpr) -> Result<Vec<MatchPattern>, ParserError> {
        // this function takes what was to the left of `->` and validates it.
        let params = match expr.expression {
            Expr::Tuple(body) => body,
            Expr::Identifier { .. } | Expr::ParserTempTypeAnnotation(_) | Expr::Array(_) => vec![expr],
            _ => return Err(self.error(&format!("Invalid parameter list for function. Found {:?}.", expr))),
        };

        // recursive validation!!!
        params
            .into_iter()
            .map(|p| self.convert_param_expr_into_pattern(p))
            .collect()
    }

    fn convert_param_expr_into_pattern(&mut self, param_expr: TypedExpr) -> Result<MatchPattern, ParserError> {
        match param_expr.expression {
            // 'x: int'
            Expr::ParserTempTypeAnnotation(pattern) => Ok(pattern),
            // 'x'
            Expr::Identifier { name } => Ok(MatchPattern::Binding { name, typ: TypeKind::ParserUnknown }),

            Expr::Assign { pattern, extra_operator: _, value } if value.is_none() => {
                Ok(*pattern)
            }

            // '[x, y]'
            Expr::Array(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_param_expr_into_pattern(element)?);
                }
                Ok(MatchPattern::Array(converted_elements))
            },

            // '(x, y)'
            Expr::Tuple(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_param_expr_into_pattern(element)?);
                }
                Ok(MatchPattern::Tuple(converted_elements))
            }

            _ => Err(self.error(&format!("Invalid syntax in parameter list. Found {:?}.", param_expr))),
        }
    }



    pub(super) fn convert_lhs_assign_into_pattern(&mut self, assign_expr: TypedExpr) -> Result<MatchPattern, ParserError> {
        match assign_expr.expression {
            Expr::Identifier { name } => {
                if name.starts_with("_") { Ok(MatchPattern::Wildcard) }
                else { Ok(MatchPattern::Place(PlaceExpr::Identifier(name))) }
            }
            Expr::Index { left, index } => {
                Ok(MatchPattern::Place(PlaceExpr::Index { left: Rc::new(*left), index: Rc::new(*index) }))
            }

            Expr::Array(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_lhs_assign_into_pattern(element)?);
                }
                Ok(MatchPattern::Array(converted_elements))
            }
            Expr::Tuple(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_lhs_assign_into_pattern(element)?);
                }
                Ok(MatchPattern::Tuple(converted_elements))
            }
            Expr::Deref { expr } => {
                match expr.expression {
                    Expr::Identifier { name } => {
                        Ok(MatchPattern::Place(PlaceExpr::Deref(name)))
                    }
                    _ => Err(self.error("^ is only allowed after identifiers in place expressions."))
                }
            }

            _ => Err(self.error(&format!("Invalid syntax in assignment expression. Found {:?}.", assign_expr))),
        }
    }
    




    pub(super) fn parse_binding_match_pattern(&mut self, type_annotation_required: bool) -> Result<MatchPattern, ParserError> {
        let pattern = match self.peek().token_type.clone() {
            TokenType::Number(num) => { self.advance(); Ok(MatchPattern::Literal(Value::Num(num))) }
            TokenType::Bool(bool) => { self.advance(); Ok(MatchPattern::Literal(Value::Bool(bool))) }
            TokenType::StringStart => {
                self.advance();
                match self.peek().token_type.clone() {
                    TokenType::StringFrag(str) => {
                        self.advance();
                        self.expect_token(TokenType::StringEnd, "- String literals are not allowed in match patterns")?;
                        Ok(MatchPattern::Literal(Value::Str(str)))
                    }
                    TokenType::StringEnd => {
                        self.advance();
                        Ok(MatchPattern::Literal(Value::Str("".to_string())))
                    }
                    _ => Err(self.error("Complex string literals are not allowed in match patterns."))
                }
            }

            TokenType::Identifier(name) => {
                self.advance();
                if name.chars().nth(0).unwrap() == '_' { Ok(MatchPattern::Wildcard) }
                else { self.parse_binding_path(name, type_annotation_required) }
            }
            TokenType::LeftBracket => {
                self.advance();
                Ok(MatchPattern::Array(self.parse_binding_pattern_list(
                    TokenType::RightBracket, type_annotation_required, "to close the array pattern"
                )?))
            }
            TokenType::LeftParen => {
                self.advance();

                let first_pattern = self.parse_binding_match_pattern(type_annotation_required)?;
                if self.optional_token(TokenType::Comma) {
                    // Tuple!
                    let mut tuple_body = vec![first_pattern];
                    tuple_body.extend(self.parse_binding_pattern_list(
                        TokenType::RightParen, type_annotation_required, "to close the tuple pattern"
                    )?);
                    Ok(MatchPattern::Tuple(tuple_body))
                }
                else {
                    self.expect_token(TokenType::RightParen, "to close the grouped pattern")?;
                    Ok(first_pattern)
                }
            }
            TokenType::ColonColon => self.parse_binding_path("".to_string(), type_annotation_required),
            _ => Err(self.error(&format!("Expected a pattern. Found '{}' instead.", self.peek())))
        }?;

        if self.optional_token(TokenType::If) {
            let body = self.parse_expression(Precedence::Arrow)?;
            Ok(MatchPattern::Conditional { pattern: Box::new(pattern), body: Rc::new(body) })
        }
        else { Ok(pattern) }
    }

    pub(super) fn parse_binding_pattern_list(&mut self, end_token: TokenType, type_annotation_required: bool, error_msg: &str) -> Result<Vec<MatchPattern>, ParserError> {
        self.parse_comma_separated(
            end_token,
            |p| p.parse_binding_match_pattern(type_annotation_required),
            error_msg
        )
    }

    fn parse_binding_path(&mut self, path_start: String, type_annotation_required: bool) ->  Result<MatchPattern, ParserError> {
        let mut segments = vec![path_start];
        while self.optional_token(TokenType::ColonColon) {
            segments.push(self.expect_identifier(&format!("after {}", TokenType::ColonColon))?);
        }

        // Binding-pattern
        if segments.len() == 1 {
            let type_annotation = if self.optional_token(TokenType::Colon) {
                self.parse_type_expression()?
            } else { TypeKind::ParserUnknown };
            return Ok(MatchPattern::Binding { name: segments.pop().unwrap(), typ: type_annotation })
        }

        // enum-pattern
        let inner_patterns = if self.optional_token(TokenType::LeftParen) {
            self.parse_binding_pattern_list(TokenType::RightParen, type_annotation_required, "to close the enum variant tuple.")?
        }
        else { Vec::new() };

        let name = segments.pop().unwrap();
        Ok(MatchPattern::EnumVariant { path: segments, name, inner_patterns })
    }
}