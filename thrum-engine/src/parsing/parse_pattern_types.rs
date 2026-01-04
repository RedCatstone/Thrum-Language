use std::rc::Rc;

use crate::{ErrType, lexing::tokens::TokenType, parsing::{Parser, Precedence, ast_structure::{Expr, MatchPattern, PlaceExpr, TupleElement, TupleMatchPattern, TupleType, TypeKind, ExprInfo, Value}}};

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expression(&mut self) -> TypeKind {
        // mut
        if self.optional_token(TokenType::Mut) {
            return TypeKind::MutPointer(Box::new(self.parse_type_expression()))
        }

        // '' Option wrapper
        if self.optional_token(TokenType::Quest) {
            return TypeKind::Struct { name: "Option".to_string(), inner_types: vec![self.parse_type_expression()] }
        }

        // 'str' or 'arr<T>'.
        let str_type = if self.optional_token(TokenType::Null) { String::from("null") }
        else if self.optional_token(TokenType::Minus) { String::from("-") }
        else if self.optional_token(TokenType::Exclamation) { String::from("!") }
        else { self.expect_identifier("to name a type") };

        // '<' start of an inner type
        let inner_types = if self.optional_token(TokenType::Less) { self.parse_type_list() }
        else { Vec::new() };

        self.type_from_str(&str_type, inner_types)
    }

    fn type_from_str(&mut self, str: &str, inner_types: Vec<TypeKind>) -> TypeKind {
        match str {
            "num" => TypeKind::Num,
            "str" => TypeKind::Str,
            "bool" => TypeKind::Bool,
            "tup" => TypeKind::Tup(inner_types.into_iter().enumerate().map(|(i, typ)| TupleType { label: i.to_string(), typ}).collect()),
            "-" => TypeKind::Void,
            "!" => TypeKind::Never,
            "_" => TypeKind::ParserUnknown,
            &_ => TypeKind::Struct { name: str.to_string(), inner_types }
        }
    }


    fn parse_type_list(&mut self) -> Vec<TypeKind> {
        self.parse_comma_separated(
            TokenType::Greater,
            |p, _| p.parse_type_expression(),
            "to close the generic types list"
        )
    }




    pub(super) fn convert_lhs_assign_into_pattern(&mut self, assign_expr: ExprInfo) -> MatchPattern {
        match assign_expr.expression {
            Expr::Identifier { name } => {
                if name.starts_with("_") { MatchPattern::Wildcard }
                else { MatchPattern::Place(PlaceExpr::Identifier(name)) }
            }
            Expr::Index { left, index } => {
                MatchPattern::Place(PlaceExpr::Index { left: Rc::new(*left), index: Rc::new(*index) })
            }

            Expr::Array(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_lhs_assign_into_pattern(element));
                }
                MatchPattern::Array(converted_elements)
            }
            Expr::Tuple(elements) => {
                let mut converted_elements = Vec::new();
                for TupleElement { label, expr } in elements {
                    converted_elements.push(TupleMatchPattern {
                        label,
                        pattern: self.convert_lhs_assign_into_pattern(expr)
                    });
                }
                MatchPattern::Tuple(converted_elements)
            }
            Expr::Deref { expr } => {
                match expr.expression {
                    Expr::Identifier { name } => {
                        MatchPattern::Place(PlaceExpr::Deref(name))
                    }
                    _ => {
                        self.error(ErrType::DefaultString("^ is only allowed after identifiers in place expressions.".to_string()));
                        MatchPattern::Wildcard
                    }
                }
            }

            _ => {
                self.error(ErrType::ParserPatternInvalidSyntax);
                MatchPattern::Wildcard
            }
        }
    }
    




    pub(super) fn parse_binding_match_pattern(&mut self, type_annotation_required: bool) -> MatchPattern {
        let pattern = match self.peek().token_type.clone() {
            TokenType::Number(num) => { self.next(); MatchPattern::Literal(Value::Num(num)) }
            TokenType::Bool(bool) => { self.next(); MatchPattern::Literal(Value::Bool(bool)) }
            TokenType::StringStart => {
                self.next();
                match self.peek().token_type.clone() {
                    TokenType::StringFrag(str) => {
                        self.next();
                        self.expect_token(TokenType::StringEnd, "- String literals are not allowed in match patterns");
                        MatchPattern::Literal(Value::Str(str))
                    }
                    TokenType::StringEnd => {
                        self.next();
                        MatchPattern::Literal(Value::Str("".to_string()))
                    }
                    _ => {
                        self.error(ErrType::ParserPatternTemplateString);
                        MatchPattern::Wildcard
                    }
                }
            }

            TokenType::Identifier(name) => {
                self.next();
                if name.chars().nth(0).unwrap() == '_' { MatchPattern::Wildcard }
                else { self.parse_binding_path(name, type_annotation_required) }
            }
            TokenType::LeftBracket => {
                self.next();
                MatchPattern::Array(self.parse_binding_pattern_list(
                    TokenType::RightBracket, type_annotation_required, "to close the array pattern"
                ))
            }
            TokenType::LeftParen => {
                self.next();
                
                let first_elem_labeled = matches!(self.peek().token_type, TokenType::Dot(_));
                let first_pattern = self.parse_tuple_binding_match_pattern("0".to_string(), type_annotation_required);
                if self.optional_token(TokenType::Comma) | first_elem_labeled {
                    // Tuple!
                    let mut tuple_body = vec![first_pattern];
                    tuple_body.extend(self.parse_comma_separated(
                        TokenType::RightParen,
                        |p, i| p.parse_tuple_binding_match_pattern((i+1).to_string(), type_annotation_required),
                        "to close the tuple pattern"
                    ));
                    tuple_body.sort_by(|a, b| a.label.cmp(&b.label));
                    MatchPattern::Tuple(tuple_body)
                }
                else {
                    self.expect_token(TokenType::RightParen, "to close the grouped pattern");
                    // if first_elem_labeled {
                    //     Err(self.error("If this is supposed to be a tuple, use a trailing comma."))
                    // }
                    first_pattern.pattern
                }
            }
            TokenType::ColonColon => self.parse_binding_path("".to_string(), type_annotation_required),
            _ => {
                self.error(ErrType::ParserPatternInvalidSyntax);
                MatchPattern::Wildcard
            }
        };

        if self.optional_token(TokenType::If) {
            let body = self.parse_expression(Precedence::Arrow);
            MatchPattern::Conditional { pattern: Box::new(pattern), body: Rc::new(body) }
        }
        else { pattern }
    }




    fn parse_tuple_binding_match_pattern(&mut self, default_label: String, type_annotation_required: bool) -> TupleMatchPattern {
        if let Some(label) = self.optional_dot_token() {
            // named tuple element
            let pattern = if self.optional_token(TokenType::Equal) {
                    self.parse_binding_match_pattern(type_annotation_required)
                }
                else {
                    let typ = if self.optional_token(TokenType::Colon) {
                        self.parse_type_expression()
                    }
                    else { TypeKind::ParserUnknown };
                    MatchPattern::Binding { name: label.clone(), typ }
                };
            
            TupleMatchPattern { label, pattern }
        }
        else {
            let pattern = self.parse_binding_match_pattern(type_annotation_required);
            TupleMatchPattern { label: default_label, pattern }
        }
    }




    pub(super) fn parse_binding_pattern_list(&mut self, end_token: TokenType, type_annotation_required: bool, error_msg: &str) -> Vec<MatchPattern> {
        self.parse_comma_separated(
            end_token,
            |p, _| p.parse_binding_match_pattern(type_annotation_required),
            error_msg
        )
    }

    fn parse_binding_path(&mut self, path_start: String, type_annotation_required: bool) ->  MatchPattern {
        let mut segments = vec![path_start];
        while self.optional_token(TokenType::ColonColon) {
            segments.push(self.expect_identifier(&format!("after {}", TokenType::ColonColon)));
        }

        // Binding-pattern
        if segments.len() == 1 {
            let type_annotation = if self.optional_token(TokenType::Colon) {
                self.parse_type_expression()
            } else { TypeKind::ParserUnknown };
            return MatchPattern::Binding { name: segments.pop().unwrap(), typ: type_annotation }
        }

        // enum-pattern
        let inner_patterns = if self.optional_token(TokenType::LeftParen) {
            self.parse_binding_pattern_list(TokenType::RightParen, type_annotation_required, "to close the enum variant tuple.")
        }
        else { Vec::new() };

        let name = segments.pop().unwrap();
        MatchPattern::EnumVariant { path: segments, name, inner_patterns }
    }
}