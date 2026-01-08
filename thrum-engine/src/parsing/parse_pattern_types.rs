use std::rc::Rc;

use crate::{ErrType, lexing::tokens::TokenType, parsing::{Parser, ast_structure::{Expr, ExprInfo, MatchPattern, MatchPatternInfo, Span, TupleElement, TupleMatchPattern, TupleType, TypeKind, Value}}};

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expression(&mut self) -> TypeKind {
        // mut
        if self.optional_token(TokenType::Mut).is_some() {
            return TypeKind::MutPointer(Box::new(self.parse_type_expression()))
        }

        // '' Option wrapper
        if self.optional_token(TokenType::Quest).is_some() {
            return TypeKind::Struct { name: "Option".to_string(), inner_types: vec![self.parse_type_expression()] }
        }

        // 'str' or 'arr<T>'.
        let str_type = if self.optional_token(TokenType::Null).is_some() { String::from("null") }
        else if self.optional_token(TokenType::Minus).is_some() { String::from("-") }
        else if self.optional_token(TokenType::Exclamation).is_some() { String::from("!") }
        else { self.expect_identifier("to name a type").1 };

        // '<' start of an inner type
        let inner_types = if self.optional_token(TokenType::Less).is_some() { self.parse_type_list() }
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
        ).1
    }




    pub(super) fn convert_lhs_assign_into_pattern(&mut self, assign_expr: ExprInfo) -> MatchPatternInfo {
        let span = assign_expr.span;
        match assign_expr.expression {
            Expr::Identifier { name, var_id: _ } => {
                if name.starts_with("_") { MatchPattern::Wildcard }
                else { MatchPattern::PlaceIdentifier { name, var_id: None } }
            }
            Expr::Index { left, index } => {
                MatchPattern::PlaceIndex { left: Rc::new(*left), index: Rc::new(*index) }
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
                    Expr::Identifier { name, var_id: _ } => {
                        MatchPattern::PlaceDeref { name, var_id: None }
                    }
                    _ => {
                        self.error_with_span(ErrType::DefaultString("^ is only allowed after identifiers in place expressions.".to_string()), span);
                        MatchPattern::Wildcard
                    }
                }
            }

            _ => {
                self.error_with_span(ErrType::ParserPatternInvalidSyntax, span);
                MatchPattern::Wildcard
            }
        }
        .to_info(span)
    }
    




    pub(super) fn parse_binding_match_pattern(&mut self, type_annotation_required: bool) -> MatchPatternInfo {
        let token = self.next();
        let s_span = token.span;
        match token.token {
            TokenType::Number(num) => MatchPattern::Literal(Value::Num(num)).to_info(s_span),
            TokenType::Bool(bool) => MatchPattern::Literal(Value::Bool(bool)).to_info(s_span),
            TokenType::StringStart => {
                let string_token = self.next();
                match string_token.token {
                    // 1 string frag case
                    TokenType::StringFrag(str) => {
                        let end_span = self.expect_token(TokenType::StringEnd, "- String literals are not allowed in match patterns");
                        MatchPattern::Literal(Value::Str(str)).to_info(s_span.merge(end_span))
                    }
                    // empty string case
                    TokenType::StringEnd => MatchPattern::Literal(Value::Str("".to_string())).to_info(s_span.merge(string_token.span)),

                    // template strings are not allowed in match patterns
                    TokenType::LeftBrace => {
                        self.error(ErrType::ParserPatternTemplateString);
                        self.recover(&[TokenType::StringEnd]);
                        MatchPattern::Wildcard.to_info(s_span.merge(string_token.span))
                    }
                    // lexer should make sure that there are no other tokens possible here
                    _ => unreachable!()
                }
            }

            TokenType::Identifier(name) => {
                if name.chars().nth(0).unwrap() == '_' {
                    MatchPattern::Wildcard
                    .to_info(s_span)
                }
                // with a :: its a pattern path
                else if let TokenType::ColonColon = self.peek().token {
                    self.next();
                    self.parse_pattern_path(name, s_span, type_annotation_required)
                }
                // else its just a binding variable!
                else {
                    let type_annotation = if self.optional_token(TokenType::Colon).is_some() {
                        self.parse_type_expression()
                    } else { TypeKind::ParserUnknown };

                    MatchPattern::Binding { name, mutable: false, typ: type_annotation, var_id: None }
                    .to_info(s_span)
                }
            }

            TokenType::Mut => {
                let (end_span, name) = self.expect_identifier("to name the mut binding");
                let type_annotation = if self.optional_token(TokenType::Colon).is_some() {
                    self.parse_type_expression()
                } else { TypeKind::ParserUnknown };

                MatchPattern::Binding { name, mutable: true, typ: type_annotation, var_id: None }
                .to_info(s_span.merge(end_span))
            }

            TokenType::LeftBracket => {
                let (end_span, elements) = self.parse_binding_pattern_list(
                    TokenType::RightBracket, type_annotation_required, "to close the array pattern"
                );
                MatchPattern::Array(elements)
                .to_info(s_span.merge(s_span.merge(end_span)))
            }

            TokenType::LeftParen => {
                let first_elem_labeled = matches!(self.peek().token, TokenType::Dot(_));
                let first_pattern = self.parse_tuple_pattern("0".to_string(), type_annotation_required);
                if self.optional_token(TokenType::Comma).is_some() | first_elem_labeled {
                    // Tuple!
                    let mut tuple_body = vec![first_pattern];
                    let (end_span, elements) = self.parse_comma_separated(
                        TokenType::RightParen,
                        |p, i| p.parse_tuple_pattern((i+1).to_string(), type_annotation_required),
                        "to close the tuple pattern"
                    );
                    tuple_body.extend(elements);
                    tuple_body.sort_by(|a, b| a.label.cmp(&b.label));
                    MatchPattern::Tuple(tuple_body)
                    .to_info(s_span.merge(end_span))
                }
                else {
                    self.expect_token(TokenType::RightParen, "to close the grouped pattern");
                    // if first_elem_labeled {
                    //     Err(self.error("If this is supposed to be a tuple, use a trailing comma."))
                    // }
                    first_pattern.pattern
                }
            }

            TokenType::ColonColon => self.parse_pattern_path("".to_string(), s_span, type_annotation_required),

            _ => {
                self.error(ErrType::ParserPatternInvalidSyntax);
                MatchPattern::Wildcard
                .to_info(s_span)
            }
        }
    }




    fn parse_tuple_pattern(&mut self, default_label: String, type_annotation_required: bool) -> TupleMatchPattern {
        if let Some((dot_span, label)) = self.optional_dot_token() {
            // named tuple element
            let pattern = if self.optional_token(TokenType::Equal).is_some() {
                    self.parse_binding_match_pattern(type_annotation_required)
                }
                else {
                    let typ = if self.optional_token(TokenType::Colon).is_some() {
                        self.parse_type_expression()
                    }
                    else { TypeKind::ParserUnknown };
                    MatchPattern::Binding { name: label.clone(), mutable: false, typ, var_id: None }
                    .to_info(dot_span)
                };
            
            TupleMatchPattern { label, pattern }
        }
        else {
            let pattern = self.parse_binding_match_pattern(type_annotation_required);
            TupleMatchPattern { label: default_label, pattern }
        }
    }




    pub(super) fn parse_binding_pattern_list(&mut self, end_token: TokenType, type_annotation_required: bool, error_msg: &str) -> (Span, Vec<MatchPatternInfo>) {
        self.parse_comma_separated(
            end_token,
            |p, _| p.parse_binding_match_pattern(type_annotation_required),
            error_msg
        )
    }

    fn parse_pattern_path(&mut self, path_start: String, first_span: Span, type_annotation_required: bool) ->  MatchPatternInfo {
        let mut path = vec![path_start];
        let mut end_span;

        loop {
            let (span, name) = self.expect_identifier(&format!("after {}", TokenType::ColonColon));
            path.push(name);
            end_span = span;
            if self.optional_token(TokenType::ColonColon).is_none() {
                break;
            }
        }

        // enum-pattern
        let inner_patterns = if self.optional_token(TokenType::LeftParen).is_some() {
            let (span, inner_patterns) = self.parse_binding_pattern_list(
                TokenType::RightParen,
                type_annotation_required,
                "to close the enum variant tuple."
            );
            end_span = span;
            inner_patterns
        }
        else { Vec::new() };

        let name = path.pop().unwrap();

        MatchPattern::EnumVariant { path, name, inner_patterns }
        .to_info(first_span.merge(end_span))
    }
}