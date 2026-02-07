use crate::{ErrType, lexing::tokens::TokenType, parsing::{Parser, ast_structure::{Expr, ExprInfo, MatchPattern, MatchPatternInfo, Span, TupleElement, TupleMatchPattern, TypeKind, Value}}};


impl Parser<'_> {
    pub(super) fn convert_lhs_assign_into_pattern(&mut self, assign_expr: ExprInfo) -> MatchPatternInfo {
        let span = assign_expr.span;

        // anything that can be on the lhs of =
        // x = 2
        // (a, b) = (1, 2)
        // map.get_mut("bla").unwrap() =
        match assign_expr.expression {
            Expr::IdentifierRef { name, mutable: false, var_id: _ } if name.starts_with('_') => {
                MatchPattern::Wildcard
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

            _ => MatchPattern::PlacePointer { expr: assign_expr }
        }
        .to_info(span)
    }
    




    pub(super) fn parse_binding_match_pattern(&mut self, default_to_ref: bool, no_type_annot: bool) -> MatchPatternInfo {
        let token = self.next();
        let s_span = token.span;
        let mut pattern = match token.token {
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
                    TokenType::StringEnd => MatchPattern::Literal(Value::Str(String::new())).to_info(s_span.merge(string_token.span)),

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
                // with a :: its a pattern path
                if let TokenType::ColonColon = self.peek().token {
                    self.next();
                    self.parse_pattern_path(name, s_span, default_to_ref)
                }
                else if name.starts_with('_') {
                    MatchPattern::Wildcard
                    .to_info(s_span)
                }
                // else its just a binding variable!
                else {
                    MatchPattern::Binding { name, mutable: false, var_id: None }
                    .to_info(s_span)
                }
            }

            TokenType::Mut => {
                let (end_span, name) = self.expect_identifier("to name the mut binding");
                MatchPattern::Binding { name, mutable: true, var_id: None }
                .to_info(s_span.merge(end_span))
            }

            TokenType::LeftBracket => {
                let (end_span, elements) = self.parse_binding_pattern_list(
                    TokenType::RightBracket, default_to_ref,  "to close the array pattern"
                );
                MatchPattern::Array(elements)
                .to_info(s_span.merge(s_span.merge(end_span)))
            }

            TokenType::LeftParen => 'l: {
                if let Some(end_span) = self.optional_token(TokenType::RightParen) {
                    break 'l MatchPattern::Tuple(Vec::new()).to_info(s_span.merge(end_span)); 
                }

                let first_pattern = self.parse_tuple_pattern("0".to_string(), default_to_ref);
                if self.optional_token(TokenType::Comma).is_some() {
                    // Tuple!
                    let mut tuple_body = vec![first_pattern];
                    let (end_span, elements) = self.parse_comma_separated(
                        TokenType::RightParen,
                        |p, i| p.parse_tuple_pattern((i+1).to_string(), default_to_ref),
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

            TokenType::ColonColon => self.parse_pattern_path(String::new(), s_span, default_to_ref),

            _ => {
                self.error(ErrType::ParserPatternInvalidSyntax);
                MatchPattern::Wildcard
                .to_info(s_span)
            }
        };

        // optional type annotation after pattern
        pattern.typ = if !no_type_annot && self.optional_token(TokenType::Colon).is_some() {
            self.parse_type_annotation(default_to_ref).typ
        } else {
            TypeKind::ParserUnknown
        };

        // or-patterns
        if self.optional_token(TokenType::Pipe).is_some() {
            let next_pattern = self.parse_binding_match_pattern(default_to_ref, false);
            pattern.span = pattern.span.merge(next_pattern.span);
            pattern.pattern = match next_pattern.pattern {
                MatchPattern::Or(mut next_patterns) => {
                    // inser the old pattern in the new vec, e.g. [pattern, ...next_patterns]
                    next_patterns.insert(0, pattern.clone());
                    MatchPattern::Or(next_patterns)
                }
                _ => MatchPattern::Or(vec![pattern.clone(), next_pattern])
            }
        }

        pattern
    }




    fn parse_tuple_pattern(&mut self, default_label: String, default_to_ref: bool) -> TupleMatchPattern {
        let (label, pattern) = self.parse_tuple_item(
            default_label,
            // no type annotation here because of the overlapping syntax: `let (label: type, label: pattern)`
            // only `(label: pattern)`.
            |p| p.parse_binding_match_pattern(default_to_ref, true),
            |pat| match &pat.pattern {
                MatchPattern::Binding { name, mutable: false, var_id: _ } => Some(name.clone()),
                _ => None
            }
        );
        TupleMatchPattern { label, pattern }
    }




    pub(super) fn parse_binding_pattern_list(&mut self, end_token: TokenType, default_to_ref: bool, error_msg: &str) -> (Span, Vec<MatchPatternInfo>) {
        self.parse_comma_separated(
            end_token,
            |p, _| p.parse_binding_match_pattern(default_to_ref, false),
            error_msg
        )
    }

    fn parse_pattern_path(&mut self, path_start: String, first_span: Span, default_to_ref: bool) ->  MatchPatternInfo {
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
                default_to_ref,
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