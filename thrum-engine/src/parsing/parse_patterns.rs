use crate::{ErrType, lexing::tokens::{Span, TokenKind}, parsing::{Parser, ast::{AstTupleElement, AstTuplePattern, Expr, ExprId, Pattern, PatternId}, parse_expressions::Precedence}};


impl Parser<'_> {
    /// it enters binding mode when it sees a `let`
    /// `x is let y` => Binding y
    /// `x is y` => CompareExpr(IdentifierRef y)
    fn parse_one_pattern(&mut self, binding_mode: bool) -> PatternId {
        let token = self.peek();
        let start = token.span;

        match token.token {
            TokenKind::Let => {
                // enter binding mode!
                self.next();
                self.parse_pattern(true)
            }

            TokenKind::Identifier => {
                let name: Box<str> = self.get_from_source(token.span).into();
                if name.starts_with('_') {
                    // x is _
                    // x is let _
                    self.next();
                    self.add_pattern(start, Pattern::Wildcard)
                }
                else if self.peek_spaces_after() == 0 && self.peek_one_further().token == TokenKind::LeftBrace {
                    // x is N{ ... }
                    // x is let N{ ... }
                    self.next(); // consume <ident>
                    self.next(); // consume '{'
                    let typ = self.add_expr(start, Expr::IdentifierRef { name: name.to_string(), mutable: false });
                    let data = self.parse_tuple_pattern(self.prev_token_span, None, binding_mode, TokenKind::RightBrace);
                    
                    self.add_pattern(start, Pattern::TypeDestructor { typ, data })
                }
                else if binding_mode {
                    // x is let a
                    self.next();
                    self.add_pattern(start, Pattern::Binding { name, mutable: false })
                } else {
                    // x is y
                    let expr = self.parse_expression_default();
                    self.add_pattern(start, Pattern::CompareExpr(expr))
                }
            }

            TokenKind::LeftParen => {
                // can be a grouped expression: `x is (1 | 2)`
                // or a tuple: `x is (1, 2)`
                self.next();
                if self.optional_token(TokenKind::RightParen) {
                    self.add_pattern(start, Pattern::Tuple(Vec::new()))
                }
                else {
                    let first_pattern = self.parse_one_tuple_pattern(binding_mode, "0".to_string());
                    if self.optional_token(TokenKind::Comma) {
                        // Tuple!
                        self.parse_tuple_pattern(start, Some(first_pattern), binding_mode, TokenKind::RightParen)
                    }
                    else {
                        // normal grouped pattern
                        self.expect_token(TokenKind::RightParen, "to close the grouped pattern");
                        // if first_pattern.label == "0" {
                        //     self.error(ErrType::DefaultString("If this is supposed to be a tuple, use a trailing comma.".to_string()));
                        // }
                        first_pattern.pattern
                    }
                }
            }

            TokenKind::Dot => {
                self.next();
                // .Some(T)
                let variant_name = self.expect_identifier("to name an enum variant");

                let attached_tuple = self.optional_token(TokenKind::LeftParen).then(|| {
                    self.parse_tuple_pattern(self.prev_token_span, None, binding_mode, TokenKind::RightParen)
                });

                self.add_pattern(start, Pattern::EnumVariant { name: variant_name, attached_tuple })
            }

            TokenKind::Exclamation => {
                self.next(); // consume '!'
                let pat = self.parse_pattern(binding_mode);
                self.add_pattern(start, Pattern::Not(pat))
            }


            // literals in binding mode:
            // `x is 5 + 2` would parse as `x is (5 + 2)`
            // `x is let 5 + 2` would parse as `(x is 5) + 2`
            TokenKind::Number | TokenKind::Bool(_) if binding_mode => {
                let expr = self.parse_prefix();
                self.add_pattern(start, Pattern::CompareExpr(expr))
            }
            
            TokenKind::Mut if binding_mode => {
                // let mut x = ...
                self.next();
                let name = self.expect_identifier("after mut").into_boxed_str();
                self.add_pattern(start, Pattern::Binding { name, mutable: true })
            }

            _ if !binding_mode => {
                // in non-binding mode any expressions are allowed
                // `x is 5`
                let expr = self.parse_expression_default();
                self.add_pattern(start, Pattern::CompareExpr(expr))
            }

            found => {
                // in binding_mode any of the arms above should've already found something...
                self.error(ErrType::ParserExpectedABindingPattern { found });
                self.add_pattern(start, Pattern::Wildcard)
            }
        }
    }


    pub(super) fn parse_pattern(&mut self, binding_mode: bool) -> PatternId {
        let mut pattern = self.parse_one_pattern(binding_mode);
        let start = self.ast.get_pattern_span(pattern);

        // or pattern!
        if self.optional_token(TokenKind::Pipe) {
            let mut patterns = vec![pattern];
            loop {
                patterns.push(self.parse_one_pattern(binding_mode));
                if !self.optional_token(TokenKind::Pipe) {
                    break;
                }
            }
            pattern = self.add_pattern(start, Pattern::Or(patterns));
        }

        // optional type annotation after pattern
        if self.optional_token(TokenKind::Colon) {
            let typ = self.parse_expression(Precedence::Assign);
            pattern = self.add_pattern(start, Pattern::Typed { pattern, typ });
        }

        // extra condition after pattern
        if self.optional_token(TokenKind::And) {
            let cond = self.parse_expression_default();
            pattern = self.add_pattern(start, Pattern::Conditional { pattern, cond });
        }

        pattern
    }




    fn parse_one_tuple_pattern(&mut self, binding_mode: bool, default_label: String) -> AstTuplePattern {
        let (label, pattern) = self.parse_tuple_item(
            default_label,
            |p| p.parse_pattern(binding_mode),
            |p, name| p.add_pattern(p.prev_token_span, Pattern::Binding { name: name.into_boxed_str(), mutable: false })
        );
        AstTuplePattern { label, pattern }
    }

    fn parse_tuple_pattern(&mut self, start: Span, first_elem: Option<AstTuplePattern>, binding_mode: bool, end_token: TokenKind) -> PatternId {
        let mut tuple_body = vec![];
        let has_first_elem = first_elem.is_some();
        if let Some(x) = first_elem {
            tuple_body.push(x);
        }

        let other_tuple_elems = self.parse_comma_separated(
            end_token,
            |p, i| {
                p.parse_one_tuple_pattern(binding_mode, (if has_first_elem { i + 1 } else { i }).to_string())
            },
            "to close the tuple"
        );
        tuple_body.extend(other_tuple_elems);
        self.add_pattern(start, Pattern::Tuple(tuple_body))
    }





    pub(super) fn convert_expr_into_assign_pattern(&mut self, expr: ExprId) -> PatternId {
        let start = self.ast.get_expr_span(expr);

        // anything that can be on the lhs of =
        // x = 2
        // (a, let b) = (1, 2)
        // map.get_mut("bla").unwrap() =
        match self.ast.get_expr(expr) {
            Expr::EmptyLet { pattern } => *pattern,

            Expr::IdentifierRef { name, mutable: false } if name.starts_with('_') => {
                self.add_pattern(start, Pattern::Wildcard)
            }

            Expr::Tuple { elems } => {
                let converted_elems = elems.clone()
                    .into_iter()
                    .map(|AstTupleElement { label, expr }|
                        AstTuplePattern { label, pattern: self.convert_expr_into_assign_pattern(expr) }
                    )
                    .collect();

                self.add_pattern(start, Pattern::Tuple(converted_elems))
            }

            _ => self.add_pattern(start, Pattern::PlacePointer(expr))
        }
    }
}