use crate::{
    ErrType, WarnType, lexing::{self, tokens::{AssignOp, Span, TokenKind, TokenSpan}},
    parsing::{Parser, ast::{AstClosure, AstEnumExpression, AstMatchArm, AstTupleElement, AstValue, Expr, ExprId}}
};



#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Assign,     // =, +=, -=, etc.
    Range,      // 1..2
    // Nullish,    // ??
    Or,         // |
    And,        // &
    Comparison, // ==, !=
    LessGreater,// <, >, <=, >=
    Is,         // is
    Sum,        // +, -
    Product,    // *, /, %
    Prefix,     // ! -
    CallIndex,  // square(X), array[i], arr.len, Option::Some
    Postfix,    // ^
}
impl Precedence {
    pub const fn get_precedence(token_type: TokenKind) -> Self {
        match token_type {
            TokenKind::Assign { .. } => Self::Assign,
            TokenKind::DotDot | TokenKind::DotDotEqual => Self::Range,
            TokenKind::Or => Self::Or,
            TokenKind::And => Self::And,
            TokenKind::EqualEqual | TokenKind::NotEqual => Self::Comparison,
            TokenKind::Less | TokenKind::Greater | TokenKind::LessEqual | TokenKind::GreaterEqual => Self::LessGreater,
            TokenKind::Is => Self::Is,
            TokenKind::Op(AssignOp::Plus | AssignOp::Minus) => Self::Sum,
            TokenKind::Op(AssignOp::Star | AssignOp::Slash | AssignOp::Percent) => Self::Product,
            TokenKind::LeftParen | TokenKind::LeftBracket | TokenKind::Dot | TokenKind::ColonColon => Self::CallIndex,
            TokenKind::Caret => Self::Postfix,
            _ => Self::Lowest,
        }
    }
}


impl Parser<'_> {
    pub(super) fn parse_expression_default(&mut self) -> ExprId {
        self.parse_expression(Precedence::Lowest)
    }

    pub(super) fn parse_expression(&mut self, precedence: Precedence) -> ExprId {
        // examples of prefixes:
        // 1
        // !(1 + 2)
        // Vec::new(data = [1, 2, 3])
        // if x { 1 } else { 2 }
        let mut left_expr = self.parse_prefix();

        // Pratt parser loop
        loop {
            let peek_op = self.peek().clone();
            let op_precedence = Precedence::get_precedence(peek_op.token);
            
            // Not an infix operator.
            if op_precedence == Precedence::Lowest { break }

            // only includes the next operator if it binds stronger than the current one.
            // 1 + 2 * 3   -> this would consume until * and only afterwards process +
            // 1 * 2 + 3   -> this would stop at * and process + afterwards
            if precedence >= op_precedence { break }
            
            // 1 level lower for right associativity
            // ** is special because 2**3**2 should get parsed as: 2**(3**2)
            // if op_token.token == TokenKind::StarStar { op_precedence = Precedence::Product }

            // operators that are not allowed to be line-split.
            // this is so semicolons are actually not needed in the language,
            // for example here the parser would normally want to keep consuming the ( as a function call
            // let a = 1
            // (a, b) = (b, a + b)
            if let TokenKind::LeftParen | TokenKind::LeftBracket | TokenKind::ColonColon | TokenKind::Is = peek_op.token
            && !self.peek_is_on_same_line() {
                break;
            }

            // special handling for normal Ops: + - * / ...
            if let TokenKind::Op(_) = peek_op.token {
                // this - is an infix operator.
                // 5
                // - 2

                // this - is not an infix operator => break
                // 5
                // -2
                if !self.peek_is_on_same_line() && self.peek_spaces_after() == 0 {
                    break;
                }

                if self.peek_is_on_same_line() && self.peek_further_is_on_same_line() {
                    // warnings:
                    if self.peek_spaces_before() != self.peek_spaces_after() {
                        self.warn(WarnType::ParserInconsistentSpacingAroundInfixOp { op: peek_op.token });
                    }
                }
            }

            self.next(); // consume the operator

            // update the left expression with the new infix result.
            left_expr = self.parse_infix(left_expr, &peek_op, op_precedence);
        }
        left_expr
    }



    fn parse_infix(&mut self, left_expr: ExprId, op: &TokenSpan, op_precedence: Precedence) -> ExprId {
        let start = self.ast.get_expr_span(left_expr);

        if let Precedence::And | Precedence::Or | Precedence::Comparison
            | Precedence::LessGreater | Precedence::Sum | Precedence::Product = op_precedence {
            let right = self.parse_expression(op_precedence);
            return self.add_expr(start, Expr::Infix { op: op.token, op_span: op.span, left: left_expr, right })
        }

        match op.token {
            TokenKind::Dot => {
                // supports both x.member and x.2
                let member = self.expect_identifier_relaxed("to name the member");
                let member_expr = self.add_expr(start, Expr::MemberAccess { left: left_expr, member });
                self.wrap_in_optional_type_instantiation(member_expr)
            },

            TokenKind::ColonColon => {
                let member = self.expect_identifier("to name the path");
                self.add_expr(start, Expr::TypeMemberAccess { left: left_expr, member })
            }
            
            TokenKind::Assign { extra_op } => {
                let pattern = self.convert_expr_into_assign_pattern(left_expr);
                let value = self.parse_expression_default();

                self.add_expr(start, Expr::Assign { pattern, value, extra_op, op_span: op.span })
            },

            TokenKind::Caret => {
                // move/clone operator
                self.add_expr(start, Expr::Move { expr: left_expr })
            },
            
            TokenKind::Is => {
                let pattern = self.parse_pattern(false);

                self.add_expr(start, Expr::Is { value: left_expr, pattern })
            }
            
            TokenKind::LeftParen => {
                let arguments = self.parse_comma_seperated_expressions(
                    TokenKind::RightParen,
                    "to close the function arguments list"
                );
                self.add_expr(start, Expr::Call { callee: left_expr, arguments })
            },

            TokenKind::LeftBracket => {
                let index = self.parse_expression_default();
                self.expect_token(TokenKind::RightBracket, "to close the index expression");
                self.add_expr(start, Expr::Index { left: left_expr, index })
            },

            TokenKind::DotDot => {
                let right = self.parse_expression_default();

                let range_type = self.add_expr(start, Expr::IdentifierRef { name: "Range".to_string(), mutable: false });
                let data = self.add_expr(start, Expr::Tuple { elems: vec![
                    AstTupleElement { label: "start".to_string(), expr: left_expr },
                    AstTupleElement { label: "end".to_string(), expr: right },
                ] });
                self.add_expr(start, Expr::TypeInstantiation { typ: range_type, data })
            }

            _ => unreachable!("parse_infix() should not be called with op_token: {op:?}")
        }
    }









    pub(super) fn parse_prefix(&mut self) -> ExprId {
        let op = self.next();
        let start = op.span;

        match op.token {
            TokenKind::Exclamation | TokenKind::Op(AssignOp::Minus) => {
                let right = self.parse_expression(Precedence::Prefix);
                self.add_expr(start, Expr::Prefix { op: op.token, right })
            }

            TokenKind::Identifier => {
                let name = self.get_from_source(op.span).to_string();
                let name_expr = self.add_expr(start, Expr::IdentifierRef { name, mutable: false });
                self.wrap_in_optional_type_instantiation(name_expr)
            }

            TokenKind::Mut => {
                let name = self.expect_identifier("after mut");
                self.add_expr(start, Expr::IdentifierRef { name, mutable: true })
            }
            
            TokenKind::Number => self.extract_number_expr_from_source(start),

            TokenKind::Bool(val) => self.add_expr(start, Expr::Literal { val: AstValue::Bool(val) }),

            TokenKind::Op(AssignOp::Star) => {
                let expr = self.parse_expression(Precedence::Prefix);
                self.add_expr(start, Expr::Move { expr })
            },

            TokenKind::LeftBrace => self.parse_block_expression(TokenKind::RightBrace, start),

            TokenKind::LeftParen => {
                // empty tuple case '()'
                if self.optional_token(TokenKind::RightParen) {
                    self.add_expr(start, Expr::Tuple { elems: Vec::new() })
                }
                else {
                    let first_elem = self.parse_one_tuple_expression("0".to_string());
                    if self.optional_token(TokenKind::Comma) {
                        // , means its a tuple!
                        // e.g. (1, 2) (1,) (x: 1,)
                        self.parse_tuple_expression(start, Some(first_elem), TokenKind::RightParen)
                    }
                    else if self.optional_token(TokenKind::Semicolon) {
                        // ; means its a tuple array
                        // (0; 4)
                        let length = self.parse_expression_default();
                        self.expect_token(TokenKind::RightParen, "to close the array expression");

                        self.add_expr(start, Expr::TupleArr { elem: first_elem.expr, length })
                    } else {
                        // normal grouped expression
                        self.expect_token(TokenKind::RightParen, "to close the grouped expression");
                        // if first_elem.label == "0" {
                        //     self.error(ErrType::DefaultString("If this is supposed to be a tuple, use a trailing comma.".to_string()));
                        // }
                        first_elem.expr
                    }
                }
            },

            TokenKind::StringStart => {
                let mut elems = Vec::new();
                let mut had_expr = false;

                while !self.optional_token(TokenKind::StringEnd) {
                    if self.optional_token(TokenKind::StringFrag) {
                        // extract the string from the source
                        // we also need to handle backslashes here
                        let source_frag = self.get_from_source(self.prev_token_span);
                        let s = lexing::lex_string_from(source_frag);

                        elems.push(self.add_expr(self.prev_token_span, Expr::Literal { val: AstValue::Str(s) }));
                    } else {
                        elems.push(self.parse_expression_default());
                        had_expr = true;
                    }
                }

                match elems[..] {
                    [first] if !had_expr => first,
                    [] => self.add_expr(start, Expr::Literal { val: AstValue::Str(String::new()) }),
                    _ => self.add_expr(start, Expr::TemplateString { elems }),
                }
            },

            TokenKind::Let => {
                // this only adds an EmptyLet expr, because let has multiple use cases
                // e.g.: `(a, let b) = 2`  `x is let .Some(a)`
                let pattern = self.parse_pattern(true);
                self.add_expr(start, Expr::EmptyLet { pattern })
            },

            TokenKind::Const => {
                let pattern = self.parse_pattern(true);
                self.expect_token(TokenKind::Assign { extra_op: None }, "to assign a value to the const.");
                let value = self.parse_expression_default();

                self.add_expr(start, Expr::Const { pattern, value })
            }
            TokenKind::Type => {
                let name = self.expect_identifier("to name the type").into_boxed_str();
                self.expect_token(TokenKind::Assign { extra_op: None }, "to assign a value to the type.");
                let value = self.parse_expression_default();

                self.add_expr(start, Expr::CustomType { name, value })
            }

            TokenKind::If => {
                let condition = self.parse_expression(Precedence::Lowest);
                let (then, alt) = self.parse_if_and_else();
                self.add_expr(start, Expr::If { condition, then, alt })
            },

            TokenKind::Ensure => {
                let condition = self.parse_expression(Precedence::Lowest);
                self.expect_token(TokenKind::Else, "after the ensure condition");
                let alt = self.parse_expression_default();
                let then = self.add_expr(start.to_0_width_right(), Expr::Void);

                self.add_expr(start, Expr::Ensure { condition, alt, then })
            },

            TokenKind::While => {
                let label = self.optional_label().unwrap_or_else(|| "while".to_string());
                let condition = self.parse_expression_default();
                let body = self.parse_arrow_or_block_expression("while");

                self.add_expr(start, Expr::While { condition, body, label })
            },

            TokenKind::For => {
                let label = self.optional_label().unwrap_or_else(|| "for".to_string());
                let pattern = self.parse_pattern(true);
                self.expect_token(TokenKind::In, "after for-loop pattern");
                let iter_expr = self.parse_expression_default();
                let body = self.parse_arrow_or_block_expression("for");

                self.add_expr(start, Expr::For { pattern, iter_expr, body, label })
            },

            TokenKind::Loop => {
                let label = self.optional_label().unwrap_or_else(|| "loop".to_string());
                let body = self.parse_arrow_or_block_expression("loop");

                self.add_expr(start, Expr::Loop { body, label })
            },

            TokenKind::Match => {
                let match_value = self.parse_expression(Precedence::Is);
                let mut arms = Vec::new();

                while self.optional_token(TokenKind::Is) {
                    let pattern = self.parse_pattern(false);
                    let body = self.parse_arrow_or_block_expression("match arm");
                    arms.push(AstMatchArm { pattern, body });
                }

                self.add_expr(start, Expr::Match { match_value, arms })
            },
            
            TokenKind::Enum => {
                // enum { Some(T), None }
                self.expect_token(TokenKind::LeftBrace, "to open the enum definition block");
                let variants = self.parse_comma_separated(
                    TokenKind::RightBrace,
                    |p, _| p.parse_enum_variant(),
                    "to close the enum definition block"
                );
                self.add_expr(start, Expr::EnumDefinition { variants })
            },

            TokenKind::Impl => {
                let typ = self.parse_expression_default();
                self.expect_token(TokenKind::LeftBrace, "to open the impl definition block");
                
                let const_exprs = self.parse_line_seperated(
                    TokenKind::RightBrace,
                    Self::parse_expression_default,
                    |_| None
                );

                self.add_expr(start, Expr::ImplBlock { typ, const_exprs })
            }

            TokenKind::ImplSelf => {
                self.add_expr(start, Expr::ImplSelf)
            }

            TokenKind::Dot => {
                // enum variant!
                let data = self.parse_enum_variant();
                self.add_expr(start, Expr::EnumVariant { data })
            }

            TokenKind::Fn => {
                let name = self.expect_identifier("to name the function").into_boxed_str();
                self.expect_token(TokenKind::LeftParen, "to open the fn definition paramter list");

                let params = self.parse_comma_separated(
                    TokenKind::RightParen,
                    |p, _| p.parse_pattern(true),
                    "to close the fn definition parameter list"
                )
                .into_boxed_slice();

                let return_type = self.optional_token(TokenKind::MinusArrow)
                    .then(|| self.parse_expression(Precedence::Lowest));

                let body = self.parse_arrow_or_block_expression("function body");
    
                self.add_expr(start, Expr::FnDefinition { name, closure: AstClosure { params, return_type, body } })
            },

            TokenKind::Pipe => {
                // Closure!
                let params = self.parse_comma_separated(
                    TokenKind::EqualArrow,
                    |p, _| p.parse_pattern(true),
                    "to close the fn definition parameter list"
                )
                .into_boxed_slice();

                let body = self.parse_expression_default();

                self.add_expr(start, Expr::Closure { closure: AstClosure { params, return_type: None, body }, requires_type_annotation: false })
            }

            TokenKind::Return => {
                let expr = self.parse_optional_expression().unwrap_or_else(|| self.add_expr(self.prev_token_span.to_0_width_right(), Expr::Void));
                self.add_expr(start, Expr::Return { expr })
            },

            TokenKind::Break => {
                let label = self.optional_label();
                let expr = self.parse_optional_expression().unwrap_or_else(|| self.add_expr(self.prev_token_span.to_0_width_right(), Expr::Void));
                self.add_expr(start, Expr::Break { expr, label })
            },

            TokenKind::Continue => {
                let label = self.optional_label();
                self.add_expr(start, Expr::Continue { label })
            }

            TokenKind::Ampersand => {
                let mutable = self.optional_token(TokenKind::Mut);
                let expr = self.parse_expression(Precedence::Prefix);
                self.add_expr(start, Expr::Borrow { expr, mutable })
            }

            _ => {
                self.error_with_span(ErrType::ParserExpectedAnExpression { found: op.token }, op.span);
                self.add_expr(start, Expr::ParserError)
            }
        }
    }





    pub(super) fn parse_block_expression(&mut self, end_token: TokenKind, start: Span) -> ExprId {
        // '{' already consumed.
        let label = self.optional_label();

        let exprs = self.parse_line_seperated(
            end_token,
            Self::parse_expression_default,
            |p| Some(p.add_expr(p.prev_token_span, Expr::Void))
        );

        self.add_expr(start, Expr::Block { exprs, label })
    }

    pub(super) fn parse_arrow_or_block_expression(&mut self, block_name: &str) -> ExprId {
        if self.optional_token(TokenKind::EqualArrow) {
            // => ...
            if !self.peek_is_on_same_line() {
                self.error(ErrType::ParserArrowExprsHaveToBeOnSameLine);
            }
            let expr = self.parse_expression_default();
            self.add_expr(self.prev_token_span, Expr::Block { exprs: vec![expr], label: None })
        }
        else if self.optional_token(TokenKind::LeftBrace) {
            // { ... }
            let label = self.optional_label();
    
            let exprs = self.parse_line_seperated(
                TokenKind::RightBrace,
                Self::parse_expression_default,
                |p| Some(p.add_expr(p.prev_token_span, Expr::Void))
            );
    
            self.add_expr(self.prev_token_span, Expr::Block { exprs, label })
        }
        else {
            self.error(ErrType::ParserExpectToken {
                expected: [TokenKind::EqualArrow, TokenKind::LeftBrace].into(),
                err_msg: format!("to open the {block_name} block"),
                found: self.peek().token
            });
            self.add_expr(self.prev_token_span, Expr::ParserError)
        }
    }


    fn parse_if_and_else(&mut self) -> (ExprId, ExprId) {
        let then = self.parse_arrow_or_block_expression("if");
        let alt = if self.optional_token(TokenKind::Else) {
            self.parse_expression_default()
        } else {
            let then_span = self.ast.get_expr_span(then);
            self.add_expr(then_span.to_0_width_right(), Expr::Void)
        };

        (then, alt)
    }

    fn parse_enum_variant(&mut self) -> AstEnumExpression {
        // Some(T)
        let variant_name = self.expect_identifier("to name an enum variant").into_boxed_str();

        let attached_tuple = self.optional_token(TokenKind::LeftBrace).then(|| {
            self.parse_tuple_expression(self.prev_token_span, None, TokenKind::RightBrace)
        });

        AstEnumExpression { variant_name, attached_tuple }
    }

    fn wrap_in_optional_type_instantiation(&mut self, wrap: ExprId) -> ExprId {
        // Number{ 3 } is only allowed if there is no space after 'Number'
        if self.peek_spaces_before() == 0 && self.optional_token(TokenKind::LeftBrace) {
            let data = self.parse_tuple_expression(self.prev_token_span, None, TokenKind::RightBrace);
            
            let wrap_span = self.ast.get_expr_span(wrap);
            self.add_expr(wrap_span, Expr::TypeInstantiation { typ: wrap, data })
        } else {
            wrap
        }
    }





    fn parse_comma_seperated_expressions(&mut self, end_token: TokenKind, err_msg: &str) -> Vec<ExprId> {
        // '[1, 2, 3]',   '(1, 2)',   dict { 1, 2 }
        self.parse_comma_separated(
            end_token,
            |p, _| p.parse_expression_default(),
            err_msg
        )
    }

    pub(super) fn parse_tuple_item<Id>(
        &mut self,
        default_label: String,
        parse_item: impl Fn(&mut Self) -> Id,
        make_shorthand: impl Fn(&mut Self, String) -> Id,
    ) -> (String, Id) {
        if self.peek_one_further().token == TokenKind::Colon {
            // its labeled!
            let label = self.expect_identifier("to label the tuple element");
            self.expect_token(TokenKind::Colon, "unreachable");

            if let TokenKind::Comma | TokenKind::RightParen | TokenKind::RightBrace = self.peek().token {
                // shorthand (x:, y:)
                (label.clone(), make_shorthand(self, label))
            } else {
                // (x: 0, y: 1)
                (label, parse_item(self))
            }
        } else {
            // unlabeled tuple (1, 2)
            (default_label, parse_item(self))
        }
    }

    fn parse_one_tuple_expression(&mut self, default_label: String) -> AstTupleElement {        
        let (label, expr) = self.parse_tuple_item(
            default_label,
            Self::parse_expression_default,
            |p, name| p.add_expr(p.prev_token_span, Expr::IdentifierRef { name, mutable: false }),
        );
        AstTupleElement { label, expr }
    }

    fn parse_tuple_expression(&mut self, start: Span, first_elem: Option<AstTupleElement>, end_token: TokenKind) -> ExprId {
        let mut tuple_body = vec![];
        let has_first_elem = first_elem.is_some();
        if let Some(x) = first_elem {
            tuple_body.push(x);
        }
        
        let other_tuple_elems = self.parse_comma_separated(
            end_token,
            |p, i| {
                p.parse_one_tuple_expression((if has_first_elem { i + 1 } else { i }).to_string())
            },
            "to close the tuple"
        );
        tuple_body.extend(other_tuple_elems);
        self.add_expr(start, Expr::Tuple { elems: tuple_body })
    }


    pub(super) fn extract_number_expr_from_source(&mut self, span: Span) -> ExprId {
        let num = self.get_from_source(span)
            .to_string()
            .replace('_', "")
            .parse();
        if let Ok(val) = num {
            self.add_expr(span, Expr::Literal { val: AstValue::Num(val) })
        } else {
            self.error(ErrType::ParserNumberParseError);
            self.add_expr(span, Expr::ParserError)
        }
    }


    fn parse_optional_expression(&mut self) -> Option<ExprId> {
        (self.peek_is_on_same_line() && self.peek_is_expression_start())
            .then(|| self.parse_expression_default())
    }
}