use crate::{lexing::tokens::TokenKind, parsing::ast_structure::{AstArena, Expr, Pattern}};



pub fn desugar_after_parsing(ast: &mut AstArena) {
    // this only loops up to the original length
    for i in 0..ast.exprs.len() {
        let expr = ast.exprs[i].clone();
        let span = ast.expr_spans[i];

        match expr {
            // ==========================================
            // desugar `while COND { BODY }` 
            // --> `loop { if COND { BODY } else break }`
            // ==========================================
            Expr::While { condition, body, label } => {
                // modify into
                let void = ast.add_expr(span, Expr::Void);
                let break_ = ast.add_expr(span, Expr::Break { expr: void, label: None });

                ast.exprs[i] = Expr::Loop {
                    label,
                    body: ast.add_expr(span, Expr::If {
                        condition,
                        then: body,
                        alt: break_
                    })
                }
            }


            // ==========================================
            // desugar `fn x() { ... }` --> `const x = |-> ...`
            // ==========================================
            Expr::FnDefinition { name, closure } => {
                ast.exprs[i] = Expr::Const {
                    pattern: ast.add_pattern(span, Pattern::Binding { name, mutable: false }),
                    value: ast.add_expr(span, Expr::Closure { closure, requires_type_annotation: true })
                }
            }
            
            // ==========================================
            // desugar `a != b`  -->  `!(a == b)`
            // ==========================================
            Expr::Infix { op: TokenKind::NotEqual, op_span, left, right } => {
                ast.exprs[i] = Expr::Prefix { 
                    op: TokenKind::Exclamation,
                    right: ast.add_expr(span, Expr::Infix { op: TokenKind::EqualEqual, op_span, left, right }) 
                };
            }
            // ==========================================
            // desugar `a <= b`  -->  `!(a > b)`
            // ==========================================
            Expr::Infix { op: TokenKind::LessEqual, op_span, left, right } => {
                ast.exprs[i] = Expr::Prefix { 
                    op: TokenKind::Exclamation,
                    right: ast.add_expr(span, Expr::Infix { op: TokenKind::Greater, op_span, left, right }) 
                };
            }
            // ==========================================
            // desugar `a >= b`  -->  `!(a < b)`
            // ==========================================
            Expr::Infix { op: TokenKind::GreaterEqual, op_span, left, right } => {
                ast.exprs[i] = Expr::Prefix { 
                    op: TokenKind::Exclamation,
                    right: ast.add_expr(span, Expr::Infix { op: TokenKind::Less, op_span, left, right }) 
                };
            }
            
            // ==========================================
            // modify `(b: ..., a: ...)`  -->  `(a: ..., b: ...)`
            // ==========================================
            Expr::Tuple { mut elems } => {
                elems.sort_by(|a, b| a.label.cmp(&b.label));
                ast.exprs[i] = Expr::Tuple { elems };
            }

            _ => {}
        }
    }

    // this only loops up to the original length
    for i in 0..ast.patterns.len() {
        let pattern = ast.patterns[i].clone();

        #[allow(clippy::single_match)]
        match pattern {
            // ==========================================
            // modify `(b: ..., a: ...)`  -->  `(a: ..., b: ...)`
            // ==========================================
            Pattern::Tuple(mut elems) => {
                elems.sort_by(|a, b| a.label.cmp(&b.label));
                ast.patterns[i] = Pattern::Tuple(elems);
            }

            _ => {}
        }
    }
}