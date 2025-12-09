use std::rc::Rc;

use crate::parsing::ast_structure::{AssignablePattern, Expr, PlaceExpr, TypedExpr, Value};





pub fn loop_over_every_ast_node(
    program: &mut [TypedExpr],
    expr_closure: impl Fn(TypedExpr) -> TypedExpr,
    pattern_closure: impl Fn(AssignablePattern) -> AssignablePattern,
) {
    let mut exprs: Vec<&mut TypedExpr> = program.iter_mut().collect();
    let mut patterns: Vec<&mut AssignablePattern> = Vec::new();
    loop {
        if let Some(expr) = exprs.pop() {
            *expr = expr_closure(std::mem::replace(
                expr,
                Expr::Void.into() // temporary placeholder
            ));

            match &mut expr.expression {
                Expr::Block(x)
                | Expr::Array(x)
                | Expr::Tuple(x)
                | Expr::TemplateString(x) => {
                    exprs.extend(x);
                }
                Expr::Prefix { right: expr, .. }
                | Expr::Loop { body: expr }
                | Expr::Deref { expr }
                | Expr::MutRef { expr }
                | Expr::Return(expr)
                | Expr::Break { expr } => {
                    exprs.push(expr);
                }
                Expr::Infix { left: expr1, right: expr2, .. }
                | Expr::Index { left: expr1, index: expr2 }
                | Expr::While { condition: expr1, body: expr2 } => {
                    exprs.push(expr1);
                    exprs.push(expr2);
                }
                Expr::Assign { pattern, value, extra_operator: _ } => {
                    patterns.push(pattern);
                    if let Some(val) = value { exprs.push(val); }
                }
                Expr::If { condition, consequence, alternative } => {
                    exprs.push(condition);
                    exprs.push(consequence);
                    exprs.push(alternative);
                }
                Expr::Case { pattern, value } => {
                    patterns.push(pattern);
                    exprs.push(value);
                }
                Expr::Match { match_value, arms } => {
                    exprs.push(match_value);
                    for arm in arms {
                        patterns.push(&mut arm.pattern);
                        exprs.push(&mut arm.body);
                    }
                }
                Expr::Call { callee: function, arguments } => {
                    exprs.push(function);
                    exprs.extend(arguments);
                }
                Expr::FnDefinition { params, body, .. }
                | Expr::Closure { params, body, .. } => {
                    for param in params { patterns.push(param); }
                    exprs.push(Rc::get_mut(body).unwrap());
                }

                // types should already be finalized
                Expr::Literal(_) | Expr::Identifier { .. } | Expr::MemberAccess { .. }  | Expr::TypePath { .. } | Expr::EnumDefinition{..}
                | Expr::Void | Expr::ParserTempTypeAnnotation(_) => { /* already finalized */ }
            }
        }
        else if let Some(pattern) = patterns.pop() {
            *pattern = pattern_closure(std::mem::replace(
                pattern,
                AssignablePattern::Wildcard, // temporary placeholder
            ));

            match pattern {
                AssignablePattern::Array(elements)
                | AssignablePattern::Tuple(elements)
                | AssignablePattern::Or(elements)
                | AssignablePattern::EnumVariant { inner_patterns: elements, .. } => {
                    patterns.extend(elements);
                }
                AssignablePattern::Binding { .. }
                | AssignablePattern::Wildcard
                | AssignablePattern::Literal(_)
                | AssignablePattern::Place(PlaceExpr::Identifier(_)
                | PlaceExpr::Deref(_)) => { /* done */ }
    
                AssignablePattern::Place(PlaceExpr::Index { left, index }) => {
                    exprs.push(Rc::get_mut(left).unwrap());
                    exprs.push(Rc::get_mut(index).unwrap());
                }
                AssignablePattern::Conditional { pattern, body } => {
                    patterns.push(pattern);
                    exprs.push(Rc::get_mut(body).unwrap());
                }
            }
        }
        else {
            break;
        }
    }
}





pub fn desugar(program: &mut [TypedExpr]) {
    loop_over_every_ast_node(
        program,
        |expr| {
            match expr.expression {
                Expr::While { condition, body } => {
                    // modify into
                    Expr::Loop {
                        body: Expr::If {
                            condition: condition,
                            consequence: body,
                            alternative: Expr::Break { expr: Expr::Void.into() }.into()
                        }.into()
                    }.into()
                }

                Expr::TemplateString(ref segments) => {
                    if segments.len() == 1 && let Expr::Literal(Value::Str(string)) = &segments.first().unwrap().expression {
                        Expr::Literal(Value::Str(string.clone())).into()
                    }
                    else { expr }
                }

                // Do nothing to other nodes
                _ => expr
            }
        },
        |pattern| { pattern }
    );
}