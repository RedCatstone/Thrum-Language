use std::rc::Rc;

use crate::{lexing::tokens::TokenType, parsing::ast_structure::{Expr, MatchPattern, PlaceExpr, TypeKind, TypedExpr, Value}};





pub fn loop_over_every_ast_node(
    program: &mut [TypedExpr],
    expr_closure: impl Fn(TypedExpr) -> TypedExpr,
    pattern_closure: impl Fn(MatchPattern) -> MatchPattern,
) {
    let mut exprs: Vec<&mut TypedExpr> = program.iter_mut().collect();
    let mut patterns: Vec<&mut MatchPattern> = Vec::new();
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
                Expr::Prefix { right: expr, operator: _ }
                | Expr::Loop { body: expr, label: _ }
                | Expr::Deref { expr }
                | Expr::MutRef { expr }
                | Expr::Return(expr)
                | Expr::Break { expr, label: _ }
                | Expr::MemberAccess { left: expr, member: _ } => {
                    exprs.push(expr);
                }
                Expr::Infix { left: expr1, right: expr2, operator: _ }
                | Expr::Index { left: expr1, index: expr2 }
                | Expr::While { condition: expr1, body: expr2, label: _ } => {
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
                Expr::Ensure { .. } => {
                    unreachable!("hmmm?")
                },
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
                Expr::FnDefinition { params, body, name: _, return_type: _ }
                | Expr::Closure { params, body, return_type: _ } => {
                    for param in params { patterns.push(param); }
                    exprs.push(Rc::get_mut(body).unwrap());
                }

                // types should already be finalized
                Expr::Literal(_) | Expr::Identifier { name: _ } | Expr::TypePath(_) | Expr::EnumDefinition{ name: _, enums: _ } | Expr::Continue { label: _ }
                | Expr::Void | Expr::ParserTempTypeAnnotation(_) => { /* already finalized */ }
            }
        }

        else if let Some(pattern) = patterns.pop() {
            *pattern = pattern_closure(std::mem::replace(
                pattern,
                MatchPattern::Wildcard, // temporary placeholder
            ));

            match pattern {
                MatchPattern::Array(elements)
                | MatchPattern::Tuple(elements)
                | MatchPattern::Or(elements)
                | MatchPattern::EnumVariant { inner_patterns: elements, path: _, name: _ } => {
                    patterns.extend(elements);
                }
                MatchPattern::Binding { name: _, typ: _ }
                | MatchPattern::Wildcard
                | MatchPattern::Literal(_)
                | MatchPattern::Place(PlaceExpr::Identifier(_)
                | PlaceExpr::Deref(_)) => { /* done */ }
    
                MatchPattern::Place(PlaceExpr::Index { left, index }) => {
                    exprs.push(Rc::get_mut(left).unwrap());
                    exprs.push(Rc::get_mut(index).unwrap());
                }
                MatchPattern::Conditional { pattern, body } => {
                    patterns.push(pattern);
                    exprs.push(Rc::get_mut(body).unwrap());
                }
            }
        }
        else {
            // no more exprs or patterns to go through...
            break;
        }
    }
}





pub fn desugar(program: &mut Vec<TypedExpr>) {
    desugar_ensure(program);

    loop_over_every_ast_node(
        program,
        |expr| {
            match expr.expression {
                // turn while loops into normal loops with a conditional break
                Expr::While { condition: while_condition, body: while_body, label: while_label } => {
                    // modify into
                    Expr::Loop {
                        label: while_label,
                        body: Expr::If {
                            condition: while_condition,
                            consequence: while_body,
                            alternative: Expr::Break { expr: Expr::Void.into(), label: None }.into()
                        }.into()
                    }
                }

                // turn 1 segment long template strings into literals
                Expr::TemplateString(ref segments)
                if segments.len() == 1 => {
                    if let Expr::Literal(Value::Str(string)) = &segments.first().unwrap().expression {
                        Expr::Literal(Value::Str(string.clone()))
                    }
                    else { unreachable!() }
                }

                Expr::Infix { operator, left, right }
                if matches!(operator, TokenType::NotEqual | TokenType::LessEqual | TokenType::GreaterEqual) => {
                    match operator {
                        // !=  ! ==
                        TokenType::NotEqual => Expr::Prefix { operator: TokenType::Exclamation, right: Expr::Infix { operator: TokenType::EqualEqual, left, right }.into() },
                        // <=  ! >
                        TokenType::LessEqual => Expr::Prefix { operator: TokenType::Exclamation, right: Expr::Infix { operator: TokenType::Greater, left, right }.into() },
                        // >=  ! <
                        TokenType::GreaterEqual => Expr::Prefix { operator: TokenType::Exclamation, right: Expr::Infix { operator: TokenType::Less, left, right }.into() },
                        // Do nothing
                        _ => unreachable!()
                    }
                }

                // turn ensure into normal if else
                Expr::Block(mut exprs) => {
                    desugar_ensure(&mut exprs);
                    Expr::Block(exprs)
                }

                // Do nothing to other nodes
                other => other
            }
            .into_with_type(expr.typ)
        },
        |pattern| { pattern }
    );
}


fn desugar_ensure(exprs: &mut Vec<TypedExpr>) {
    // {
    //     ensure case ?y = x else { return }
    //     print(y)
    //     return true
    // }
    // {
    //     if case ?y = x {
    //         print(y)
    //         return true
    //     }
    //     else { return }
    // }
    if let Some(ensure_index) = exprs.iter().position(|x| matches!(x.expression, Expr::Ensure { .. })) {
        let exprs_after_ensure = exprs.split_off(ensure_index + 1);

        let Expr::Ensure { condition, mut alternative } = exprs.pop().unwrap().expression
        else { unreachable!() };

        alternative.typ = TypeKind::Never;

        exprs.push(Expr::If {
            condition,
            consequence: Box::new(Expr::Block(exprs_after_ensure).into()),
            alternative
        }.into());
    }
}