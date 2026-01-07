use std::rc::Rc;

use crate::{Program, lexing::tokens::{TokenSpan, TokenType}, parsing::ast_structure::{Expr, ExprInfo, MatchPattern, MatchPatternInfo, Span, TypeKind, Value}};





pub fn loop_over_every_ast_node(
    program: &mut ExprInfo,
    expr_closure: impl Fn(ExprInfo) -> ExprInfo,
    pattern_closure: impl Fn(MatchPatternInfo) -> MatchPatternInfo,
    type_closure: impl Fn(TypeKind) -> TypeKind,
) {
    let mut exprs: Vec<&mut ExprInfo> = vec![program];
    let mut patterns: Vec<&mut MatchPatternInfo> = Vec::new();
    let mut types: Vec<&mut TypeKind> = Vec::new();

    loop {
        if let Some(expr) = exprs.pop() {
            *expr = expr_closure(std::mem::replace(
                expr,
                Expr::Void.to_info(Span::invalid()) // temporary placeholder
            ));
            types.push(&mut expr.typ);

            match &mut expr.expression {
                Expr::Block { exprs: x, drops_vars: _ }
                | Expr::Array(x)
                | Expr::TemplateString(x) => {
                    exprs.extend(x);
                }
                Expr::Tuple(elements) => {
                    for elem in elements { exprs.push(&mut elem.expr) }
                }
                Expr::Prefix { right: expr, operator: _ }
                | Expr::Loop { body: expr, label: _ }
                | Expr::Deref { expr }
                | Expr::MutRef { expr }
                | Expr::Return(expr)
                | Expr::Break { expr, label: _ }
                | Expr::MemberAccess { left: expr, member: _, resolved_index: _ } => {
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
                Expr::If { condition, then, alt }
                | Expr::Ensure { condition, alt, then } => {
                    exprs.push(condition);
                    exprs.push(then);
                    exprs.push(alt);
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
                Expr::Call { callee, arguments } => {
                    exprs.push(callee);
                    exprs.extend(arguments);
                }
                Expr::FnDefinition { params, body, return_type, name: _, var_id: _ }
                | Expr::Closure { params, body, return_type } => {
                    for param in params { patterns.push(param); }
                    exprs.push(Rc::get_mut(body).unwrap());
                    types.push(return_type);
                }

                // types should already be finalized
                Expr::Literal(_) | Expr::Identifier { name: _, var_id: _ } | Expr::TypePath(_) | Expr::EnumDefinition{ name: _, enums: _ } | Expr::Continue { label: _ }
                | Expr::Void => { /* already finalized */ }
            }
        }

        else if let Some(pattern) = patterns.pop() {
            *pattern = pattern_closure(std::mem::replace(
                pattern,
                MatchPattern::Wildcard.to_info(Span::invalid()), // temporary placeholder
            ));
            types.push(&mut pattern.typ);

            match &mut pattern.pattern {
                MatchPattern::Array(elements)
                | MatchPattern::Or(elements)
                | MatchPattern::EnumVariant { inner_patterns: elements, path: _, name: _ } => {
                    patterns.extend(elements);
                }
                MatchPattern::Tuple(elements) => {
                    for elem in elements { patterns.push(&mut elem.pattern) }
                }
                MatchPattern::Binding { typ, name: _, mutable: _, var_id: _ } => {
                    types.push(typ);
                }
                | MatchPattern::Wildcard
                | MatchPattern::Literal(_)
                | MatchPattern::PlaceIdentifier { name: _, var_id: _ }
                | MatchPattern::PlaceDeref { name: _, var_id: _ } => { /* done */ }
    
                MatchPattern::PlaceIndex { left, index } => {
                    exprs.push(Rc::get_mut(left).unwrap());
                    exprs.push(Rc::get_mut(index).unwrap());
                }
                MatchPattern::Conditional { pattern, body } => {
                    patterns.push(pattern);
                    exprs.push(Rc::get_mut(body).unwrap());
                }
            }
        }
        else if let Some(typ) = types.pop() {
            *typ = type_closure(std::mem::replace(
                typ,
                TypeKind::TypeError, // temporary placeholder
            ));

            match typ {
                TypeKind::Arr(t)
                | TypeKind::MutPointer(t) => {
                    types.push(t);
                }
                TypeKind::Tup(tuple_types) => {
                    for tt in tuple_types {
                        types.push(&mut tt.typ);
                    }
                }
                TypeKind::Struct { inner_types, name: _ } => {
                    types.extend(inner_types)
                }
                TypeKind::Fn { param_types, return_type } => {
                    types.extend(param_types);
                    types.push(return_type);
                }
                TypeKind::Num
                | TypeKind::Str
                | TypeKind::Bool
                | TypeKind::Enum { .. }
                | TypeKind::Inference(_)
                | TypeKind::TypeError
                | TypeKind::Void
                | TypeKind::Never
                | TypeKind::ParserUnknown => {}
            }
        }
        else {
            // no more exprs, patterns or types to go through...
            break;
        }
    }
}





pub fn desugar_after_parsing(program: &mut Program) {
    loop_over_every_ast_node(
        program.ast.as_mut().unwrap(),
        |expr| {
            match expr.expression {
                // turn while loops into normal loops with a conditional break
                Expr::While { condition: while_condition, body: while_body, label: while_label } => {
                    // modify into
                    Expr::Loop {
                        label: while_label,
                        body: Box::new(Expr::If {
                            condition: while_condition,
                            then: while_body,
                            alt: Box::new(Expr::Break {
                                expr: Box::new(Expr::Void.to_info(expr.span)),
                                label: None
                            }
                            .to_info(expr.span))
                        }
                        .to_info(expr.span))
                    }
                }

                // turn 1 segment long template strings into literals
                Expr::TemplateString(segments)
                if segments.len() == 1 => {
                    if let Expr::Literal(Value::Str(string)) = &segments.first().unwrap().expression {
                        Expr::Literal(Value::Str(string.clone()))
                    }
                    else { Expr::TemplateString(segments) }
                }

                Expr::Infix { operator, left, right }
                if matches!(operator.token, TokenType::NotEqual | TokenType::LessEqual | TokenType::GreaterEqual) => {
                    match operator.token {
                        // !=  ! ==
                        TokenType::NotEqual => Expr::Prefix {
                            operator: TokenType::Exclamation,
                            right: Box::new(Expr::Infix { operator: TokenSpan { token: TokenType::EqualEqual, span: expr.span }, left, right }.to_info(expr.span))
                        },
                        // <=  ! >
                        TokenType::LessEqual => Expr::Prefix {
                            operator: TokenType::Exclamation,
                            right: Box::new(Expr::Infix { operator: TokenSpan { token: TokenType::Greater, span: expr.span }, left, right }.to_info(expr.span))
                        },
                        // >=  ! <
                        TokenType::GreaterEqual => Expr::Prefix {
                            operator: TokenType::Exclamation,
                            right: Box::new(Expr::Infix { operator: TokenSpan { token: TokenType::Less, span: expr.span }, left, right }.to_info(expr.span))
                        },
                        // Do nothing
                        _ => unreachable!()
                    }
                }

                // turn ensure into normal if else
                Expr::Block { mut exprs, drops_vars } => {
                    desugar_ensure(&mut exprs);
                    Expr::Block { exprs, drops_vars }
                }

                // Do nothing to other nodes
                other => other
            }
            .to_info(expr.span)
        },
        |pattern| { pattern },
        |typ| { typ }
    );
}



fn desugar_ensure(exprs: &mut Vec<ExprInfo>) {
    // puts all expressions after an ensure expression into its then block.
    if let Some(ensure_index) = exprs.iter().position(|x| matches!(x.expression, Expr::Ensure { .. })) {
        // take everything AFTER ensure (inside this block expr)
        let exprs_after_ensure = exprs.split_off(ensure_index + 1);

        // take the ensure node itself
        let Expr::Ensure { then, .. } = &mut exprs.last_mut().unwrap().expression
        else { unreachable!() };
        // exprs_after_ensure get put into the then block
        then.expression = Expr::Block { exprs: exprs_after_ensure, drops_vars: Vec::new() };
    }
}