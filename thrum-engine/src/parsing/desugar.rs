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
                Expr::Block { exprs: x, drops_vars: _, label: _ }
                | Expr::Array(x)
                | Expr::TemplateString(x) => {
                    exprs.extend(x);
                }
                Expr::Tuple(elements) => {
                    for elem in elements { exprs.push(&mut elem.expr) }
                }
                Expr::Prefix { right: expr, operator: _ }
                | Expr::Loop { body: expr, label: _ }
                | Expr::Move { expr, auto_clone: _ }
                | Expr::Return(expr)
                | Expr::Break { expr, label: _ }
                | Expr::MemberAccess { left: expr, member: _, resolved_index: _ } => {
                    exprs.push(expr);
                }
                Expr::Infix { left, right, operator: _ }
                | Expr::Index { left, index: right }
                | Expr::While { condition: left, body: right, label: _ } => {
                    exprs.push(left);
                    exprs.push(right);
                }
                Expr::Assign { pattern, value, extra_operator: _, op_span: _ } => {
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
                Expr::FnDefinition { params, body, return_type_annotation: _, name: _, var_id: _ }
                | Expr::Closure { params, body, return_type_annotation: _ } => {
                    for param in params {
                        patterns.push(param);
                    }
                    exprs.push(body);
                }

                // types should already be finalized
                Expr::Literal(_)
                | Expr::IdentifierRef { name: _, mutable: _, var_id: _ }
                | Expr::EnumDefinition{ name: _, variants: _ }
                | Expr::TypePath(_)
                | Expr::Continue { label: _ }
                | Expr::Void => { /* already finalized */ }
            }
        }

        else if let Some(pattern) = patterns.pop() {
            *pattern = pattern_closure(std::mem::replace(
                pattern,
                MatchPattern::Wildcard.to_info(Span::invalid()), // temporary placeholder
            ));

            match &mut pattern.pattern {
                MatchPattern::Array(elements)
                | MatchPattern::Or(elements)
                | MatchPattern::EnumVariant { inner_patterns: elements, path: _, name: _ } => {
                    patterns.extend(elements);
                }
                MatchPattern::Tuple(elements) => {
                    for elem in elements { patterns.push(&mut elem.pattern) }
                }
                | MatchPattern::Wildcard
                | MatchPattern::Binding { name: _, mutable: _, var_id: _ }
                | MatchPattern::Literal(_) => { /* done */ }

                MatchPattern::PlacePointer { expr } => {
                    exprs.push(expr);
                },
    
                MatchPattern::Conditional { pattern, body } => {
                    patterns.push(pattern);
                    exprs.push(body);
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
                | TypeKind::Pointer { inner: t, mutable: _, borrows_var: _ } => {
                    types.push(t);
                }
                TypeKind::Tup(tuple_types) => {
                    for tt in tuple_types {
                        types.push(&mut tt.typ);
                    }
                }
                TypeKind::CustomType { name: _, generic_types } => {
                    types.extend(generic_types);
                }
                TypeKind::Fn { param_types, return_type } => {
                    types.extend(param_types);
                    types.push(return_type);
                }
                TypeKind::Num
                | TypeKind::Str
                | TypeKind::Bool
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
                Expr::While { condition: w_condition, body: w_body, label: w_label } => {
                    // modify into
                    Expr::Loop {
                        label: w_label,
                        body: Box::new(Expr::If {
                            condition: w_condition,
                            then: w_body,
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
                if operator.token == TokenType::NotEqual => {
                    // !=   ! ==
                    Expr::Prefix {
                        operator: TokenType::Exclamation,
                        right: Box::new(Expr::Infix { operator: TokenSpan { token: TokenType::EqualEqual, span: expr.span }, left, right }.to_info(expr.span))
                    }
                }
                Expr::Infix { operator, left, right }
                if operator.token == TokenType::LessEqual => {
                    // <=   ! >
                    Expr::Prefix {
                        operator: TokenType::Exclamation,
                        right: Box::new(Expr::Infix { operator: TokenSpan { token: TokenType::Greater, span: expr.span }, left, right }.to_info(expr.span))
                    }
                }
                Expr::Infix { operator, left, right }
                if operator.token == TokenType::GreaterEqual => {
                    // >=   ! <
                    Expr::Prefix {
                        operator: TokenType::Exclamation,
                        right: Box::new(Expr::Infix { operator: TokenSpan { token: TokenType::Less, span: expr.span }, left, right }.to_info(expr.span))
                    }
                }
                
                // x[...] += 1 desugars to:
                // { let priv = x[...]; priv^ = priv^ + 1 }
                Expr::Assign { pattern, extra_operator, op_span, value }
                if extra_operator.is_some() && value.is_some() => {
                    let Some(op) = extra_operator else { unreachable!() };
                    let MatchPattern::PlacePointer { expr } = pattern.pattern else { panic!("todo fix this") };

                    let expr_span = expr.span;
                    let var_name = "priv".to_string();
                    let move_priv = Expr::Move {
                        expr: Box::new(Expr::IdentifierRef { name: var_name, mutable: false, var_id: None }.to_info(expr.span)),
                        auto_clone: false
                    }.to_info(expr.span);

                    Expr::Block {
                        exprs: vec![
                            Expr::Assign {
                                pattern: Box::new(MatchPattern::Binding { name: "priv".to_string(), mutable: false, var_id: None }.to_info(expr.span)),
                                extra_operator: None,
                                op_span: Span::invalid(),
                                value: Some(Box::new(expr))
                            }.to_info(expr_span),

                            Expr::Assign {
                                pattern: Box::new(MatchPattern::PlacePointer { expr: move_priv.clone() }.to_info(expr_span)),
                                extra_operator: None,
                                op_span: Span::invalid(),
                                value: Some(Box::new(
                                    Expr::Infix {
                                        operator: TokenSpan { token: op, span: op_span },
                                        left: Box::new(move_priv),
                                        right: value.unwrap()
                                    }.to_info(expr_span)
                                ))
                            }.to_info(expr_span)
                        ],
                        label: None,
                        drops_vars: Vec::new()
                    }
                }

                // turn ensure into normal if else
                Expr::Block { mut exprs, drops_vars, label } => {
                    desugar_ensure(&mut exprs);
                    Expr::Block { exprs, drops_vars, label }
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
    if let Some(ensure_index) = exprs.iter().position(|x| matches!(x.expression, Expr::Ensure { .. })) {
        // take everything AFTER ensure (inside this block expr)
        let exprs_after_ensure = exprs.split_off(ensure_index + 1);

        // take the ensure Expr itself
        let Expr::Ensure { then, .. } = &mut exprs.last_mut().unwrap().expression
        else { unreachable!() };
        // and put exprs_after_ensure into its then block
        then.expression = Expr::Block { exprs: exprs_after_ensure, drops_vars: Vec::new(), label: None };
    }
}