use std::{collections::{HashMap, HashSet}, rc::Rc};

use crate::{
    ErrType,
    lexing::tokens::TokenType,
    nativelib::ThrumType,
    parsing::ast_structure::{DefinedTypeKind, Expr, ExprInfo, MatchPattern, TupleElement, TupleType, TypeKind, Value}
};
use crate::typing::TypeChecker;




#[derive(Default, Clone)]
pub struct ExprContext {
    expected_type: Option<TypeKind>,
    allow_conditional_bindings: bool,
}
impl ExprContext {
    fn expect(&self, typ: TypeKind) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.expected_type = Some(typ);
        new_ctx
    }
    fn allow_conditional_bindings(&self) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.allow_conditional_bindings = true;
        new_ctx
    }
}



impl<'a> TypeChecker<'a> {
    pub(super) fn check_expression(&mut self, expr: &mut ExprInfo, old_ctx: &ExprContext) {
        let mut ctx = ExprContext::default();
        let mut is_never = false;

        let inferred_type = match &mut expr.expression {
            Expr::Literal(val) => self.check_literal(val),

            Expr::Identifier { name } => self.check_identifier(name),

            Expr::TemplateString(parts) => {
                for part in parts {
                    self.check_expression(part, &ctx);
                    if self.prune(&part.typ).is_never() { is_never = true }
                }
                TypeKind::Str
            }

            Expr::Tuple(elements) => {
                let mut tuple_types = Vec::new();

                for TupleElement { label, expr } in elements {
                    self.check_expression(expr, &ctx);
                    if self.prune(&expr.typ).is_never() { is_never = true }
                    tuple_types.push(TupleType { label: label.clone(), typ: expr.typ.clone() });
                }

                TypeKind::Tup(tuple_types)
            }

            Expr::Array(elements) => {
                let mut arr_types = Vec::new();

                for element in elements {
                    self.check_expression(element, &ctx);
                    arr_types.push(element.typ.clone());
                }

                let arr_type = self.unify_type_vec(&arr_types);
                if arr_type.is_never() { is_never = true }
                TypeKind::Arr(Box::new(arr_type))
            }

            Expr::Index { left, index } => {
                let arr_element_type = self.new_inference_type();
                self.check_expression(left, &ctx.expect(TypeKind::Arr(Box::new(arr_element_type.clone()))));
                self.check_expression(index, &ctx.expect(TypeKind::Num));
                if self.prune(&left.typ).is_never() { is_never = true }
                if self.prune(&index.typ).is_never() { is_never = true }
                arr_element_type
            }

            Expr::Block(body) => self.check_block(body, &ctx),

            Expr::Prefix { operator, right } => {
                match operator {
                    TokenType::Exclamation => {
                        self.check_expression(right, &ctx.expect(TypeKind::Bool));
                        right.typ.clone()
                    }
                    TokenType::Minus => {
                        self.check_expression(right, &ctx.expect(TypeKind::Num));
                        right.typ.clone()
                    }
                    _ => unreachable!("Unsupported prefix operator: {}", operator)
                }
            }

            Expr::Infix { operator, left, right } => {
                if *operator == TokenType::Ampersand && old_ctx.allow_conditional_bindings {
                    ctx.allow_conditional_bindings = true;
                }
                self.check_expression(left, &ctx);
                self.check_expression(right, &ctx);
                self.check_infix(operator, &left.typ, &right.typ)
            }

            Expr::If { condition, then, alt } => {
                self.check_expression(condition, &ctx.expect(TypeKind::Bool).allow_conditional_bindings());
    
                self.check_expression(then, &ctx);
                self.check_expression(alt, &ctx.expect(then.typ.clone()));

                then.typ.clone()
            },

            Expr::Ensure { condition, alt, then } => {
                self.check_expression(condition, &ctx.expect(TypeKind::Bool).allow_conditional_bindings());
    
                self.check_expression(alt, &ctx.expect(TypeKind::Never));
                self.check_expression(then, &ctx);

                TypeKind::Void
            }

            Expr::Match { match_value, arms: cases } => {
                self.check_expression(match_value, &ctx);

                let mut cases_to_cover = match self.prune(&match_value.typ) {
                    TypeKind::Bool => Some(HashSet::from([true.to_string(), false.to_string()])),
                    _ => None
                };
    
                let mut has_unfailable_arm = false;
                let mut arm_types = Vec::new();
    
                for arm in cases {
                    self.env.enter_scope();
                    match arm.pattern {
                        MatchPattern::Literal(Value::Bool(bool)) => {
                            cases_to_cover.as_mut().unwrap().remove(&bool.to_string());
                        }
                        _ => { }
                    }
                    let pattern_type = self.check_binding_pattern(&mut arm.pattern, true);
                    self.unify_types(&match_value.typ, &pattern_type.typ);

                    if !pattern_type.can_fail { has_unfailable_arm = true; }

                    self.check_expression(&mut arm.body, &ctx);
                    arm_types.push(arm.body.typ.clone());
                    self.env.exit_scope();
                }

                if !has_unfailable_arm && match &cases_to_cover {
                    None => false,
                    Some(remaining_cases) => !remaining_cases.is_empty()
                } {
                    self.error(crate::ErrType::TyperPatternDoesntCoverAllCases(cases_to_cover.unwrap().iter().cloned().collect()));
                }

                self.unify_type_vec(&arm_types)
            },

            Expr::Loop { body, label } => {
                let loop_break_type = self.new_inference_type();
                self.current_break_types.push((label.to_string(), loop_break_type.clone()));
                self.check_expression(body, &ctx.expect(TypeKind::Void));
                self.current_break_types.pop().unwrap();
                if loop_break_type == self.prune(&loop_break_type) {
                    // loop doesn't have any breaks -> infinite loop
                    self.unify_types(&loop_break_type, &TypeKind::Never);
                }
                loop_break_type
            },
            
            Expr::Assign { pattern, extra_operator, value } => {
                let pattern_type = self.check_binding_pattern(pattern, true);
                if pattern_type.can_fail {
                    self.error(ErrType::TyperFailableLetPattern);
                }
                // TODO
                if let Some(val) = value {
                    self.check_expression(val, &ctx.expect(pattern_type.typ.clone()));
                    if val.typ.is_never() { is_never = true }
                    if *extra_operator != TokenType::Equal {
                        self.check_infix(extra_operator, &pattern_type.typ, &val.typ);
                    }
                }
                TypeKind::Void
            },

            Expr::Case { pattern, value } => {
                let pattern_type = self.check_binding_pattern(pattern, true);
                self.check_expression(value, &ctx.expect(pattern_type.typ));
                if value.typ.is_never() { is_never = true }
                else if !pattern_type.vars.is_empty() && !old_ctx.allow_conditional_bindings {
                    self.error(ErrType::TyperInvalidBindingCaseExpr);
                }
                TypeKind::Bool
            },

            Expr::MutRef { expr } => {
                self.check_expression(expr, &ctx);
                match &mut expr.expression {
                    Expr::Identifier { .. } => {
                        TypeKind::MutPointer(Box::new(expr.typ.clone()))
                    }
                    _ => self.error(ErrType::DefaultString("Cannot borrow non identifier as mut.".to_string()))
                }
            }

            Expr::Deref { expr } => {
                let inner_typ = self.new_inference_type();
                self.check_expression(expr, &ctx.expect(TypeKind::MutPointer(Box::new(inner_typ.clone()))));
                inner_typ
            }

            Expr::FnDefinition { params, return_type, body, .. } => {
                self.check_fn_expression(params, return_type, body, &ctx);
                if body.typ.is_never() { is_never = true }
                TypeKind::Void
            }
            Expr::Closure { params, return_type: return_value, body } => {
                self.check_fn_expression(params, return_value, body, &ctx)
            }

            Expr::Return(ret) => {
                let curr_return_type = self.current_function_return_type.clone()
                    .unwrap_or_else(|| TypeKind::Never/* self.error("'return' is only allowed inside functions.".to_string()) */);

                self.check_expression(ret, &ctx.expect(curr_return_type));

                TypeKind::Never
            },

            Expr::Break { expr, label } => {
                let curr_break_type = if self.current_break_types.is_empty() {
                    self.error(ErrType::TyperBreakOutsideLoop)
                }
                else if let Some(break_label) = label {
                    // break with a label -> find the closest loop with that label
                    match self.current_break_types
                        .iter().rev()
                        .find(|(x_label, _typ)| x_label == break_label) {
                            Some((_label, typ)) => typ.clone(),
                            None => self.error(ErrType::TyperUndefinedLoopLabel(break_label.to_string(), self.current_break_types.iter().map(|(l, _)| l.to_string()).collect()))
                        }
                } else {
                    // break without a label -> just break to the current loop
                    self.current_break_types.last().unwrap().1.clone()
                };

                self.check_expression(expr, &ctx.expect(curr_break_type));

                TypeKind::Never
            },

            Expr::Continue { label } => {
                if let Some(continue_label) = label
                    && !self.current_break_types
                        .iter().rev()
                        .any(|(x_label, _typ)| x_label == continue_label) {
                            self.error(ErrType::TyperUndefinedLoopLabel(continue_label.to_string(), self.current_break_types.iter().map(|(l, _)| l.to_string()).collect()));
                        }
                TypeKind::Never
            }

            Expr::Call { callee, arguments } => {
                self.check_expression(callee, &ctx);
    
                let mut call_param_types = Vec::new();
                for arg in arguments {
                    self.check_expression(arg, &ctx);
                    call_param_types.push(arg.typ.clone());
                }

                match &mut callee.typ {
                    TypeKind::Fn { param_types, return_type } => {
                        if param_types.len() != call_param_types.len() {
                            self.error(ErrType::TyperTooManyArguments(param_types.len(), call_param_types.len()))
                        }
                        else {
                            for (param_type, arg_type) in param_types.iter().zip(call_param_types.iter()) {
                                self.unify_types(param_type, arg_type);
                            }
                            *return_type.clone()
                        }
                    }
                    TypeKind::Inference(id) => {
                        // TODO not the best code, but works for now
                        let return_type = self.new_inference_type();
                        let fn_type = TypeKind::Fn { param_types: call_param_types, return_type: Box::new(return_type.clone()) };
                        self.unify_types(&fn_type, &TypeKind::Inference(*id));
                        return_type
                    }
                    _ => self.error(ErrType::TyperCantCallNonFnType(callee.typ.clone()))
                }
            },

            
            Expr::MemberAccess { left, member, resolved_index } => {
                self.check_expression(left, &ctx);
                if let TypeKind::Tup(elements) = self.prune(&left.typ) {
                    let member_index = elements.iter().position(|elem| elem.label == *member);
                    *resolved_index = member_index;

                    match member_index {
                        Some(i) => elements[i].typ.clone(),
                        None => self.error(ErrType::TyperTupleDoesntHaveMember(TypeKind::Tup(elements), member.to_string()))
                    }
                }
                else { todo!() }
            }

            Expr::TypePath(segments) => self.check_path_expression(segments),

            Expr::EnumDefinition { name, enums } => {
                self.checked_define_type(name.clone(), ThrumType {
                    typ: DefinedTypeKind::Enum {
                        name: name.to_string(),
                        inner_types: enums.iter_mut()
                            .map(|x| (x.name.clone(), x.inner_types.clone()))
                            .collect()
                    },
                    values: HashMap::new()
                });
                TypeKind::Void
            },

            Expr::Void => TypeKind::Void,

            Expr::While { .. } => unreachable!("should be desugared already..."),


        };

        if inferred_type.is_never() { is_never = true }

        expr.typ = if is_never { TypeKind::Never }
            else { self.prune(&inferred_type) };
        
        if let Some(expected) = &old_ctx.expected_type {
            self.unify_types(expected, &expr.typ);
        }
    }



    pub(super) fn check_literal(&mut self, val: &mut Value) -> TypeKind {
        match val {
            Value::Num(_) => TypeKind::Num,
            Value::Str(_) => TypeKind::Str,
            Value::Bool(_) => TypeKind::Bool,
            _ => unreachable!() // other values are not used in the parser
        }
    }


    pub(super) fn check_identifier(&mut self, name: &str) -> TypeKind {
        match self.env.lookup_variable(name) {
            Some(x) => x,
            None => return self.error(ErrType::TyperUndefinedIdentifier(name.to_string()))
        }
    }


    pub(super) fn check_block(&mut self, body: &mut [ExprInfo], ctx: &ExprContext) -> TypeKind {
        self.env.enter_scope();

        // 1. define FnDefinitions
        for expr in body.iter_mut() {
            if let Expr::FnDefinition { name, params, return_type, .. } = &mut expr.expression {
                let fn_typ = self.get_fn_type(params, return_type, false);
                self.checked_define_variable(name.clone(), fn_typ);
            }
        }

        // normal pass
        let mut is_never = false;
        let block_drop_type = if let Some((last_expr, other_exprs)) = body.split_last_mut() {
            for expr in other_exprs {
                self.check_expression(expr, ctx);
                if expr.typ.is_never() { is_never = true }
            }
            // conditional bindings are allowed in the last expression of a block, because the current scope is gonna end after this expression anyways.
            // this isn't reeeaally needed, but it definitely can't hurt to allow.
            // let x = { case ?x = ... and x > 3 }
            self.check_expression(last_expr, &ctx.allow_conditional_bindings());
            if is_never { TypeKind::Never } else { last_expr.typ.clone() }
        } else {
            // Empty block returns Void
            TypeKind::Void
        };

        self.env.exit_scope();
        block_drop_type
    }


    fn check_infix(&mut self, operator: &TokenType, left_type: &TypeKind, right_type: &TypeKind) -> TypeKind {
        match operator {
            // num/str operators
            TokenType::Plus | TokenType::Greater | TokenType::Less /* | TokenType::GreaterEqual | TokenType::LessEqual */ => {
                self.unify_types(left_type, right_type);
                let unified_type = self.prune(left_type);
                let returned_type = if *operator == TokenType::Plus { unified_type.clone() } else { TypeKind::Bool };
                match unified_type {
                    TypeKind::Num | TypeKind::Str => returned_type,
                    // let it propagate, it will be solved later.
                    TypeKind::Inference(_) => returned_type,
                    _ => self.error(ErrType::TyperInvalidOperatorOnType(operator.clone(), unified_type))
                }
            }

            // num operators
            TokenType::Minus | TokenType::Star | TokenType::Slash | TokenType::Percent | TokenType::StarStar => {
                self.unify_types(left_type, &TypeKind::Num);
                self.unify_types(right_type, &TypeKind::Num);
                TypeKind::Num
            }

            // comparison operators
            TokenType::EqualEqual /* | TokenType::NotEqual */ => {
                self.unify_types(left_type, right_type);
                let unified_type = self.prune(left_type);
                match unified_type {
                    TypeKind::Num | TypeKind::Str | TypeKind::Bool | TypeKind::Arr(_) | TypeKind::Tup {.. } | TypeKind::Void => TypeKind::Bool,
                    // TypeKind::Inference(_) => TypeKind::Bool,
                    _ => self.error(ErrType::TyperInvalidOperatorOnType(TokenType::EqualEqual, unified_type))
                }
            }

            // boolean operators
            TokenType::Ampersand | TokenType::Pipe => {
                self.unify_types(&TypeKind::Bool, left_type);
                self.unify_types(&TypeKind::Bool, right_type);
                TypeKind::Bool
            }
            _ => unreachable!("Unsupported infix operator: {:?}", operator)
        }
    }


    fn get_fn_type(&mut self, params: &mut [MatchPattern], return_type: &mut TypeKind, define_params: bool) -> TypeKind {
        let mut param_types = Vec::new();
        for param_pattern in params.iter_mut() {
            let pattern_type = self.check_binding_pattern(param_pattern, define_params);

            if pattern_type.has_place { self.error(ErrType::TyperFnParamPlacePatterns); }
            if pattern_type.can_fail { self.error(ErrType::TyperFailableFnParamPatterns); }

            param_types.push(pattern_type.typ);
        }
        if *return_type == TypeKind::ParserUnknown {
            *return_type = self.new_inference_type();
        }
        TypeKind::Fn {
            param_types,
            return_type: Box::new(return_type.clone()),
        }
    }


    fn check_fn_expression(&mut self, params: &mut [MatchPattern], return_type: &mut TypeKind, body: &mut Rc<ExprInfo>, ctx: &ExprContext) -> TypeKind {
        self.env.enter_scope();
        // check the fn_type in a new scope to also define the function parameters
        let fn_type = self.get_fn_type(params, return_type, true);

        // set the return context to this functions return type
        let previous_function_return_type = self.current_function_return_type.clone();
        self.current_function_return_type = Some(return_type.clone());

        let body_mut = Rc::get_mut(body).unwrap();
        self.check_expression(body_mut, &ctx.expect(return_type.clone()));

        // reset return context
        self.current_function_return_type = previous_function_return_type;
        self.env.exit_scope();

        if body_mut.typ.is_never() { TypeKind::Never }
        else { fn_type }
    }


    fn check_path_expression(&mut self, segments: &[String]) -> TypeKind {
        let mut curr_module = &self.library;

        for (i, segment) in segments.iter().enumerate() {
            // try to descend into a sub module
            if let Some(sub_module) = curr_module.sub_modules.get(segment) {
                curr_module = sub_module;
                continue;
            }

            // else check for types (e.g. str::len)
            else if let Some(module_type) = curr_module.types.get(segment) {
                let remaining_segments = &segments[(i + 1)..];
                match remaining_segments {
                    // 0 remaining segments -> type
                    [] => return module_type.typ.clone().to_typekind(),
                    // 1 remaining, meaning its a value defined on that type.
                    [last_segment] => {
                        if let Some(type_val) = module_type.values.get(last_segment) {
                            return type_val.typ.clone()
                        }
                    }
                    // 2 or more remaining segments
                    _ => return self.error(ErrType::DefaultString("type path had 2 or more remaining segments.".to_string()))
                }
            }

            // else check for consts/functions (e.g. io::print)
            else if let Some(module_val) = curr_module.values.get(segment) {
                if i == segments.len() - 1 {
                    return module_val.typ.clone()
                }
                else {
                    return self.error(ErrType::DefaultString("value path too long.".to_string()))
                }
            }

            return self.error(ErrType::DefaultString(format!("segment {segment} could not be found...")));
        }

        self.error(ErrType::DefaultString(format!("'{}' could not be found...", segments.join("::"))))
    }
}