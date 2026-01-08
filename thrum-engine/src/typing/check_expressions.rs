use std::{collections::{HashMap, HashSet}, rc::Rc};

use crate::{
    ErrType,
    lexing::tokens::{TokenSpan, TokenType},
    nativelib::ThrumType,
    parsing::ast_structure::{DefinedTypeKind, Expr, ExprInfo, MatchPattern, MatchPatternInfo, Span, TupleElement, TupleType, TypeKind, Value}, typing::BreakTypeInfo
};
use crate::typing::Typechecker;




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



impl<'a> Typechecker<'a> {
    pub(super) fn check_expression(&mut self, expr: &mut ExprInfo, old_ctx: &ExprContext) {
        let mut ctx = ExprContext::default();
        let mut is_never = false;

        let inferred_type = match &mut expr.expression {
            Expr::Literal(val) => self.check_literal(val),

            Expr::Identifier { name, var_id } => {
                self.use_variable(name, false, expr.span, var_id)
            }

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

                let arr_type = self.unify_type_vec(&arr_types, expr.span);
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

            Expr::Block { exprs, label, drops_vars } => {
                self.enter_scope();

                // 1. define FnDefinitions
                for expr in exprs.iter_mut() {
                    if let Expr::FnDefinition { name, params, return_type, var_id, .. } = &mut expr.expression {
                        let fn_typ = self.get_fn_type(params, return_type, false);
                        let var = self.define_variable(name.clone(), false, true, fn_typ, expr.span);
                        *var_id = Some(var.var_id);
                    }
                }

                // normal pass
                let mut is_never = false;
                let block_drop_type = if let Some((last_expr, other_exprs)) = exprs.split_last_mut() {
                    // label logic
                    let snap_before_block = self.snap_label_before(label);

                    // actual expression compiling
                    for expr in other_exprs {
                        self.check_expression(expr, &ctx);
                        if expr.typ.is_never() { is_never = true }
                    }
                    // conditional bindings are allowed in the last expression of a block, because the current scope is gonna end after this expression anyways.
                    // this isn't reeeaally needed, but it definitely can't hurt to allow.
                    // let x = { case ?x = ... and x > 3 }
                    self.check_expression(last_expr, &ctx.allow_conditional_bindings());
                    if last_expr.typ.is_never() { is_never = true }

                    // label logic again
                    if let Some(label) = label {
                        let mut break_type_info = self.current_break_types.pop().unwrap();
                        assert_eq!(break_type_info.label, *label);
                        self.unify_types(&last_expr.typ, &break_type_info.typ, last_expr.span);

                        // 1 more snapshot after the full block executed
                        break_type_info.snapshots_from_breaks.push(self.snapshot_branch_vars_init_state(is_never));
                        self.merge_vars_init_states(snap_before_block.unwrap(), &break_type_info.snapshots_from_breaks);
                    };
                    
                    if is_never { TypeKind::Never } else { last_expr.typ.clone() }
                } else {
                    // Empty block returns Void
                    TypeKind::Void
                };

                *drops_vars = self.exit_scope();
                block_drop_type
            },

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
                if let TokenType::Ampersand = operator.token && old_ctx.allow_conditional_bindings {
                    ctx.allow_conditional_bindings = true;
                }
                self.check_expression(left, &ctx);
                self.check_expression(right, &ctx);
                self.check_infix(operator, &left.typ, &right.typ)
            }

            Expr::If { condition, then, alt } => {
                let ctx: &ExprContext = &ctx;
                self.check_expression(condition, &ctx.expect(TypeKind::Bool).allow_conditional_bindings());

                let snap = self.snapshot_first_vars_init_state();

                self.check_expression(then, ctx);
                let then_snap = self.snapshot_branch_vars_init_state(then.typ.is_never());
                self.restore_vars_init_state(&snap);

                self.check_expression(alt, &ctx.expect(then.typ.clone()));
                let alt_snap = self.snapshot_branch_vars_init_state(alt.typ.is_never());
                self.restore_vars_init_state(&snap);

                self.merge_vars_init_states(snap, &[then_snap, alt_snap]);

                then.typ.clone()
            },

            Expr::Ensure { condition, alt, then } => {
                self.check_expression(condition, &ctx.expect(TypeKind::Bool).allow_conditional_bindings());
                
                let snap = self.snapshot_first_vars_init_state();
    
                self.check_expression(alt, &ctx);
                if !alt.typ.is_never() {
                    self.type_mismatch(TypeKind::Never, alt.typ.clone(), alt.span);
                }

                // since alt.typ is supposed to always be TypeKind::Never only the then branch snapshot matters
                self.restore_vars_init_state(&snap);

                self.check_expression(then, &ctx);
                then.typ.clone()
            }

            Expr::Match { match_value, arms: cases } => {
                self.check_expression(match_value, &ctx);

                let original_snap = self.snapshot_first_vars_init_state();
                let mut arm_snapshots = Vec::new();

                let mut cases_to_cover = match self.prune(&match_value.typ) {
                    TypeKind::Bool => Some(HashSet::from([true.to_string(), false.to_string()])),
                    _ => None
                };
    
                let mut has_unfailable_arm = false;
                let mut arm_types = Vec::new();
    
                for arm in cases {
                    self.enter_scope();
                    match arm.pattern.pattern {
                        MatchPattern::Literal(Value::Bool(bool)) => {
                            cases_to_cover.as_mut().unwrap().remove(&bool.to_string());
                        }
                        _ => { }
                    }
                    self.check_binding_pattern(&mut arm.pattern, &mut None, true, true);
                    self.unify_types(&match_value.typ, &arm.pattern.typ, arm.pattern.span);

                    if !arm.pattern.can_fail { has_unfailable_arm = true; }

                    self.check_expression(&mut arm.body, &ctx);
                    arm_types.push(arm.body.typ.clone());
                    arm_snapshots.push(self.snapshot_branch_vars_init_state(arm.body.typ.is_never()));
                    self.restore_vars_init_state(&original_snap);

                    // ignoring dropped vars because patterns store what vars they define themselves.
                    self.exit_scope();
                }

                if !has_unfailable_arm && match &cases_to_cover {
                    None => false,
                    Some(remaining_cases) => !remaining_cases.is_empty()
                } {
                    self.error(crate::ErrType::TyperPatternDoesntCoverAllCases(cases_to_cover.unwrap().iter().cloned().collect()), expr.span);
                }

                self.merge_vars_init_states(original_snap, &arm_snapshots);

                self.unify_type_vec(&arm_types, match_value.span)
            },

            Expr::Loop { body, label } => {
                let loop_break_type = self.new_inference_type();

                let snap_before_loop = self.snap_label_before(&mut Some(label.clone()));

                // check expression
                self.check_expression(body, &ctx.expect(TypeKind::Void));

                // label logic again
                let break_type_info = self.current_break_types.pop().unwrap();
                assert_eq!(break_type_info.label, *label);
                if loop_break_type == self.prune(&loop_break_type) {
                    // loop doesn't have any breaks -> infinite loop -> TypeKind::Never
                    self.unify_types(&loop_break_type, &TypeKind::Never, expr.span);
                }
                self.merge_vars_init_states(snap_before_loop.unwrap(), &break_type_info.snapshots_from_breaks);

                loop_break_type
            },
            
            Expr::Assign { pattern, extra_operator, value } => {
                self.check_binding_pattern(pattern, &mut None, value.is_some(), true);
                if pattern.can_fail {
                    self.error(ErrType::TyperFailableLetPattern, expr.span);
                }
                if let Some(val) = value {
                    self.check_expression(val, &ctx.expect(pattern.typ.clone()));
                    if val.typ.is_never() { is_never = true }
                    if extra_operator.token != TokenType::Equal {
                        self.check_infix(extra_operator, &pattern.typ, &val.typ);
                    }
                }
                TypeKind::Void
            },

            Expr::Case { pattern, value } => {
                self.check_binding_pattern(pattern, &mut None, true, true);
                self.check_expression(value, &ctx.expect(pattern.typ.clone()));
                if value.typ.is_never() { is_never = true }
                else if !pattern.vars_defined.is_empty() && !old_ctx.allow_conditional_bindings {
                    self.error(ErrType::TyperInvalidBindingCaseExpr, expr.span);
                }
                TypeKind::Bool
            },

            Expr::MutRef { expr } => {
                self.check_expression(expr, &ctx);
                match &mut expr.expression {
                    Expr::Identifier { .. } => {
                        TypeKind::MutPointer(Box::new(expr.typ.clone()))
                    }
                    _ => self.error(ErrType::DefaultString("Cannot borrow non identifier as mut.".to_string()), expr.span)
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
                // this is None if it couldn't find where to break to (already errored)
                let break_info = self.find_loop_label(label, expr.span);
                
                let ctx = if let Some(i) = break_info { &ctx.expect(i.typ.clone()) } else { &ctx };
                self.check_expression(expr, ctx);

                // the current init var states need to be pushed here to correctly handle stuff like this:
                // let x
                // { #bloc
                //     if false {
                //         x = 5
                //     }
                //     else {
                //         x = 3
                //         break #bloc
                //     }
                // }
                // x  // x is initialized in every possible branch -> 
                let snap =  self.snapshot_branch_vars_init_state(expr.typ.clone().is_never());

                // refind break_info to make the borrow checker happy
                let break_info = self.find_loop_label(label, expr.span);
                if let Some(break_info) = break_info {
                    break_info.snapshots_from_breaks.push(snap);
                }

                TypeKind::Never
            },

            Expr::Continue { label } => {
                if let Some(continue_label) = label
                    && !self.current_break_types
                        .iter().rev()
                        .any(|x| x.label == *continue_label) {
                            self.error(ErrType::TyperUndefinedLoopLabel(
                                continue_label.to_string(),
                                self.current_break_types.iter().map(|x| x.label.to_string()).collect()
                            ), expr.span);
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
                            self.error(ErrType::TyperTooManyArguments(param_types.len(), call_param_types.len()), expr.span)
                        }
                        else {
                            for (param_type, arg_type) in param_types.iter().zip(call_param_types.iter()) {
                                self.unify_types(param_type, arg_type, callee.span);
                            }
                            *return_type.clone()
                        }
                    }
                    TypeKind::Inference(id) => {
                        // TODO not the best code, but works for now
                        let return_type = self.new_inference_type();
                        let fn_type = TypeKind::Fn { param_types: call_param_types, return_type: Box::new(return_type.clone()) };
                        self.unify_types(&fn_type, &TypeKind::Inference(*id), callee.span);
                        return_type
                    }
                    _ => self.error(ErrType::TyperCantCallNonFnType(callee.typ.clone()), callee.span)
                }
            },

            
            Expr::MemberAccess { left, member, resolved_index } => {
                self.check_expression(left, &ctx);
                if let TypeKind::Tup(elements) = self.prune(&left.typ) {
                    let member_index = elements.iter().position(|elem| elem.label == *member);
                    *resolved_index = member_index;

                    match member_index {
                        Some(i) => elements[i].typ.clone(),
                        None => self.error(ErrType::TyperTupleDoesntHaveMember(TypeKind::Tup(elements), member.to_string()), expr.span)
                    }
                }
                else { todo!() }
            }

            Expr::TypePath(segments) => self.check_path_expression(segments, expr.span),

            Expr::EnumDefinition { name, enums } => {
                self.define_type(name.clone(), ThrumType {
                    typ: DefinedTypeKind::Enum {
                        name: name.to_string(),
                        inner_types: enums.iter_mut()
                            .map(|x| (x.name.clone(), x.inner_types.clone()))
                            .collect()
                    },
                    values: HashMap::new()
                }, expr.span);
                TypeKind::Void
            },

            Expr::Void => TypeKind::Void,

            Expr::While { .. } => unreachable!("should be desugared already..."),


        };

        if inferred_type.is_never() { is_never = true }

        expr.typ = if is_never { TypeKind::Never }
            else { self.prune(&inferred_type) };
        
        if let Some(expected) = &old_ctx.expected_type {
            self.unify_types(expected, &expr.typ, expr.span);
        }
    }


    fn find_loop_label(&mut self, label: &mut Option<String>, span: Span) -> Option<&mut BreakTypeInfo> {
        if self.current_break_types.is_empty() {
            self.error(ErrType::TyperBreakOutsideLoop, span);
            None
        }
        else if let Some(target_label) = label {
            // rposition to search from the back (innermost loop out).
            let found_index = self.current_break_types
                .iter()
                .rposition(|info| info.label == *target_label);

            match found_index {
                Some(idx) => {
                    // found the label, return the break type
                    Some(&mut self.current_break_types[idx])
                }
                None => {
                    // couldn't find the label -> report error
                    let available_labels = self.current_break_types.iter().map(|info| info.label.clone()).collect();
                    self.error(ErrType::TyperUndefinedLoopLabel(
                        target_label.to_string(),
                        available_labels
                    ), span);
                    
                    None
                }
            }
        } else {
            // Break without label -> grab the last one
            Some(self.current_break_types.last_mut().unwrap())
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


    


    fn check_infix(&mut self, operator: &TokenSpan, left_type: &TypeKind, right_type: &TypeKind) -> TypeKind {
        match operator.token {
            // num/str operators
            TokenType::Plus | TokenType::Greater | TokenType::Less /* | TokenType::GreaterEqual | TokenType::LessEqual */ => {
                self.unify_types(left_type, right_type, operator.span);
                let unified_type = self.prune(left_type);
                let returned_type = if let TokenType::Plus = operator.token { unified_type.clone() } else { TypeKind::Bool };
                match unified_type {
                    TypeKind::Num | TypeKind::Str => returned_type,
                    // let it propagate, it will be solved later.
                    TypeKind::Inference(_) => returned_type,
                    _ => self.error(ErrType::TyperInvalidOperatorOnType(operator.token.clone(), unified_type), operator.span)
                }
            }

            // num operators
            TokenType::Minus | TokenType::Star | TokenType::Slash | TokenType::Percent | TokenType::StarStar => {
                self.unify_types(left_type, &TypeKind::Num, operator.span);
                self.unify_types(right_type, &TypeKind::Num, operator.span);
                TypeKind::Num
            }

            // comparison operators
            TokenType::EqualEqual /* | TokenType::NotEqual */ => {
                self.unify_types(left_type, right_type, operator.span);
                let unified_type = self.prune(left_type);
                match unified_type {
                    TypeKind::Num | TypeKind::Str | TypeKind::Bool | TypeKind::Arr(_) | TypeKind::Tup {.. } | TypeKind::Void => TypeKind::Bool,
                    // TypeKind::Inference(_) => TypeKind::Bool,
                    _ => self.error(ErrType::TyperInvalidOperatorOnType(TokenType::EqualEqual, unified_type), operator.span)
                }
            }

            // boolean operators
            TokenType::Ampersand | TokenType::Pipe => {
                self.unify_types(&TypeKind::Bool, left_type, operator.span);
                self.unify_types(&TypeKind::Bool, right_type, operator.span);
                TypeKind::Bool
            }
            _ => unreachable!("Unsupported infix operator: {:?}", operator)
        }
    }


    fn get_fn_type(&mut self, params: &mut [MatchPatternInfo], return_type: &mut TypeKind, define_params: bool) -> TypeKind {
        let mut param_types = Vec::new();
        for param_pattern in params.iter_mut() {
            self.check_binding_pattern(param_pattern, &mut None, true, define_params);

            if param_pattern.has_place { self.error(ErrType::TyperFnParamPlacePatterns, param_pattern.span); }
            if param_pattern.can_fail { self.error(ErrType::TyperFailableFnParamPatterns, param_pattern.span); }

            param_types.push(param_pattern.typ.clone());
        }
        if *return_type == TypeKind::ParserUnknown {
            *return_type = self.new_inference_type();
        }
        TypeKind::Fn {
            param_types,
            return_type: Box::new(return_type.clone()),
        }
    }


    fn check_fn_expression(&mut self, params: &mut [MatchPatternInfo], return_type: &mut TypeKind, body: &mut Rc<ExprInfo>, ctx: &ExprContext) -> TypeKind {
        self.enter_scope();

        // check the fn_type in a new scope to also define the function parameters
        let fn_type = self.get_fn_type(params, return_type, true);

        // set the return context to this functions return type
        let previous_function_return_type = self.current_function_return_type.clone();
        self.current_function_return_type = Some(return_type.clone());

        let body_mut = Rc::get_mut(body).unwrap();
        self.check_expression(body_mut, &ctx.expect(return_type.clone()));

        // drop all param vars
        self.exit_scope();

        // reset return context
        self.current_function_return_type = previous_function_return_type;

        if body_mut.typ.is_never() { TypeKind::Never }
        else { fn_type }
    }


    fn check_path_expression(&mut self, segments: &[String], span: Span) -> TypeKind {
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
                    _ => return self.error(ErrType::DefaultString("type path had 2 or more remaining segments.".to_string()), span)
                }
            }

            // else check for consts/functions (e.g. io::print)
            else if let Some(module_val) = curr_module.values.get(segment) {
                if i == segments.len() - 1 {
                    return module_val.typ.clone()
                }
                else {
                    return self.error(ErrType::DefaultString("value path too long.".to_string()), span)
                }
            }

            return self.error(ErrType::DefaultString(format!("segment {segment} could not be found...")), span);
        }

        self.error(ErrType::DefaultString(format!("'{}' could not be found...", segments.join("::"))), span)
    }
}