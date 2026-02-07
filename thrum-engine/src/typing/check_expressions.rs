use std::collections::HashMap;

use crate::{
    ErrType,
    lexing::tokens::TokenType,
    nativelib::ThrumType,
    parsing::ast_structure::{DefinedTypeKind, Expr, ExprInfo, MatchPatternInfo, Span, TupleElement, TupleType, TypeKind, Value}, typing::{BreakTypeInfo, Typechecker, check_patterns::CheckPatternMode}
};




#[derive(Default, Clone)]
pub struct ExprContext {
    expected_type: Option<TypeKind>,
    allow_conditional_bindings: bool,
}
impl ExprContext {
    pub fn expect(&self, typ: TypeKind) -> Self {
        Self {
            expected_type: Some(typ),
            ..self.clone()
        }
    }
    pub fn allow_conditional_bindings(&self) -> Self {
        Self {
            allow_conditional_bindings: true,
            ..self.clone()
        }
    }
}



impl Typechecker<'_> {
    pub(super) fn check_expression(&mut self, expr: &mut ExprInfo, old_ctx: &ExprContext) {
        let mut ctx = ExprContext::default();
        let mut is_never = false;

        let inferred_type = match &mut expr.expression {
            Expr::Literal(val) => self.check_literal(val),

            Expr::IdentifierRef { name, mutable, var_id } => {
                self.make_variable_ref(name, *mutable, expr.span, var_id)
            }

            Expr::TemplateString(parts) => {
                for part in parts {
                    self.check_expression(part, &ctx);
                    if self.prune(&part.typ, None).is_never() { is_never = true }
                }
                TypeKind::Str
            }

            Expr::Tuple(elements) => {
                let mut tuple_types = Vec::new();

                for TupleElement { label, expr } in elements {
                    self.check_expression(expr, &ctx);
                    self.insert_deref_if_pointer(expr);
                    if self.prune(&expr.typ, None).is_never() { is_never = true }
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
                if self.prune(&left.typ, None).is_never() { is_never = true }
                if self.prune(&index.typ, None).is_never() { is_never = true }
                arr_element_type
            }

            Expr::Block { exprs, label, drops_vars } => {
                self.enter_scope();

                // 1. define FnDefinitions
                for expr in exprs.iter_mut() {
                    if let Expr::FnDefinition { name, params, return_type_annotation, var_id, .. } = &mut expr.expression {
                        let fn_typ = self.get_fn_type(params, &mut return_type_annotation.typ);
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
                    }

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
                self.fully_deref_expression(left);
                self.fully_deref_expression(right);
                self.check_infix(&operator.token, operator.span, &left.typ, &right.typ)
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

            Expr::Match { match_value, arms } => {
                self.check_expression(match_value, &ctx);

                let original_snap = self.snapshot_first_vars_init_state();
                let mut arm_snapshots = Vec::new();

                let arm_drop_type = self.new_inference_type();
                let mut covered_cases = Vec::new();

                for arm in arms {
                    self.enter_scope();

                    self.check_match_pattern(&mut arm.pattern, &match_value.typ, CheckPatternMode::Set, &mut None);
                    covered_cases.extend(std::mem::take(&mut arm.pattern.covered_cases));

                    self.check_expression(&mut arm.body, &ctx.expect(arm_drop_type.clone()));

                    arm_snapshots.push(self.snapshot_branch_vars_init_state(arm.body.typ.is_never()));

                    // ignoring dropped vars here because patterns store what vars they define themselves.
                    self.exit_scope();

                    self.restore_vars_init_state(&original_snap);
                }

                let missing_cases = self.convert_to_missing_cases(&covered_cases);
                if !missing_cases.is_empty() {
                    self.error(crate::ErrType::TyperPatternDoesntCoverAllCases { remaining: missing_cases }, expr.span);
                }

                self.merge_vars_init_states(original_snap, &arm_snapshots);
                arm_drop_type
            },

            Expr::Loop { body, label } => {
                let loop_break_type = self.new_inference_type();

                let snap_before_loop = self.snap_label_before(&mut Some(label.clone()));

                // check expression
                self.check_expression(body, &ctx.expect(TypeKind::Void));

                // label logic again
                let break_type_info = self.current_break_types.pop().unwrap();
                assert_eq!(break_type_info.label, *label);
                if loop_break_type == self.prune(&loop_break_type, None) {
                    // loop doesn't have any breaks -> infinite loop -> TypeKind::Never
                    self.unify_types(&loop_break_type, &TypeKind::Never, expr.span);
                }
                self.merge_vars_init_states(snap_before_loop.unwrap(), &break_type_info.snapshots_from_breaks);

                loop_break_type
            },

            Expr::Assign { pattern, value, extra_operator: _ /* desugared */, op_span: _ } => {
                if let Some(val) = value {
                    self.check_expression(val, &ctx);
                    if val.typ.is_never() { is_never = true }

                    self.check_match_pattern(pattern, &val.typ, CheckPatternMode::Set, &mut None);
                }
                else {
                    let value_typ = self.new_inference_type();
                    self.check_match_pattern(pattern, &value_typ, CheckPatternMode::NoInit, &mut None);
                }

                let missing_cases = self.convert_to_missing_cases(&pattern.covered_cases);
                if !missing_cases.is_empty() {
                    self.error(ErrType::TyperFailableLetPattern { remaining: missing_cases }, expr.span);
                }
                TypeKind::Void
            },

            Expr::Case { pattern, value } => {
                self.check_expression(value, &ctx);
                if value.typ.is_never() { is_never = true }
                self.check_match_pattern(pattern, &value.typ, CheckPatternMode::Set, &mut None);

                if !pattern.vars_defined.is_empty() && !old_ctx.allow_conditional_bindings {
                    self.error(ErrType::TyperInvalidBindingCaseExpr, expr.span);
                }
                TypeKind::Bool
            },

            Expr::Move { expr, auto_clone } => {
                self.check_expression(expr, &ctx);

                if let TypeKind::Pointer { mutable, inner, borrows_var } = &mut expr.typ {
                    *auto_clone = self.prune(inner, Some(expr.span)).is_auto_clone();
                    if !*auto_clone {
                        self.move_variable(borrows_var, expr.span);
                    }
                    *inner.clone()
                }
                else {
                    self.error(ErrType::TyperCantDerefNonPointerType { typ: expr.typ.clone() }, expr.span)
                }
            },

            Expr::FnDefinition { params, return_type_annotation, body, .. } => {
                self.check_fn_expression(params, &mut return_type_annotation.typ, body, &ctx);
                if body.typ.is_never() { is_never = true }
                TypeKind::Void
            }
            Expr::Closure { params, return_type_annotation: return_value, body } => {
                self.check_fn_expression(params, &mut return_value.typ, body, &ctx)
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

                let ctx = if let Some(info) = break_info {
                    &ctx.expect(info.typ.clone())
                } else {
                    &ctx
                };
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
                let snap = self.snapshot_branch_vars_init_state(expr.typ.is_never());

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
                            self.error(ErrType::TyperUndefinedLoopLabel {
                                label: continue_label.clone(),
                                available: self.current_break_types.iter().map(|x| x.label.clone()).collect()
                            }, expr.span);
                        }
                TypeKind::Never
            }

            Expr::Call { callee, arguments } => {
                self.check_expression(callee, &ctx);
                self.fully_deref_expression(callee);

                let mut call_param_types = Vec::new();
                for arg in arguments {
                    self.check_expression(arg, &ctx);
                    call_param_types.push(arg.typ.clone());
                }

                match self.prune(&callee.typ, Some(callee.span)) {
                    TypeKind::Fn { param_types, return_type } => {
                        if param_types.len() != call_param_types.len() {
                            self.error(ErrType::TyperTooManyArguments { expected: param_types.len(), found: call_param_types.len() }, expr.span)
                        }
                        else {
                            for (param_type, arg_type) in param_types.iter().zip(call_param_types.iter()) {
                                self.unify_types(param_type, arg_type, callee.span);
                            }
                            *return_type
                        }
                    }
                    _ => self.error(ErrType::TyperCantCallNonFnType { typ: callee.typ.clone() }, callee.span)
                }
            },


            Expr::MemberAccess { left, member, resolved_index } => {
                self.check_expression(left, &ctx);
                if let TypeKind::Tup(elements) = self.prune(&left.typ, Some(expr.span)) {
                    let member_index = elements.iter().position(|elem| elem.label == *member);
                    *resolved_index = member_index;

                    match member_index {
                        Some(i) => elements[i].typ.clone(),
                        None => self.error(ErrType::TyperTupleDoesntHaveMember { tup: TypeKind::Tup(elements), member: member.clone() }, expr.span)
                    }
                }
                else { todo!() }
            }

            Expr::TypePath(segments) => self.check_path_expression(segments, expr.span),

            Expr::EnumDefinition { name, variants } => {
                self.define_type(name.clone(), ThrumType {
                    typ: DefinedTypeKind::Enum {
                        name: name.clone(),
                        variants: variants.clone()
                    },
                    values: HashMap::new()
                }, expr.span);
                TypeKind::Void
            },

            Expr::Void => TypeKind::Void,

            Expr::While { .. } => unreachable!("should be desugared already..."),


        };

        expr.typ = if is_never { TypeKind::Never } else { inferred_type };

        if let Some(expected) = &old_ctx.expected_type {
            // if type is know we can do auto-deref stuff. otherwise just unify
            if matches!(self.prune(expected, None), TypeKind::Inference(_)) {
                self.unify_types(expected, &expr.typ, expr.span);
            } else {
                self.unify_expression_with_type(expr, expected);
            }
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

            if let Some(idx) = found_index {
                // found the label, return the break type
                Some(&mut self.current_break_types[idx])
            } else {
                // couldn't find the label -> report error
                let available_labels = self.current_break_types.iter().map(|info| info.label.clone()).collect();
                self.error(ErrType::TyperUndefinedLoopLabel {
                    label: target_label.clone(),
                    available: available_labels
                }, span);

                None
            }
        } else {
            // Break without label -> grab the last one
            Some(self.current_break_types.last_mut().unwrap())
        }
    }



    pub(super) fn check_literal(&self, val: &Value) -> TypeKind {
        match val {
            Value::Num(_) => TypeKind::Num,
            Value::Str(_) => TypeKind::Str,
            Value::Bool(_) => TypeKind::Bool,
            _ => unreachable!() // other values are not used in the parser
        }
    }





    fn check_infix(&mut self, operator: &TokenType, op_span: Span, left: &TypeKind, right: &TypeKind) -> TypeKind {
        match operator {
            // num/str operators
            TokenType::Plus | TokenType::Greater | TokenType::Less /* | TokenType::GreaterEqual | TokenType::LessEqual */ => {
                self.unify_types(left, right, op_span);
                let pruned_left = self.prune(left, Some(op_span));
                let pruned_right = self.prune(right, None);
                let returned_type = if let TokenType::Plus = operator { pruned_left.clone() } else { TypeKind::Bool };
                match pruned_left {
                    TypeKind::Num | TypeKind::Str => returned_type,
                    _ => self.error(ErrType::TyperInvalidOperatorOnType { op: operator.clone(), type_a: pruned_left, type_b: pruned_right }, op_span)
                }
            }

            // num operators
            TokenType::Minus | TokenType::Star | TokenType::Slash | TokenType::Percent | TokenType::StarStar => {
                self.unify_types(left, &TypeKind::Num, op_span);
                self.unify_types(right, &TypeKind::Num, op_span);
                TypeKind::Num
            }

            // comparison operators
            TokenType::EqualEqual /* | TokenType::NotEqual */ => {
                self.unify_types(left, right, op_span);
                let pruned_left = self.prune(left, Some(op_span));
                let pruned_right = self.prune(right, None);
                match pruned_left {
                    TypeKind::Num | TypeKind::Str | TypeKind::Bool | TypeKind::Arr(_) | TypeKind::Tup {.. } | TypeKind::Void => TypeKind::Bool,
                    _ => self.error(ErrType::TyperInvalidOperatorOnType { op: operator.clone(), type_a: pruned_left, type_b: pruned_right }, op_span)
                }
            }

            // boolean operators
            TokenType::Ampersand | TokenType::Pipe => {
                self.unify_types(&TypeKind::Bool, left, op_span);
                self.unify_types(&TypeKind::Bool, right, op_span);
                TypeKind::Bool
            }
            _ => unreachable!("Unsupported infix operator: {:?}", operator)
        }
    }


    fn get_fn_type(&mut self, params: &mut [MatchPatternInfo], return_type: &mut TypeKind) -> TypeKind {
        let mut param_types = Vec::new();
        for param in params.iter_mut() {
            param_types.push(self.get_match_pattern_type(param));
        }
        if *return_type == TypeKind::ParserUnknown {
            *return_type = self.new_inference_type();
        }
        TypeKind::Fn {
            param_types,
            return_type: Box::new(return_type.clone()),
        }
    }


    fn check_fn_expression(&mut self, params: &mut [MatchPatternInfo], return_type: &mut TypeKind, body: &mut ExprInfo, ctx: &ExprContext) -> TypeKind {
        self.enter_scope();

        // check the fn_type in a new scope to also define the function parameters
        let fn_type = self.get_fn_type(params, return_type);
        for param in params {
            let expected_type = self.new_inference_type();
            self.check_match_pattern(param, &expected_type, CheckPatternMode::Set, &mut None);
        }

        // set the return context to this functions return type
        let previous_function_return_type = self.current_function_return_type.clone();
        self.current_function_return_type = Some(return_type.clone());

        self.check_expression(body, &ctx.expect(return_type.clone()));

        // drop all param vars
        self.exit_scope();

        // reset return context
        self.current_function_return_type = previous_function_return_type;

        if body.typ.is_never() { TypeKind::Never }
        else { fn_type }
    }



    // this function allows auto-deref to autoclone types
    // e.g. expr.typ: &&bool, expected: bool -> works, inserts 2 moves
    // e.g. expr.typ: &&Vec, expected: &Vec -> works, inserts 1 move
    fn unify_expression_with_type(&mut self, expr: &mut ExprInfo, expected_type: &TypeKind) {
        let (expr_p_count, is_auto_clone_after) = self.count_initial_pointers(&expr.typ, expr.span);
        let (expected_p_count, _) = self.count_initial_pointers(expected_type, expr.span);

        let before_typ = self.prune(&expr.typ, None);

        // if we have more pointers than the expected_type, AND everything that needs to be derefed is auto_clone
        if expr_p_count > expected_p_count && (expected_p_count != 0 || is_auto_clone_after) {
            for _ in 0..(expr_p_count - expected_p_count) {
                match self.prune(&expr.typ, Some(expr.span)) {
                    TypeKind::Pointer { mutable: _, inner, borrows_var: _ } => {
                        self.insert_deref(expr, *inner, true);
                    }
                    _ => unreachable!("i just checked the amount of pointers, so this is unreachable \
                        ({expr_p_count}, {expected_p_count}, {is_auto_clone_after}) ({expected_type}, {before_typ})")
                }
            }
        }
        // just unify after dereferencing pointers
        self.unify_types(expected_type, &expr.typ, expr.span);
    }

    fn count_initial_pointers(&mut self, typ: &TypeKind, err_span: Span) -> (usize, bool) {
        let mut curr_typ = typ.clone();
        let mut count = 0;

        while let TypeKind::Pointer { inner, .. } = self.prune(&curr_typ, Some(err_span)) {
            curr_typ = *inner.clone();
            count += 1;
        }

        let typ_after_pointers_auto_clone = self.prune(&curr_typ, Some(err_span)).is_auto_clone();
        (count, typ_after_pointers_auto_clone)
    }


    fn fully_deref_expression(&mut self, expr: &mut ExprInfo) {
        while let TypeKind::Pointer { mutable: _, inner, borrows_var: _ }
            = self.prune(&expr.typ, Some(expr.span)) {
                let auto_clone = self.prune(&inner, Some(expr.span)).is_auto_clone();
                self.insert_deref(expr, *inner, auto_clone);
        }
    }


    pub fn dereference_typ(&mut self, typ:  &TypeKind, span: Span) -> TypeKind {
        match self.prune(typ, Some(span)) {
            TypeKind::Pointer { mutable: _, inner, borrows_var: _ } => *inner,
            x => self.error(ErrType::TyperCantDerefNonPointerType { typ: x }, span),
        }
    }

    fn insert_deref_if_pointer(&mut self, expr: &mut ExprInfo) -> bool {
        match self.prune(&expr.typ, Some(expr.span)) {
            TypeKind::Pointer { mutable: _, inner, borrows_var } => {
                let auto_clone = self.prune(&inner, Some(expr.span)).is_auto_clone();
                if !auto_clone {
                    self.move_variable(&borrows_var, expr.span);
                }
                self.insert_deref(expr, *inner, auto_clone);
                true
            }
            _ => { false /* do nothing */ }
        }
    }

    fn insert_deref(&self, expr: &mut ExprInfo, inner: TypeKind, auto_clone: bool) {
        *expr = ExprInfo {
            span: expr.span,
            typ: inner,
            expression: Expr::Move {
                // replace with random junk
                // thrum supports this without the random junk ;p
                expr: Box::new(std::mem::replace(expr, Expr::Void.to_info(Span::invalid()))),
                auto_clone
            }
        };
    }



    fn check_path_expression(&mut self, segments: &[String], span: Span) -> TypeKind {
        let mut curr_module = &self.library;

        for (i, segment) in segments.iter().enumerate() {
            // try to go into a sub module
            if let Some(sub_module) = curr_module.sub_modules.get(segment) {
                curr_module = sub_module;
                continue;
            }

            // else check for types (e.g. str::len)
            else if let Some(module_type) = curr_module.types.get(segment) {
                let remaining_segments = &segments[(i + 1)..];
                match remaining_segments {
                    // 0 remaining segments -> type
                    [] => return TypeKind::CustomType { name: segment.clone(), generic_types: vec![/* TODO */] },
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
                return self.error(ErrType::DefaultString("value path too long.".to_string()), span)
            }

            return self.error(ErrType::DefaultString(format!("segment {segment} could not be found...")), span);
        }

        self.error(ErrType::DefaultString(format!("'{}' could not be found...", segments.join("::"))), span)
    }
}