use crate::{
    ErrType, lexing::tokens::{AssignOp, Span, TokenKind},
    parsing::ast_structure::{AstClosure, AstEnumExpression, AstTupleElement, AstValue, Expr, ExprId, PatternId},
    typing::{EnumId, Type, TypeChecker, TypeId, TypeTuple, TypeVarId, check_patterns::{CheckPatternVars, PatternSpace},
    type_environment::TypeVarConstVal}
};




#[derive(Default, Clone)]
pub struct CheckExprCtx {
    expected_type: Option<TypeId>,

    // e.g. `if option is let .Some(x) { ... }`
    allow_is_expr_bindings: bool,

    // e.g. `x = 4` -> `mut x = 4`
    // or `x^` -> `mut x^`
    // this setting starts from move expressions and place-pointer patterns
    // and sifts through array[access], member.access
    auto_borrow_mut: bool,

    // (1, x) - tuples are mode Once, x is gonna be autoderefed
    // x()    - function calls are fully derefed, so even if x is a pointer to a pointer to a pointer to a pointer to a pointer to a pointer it will still work
    deref_mode: AutoDerefMode,
}
#[derive(Default, Clone)]
pub enum AutoDerefMode {
    #[default]
    None, Once, Fully, LeaveOnePointer
}
impl CheckExprCtx {
    pub fn expect(&self, typ: TypeId) -> Self {
        Self { expected_type: Some(typ), ..self.clone() }
    }
    pub fn maybe_expect(&self, typ: Option<TypeId>) -> Self {
        Self { expected_type: typ, ..self.clone() }
    }
    pub fn allow_conditional_bindings(&self) -> Self {
        Self { allow_is_expr_bindings: true, ..self.clone() }
    }
    pub fn auto_borrow_mut(&self) -> Self {
        Self { auto_borrow_mut: true, ..self.clone() }
    }
    pub fn auto_deref(&self, mode: AutoDerefMode) -> Self {
        Self { deref_mode: mode, ..self.clone() }
    }
}



impl TypeChecker<'_> {
    pub(super) fn check_expression(&mut self, check_expr: ExprId, is_never: &mut bool, old_ctx: &CheckExprCtx) -> TypeId {
        let mut ctx = CheckExprCtx::default();

        let span = self.ast.get_expr_span(check_expr);
        let expr_expr = self.ast.get_expr(check_expr);

        let mut inferred_type = match expr_expr {
            Expr::Literal { val } => self.check_literal(val),

            Expr::IdentifierRef { name, mutable } => {
                let mutable = *mutable || old_ctx.auto_borrow_mut;
                self.make_variable_ref(name, mutable, check_expr)
            }

            Expr::TemplateString { elems } => {
                for &elem in elems {
                    self.check_expression(elem, is_never, &ctx);
                }
                TypeId::STR
            }

            Expr::Tuple { elems } => {
                let tuple_types = elems.iter()
                    .map(|AstTupleElement { label, expr }| TypeTuple {
                        label: label.clone(),
                        typ: self.check_expression(*expr, is_never, &ctx.auto_deref(AutoDerefMode::Once))
                    })
                    .collect();
                    
                self.add_type(Type::Tup(tuple_types))
            }

            Expr::TupleArr { elem, length } => {
                let elem_type = self.check_expression(*elem, is_never, &ctx.auto_deref(AutoDerefMode::Once));
                self.check_expression(*length, is_never, &ctx.auto_deref(AutoDerefMode::Fully).expect(TypeId::NUM));

                let const_length = match self.ast.get_expr(*length) {
                    Expr::Literal { val: AstValue::Num(num) } => *num as usize,
                    _ => 0
                };

                self.add_type(Type::TupArr(elem_type, const_length))
            }

            Expr::Index { left, index } => {
                let arr_ctx = if old_ctx.auto_borrow_mut { &ctx.auto_borrow_mut() } else { &ctx };
                let left_pointer_type = self.check_expression(*left, is_never, &arr_ctx.auto_deref(AutoDerefMode::LeaveOnePointer));

                let left_span = Some(self.ast.get_expr_span(*left));
                match self.prune_type_once(left_pointer_type, left_span) {
                    Type::Pointer { inner, mutable, borrows_var } => {

                        let arr_inner_type = match self.prune_type_once(inner, left_span) {
                            Type::TupArr(inner, _) => inner,
                            Type::Tup(elems) => {
                                if elems.is_empty() {
                                    self.error(ErrType::TyperCantIndexEmptyTuple { typ: Type::Tup(elems) }, span)
                                } else {
                                    // tuple indexing is allowed if all types are equal
                                    let first_typ = elems[0].typ;
                                    if elems.iter().all(|e| e.typ == first_typ) {
                                        first_typ
                                    } else {
                                        self.error(ErrType::TyperCantIndexHeterogenousTuple { typ: Type::Tup(elems) }, span)
                                    }
                                }
                            }
                            Type::Error => TypeId::ERROR,
                            typ => self.error(ErrType::TyperCantIndexNonArrType { typ }, span)
                        };
        
                        self.check_expression(*index, is_never, &ctx.expect(TypeId::NUM));
        
                        self.add_type(Type::Pointer { inner: arr_inner_type, mutable, borrows_var })
                    },
                    _ => TypeId::ERROR
                }
            }

            Expr::Block { exprs, label } => {
                self.enter_scope();

                // this collects type information for enums, fndefinitions, and more probably
                self.hoisting_pass(exprs);

                // normal pass
                let last_type = if let Some((&last_expr, other_exprs)) = exprs.split_last() {
                    // label logic
                    let snap_before_block = label.as_ref().map(|l| self.before_check_label_logic(check_expr, l));

                    // actual expression compiling
                    for &expr in other_exprs {
                        self.check_expression(expr, is_never, &ctx);
                    }

                    let last_type = self.check_expression(last_expr, is_never, &ctx.allow_conditional_bindings());

                    // label logic again
                    if let Some(label) = label {
                        let mut label_info = self.curr_label_infos.pop().unwrap();
                        assert_eq!(label_info.label, *label);
                        self.unify_types(last_type, label_info.typ, self.ast.get_expr_span(last_expr));

                        // 1 more snapshot after the full block executed
                        label_info.break_snapshots.push(self.snapshot_branch_vars_state(*is_never));
                        self.merge_vars_states(snap_before_block.unwrap(), &label_info.break_snapshots);
                    }

                    last_type
                } else {
                    // Empty block drops Void
                    TypeId::VOID
                };
                self.exit_scope();
                last_type
            },

            Expr::Prefix { op, right } => {
                match op {
                    TokenKind::Exclamation =>         self.check_expression(*right, is_never, &ctx.expect(TypeId::BOOL)),
                    TokenKind::Op(AssignOp::Minus) => self.check_expression(*right, is_never, &ctx.expect(TypeId::NUM)),
                    _ => unreachable!("Unsupported prefix op: {op} (Parser Issue)")
                }
            }

            Expr::Infix { op, op_span, left, right } => {
                if let TokenKind::And = op && old_ctx.allow_is_expr_bindings {
                    ctx = ctx.allow_conditional_bindings();
                }
                let left_type = self.check_expression(*left, is_never, &ctx.auto_deref(AutoDerefMode::Fully));

                // because of short-circuiting right is not always evaluated, meaning that a Never-expr might not trigger
                let right_is_never = &mut false;
                let right_type = self.check_expression(*right, right_is_never, &ctx.auto_deref(AutoDerefMode::Fully));
                if *right_is_never && !matches!(op, TokenKind::And | TokenKind::Or) { *is_never = true; }

                self.check_infix(*op, *op_span, left_type, right_type)
            }

            Expr::If { condition, then, alt } => {
                self.check_expression(*condition, is_never, &ctx.expect(TypeId::BOOL).allow_conditional_bindings());

                let snap = self.snapshot_vars_state();

                let mut then_is_never = false;
                let then_typ = self.check_expression(*then, &mut then_is_never, &ctx);
                let then_snap = self.snapshot_branch_vars_state(then_is_never);
                self.restore_vars_state(&snap);
                
                let mut alt_is_never = false;
                let alt_ctx = if then_is_never { ctx.clone() } else { ctx.expect(then_typ) };
                let alt_typ = self.check_expression(*alt, &mut alt_is_never, &alt_ctx);
                let alt_snap = self.snapshot_branch_vars_state(alt_is_never);
                self.merge_vars_states(snap, &[then_snap, alt_snap]);

                // determine the final type:
                match (then_is_never, alt_is_never) {
                    (true, true) => TypeId::NEVER,
                    (true, false) => alt_typ,
                    (false, true) => then_typ,
                    (false, false) => {
                        self.unify_types(then_typ, alt_typ, span);
                        then_typ
                    }
                }
            },

            Expr::Ensure { condition, alt, then } => {
                self.check_expression(*condition, is_never, &ctx.expect(TypeId::BOOL).allow_conditional_bindings());

                let snap = self.snapshot_vars_state();

                let alt_type = self.check_expression(*alt, &mut false, &ctx);
                if alt_type != TypeId::NEVER {
                    let pruned_alt_type = self.prune_type_once(alt_type, None);
                    self.type_mismatch(Type::Never, pruned_alt_type, self.ast.get_expr_span(*alt));
                }

                // since alt.typ is always be Type::Never only the then branch snapshot matters
                self.restore_vars_state(&snap);

                self.check_expression(*then, is_never, &ctx)
            }

            Expr::Match { match_value, arms } => {
                let match_val_type = self.check_expression(*match_value, is_never, &ctx);

                let original_snap = self.snapshot_vars_state();
                let mut arm_snapshots = Vec::new();

                let mut arm_expr_types = Vec::new();
                let mut covered_cases = Vec::new();

                let mut all_arms_never = true;

                for arm in arms {
                    self.enter_scope();

                    let (_, arm_covered) = self.check_match_pattern(
                        arm.pattern, Some(match_val_type), true, None, &mut CheckPatternVars::Collect(&mut Vec::new())
                    );
                    covered_cases.extend(arm_covered);
                    
                    let mut arm_never = false;
                    arm_expr_types.push(self.check_expression(arm.body, &mut arm_never, &ctx));
                    if !arm_never { all_arms_never = false }
                    self.exit_scope();
                    
                    arm_snapshots.push(self.snapshot_branch_vars_state(arm_never));

                    self.restore_vars_state(&original_snap);
                }

                if all_arms_never { *is_never = true; }

                let missing_cases = PatternSpace::covered_to_missing_cases(&covered_cases);
                if !missing_cases.is_empty() {
                    self.error(crate::ErrType::TyperPatternDoesntCoverAllCases { remaining: missing_cases }, span);
                }

                self.merge_vars_states(original_snap, &arm_snapshots);
                self.unify_type_vec(&arm_expr_types, span)
            },

            Expr::Loop { body, label } => {
                let loop_break_type = self.new_infer_type();

                let snap_before_loop = self.before_check_label_logic(check_expr, label);

                // check expression
                self.check_expression(*body, &mut false, &ctx.expect(TypeId::VOID));

                // label logic again
                let label_info = self.curr_label_infos.pop().unwrap();
                assert_eq!(label_info.label, *label);
                if loop_break_type == self.prune_id_once(loop_break_type) {
                    // loop doesn't have any breaks -> infinite loop -> Type::Never
                    self.unify_types(loop_break_type, TypeId::NEVER, span);
                }
                self.merge_vars_states(snap_before_loop, &label_info.break_snapshots);

                loop_break_type
            },

            Expr::Assign { pattern, value, extra_op, op_span } => {
                let pattern_type = self.check_assign_pattern_and_value(
                    *pattern, Some(*value), is_never, true, false, None
                );

                if let Some(extra_op) = extra_op {
                    let infixed_typ = self.check_infix(TokenKind::Op(*extra_op), *op_span, pattern_type, pattern_type);
                    self.unify_types(pattern_type, infixed_typ, *op_span);
                }
                TypeId::VOID
            },

            Expr::EmptyLet { pattern } => {
                self.check_assign_pattern_and_value(*pattern, None, is_never, true, false, None);
                TypeId::VOID
            }

            Expr::Is { value, pattern } => {
                self.check_assign_pattern_and_value(
                    *pattern, Some(*value), is_never, old_ctx.allow_is_expr_bindings, true, None
                );

                TypeId::BOOL
            },

            Expr::Move { expr } => {
                let expr_type = self.check_expression(*expr, is_never, &ctx.auto_borrow_mut());
                let expr_type = self.prune_type_once(expr_type, Some(span));

                if let Type::Pointer { mutable, inner, borrows_var } = expr_type {
                    let pruned_inner = self.prune_type_once(inner, Some(span));
                    let auto_clone = self.is_auto_clone(&pruned_inner, span);
                    if auto_clone {
                        self.clone_variable(borrows_var, span);
                    } else {
                        self.move_variable(borrows_var, span);
                        self.typed_ast.move_expr.insert(check_expr);
                    }
                    if !mutable {
                        self.error(ErrType::TyperCantDerefNonMutPointerType { typ: expr_type }, span);
                    }
                    inner
                } else {
                    self.error(ErrType::TyperCantDerefNonPointerType { typ: expr_type }, span)
                }
            },

            
            Expr::Closure { closure, requires_type_annotation } => {
                let id = self.compiled_functions.reserve_slot(check_expr);
                self.typed_ast.resolved_closure_fn_id.insert(check_expr, id);
                
                self.get_fn_type(closure, *requires_type_annotation)
            }

            Expr::Return { expr } => {
                let curr_return_type = self.curr_function_return_type
                    .unwrap_or_else(|| self.error(ErrType::TyperReturnOutsideFunction, span));

                self.check_expression(*expr, &mut false, &ctx.expect(curr_return_type));

                TypeId::NEVER
            },

            Expr::Break { expr, label } => {
                // this is None if it couldn't find where to break to (already errored)
                if let Some(info) = self.find_loop_label(label.as_deref(), span) {
                    ctx = ctx.expect(info.typ);
                }
                let mut expr_is_never = false;
                self.check_expression(*expr, &mut expr_is_never, &ctx);

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
                let snap = self.snapshot_branch_vars_state(expr_is_never);

                // refind break_info to make the borrow checker happy
                if let Some(info) = self.find_loop_label(label.as_deref(), span) {
                    info.break_snapshots.push(snap);
                    let break_to = info.expr;
                    self.typed_ast.resolved_labels.insert(check_expr, break_to);
                }

                TypeId::NEVER
            },

            Expr::Continue { label } => {
                if let Some(info) = self.find_loop_label(label.as_deref(), span) {
                    let break_to = info.expr;
                    self.typed_ast.resolved_labels.insert(check_expr, break_to);
                }
                TypeId::NEVER
            }

            Expr::Call { callee, arguments } => {
                let callee_type = self.check_expression(*callee, is_never, &ctx.auto_deref(AutoDerefMode::Fully));
                let callee_span = self.ast.get_expr_span(*callee);

                let call_param_types: Vec<TypeId> = arguments.iter()
                    .map(|&arg| self.check_expression(arg, is_never, &ctx))
                    .collect();

                match self.prune_type_once(callee_type, Some(callee_span)) {
                    Type::Fn { param_types, return_type } => {
                        if param_types.len() == call_param_types.len() {
                            for (param_type, arg_type) in param_types.into_iter().zip(call_param_types) {
                                self.unify_types(param_type, arg_type, callee_span);
                            }
                            return_type
                        } else {
                            self.error(ErrType::TyperWrongNumberOfArguments { expected: param_types.len(), found: call_param_types.len() }, span)
                        }
                    }
                    Type::Error => TypeId::ERROR,
                    typ => self.error(ErrType::TyperCantCallNonFnType { typ }, callee_span)
                }
            },


            Expr::MemberAccess { left, member } => {
                if old_ctx.auto_borrow_mut { ctx.auto_borrow_mut = true }
                let left_type = self.check_expression(*left, is_never, &ctx.auto_deref(AutoDerefMode::Fully));
                let left_span = self.ast.get_expr_span(*left);
                
                match self.prune_type_once(left_type, Some(left_span)) {
                    Type::Tup(elems) => {
                        let member_index = elems.iter().position(|elem| elem.label == *member);

                        match member_index {
                            Some(i) => {
                                // modify the ast
                                self.typed_ast.resolved_tuple_indices.insert(check_expr, i);
                                elems[i].typ
                            }
                            None => self.error(ErrType::TyperTupleDoesntHaveMember { tup: Type::Tup(elems), member: member.clone() }, span)
                        }
                    }
                    _ => todo!("dot operator on other types...")
                }
            }

            Expr::EnumVariant { data: AstEnumExpression { variant_name, attached_tuple } } => {
                // using `.Variant` syntax requires that the Typechecker knows the Enumtype.
                if let Some((enum_id, variant_index, attached_type)) = self.check_enum_variant(variant_name, old_ctx.expected_type, span) {
                    if let Some(tup) = attached_tuple {
                        self.check_expression(*tup, is_never, &ctx.expect(attached_type));
                    } else {
                        // if the variant had no data, then the defined variant shouldn't have data either!
                        self.unify_types(TypeId::VOID, attached_type, span);
                    }
                    self.typed_ast.resolved_enum_variant.insert(check_expr, (enum_id, variant_index));

                    old_ctx.expected_type.unwrap()
                } else {
                    TypeId::ERROR
                }
            }

            Expr::TypeInstantiation { typ, data } => {
                let meta_id = self.check_annotation_meta_type_id(*typ);
                match self.prune_type_once(meta_id, Some(span)) {
                    Type::NewType(inner_new_type) => {
                        // for now i only support 1 piece of data in here
                        let [first_data] = data.as_slice() else {
                            panic!("multiple things here not yet implemented")
                        };
                        // now check the data with the expected type
                        self.check_expression(*first_data, is_never, &ctx.expect(inner_new_type));
                        meta_id
                    }
                    Type::Error => TypeId::ERROR,
                    t => self.error(ErrType::TyperCantInstantiateNonNewtypeType { typ: t }, self.ast.get_expr_span(*typ))
                }
            }

            Expr::TypeMemberAccess { .. } => todo!(),

            Expr::ParserError => TypeId::ERROR,
            Expr::Void


            // --- CONST STUFF ---
            // already handled in the hoisting phase
            | Expr::Const { .. } => TypeId::VOID,
            Expr::Newtype { expr } => {
                self.check_expression(*expr, is_never, &ctx.expect(TypeId::TYPE));
                TypeId::TYPE
            }
            Expr::EnumDefinition { variants } => {
                for variant in variants {
                    if let Some(tup) = variant.attached_tuple {
                        self.check_expression(tup, is_never, &ctx);
                    }
                }
                TypeId::TYPE
            }

            // should be desugared stuff
            Expr::While { .. } | Expr::FnDefinition { .. } => unreachable!("should be desugared already..."),
        };

        if inferred_type == TypeId::NEVER { *is_never = true; }
        
        // insert the inferred_type into the typed_ast
        inferred_type = if *is_never { TypeId::NEVER } else { inferred_type };
        self.typed_ast.expr_types[check_expr.0 as usize] = inferred_type;

        // handle AutoDerefModes
        match old_ctx.deref_mode {
            AutoDerefMode::None => {/* do nothing */}
            AutoDerefMode::Once => {
                self.deref_if_pointer(check_expr);
                inferred_type = self.typed_ast.expr_types[check_expr.0 as usize];
            }
            AutoDerefMode::Fully => {
                while self.deref_if_pointer(check_expr) { }
                inferred_type = self.typed_ast.expr_types[check_expr.0 as usize];
            }
            AutoDerefMode::LeaveOnePointer => {
                if 0 == self.count_initial_pointers(inferred_type, span).0 {
                    let pruned_infered = self.prune_type_once(inferred_type, None);
                    self.type_mismatch(
                        Type::Pointer { inner: TypeId::ERROR, mutable: false, borrows_var: TypeVarId(0) },
                        pruned_infered, span
                    );
                }
                while 1 < self.count_initial_pointers(self.typed_ast.expr_types[check_expr.0 as usize], span).0 {
                    self.deref_if_pointer(check_expr);
                }
            }
        }

        // unify it with the expected type (if something was expected)
        if let Some(expected) = old_ctx.expected_type {
            // if type is know we can do auto-deref stuff. otherwise just unify
            match self.prune_type_once(expected, None) {
                Type::Infer(_) => self.unify_types(expected, inferred_type, span),
                Type::Error => {}
                _ => {
                    self.unify_expression_with_type(check_expr, expected);
                    inferred_type = self.typed_ast.expr_types[check_expr.0 as usize];
                }
            }
        }

        // return the new updated infered type
        inferred_type
    }



    fn check_literal(&mut self, val: &AstValue) -> TypeId {
        match val {
            AstValue::Num(_) => self.add_type(Type::Num),
            AstValue::Str(_) => self.add_type(Type::Str),
            AstValue::Bool(_) => self.add_type(Type::Bool),
        }
    }





    fn check_infix(&mut self, op: TokenKind, op_span: Span, left: TypeId, right: TypeId) -> TypeId {
        match op {            
            TokenKind::EqualEqual | TokenKind::Greater | TokenKind::Less /*| TokenType::GreaterEqual | TokenType::LessEqual */ => {
                self.unify_types(left, right, op_span);
                TypeId::BOOL
            }

            // num operators
            TokenKind::Op(AssignOp::Plus | AssignOp::Minus | AssignOp::Star | AssignOp::Slash | AssignOp::Percent) => {
                self.unify_types(TypeId::NUM, left, op_span);
                self.unify_types(TypeId::NUM, right, op_span);
                TypeId::NUM
            }

            // boolean operators
            TokenKind::And | TokenKind::Or => {
                self.unify_types(TypeId::BOOL, left, op_span);
                self.unify_types(TypeId::BOOL, right, op_span);
                TypeId::BOOL
            }
            _ => unreachable!("Unsupported infix operator: {:?}", op)
        }
    }



    fn hoisting_pass(&mut self, exprs: &[ExprId]) {
        // 1. it collects all const exprs and defines them as Unresolved.
        for &expr in exprs {
            if let Expr::Const { pattern, value: _ } = self.ast.get_expr(expr) {
                self.mark_vars_in_pattern_as_const(*pattern, TypeVarConstVal::NotYetTypechecked(expr));
            }
        }

        // 2. typechecks all collected Unresolved vars.
        // if one depends on another, typecheck another first, then back to first.
        // if there is a self-dependency-cycle, throw an error
        while let Some(expr) = self.var_scopes.last().unwrap()
        .values()
        .find_map(|&var_id| if let TypeVarConstVal::NotYetTypechecked(x) = self.typed_ast.get_var(var_id).const_val { Some(x) } else { None }) {
            
            self.check_evaluate_and_bind_const(expr);
        }
    }


    pub(super) fn check_assign_pattern_and_value(
        &mut self, pattern: PatternId, value: Option<ExprId>, is_never: &mut bool, can_bind_vars: bool, can_fail: bool, const_update: Option<TypeVarConstVal>
    ) -> TypeId {
        let mut pattern_type = self.get_pattern_type(pattern);

        if let Some(val) = value {
            let check_typ = self.check_expression(val, is_never, &CheckExprCtx::default().maybe_expect(pattern_type));
            pattern_type = Some(check_typ);
        }

        let mut vars_defined = Vec::new();
        let (pattern_type, covered) = self.check_match_pattern(
            pattern, pattern_type, value.is_some(), const_update,
            &mut CheckPatternVars::Collect(&mut vars_defined)
        );

        let span = self.ast.get_pattern_span(pattern);
        let remaining = PatternSpace::covered_to_missing_cases(&covered);
        if !can_fail && !remaining.is_empty() {
            self.error(ErrType::TyperFailableAssignPattern { remaining }, span);
        }
        if !can_bind_vars && !vars_defined.is_empty() {
            self.error(ErrType::TyperInvalidBindingCaseExpr, span);
        }
        pattern_type
    }



    pub(super) fn get_fn_type(&mut self, closure: &AstClosure, type_annotation_required: bool) -> TypeId {
        let param_types = closure.params.iter()
            .map(|&param| {
                    if let Some(typ) = self.get_pattern_type(param) {
                        typ
                    } else {
                        if type_annotation_required {
                            self.error(ErrType::TyperRequiresTypeAnnotation, self.ast.get_pattern_span(param));
                        }
                        self.new_infer_type()
                    }
                }
            )
            .collect();

        let return_type = if let Some(ret) = closure.return_type {
            self.check_annotation_meta_type_id(ret)
        } else if type_annotation_required {
            TypeId::VOID
        } else {
            self.new_infer_type()
        };
        self.add_type(Type::Fn { param_types, return_type })
    }


    pub(super) fn check_fn_expression(&mut self, closure: &AstClosure, fn_type: TypeId) -> TypeId {
        // check the fn_type in a new scope to also define the function parameters
        self.enter_scope();

        let Type::Fn { param_types, return_type } = self.typed_ast.get_type(fn_type) else {
            unreachable!("this function should only be called with valid function types")
        };
        assert_eq!(param_types.len(), closure.params.len());

        for (&param_pattern, param_type) in closure.params.iter().zip(param_types) {
            let (_, covered) = self.check_match_pattern(
                param_pattern, Some(param_type), true, None, &mut CheckPatternVars::Collect(&mut Vec::new())
            );
            let remaining = PatternSpace::covered_to_missing_cases(&covered);
            if !remaining.is_empty() {
                let param_span = self.ast.get_pattern_span(param_pattern);
                self.error(ErrType::TyperPatternDoesntCoverAllCases { remaining }, param_span);
            }
        }

        // set the return context to this functions return type
        let backup = self.curr_function_return_type;
        self.curr_function_return_type = Some(return_type);
        
        self.check_expression(closure.body, &mut false,  &CheckExprCtx::default().expect(return_type));
        self.exit_scope();

        // reset return context
        self.curr_function_return_type = backup;

        fn_type
    }




    pub(super) fn check_enum_variant(&mut self, variant_name: &str, expected_type: Option<TypeId>, span: Span) -> Option<(EnumId, usize, TypeId)> {
        // using `.Variant` syntax requires that the Typechecker knows the Enumtype.
        let Some(expected) = expected_type else {
            self.error(ErrType::TyperRequiresTypeAnnotation, span);
            return None
        };
        let pruned_expected = self.prune_type_once(expected, Some(span));

        // make sure that the expected tpye is an Enum
        match pruned_expected {
            Type::Enum(enum_id) => {
                let enum_def = &self.typed_ast.enum_defs[enum_id.0 as usize];

                // try to find the correct .Variant
                let find_variant = enum_def.variants.iter().position(|(name, _)| **name == *variant_name);
                let Some(variant_index) = find_variant else {
                    self.error(ErrType::TyperEnumDoesntHaveVariant { 
                        enum_: pruned_expected, 
                        variant: variant_name.into()
                    }, span);
                    return None;
                };

                let attached_type = enum_def.variants[variant_index].1;

                Some((enum_id, variant_index, attached_type))
            }
            Type::Error => None,
            typ => { self.error(ErrType::TyperExpectedTypeIsntAnEnum { typ }, span); None }
        }
    }



    // this function allows auto-deref to autoclone types
    // e.g. expr.typ: &&bool, expected: bool -> works, inserts 2 moves
    // e.g. expr.typ: &&Vec, expected: &Vec -> works, inserts 1 move
    fn unify_expression_with_type(&mut self, expr: ExprId, expected_type: TypeId) {
        let expr_type = self.typed_ast.get_expr_type(expr);
        let span = self.ast.get_expr_span(expr);
        
        let (expr_p_count, is_auto_clone_after) = self.count_initial_pointers(expr_type, span);
        let (expected_p_count, _) = self.count_initial_pointers(expected_type, span);

        let before_typ = self.prune_type_once(expr_type, None);

        // if we have more pointers than the expected_type, AND everything that needs to be derefed is auto_clone
        if expr_p_count > expected_p_count && (expected_p_count != 0 || is_auto_clone_after) {
            for _ in 0..(expr_p_count - expected_p_count) {
                match self.prune_type_once(expr_type, Some(span)) {
                    Type::Pointer { inner, mutable: _, borrows_var: _ } => {
                        self.insert_deref(expr, false, inner);
                    }
                    _ => unreachable!("i just checked the amount of pointers, so this is unreachable \
                        ({expr_p_count}, {expected_p_count}, {is_auto_clone_after}) ({expected_type:?}, {before_typ:?})")
                }
            }
        }
        // just unify after dereferencing pointers
        let new_expr_type = self.typed_ast.get_expr_type(expr);
        self.unify_types(expected_type, new_expr_type, span);
    }

    fn count_initial_pointers(&mut self, typ: TypeId, err_span: Span) -> (usize, bool) {
        let mut curr_typ = typ;
        let mut count = 0;

        while let Type::Pointer { inner, .. } = self.prune_type_once(curr_typ, Some(err_span)) {
            curr_typ = inner;
            count += 1;
        }

        let final_pruned = self.prune_type_once(curr_typ, Some(err_span));
        let final_pruned_auto_clone = self.is_auto_clone(&final_pruned, err_span);
        (count, final_pruned_auto_clone)
    }

    pub fn deref_if_pointer(&mut self, expr: ExprId) -> bool {
        let span = self.ast.get_expr_span(expr);
        let typ = self.typed_ast.get_expr_type(expr);
        match self.prune_type_once(typ, Some(span)) {
            Type::Pointer { inner, mutable: _, borrows_var } => {
                let prune_inner = self.prune_type_once(inner, Some(span));
                let auto_clone = self.is_auto_clone(&prune_inner, span);
                if auto_clone {
                    self.clone_variable(borrows_var, span);
                } else {
                    self.move_variable(borrows_var, span);
                }
                self.insert_deref(expr, !auto_clone, inner);
                true
            }
            _ => false
        }
    }

    fn insert_deref(&mut self, expr: ExprId, moves: bool, new_type: TypeId) {
        *self.typed_ast.auto_derefs.entry(expr).or_default() += 1;
        self.typed_ast.expr_types[expr.0 as usize] = new_type;
        if moves {
            self.typed_ast.move_expr.insert(expr);
        }
        // println!("derefed: {:?}", self.ast.get_expr(expr));
    }

    fn is_auto_clone(&mut self, typ: &Type, span: Span) -> bool {
        match typ {
            Type::Num
            | Type::Bool
            | Type::Pointer { .. }
            | Type::Fn { .. }
            | Type::MetaType
            | Type::Error => true,

            Type::Str
            | Type::TupArr(_, _)
            | Type::Tup(_)
            | Type::Void
            | Type::Never
            | Type::Enum(_) => false,

            Type::NewType(id) => {
                let inner = self.prune_type_once(*id, Some(span));
                self.is_auto_clone(&inner, span)
            }

            Type::Infer(_) => unreachable!("is_auto_clone() should not be called with type {typ:?}")
        }
    }
}