use crate::{
    ErrType, lexing::tokens::{AssignOp, Span, TokenKind},
    parsing::ast::{AstClosure, AstEnumExpression, AstTupleElement, AstValue, Expr, ExprId, PatternId},
    typing::{CustomTypeId, EnumId, EnumRefinement, EnumFlowType, ResolvedMemberAccess, ResolvedTypeInstantiation, Type, TypeChecker, TypeId, FlowType, TypeTuple, TypeVarId, check_patterns::CheckPatternVars, exhaustiveness::PatternSpace, type_vars::{PatternOrVarId, TypeVarConstVal}}, vm_compiling::RuntimeValue
};




#[derive(Default, Clone)]
pub struct CheckExprCtx {
    expected_type: Option<TypeId>,

    // e.g. `if option is let .Some(x) { ... }`
    allow_is_expr_bindings: bool,

    // e.g. `tup.x = 4` -> `mut tup.x = 4`
    // this setting starts from move expressions and place-pointer patterns
    // and sifts through array[access], member.access
    auto_borrow_mut: bool,

    // (1, x) - tuples are mode Once, x is gonna be autoderefed
    // x()    - function calls are fully derefed, so even if x is a pointer to a pointer to a pointer to a pointer to a pointer to a pointer it will still work
    deref_mode: AutoDerefMode,

    // if its in a const context, runtime values aren't available
    // e.g. `let x = 5; let y: x = ...` doesn't work, x isn't const
    is_const: bool,
}
#[derive(Default, Clone, Copy)]
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
    pub fn is_const(&self) -> Self {
        Self { is_const: true, ..self.clone() }
    }
}



impl TypeChecker<'_> {
    pub(super) fn check_expression(&mut self, check_expr: ExprId, is_never: &mut bool, old_ctx: &CheckExprCtx) -> TypeId {
        let mut ctx = CheckExprCtx::default();
        if old_ctx.is_const {
            ctx.is_const = true;
        }

        let span = self.ast.get_expr_span(check_expr);
        let expr_expr = self.ast.get_expr(check_expr);

        let mut inferred_type = match expr_expr {
            Expr::Literal { val } => self.check_literal(val),

            Expr::IdentifierRef { name, mutable } => {
                let mutable = *mutable || old_ctx.auto_borrow_mut;
                self.make_variable_ref(name, mutable, old_ctx.is_const, check_expr)
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
                    
                self.type_arena.add_type(Type::Tup(tuple_types))
            }

            Expr::TupleArr { elem, length } => {
                let elem_type = self.check_expression(*elem, is_never, &ctx.auto_deref(AutoDerefMode::Once));
                self.check_expression(*length, is_never, &ctx.auto_deref(AutoDerefMode::Fully).expect(TypeId::NUM).is_const());

                match self.evaluate_expr(*length) {
                    Some(RuntimeValue::Num(num)) => {
                        let const_length = num as usize;
                        self.typed_ast.resolved_tuple_arr_length.insert(check_expr, const_length);
                        self.type_arena.add_type(Type::TupArr(elem_type, const_length))
                    }
                    Some(other) => unreachable!("expected a num from typechecking, but got {other}"),
                    None => TypeId::ERROR
                }
            }

            Expr::Index { left, index } => {
                let arr_ctx = if old_ctx.auto_borrow_mut { &ctx.auto_borrow_mut() } else { &ctx };
                let left_pointer_type = self.check_expression(*left, is_never, &arr_ctx.auto_deref(AutoDerefMode::LeaveOnePointer));

                let left_span = self.ast.get_expr_span(*left);
                if let Type::Borrow { inner, mutable, borrows_var } = self.prune_type_once_infer_err(left_pointer_type, left_span) {

                    let arr_inner_type = match self.prune_type_once_infer_err(inner, left_span) {
                        Type::TupArr(inner, _) => inner,
                        Type::Tup(elems) => {
                            if elems.is_empty() {
                                self.error(ErrType::TyperCantIndexEmptyTuple { typ: self.fmt_type(inner) }, span)
                            } else {
                                // tuple indexing is allowed if all types are equal
                                let first_typ = elems[0].typ;
                                if elems.iter().all(|e| e.typ == first_typ) {
                                    first_typ
                                } else {
                                    self.error(ErrType::TyperCantIndexHeterogenousTuple { typ: self.fmt_type(inner) }, span)
                                }
                            }
                        }
                        Type::Error => TypeId::ERROR,
                        _ => self.error(ErrType::TyperCantIndexNonArrType { typ: self.fmt_type(inner) }, span)
                    };
    
                    self.check_expression(*index, is_never, &ctx.expect(TypeId::NUM));
    
                    self.type_arena.add_type(Type::Borrow { inner: arr_inner_type, mutable, borrows_var })
                }
                else {
                    let arr_type = self.type_arena.add_type(Type::TupArr(TypeId::ERROR, 1));
                    self.type_mismatch(arr_type, left_pointer_type, span)
                }
            }

            Expr::Block { exprs, label } => {
                self.enter_scope();

                // this collects type information for enums, fndefinitions, and more probably
                self.hoisting_pass(exprs, true);

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
                    self.type_mismatch(TypeId::NEVER, alt_type, self.ast.get_expr_span(*alt));
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

                let missing_cases = PatternSpace::covered_to_missing_cases(&covered_cases, &self.typed_ast.enum_defs);
                if !missing_cases.is_empty() {
                    self.error(crate::ErrType::TyperPatternDoesntCoverAllCases {
                        remaining: PatternSpace::display_patterns(&missing_cases, &self.typed_ast.enum_defs)
                    }, span);
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
                let value_type = self.check_assign_pattern_and_value(
                    *pattern, Some(*value), is_never, true, false, extra_op.is_some(), None
                );

                if let Some(extra_op) = extra_op {
                    let infixed_typ = self.check_infix(TokenKind::Op(*extra_op), *op_span, value_type, value_type);
                    self.unify_types(value_type, infixed_typ, *op_span);
                }
                TypeId::VOID
            },

            Expr::EmptyLet { pattern } => {
                self.check_assign_pattern_and_value(*pattern, None, is_never, true, false, false, None);
                TypeId::VOID
            }

            Expr::Is { value, pattern } => {
                self.check_assign_pattern_and_value(
                    *pattern, Some(*value), is_never, old_ctx.allow_is_expr_bindings, true, false, None
                );

                TypeId::BOOL
            },

            Expr::Move { expr } => {
                let expr_type = self.check_expression(*expr, is_never, &ctx);

                match self.prune_type_once_infer_err(expr_type, span) {
                    Type::Borrow { inner, mutable: _, borrows_var } => {
                        let auto_clone = self.check_deref_memory_rules(inner, borrows_var, span);

                        if !auto_clone {
                            self.typed_ast.move_expr.insert(check_expr);
                        }
                        inner
                    }
                    Type::Error => TypeId::ERROR,
                    _ => self.error(ErrType::TyperCantDerefNonPointerType { typ: self.fmt_type(expr_type) }, span)
                }
            },

            Expr::Borrow { expr, mutable: _ } => {
                self.check_expression(*expr, is_never, &ctx.auto_deref(AutoDerefMode::Fully).expect(TypeId::TYPE).is_const())
            }
            
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

                match self.prune_type_once_infer_err(callee_type, callee_span) {
                    Type::Fn { param_types, return_type } => {
                        
                        // `4.square()` is sugar for `u32.square(4)`, this handles that:
                        let extra_arg = 
                        if let Some(ResolvedMemberAccess::MemberWithSelfSugar { self_sugar_expr, .. }) = self.typed_ast.resolved_member_access.get(callee) {
                            let first_arg_type = self.typed_ast.get_expr_type(*self_sugar_expr);
                            if let Some(first_param) = param_types.first() {
                                self.unify_types(*first_param, first_arg_type, self.ast.get_expr_span(*self_sugar_expr));
                                // if there isn't a first param it will error on the arg_count check anyways
                            }
                            1
                        } else {
                            0
                        };

                        if param_types.len() == arguments.len() + extra_arg {
                            for (param_type, arg) in param_types[extra_arg..].iter().zip(arguments) {
                                self.check_expression(*arg, is_never, &ctx.expect(*param_type));
                            }
                        } else {
                            self.error(ErrType::TyperWrongNumberOfArguments { expected: param_types.len(), found: arguments.len() + extra_arg }, span);
                        }

                        return_type
                    }
                    Type::Error => TypeId::ERROR,
                    _ => self.error(ErrType::TyperCantCallNonFnType { typ: self.fmt_type(callee_type) }, callee_span)
                }
            },


            Expr::MemberAccess { left, member: _ } => {
                if old_ctx.auto_borrow_mut { ctx.auto_borrow_mut = true }

                let left_type = self.check_expression(*left, is_never, &ctx);
                self.check_member_access(left_type, check_expr, None, false, old_ctx.expected_type)
            }


            Expr::EnumVariant { data: AstEnumExpression { variant_name, attached_tuple } } => {
                // using `.Variant` syntax requires that the Typechecker knows the expected Enumtype.
                if let Some(
                    (enum_id, variant_index, attached_type, refine_type)
                ) = self.check_enum_variant(variant_name, old_ctx.expected_type, Some(span), false) {

                    if let Some(tup) = attached_tuple {
                        self.check_expression(*tup, is_never, &ctx.expect(attached_type));
                    } else {
                        // if the variant had no data, then the defined variant shouldn't have data either!
                        self.unify_types(TypeId::VOID, attached_type, span);
                    }
                    self.typed_ast.resolved_enum_variant.insert(check_expr, (enum_id, variant_index));

                    refine_type
                } else {
                    TypeId::ERROR
                }
            }

            Expr::TypeInstantiation { typ, data } => {
                let meta_id = self.check_annotation_meta_type_id(*typ, true);

                if let Type::RefinedEnum(refined_inner, EnumRefinement::ExactVariant(i)) = self.prune_type_once_infer_err(meta_id, span) {
                    // its a refined constructor. e.g. `type X = Opt.Some; X{ 3 }`

                    let enum_id = self.get_wrapped_enum_id(refined_inner).unwrap();
                    let attached_type = self.typed_ast.enum_defs[enum_id.0 as usize].variants[i].1;
                    self.check_instantiation_payload(attached_type, check_expr, *data, is_never, &ctx);
                    self.typed_ast.resolved_type_instantian.insert(check_expr, ResolvedTypeInstantiation::EnumVariant(i));

                    // Return the Soft Specialized instance, wrapping the base_type!
                    let spec = self.add_type_refinement(FlowType::Enum(EnumFlowType::CurrVariant(i)));
                    self.type_arena.add_type(Type::FlowType(refined_inner, spec))
                }
                else {
                    match self.prune_type_once_infer_err(meta_id, span) {
                        Type::CustomType(custom_id, inner_new_type) => {
    
                            let inner = self.check_instantiation_payload(inner_new_type, check_expr, *data, is_never, &ctx);
    
                            // `N{ 2 }` returns the type `N`
                            self.type_arena.add_type(Type::CustomType(custom_id, inner))
                        }
                        Type::Error => TypeId::ERROR,
                        _ => self.error(ErrType::TyperMustBeCustomtypeType { typ: self.fmt_type(meta_id) }, self.ast.get_expr_span(*typ))
                    }
                }
            }

            Expr::ImplBlock { typ, const_exprs } => {
                let meta_type = self.check_annotation_meta_type_id(*typ, true);
                
                if let Type::CustomType(custom_id, _) = self.prune_type_once_infer_err(meta_type, span) {
                    let self_before = self.curr_impl_self.replace(meta_type);

                    // add the impl-scope as a normal var scope,
                    // all consts will just end up in there then!1!!
                    let impl_scope = std::mem::take(&mut self.custom_types[custom_id.0 as usize].impls);
                    self.var_scopes.push(impl_scope);

                    self.hoisting_pass(const_exprs, false);

                    // and insert the impl scope back to where it came from
                    self.custom_types[custom_id.0 as usize].impls = self.var_scopes.pop().unwrap();

                    // println!("Added impl for type: {}", self.fmt_type(meta_type));

                    self.curr_impl_self = self_before;
                } else {
                    self.error(ErrType::TyperCantImplNonCustomType { typ: self.fmt_type(meta_type) }, span);
                }

                TypeId::VOID
            }

            Expr::ImplSelf => {
                if let Some(id) = self.curr_impl_self {
                    self.typed_ast.resolved_impl_self_type.insert(check_expr, id);
                    TypeId::TYPE
                } else {
                    self.error(ErrType::TyperSelfOutsideImplBlock, span)
                }
            }

            Expr::TypeMemberAccess { .. } => todo!(),

            Expr::ParserError => TypeId::ERROR,
            Expr::Void


            // --- CONST STUFF ---
            // already handled in the hoisting phase
            | Expr::Const { .. } | Expr::CustomType { .. } => TypeId::VOID,
            
            Expr::EnumDefinition { variants } => {
                // the actual enum-type is defined in the hoisting_pass
                for variant in variants {
                    if let Some(tup) = variant.attached_tuple {
                        self.check_expression(tup, is_never, &ctx);
                    }
                }
                TypeId::TYPE
            }

            // should be desugared stuff
            Expr::While { .. } | Expr::For { .. } | Expr::FnDefinition { .. } => unreachable!("should be desugared already... {expr_expr:?}"),
        };
        
        if inferred_type == TypeId::NEVER { *is_never = true; }
        if *is_never { inferred_type = TypeId::NEVER; }

        self.typed_ast.expr_types[check_expr.0 as usize] = inferred_type;

        inferred_type = self.handle_deref_mode(old_ctx.deref_mode, check_expr);

        // unify it with the expected type (if something was expected)
        if let Some(expected) = old_ctx.expected_type {
            inferred_type = self.auto_deref_to_expected_type(check_expr, expected);
            self.unify_types(expected, inferred_type, span);
        }

        inferred_type
    }




    fn check_literal(&mut self, val: &AstValue) -> TypeId {
        match val {
            AstValue::Num(_) => self.type_arena.add_type(Type::Num),
            AstValue::Str(_) => self.type_arena.add_type(Type::Str),
            AstValue::Bool(_) => self.type_arena.add_type(Type::Bool),
        }
    }


    fn check_infix(&mut self, op: TokenKind, op_span: Span, left: TypeId, right: TypeId) -> TypeId {
        // for now left and right have to be the same type, so just unify them.
        self.unify_types(left, right, op_span);

        let left_type = self.prune_type_once_infer_err(left, op_span);

        if let Type::CustomType(_, id) = left_type {
            // if its a customType, try to unify the inner types for now
            // TODO: make better
            let infixed_type = self.check_infix(op, op_span, id, id);

            // if its the same type as id, it returns the custom type again
            // e.g. `N{ 2 } * N{ 2 } == N{ 4 }`
            return if self.are_types_equivalent_ignore_notes(infixed_type, id) {
                left
            } else {
                infixed_type
            }
        }

        match op {            
            TokenKind::EqualEqual | TokenKind::Greater | TokenKind::Less /*| TokenType::GreaterEqual | TokenType::LessEqual */ => {
                TypeId::BOOL
            }

            // num operators
            TokenKind::Op(AssignOp::Plus | AssignOp::Minus | AssignOp::Star | AssignOp::Slash | AssignOp::Percent) => {
                self.unify_types(TypeId::NUM, left, op_span);
                TypeId::NUM
            }

            // boolean operators
            TokenKind::And | TokenKind::Or => {
                self.unify_types(TypeId::BOOL, left, op_span);
                TypeId::BOOL
            }
            _ => unreachable!("Unsupported infix operator: {:?}", op)
        }
    }



    fn hoisting_pass(&mut self, exprs: &[ExprId], allow_non_const: bool) {
        // 1. it collects all const exprs and defines them as Unresolved.
        for &expr in exprs {
            match self.ast.get_expr(expr) {
                Expr::Const { pattern, value } => {
                    self.mark_vars_in_pattern_as_const(*pattern, TypeVarConstVal::NotYetTypechecked { value: *value, bind_to: PatternOrVarId::Pattern(*pattern) });
                }
                Expr::CustomType { name, value } => {
                    let expr_span = self.ast.get_expr_span(expr);

                    let guess_var_id = TypeVarId(self.typed_ast.vars.len().try_into().unwrap());
                    let var_id = self.define_variable(
                        name, TypeId::TYPE, false, true, expr_span,
                        TypeVarConstVal::NotYetTypechecked { value: *value, bind_to: PatternOrVarId::CustomTypeVarId(guess_var_id) }
                    );
                    assert_eq!(guess_var_id, var_id);
                }
                _ => if !allow_non_const {
                    self.error(ErrType::TyperRuntimeValuesArentAllowedInImplBlocks, self.ast.get_expr_span(expr));
                }
            }
        }

        // 2. typechecks all collected Unresolved vars.
        // if one depends on another, typecheck another first, then back to first.
        // if there is a self-dependency-cycle, throw an error
        while let Some((value, bind_to)) = self.var_scopes.last().unwrap()
        .scope.values()
        .find_map(|&var_id| if let TypeVarConstVal::NotYetTypechecked { value, bind_to }
        = self.typed_ast.get_var(var_id).const_val { Some((value, bind_to)) } else { None }) {
            // found a var that was not checked yet!
            self.check_evaluate_and_bind_const(value, bind_to);
        }
    }


    pub(super) fn check_assign_pattern_and_value(
        &mut self, pattern: PatternId, value: Option<ExprId>,
        is_never: &mut bool, can_bind_vars: bool, can_fail: bool, fully_deref_value: bool,
        const_update: Option<TypeVarConstVal>
    ) -> TypeId {
        let mut pattern_type = self.get_pattern_type(pattern);

        if let Some(val) = value {
            let mut ctx = CheckExprCtx::default().maybe_expect(pattern_type);
            if const_update.is_some() {
                ctx = ctx.is_const();
            }
            if fully_deref_value {
                ctx.deref_mode = AutoDerefMode::Fully;
            }
            let check_typ = self.check_expression(val, is_never, &ctx);
            pattern_type = Some(check_typ);
        }

        let mut vars_defined = Vec::new();
        let (pattern_type, covered) = self.check_match_pattern(
            pattern, pattern_type, value.is_some(), const_update,
            &mut CheckPatternVars::Collect(&mut vars_defined)
        );

        let span = self.ast.get_pattern_span(pattern);
        let remaining = PatternSpace::covered_to_missing_cases(&covered, &self.typed_ast.enum_defs);
        if !can_fail && !remaining.is_empty() {
            self.error(ErrType::TyperFailableAssignPattern {
                remaining: PatternSpace::display_patterns(&remaining, &self.typed_ast.enum_defs)
            }, span);
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
                    } else if type_annotation_required {
                        self.error(ErrType::TyperRequiresTypeAnnotation, self.ast.get_pattern_span(param))
                    } else {
                        self.new_infer_type()
                    }
                }
            )
            .collect();

        let return_type = if let Some(ret) = closure.return_type {
            self.check_annotation_meta_type_id(ret, true)
        } else if type_annotation_required {
            TypeId::VOID
        } else {
            self.new_infer_type()
        };
        self.type_arena.add_type(Type::Fn { param_types, return_type })
    }


    pub(super) fn check_fn_expression(&mut self, closure: &AstClosure, fn_type: TypeId) -> TypeId {
        // check the fn_type in a new scope to also define the function parameters
        self.enter_scope();

        let Type::Fn { param_types, return_type } = self.type_arena.get_type(fn_type) else {
            unreachable!("this function should only be called with valid function types")
        };
        assert_eq!(param_types.len(), closure.params.len());

        for (&param_pattern, param_type) in closure.params.iter().zip(param_types) {
            let (_, covered) = self.check_match_pattern(
                param_pattern, Some(param_type), true, None, &mut CheckPatternVars::Collect(&mut Vec::new())
            );
            let remaining = PatternSpace::covered_to_missing_cases(&covered, &self.typed_ast.enum_defs);
            if !remaining.is_empty() {
                let param_span = self.ast.get_pattern_span(param_pattern);
                self.error(ErrType::TyperPatternDoesntCoverAllCases {
                    remaining: PatternSpace::display_patterns(&remaining, &self.typed_ast.enum_defs)
                }, param_span);
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



    /// this might be the most complicated function in this compiler...
    fn check_member_access(
        &mut self, left_type_id: TypeId, member_expr: ExprId,
        came_from_ref: Option<(bool, Option<TypeVarId>)>, came_from_meta: bool,
        expected_type: Option<TypeId>
    ) -> TypeId {
        let Expr::MemberAccess { left, member } = self.ast.get_expr(member_expr) else {
            unreachable!("this function is only called with MemberAccess exprs.")
        };
        let left_span = self.ast.get_expr_span(*left);
        let left_type = self.prune_type_once_infer_err(left_type_id, left_span);

        // check the member for special types first:
        match &left_type {
            Type::Tup(elems) if !came_from_meta => {
                // for tuples check if `member` matches a label
                // e.g. `(1, 2).0` or `(x: 1, y: 2).y`
                let member_index = elems.iter().position(|elem| elem.label == *member);

                if let Some(index) = member_index {
                    // if we came from a ref keep that ref
                    return if let Some((mutable, borrows_var)) = came_from_ref {
                        self.typed_ast.resolved_member_access.insert(member_expr, ResolvedMemberAccess::TupleRefIndex { index });
                        self.type_arena.add_type(Type::Borrow { inner: elems[index].typ, mutable, borrows_var })
                    } else {
                        self.typed_ast.resolved_member_access.insert(member_expr, ResolvedMemberAccess::TupleIndex { index });
                        elems[index].typ
                    }
                }
            }

            Type::MetaType if !came_from_meta => {
                // for metatypes it needs to first compile the type to get the actual `TypeId`
                // then it checks for impls with the correct member
                // e.g. `i32.MAX`
                if came_from_ref.is_some() {
                    // if this is a ref to a metatype, deref it first.
                    assert!(self.deref_if_pointer(*left));
                }
                let meta_type_id = self.check_annotation_meta_type_id(*left, false);

                // if its an enum, check if the member is a variant
                let expr_expects_type = expected_type == Some(TypeId::TYPE);
                if let Some(
                    (_, variant_index, attached_type, refined_type)
                ) = self.check_enum_variant(member, Some(meta_type_id), None, expr_expects_type) {
                    // found a variant!

                    return if expr_expects_type {
                        // if it expects a type, return a hard specialized type.
                        // e.g. `let x: Option.Some`, this can never be assigned any other enum variants, only .Some{ ... }
                        let constant = RuntimeValue::Type(refined_type);
                        self.typed_ast.resolved_member_access.insert(member_expr, ResolvedMemberAccess::Member { constant });

                        self.type_arena.add_type(Type::Borrow { inner: TypeId::TYPE, mutable: false, borrows_var: None })
                    }
                    else {
                        // otherwise, its a runtime enum value
                        if self.prune_type_once(attached_type) == Type::Void {
                            // if the variant has no data, return a soft specialized type, e.g. `Option.None`
                            // soft allows it to unify with `.Some` later
                            self.typed_ast.resolved_member_access.insert(member_expr, ResolvedMemberAccess::EnumWithNoData { i: variant_index });
                            refined_type
                        } else {
                            // has data => error. `Option.Some` without anything extra is invalid
                            self.error(ErrType::TyperVariantRequiresData { variant: member.clone() }, left_span)
                        }
                    }
                }

                return self.check_member_access(meta_type_id, member_expr, None, true, expected_type);
            }

            // ignore specs here
            Type::FlowType(spec_type, _) => {
                return self.check_member_access(*spec_type, member_expr, came_from_ref, came_from_meta, expected_type)
            }

            Type::Error => return TypeId::ERROR,
            _ => {}
        }


        // if there wasn't any type specific things that matched check the types impls
        if let Some((constant, typ)) = self.check_type_impl_const(&left_type, member) {
            let resolved = if came_from_meta {
                // `i32.square(5)`
                ResolvedMemberAccess::Member { constant }
            } else {
                // `5.square()`
                ResolvedMemberAccess::MemberWithSelfSugar { constant, self_sugar_expr: *left }
            };
            self.typed_ast.resolved_member_access.insert(member_expr, resolved);
            return typ
        }


        // if it still didn't find anything, recursively check inner types (if any)
        match left_type {
            Type::CustomType(custom_id, custom_inner) => {
                // recursively check member_access for CustomTypes inner
                let resolved_type = self.check_member_access(custom_inner, member_expr, came_from_ref, came_from_meta, expected_type);

                // and if needed wrap it back in the custom_id
                return self.member_access_magic_wrapping(resolved_type, custom_id, custom_inner, member_expr)
            }

            Type::Borrow { inner, mutable, borrows_var } => {
                // if we already came from a borrow before this borrow, just deref it
                // this function does not support (double ref).access
                if came_from_ref.is_some() {
                    assert!(self.deref_if_pointer(*left));
                }
                
                // now continue checking the member_access using the inner borrow type.
                // also keep track of the borrows_var
                // e.g. `tup.y` tup is a ref, so the final thing will also be a ref.
                return self.check_member_access(inner, member_expr, Some((mutable, borrows_var)), came_from_meta, expected_type)
            }

            _ => {}
        }


        self.error(ErrType::TyperTypeDoesntHaveMember { typ: self.fmt_type(left_type_id), member: member.clone() }, left_span)
    }


    fn check_type_impl_const(&mut self, typ: &Type, member: &str) -> Option<(RuntimeValue, TypeId)> {
        println!("checking impl for {typ:?}");

        if let Type::CustomType(custom_id, _) = typ
        && let Some(&member) = self.custom_types[custom_id.0 as usize].impls.scope.get(member) {
            // found a member!
            
            match &self.typed_ast.get_var(member).const_val {
                TypeVarConstVal::Evaluated(constant) => Some((constant.clone(), self.make_var_id_ref(member, false))),
                other => unreachable!("should be an evaluated const... {other:?}")
            }
        } else {
            None
        }
    }


    /// Magic wrapping for impl consts
    /// if the `resolved_type` evaluates to the inner type, it gets lifted back to the `CustomType`.
    /// e.g.
    /// `const N = num`
    /// `N{ 4 }.square()`  // should result in `N`, even though `num.square()` itself returns num
    fn member_access_magic_wrapping(&mut self, resolved_type: TypeId, custom_id: CustomTypeId, custom_inner: TypeId, member_expr: ExprId) -> TypeId {

        // member access yielded the inner type
        // e.g. `const TRUE = true` turns into `CustomBool`
        if self.are_types_equivalent_ignore_notes(custom_inner, resolved_type) {
            self.type_arena.add_type(Type::CustomType(custom_id, resolved_type))
        }
        
        // member access yielded a function
        // e.g. `fn(bool) -> bool` becomes `fn(CustomBool) -> CustomBool`
        else if let Type::Fn { param_types, return_type } = self.prune_type_once(resolved_type) {
            let new_return = if self.are_types_equivalent_ignore_notes(custom_inner, return_type) {
                self.type_arena.add_type(Type::CustomType(custom_id, return_type))
            } else {
                return_type
            };

            let new_params = param_types.into_iter().map(|p| {
                if self.are_types_equivalent_ignore_notes(custom_inner, p) {
                    self.type_arena.add_type(Type::CustomType(custom_id, p))
                } else {
                    p
                }
            }).collect();

            self.type_arena.add_type(Type::Fn { param_types: new_params, return_type: new_return })
        }
        
        // member access yielded a MetaType
        // e.g. `const X: type = Opt2.Some`
        // adjust the typed_ast note
        else {
            let Some(res) = self.typed_ast.resolved_member_access.get_mut(&member_expr) else {
                unreachable!("every member_access_expr should get a note here... {}", self.fmt_type(resolved_type))
            };
            if let ResolvedMemberAccess::Member { constant: RuntimeValue::Type(inner_val_type) } = *res
            && self.are_types_equivalent_ignore_notes(custom_inner, inner_val_type) {

                let new_constant = RuntimeValue::Type(self.type_arena.add_type(Type::CustomType(custom_id, inner_val_type)));
                self.typed_ast.resolved_member_access.insert(member_expr, ResolvedMemberAccess::Member { constant: new_constant });
            }

            resolved_type
        }
    }




    pub(super) fn check_enum_variant(
        &mut self, variant_name: &str, expected_type: Option<TypeId>, err_span: Option<Span>, refine_hard: bool
    ) -> Option<(EnumId, usize, TypeId, TypeId)> {
        // using `.Variant` syntax requires that the Typechecker knows the Enumtype.
        let Some(expected) = expected_type else {
            if let Some(span) = err_span {
                self.error(ErrType::TyperRequiresTypeAnnotation, span);
            }
            return None
        };

        if let Some(enum_id) = self.get_wrapped_enum_id(expected) {
            let enum_def = &self.typed_ast.enum_defs[enum_id.0 as usize];

            // try to find the correct .Variant
            let Some(variant_index) = enum_def.variants.iter().position(|(name, _)| **name == *variant_name) else {
                if let Some(span) = err_span {
                    self.error(ErrType::TyperEnumDoesntHaveVariant { 
                        enum_: self.fmt_type(expected), 
                        variant: variant_name.into()
                    }, span);
                }
                return None;
            };
            
            let attached_type = enum_def.variants[variant_index].1;

            // refine/specialize it and wrap it back in the custom types
            // so this function will turn `CustomType(1, Enum<0>)` -> `Refined<CustomType(1, Enum<0>), Soft<0>>`
            let refine_type = if refine_hard {
                self.type_arena.add_type(Type::RefinedEnum(expected, EnumRefinement::ExactVariant(variant_index)))
            } else {
                let spec_id = self.add_type_refinement(FlowType::Enum(EnumFlowType::CurrVariant(variant_index)));
                self.type_arena.add_type(Type::FlowType(expected, spec_id))
            };

            Some((enum_id, variant_index, attached_type, refine_type))
        }
        else {
            if let Some(span) = err_span {
                self.error(ErrType::TyperExpectedTypeIsntAnEnum { typ: self.fmt_type(expected) }, span);
            }
            None
        }
    }


    pub fn peel_custom_types(&mut self, mut id: TypeId, err_span: Span) -> (TypeId, Vec<CustomTypeId>) {
        let mut wrappers = Vec::new();
        while let Type::CustomType(custom_id, inner) = self.prune_type_once_infer_err(id, err_span) {
            wrappers.push(custom_id);
            id = inner;
        }
        (id, wrappers)
    }

    pub fn wrap_back_in_custom_types(&mut self, mut core: TypeId, wrappers: &[CustomTypeId]) -> TypeId {
        for &custom_id in wrappers.iter().rev() {
            core = self.type_arena.add_type(Type::CustomType(custom_id, core));
        }
        core
    }

    #[must_use]
    pub fn get_wrapped_enum_id(&self, mut id: TypeId) -> Option<EnumId> {
        loop {
            match self.prune_type_once(id) {
                Type::CustomType(_, inner) | Type::FlowType(inner, _) => {
                    id = inner;
                }
                Type::Enum(enum_id) => return Some(enum_id),
                _ => return None
            }
        }
    }



    fn check_instantiation_payload(&mut self, instance_type: TypeId, check_expr: ExprId, data: ExprId, is_never: &mut bool, ctx: &CheckExprCtx) -> TypeId {
        let span = self.ast.get_expr_span(check_expr);

        match self.prune_type_once_infer_err(instance_type, span) {
            Type::Error => TypeId::ERROR,
            Type::Tup(_) => {
                // if it expects a tuple, e.g. `type Point = { num, num }; Point{ 1, 2 }`
                // then just typecheck normally. (`data` is already a tuple expr)
                self.typed_ast.resolved_type_instantian.insert(check_expr, ResolvedTypeInstantiation::Tuple);
                self.check_expression(data, is_never, &ctx.expect(instance_type))
            }
            _ => {
                // if it doesn't expect a tuple, it needs to extract the first element.
                // e.g. `type N = num; N{ 3 }`
                let Expr::Tuple { elems } = self.ast.get_expr(data) else {
                    unreachable!("this is always a tuple.")
                };
                if let [first] = elems.as_slice() && first.label == "0" {
                    self.typed_ast.resolved_type_instantian.insert(check_expr, ResolvedTypeInstantiation::NewType);
                    self.check_expression(first.expr, is_never, &ctx.expect(instance_type))
                }
                else {
                    self.error(ErrType::TyperNewTypesExpectOneUnlabeledExpr, span)
                }
            }
        }
    }
    



    /// this function allows auto-deref to autoclone types
    /// e.g. expr.typ: &&bool, expected: bool -> works, inserts 2 moves
    /// e.g. expr.typ: &&Vec, expected: &Vec -> works, inserts 1 move
    fn auto_deref_to_expected_type(&mut self, expr: ExprId, expected_type: TypeId) -> TypeId {
        let expr_type = self.typed_ast.get_expr_type(expr);
        let span = self.ast.get_expr_span(expr);
        
        if let Some((expr_p_count, is_auto_clone_after)) = self.count_initial_pointers(expr_type)
        && let Some((expected_p_count, _)) = self.count_initial_pointers(expected_type) {

            // if we have more pointers than the expected_type, AND everything that needs to be derefed is auto_clone
            if expr_p_count > expected_p_count && (expected_p_count != 0 || is_auto_clone_after) {
                for _ in 0..(expr_p_count - expected_p_count) {
                    let Type::Borrow { inner, .. } = self.prune_type_once_infer_err(expr_type, span) else {
                        unreachable!("i just checked the amount of pointers, so this is unreachable \
                            ({expr_p_count}, {expected_p_count}, {is_auto_clone_after}) ({}, {})", self.fmt_type(expr_type), self.fmt_type(expected_type))
                    };
                    self.insert_deref(expr, false, inner);
                }
            }
        }

        self.typed_ast.get_expr_type(expr)
    }

    fn count_initial_pointers(&self, typ: TypeId) -> Option<(usize, bool)> {
        let mut curr_typ = typ;
        let mut count = 0;

        while let Type::Borrow { inner, .. } = self.prune_type_once(curr_typ) {
            curr_typ = inner;
            count += 1;
        }

        self.is_auto_clone(curr_typ)
            .map(|final_auto_clone| (count, final_auto_clone))
    }



    pub fn handle_deref_mode(&mut self, deref_mode: AutoDerefMode, check_expr: ExprId) -> TypeId {
        match deref_mode {
            AutoDerefMode::None => {/* do nothing */}
            AutoDerefMode::Once => {
                self.deref_if_pointer(check_expr);
            }
            AutoDerefMode::Fully => {
                while self.deref_if_pointer(check_expr) { }
            }
            AutoDerefMode::LeaveOnePointer => {
                while let Some((2.., _)) = self.count_initial_pointers(self.typed_ast.get_expr_type(check_expr)) {
                    assert!(self.deref_if_pointer(check_expr));
                }
            }
        }
        self.typed_ast.get_expr_type(check_expr)
    }


    pub fn deref_if_pointer(&mut self, expr: ExprId) -> bool {
        let span = self.ast.get_expr_span(expr);
        let typ = self.typed_ast.get_expr_type(expr);

        match self.prune_type_once_infer_err(typ, span) {
            Type::Borrow { inner, mutable: _, borrows_var } => {
                let auto_clone = self.check_deref_memory_rules(inner, borrows_var, span);
                self.insert_deref(expr, !auto_clone, inner);
                true
            }
            _ => false
        }
    }

    fn check_deref_memory_rules(&mut self, inner: TypeId, borrows_var: Option<TypeVarId>, span: Span) -> bool {
        if let Some(auto_clone) = self.is_auto_clone(inner) {
            if auto_clone {
                if let Some(x) = borrows_var {
                    self.clone_variable(x, span);
                }
            } else {
                // if it isn't autoclone, it needs to move it
                if let Some(x) = borrows_var {
                    self.move_variable(x, span);
                }
                else {
                    self.error(ErrType::TyperCantDerefUnknownPointerType, span); 
                }
            }
            auto_clone
        } else {
            self.error(ErrType::TyperTypeMustBeKnownHere { typ: self.fmt_type(inner) }, span);
            true  // just return true here for less chained errors
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

    /// Some(true) - is autoclone
    /// Some(false) - isn't autoclone
    /// None - infer error
    fn is_auto_clone(&self, typ: TypeId) -> Option<bool> {
        match self.prune_type_once(typ) {
            Type::Num
            | Type::Bool
            | Type::Borrow { .. }
            | Type::Fn { .. }
            | Type::MetaType
            | Type::Void
            | Type::Never
            | Type::Error => Some(true),

            Type::Str
            | Type::TupArr(_, _)
            | Type::Tup(_)
            | Type::Enum(_) => Some(false),

            Type::CustomType(_, inner) | Type::FlowType(inner, _) | Type::RefinedEnum(inner, _) => {
                self.is_auto_clone(inner)
            }

            Type::Infer(_) => None
        }
    }
}