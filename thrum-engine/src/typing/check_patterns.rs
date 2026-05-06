use std::collections::HashMap;
use crate::{
    ErrType, parsing::ast::{AstTuplePattern, AstValue, Expr, ExprId, Pattern, PatternId}, typing::{Type, TypeChecker, TypeId, TypeTuple, TypeVarId, check_expressions::CheckExprCtx, exhaustiveness::PatternSpace, type_vars::TypeVarConstVal}, vm_compiling::RuntimeValue
};


// the 'ast lifetime lives as long as the TypeChecker itself
// and 'short is just short
pub enum CheckPatternVars<'ast, 'short> {
    Collect(&'short mut Vec<(&'ast str, TypeVarId)>),
    // every piece of an Or-pattern needs to define the exact same TypevarIds.
    Expect {
        // this has the expected vars from the first Or-pattern in it.
        vars: &'short mut HashMap<&'ast str, TypeVarId>,
        // if a pattern binds a variable that is not in the hashmap, its name gets added to here for a combined error message.
        bound_too_many: &'short mut Vec<&'ast str>
    }
}


impl<'ast> TypeChecker<'ast> {
    pub(super) fn check_match_pattern(
        &mut self,
        pattern: PatternId,
        expected_type: Option<TypeId>,
        has_value: bool,
        const_update: Option<TypeVarConstVal>,
        vars_defined: &mut CheckPatternVars<'ast, '_>
    ) -> (TypeId, Vec<PatternSpace>) {
        let span = self.ast.get_pattern_span(pattern);
        let pattern_pattern = self.ast.get_pattern(pattern);

        let mut covered_cases = Vec::new();

        let inferred_type = match pattern_pattern {
            Pattern::Binding { name, mutable } => {
                // bindings can't fail, so any case is covered here now.
                covered_cases.push(PatternSpace::All);

                match vars_defined {
                    // if an Or-pattern expects vars to be defined,
                    // then it needs to make sure that the exact same variables with the same VarIDs are defined.
                    
                    CheckPatternVars::Expect { vars, bound_too_many } => {
                        // Expect means that it already checked the first pattern of the OR-chain and its now expecting vars
                        if let Some(var_id) = vars.remove(&**name) {
                            self.typed_ast.resolved_pattern_var.insert(pattern, var_id);
                            self.typed_ast.vars[var_id.0 as usize].typ
                        } else {
                            bound_too_many.push(name);
                            TypeId::ERROR
                        }
                    }
                    CheckPatternVars::Collect(vars) => {
                        // we are parsing the first OR-pattern (or no OR-pattern)
                        let typ = expected_type.unwrap_or_else(|| self.new_infer_type());

                        let var_id = if let Some(const_val) = const_update {
                            // if we are checking a const pattern, e.g. `const x = 5`
                            // it already knows about `x` and just needs to update it
                            let existing_id = self.typed_ast.resolved_pattern_var[&pattern];
                            let var = self.typed_ast.get_var_mut(existing_id);
                            var.typ = typ;
                            var.const_val = const_val;
                            existing_id
                        } else {
                            self.define_variable(name, typ, *mutable, has_value, span, TypeVarConstVal::No)
                        };

                        self.typed_ast.resolved_pattern_var.insert(pattern, var_id);
                        vars.push((name, var_id));
                        typ
                    }
                }
            }

            Pattern::Tuple(elems) => {
                let mut tuple_types = Vec::new();
                let mut tuple_covered_cases = Vec::new();
                let mut mismatch = false;

                // we need to split the expected type up, and pass that
                let expected_elems = expected_type.and_then(|exp| {
                    match self.prune_type_once(exp, None) {
                        Type::Tup(fields) => Some(fields),
                        _ => None
                    }
                });

                for AstTuplePattern { label, pattern: p } in elems {
                    let elem_expected_type = expected_elems.as_ref()
                        .map(|elems| elems.iter()
                            .find(|x| &x.label == label)
                            .map_or_else(
                                || { mismatch = true; TypeId::ERROR },
                                |x| x.typ
                            )
                        );

                    let (typ, covered) = self.check_match_pattern(
                        *p, elem_expected_type, has_value, const_update.clone(), vars_defined
                    );
                    tuple_types.push(TypeTuple { label: label.clone(), typ });
                    tuple_covered_cases.push(covered);
                }

                covered_cases = PatternSpace::tuple_cartesian_product(&tuple_covered_cases);
                let typ = Type::Tup(tuple_types);

                if mismatch {
                    let expected = self.prune_type_once(expected_type.unwrap(), None);
                    self.type_mismatch(expected, typ, span)
                } else {
                    self.type_arena.add_type(typ)
                }
            }

            Pattern::Wildcard => {
                covered_cases.push(PatternSpace::All);
                self.new_infer_type()
            }

            Pattern::Not(pat) => {
                let mut inner_vars = Vec::new();
                let (typ, covered) = self.check_match_pattern(
                    *pat, expected_type, has_value, const_update,
                    &mut CheckPatternVars::Collect(&mut inner_vars)
                );

                if !inner_vars.is_empty() {
                    // it doesn't make sense for a not-pattern to bind vars.
                    // even something double-negated like `... is let !(!x, 5)`
                    // means either bind x OR 2nd is not 5, which doesn't make sense.
                    // and `... is let !!x` can just be simplified to `... is let x`
                    self.error(ErrType::TyperNotPatternCantBindVars, span);
                }

                // invert the covered cases
                covered_cases.extend(PatternSpace::covered_to_missing_cases(&covered, &self.typed_ast.enum_defs));

                typ
            }

            Pattern::Or(inner_patterns) => {
                let (&first_pattern, other_patterns) = inner_patterns.split_first()
                    .expect("Parser makes sure that there are always at least 2 patterns here");

                let mut or_vars_defined = Vec::new();
                // check the first pattern normal
                let (first_type, covered) = self.check_match_pattern(
                    first_pattern, expected_type, has_value, const_update.clone(), &mut CheckPatternVars::Collect(&mut or_vars_defined)
                );
                covered_cases.extend(covered);
                
                let first_pattern_vars: HashMap<&str, TypeVarId> = or_vars_defined.into_iter().collect();
                
                for &p in other_patterns {
                    let mut expected_vars = first_pattern_vars.clone();
                    let mut bound_too_many = Vec::new();
                    let (typ, covered) = self.check_match_pattern(
                        p, expected_type, has_value, const_update.clone(),
                        &mut CheckPatternVars::Expect { vars: &mut expected_vars, bound_too_many: &mut bound_too_many }
                    );
                    covered_cases.extend(covered);
                    self.unify_types(first_type, typ, span);

                    if !expected_vars.is_empty() {
                        self.error(ErrType::TyperOrPatternDoesntBindVars { vars:
                            expected_vars.into_keys().map(str::to_string).collect()
                        }, span);
                    }
                    if !bound_too_many.is_empty() {
                        self.error(ErrType::TyperOrPatternBindsVarsTooMuch { vars:
                            bound_too_many.into_iter().map(str::to_string).collect()
                        }, span);
                    }
                }
                first_type
            }

            Pattern::Conditional { pattern: p, cond } => {
                let (typ, _) = self.check_match_pattern(*p, expected_type, has_value, const_update, vars_defined);
                self.check_expression(*cond, &mut false, &CheckExprCtx::default().expect(TypeId::BOOL));

                typ
                // missing cases defaults to NotCovered here, which is correct
            }

            Pattern::TypeDestructor { typ, data } => {
                // basically copy-pasted from check_expressions.rs `Expr::TypeInstantiation`
                let meta_id = self.check_annotation_meta_type_id(*typ, true);
                match self.prune_type_once(meta_id, Some(span)) {
                    Type::CustomType(inner_new_type) => {

                        match self.prune_type_once(inner_new_type, Some(span)) {
                            Type::Error => TypeId::ERROR,
                            Type::Tup(_) => {
                                // if it expects a tuple, e.g. `type Point = { num, num }`
                                // then just typecheck normally. (`data` is already a tuple expr)
                                let (typ, covered) = self.check_match_pattern(
                                    *data, Some(inner_new_type), has_value, const_update, vars_defined
                                );
                                covered_cases = covered;
                                typ
                            }
                            _ => {
                                // if it doesn't expect a tuple, it needs to extract the first element.
                                let Pattern::Tuple(elems) = self.ast.get_pattern(*data) else {
                                    unreachable!("this is always a tuple.")
                                };
                                if let [first] = elems.as_slice() && first.label == "0" {
                                    self.typed_ast.resolved_type_destruction_not_a_tuple.insert(pattern);
                                    let (typ, covered) = self.check_match_pattern(
                                        first.pattern, Some(inner_new_type), has_value, const_update, vars_defined
                                    );
                                    covered_cases = covered;
                                    typ
                                }
                                else {
                                    self.error(ErrType::TyperNewTypesExpectOneUnlabeledExpr, span)
                                }
                            }
                        };

                        // `N{ 2 }` returns the type `N`
                        meta_id
                    }
                    Type::Error => TypeId::ERROR,
                    t => self.error(ErrType::TyperMustBeCustomtypeType { typ: t }, self.ast.get_expr_span(*typ))
                }
            }

            Pattern::EnumVariant { name, attached_tuple } => {
                // using `.Variant` syntax requires that the Typechecker knows the Enumtype.
                if let Some((enum_id, variant_index, attached_type)) = self.check_enum_variant(name, expected_type, span) {
                    if let Some(tup) = attached_tuple {
                        let (_, covered) = self.check_match_pattern(
                            *tup, Some(attached_type), has_value, const_update, vars_defined
                        );
                        for covered in covered {
                            covered_cases.push(PatternSpace::EnumVariant {
                                enum_id,
                                variant_index,
                                attached_tuple: Box::new(covered)
                            });
                        }
                    } else {
                        // if the variant had no data, then the defined variant shouldn't have data either!
                        self.unify_types(TypeId::VOID, attached_type, span);
                        covered_cases.push(PatternSpace::EnumVariant {
                            enum_id,
                            variant_index,
                            attached_tuple: Box::new(PatternSpace::All)
                        });
                    }
                    self.typed_ast.resolved_enum_variant_pattern.insert(pattern, (enum_id, variant_index));

                    expected_type.unwrap()
                } else {
                    TypeId::ERROR
                }
            }

            Pattern::CompareExpr(expr) => {                
                let expr_type = self.check_expression(*expr, &mut false, &CheckExprCtx::default());

                // if there the compare expr is a simple literal, or a const value
                // then add that to covered_cases
                match self.ast.get_expr(*expr) {
                    Expr::Literal { val: AstValue::Bool(b) } => covered_cases.push(PatternSpace::Bool(*b)),
                    Expr::IdentifierRef { .. } => {
                        if let Some(var_id) = self.typed_ast.resolved_expr_var.get(expr) {
                            let var = self.typed_ast.get_var(*var_id);
                            if let TypeVarConstVal::Evaluated(val) = &var.const_val {
                                #[allow(clippy::collapsible_match, clippy::single_match)]
                                match val {
                                    RuntimeValue::Bool(b) => covered_cases.push(PatternSpace::Bool(*b)),
                                    _ => {}
                                }
                            }
                        }
                    }
                    _ => {}
                }

                expr_type
            }

            Pattern::PlacePointer(expr) => {                
                let expr_type = self.check_expression(*expr, &mut false, &CheckExprCtx::default().auto_borrow_mut());

                // PlacePointer can never fail, so any case is covered here now.
                covered_cases.push(PatternSpace::All);

                let pruned_expr = self.prune_type_once(expr_type, Some(span));
                match pruned_expr {
                    Type::Borrow { inner, mutable: true, borrows_var } => {
                        if let Some(x) = borrows_var {
                            self.update_variable(x, span);
                        }
                        inner
                    }
                    Type::Error => TypeId::ERROR,
                    _ => self.type_mismatch(
                        Type::Borrow { inner: TypeId::ERROR, mutable: true, borrows_var: None }, pruned_expr, span
                    )
                }
            }

            // we are actually gonna ignore this type annotation.
            // in let bindings: e.g. `let x: u32 = 5`
            // the type is first checked
            // then the expr with that type
            // then the full pattern with that type, so no need here
            Pattern::Typed { pattern, typ: _ } => {
                let (pattern_typ, covered) = self.check_match_pattern(
                    *pattern, expected_type, has_value, const_update, vars_defined
                );
                covered_cases.extend(covered);
                // self.unify_types(typ, other, span);
                pattern_typ
            }
        };

        if let Some(expected) = expected_type {
            self.unify_types(expected, inferred_type, span);
        }

        (inferred_type, covered_cases)
    }





    pub(super) fn check_annotation_meta_type_id(&mut self, expr: ExprId, needs_typechecking: bool) -> TypeId {
        // typecheck first
        if needs_typechecking {
            self.check_expression(expr, &mut false, &CheckExprCtx::default().expect(TypeId::TYPE).is_const());
        }

        // then evaluate
        self.evaluate_expr(expr).map_or(
            TypeId::ERROR,
            |val| self.extract_meta_type_from_runtime_val(val, self.typed_ast.expr_types[expr.0 as usize])
        )
    }

    pub(super) fn extract_meta_type_from_runtime_val(&mut self, val: RuntimeValue, expected_type: TypeId) -> TypeId {
        match val {
            RuntimeValue::Type(id) => id,
            RuntimeValue::Tup(elems) => {
                let typ = self.prune_type_once(expected_type, None);
                let Type::Tup(expected_tup) = typ else {
                    unreachable!("type mismatch at runtime!? {typ}")
                };

                let meta_elems = elems.into_iter()
                    .zip(expected_tup)
                    .map(|(val_elem, type_elem)| {
                        TypeTuple {
                            label: type_elem.label,
                            typ: self.extract_meta_type_from_runtime_val(val_elem, type_elem.typ)
                        }
                    }).collect();

                self.type_arena.add_type(Type::Tup(meta_elems))
            }
            _ => unreachable!("not a meta type?! {val} \nexpected: {}", self.type_arena.get_type(expected_type))
        }
    }


    pub(super) fn get_pattern_type(&mut self, pattern: PatternId) -> Option<TypeId> {
        if let Pattern::Typed { pattern: _, typ } = self.ast.get_pattern(pattern) {
            Some(self.check_annotation_meta_type_id(*typ, true))
        } else {
            None
        }
        // match &pattern.pattern {
        //     Pattern::Binding { .. }
        //     | Pattern::Wildcard
        //     | Pattern::Or(_)
        //     | Pattern::Array(_) => self.error(ErrType::TyperRequiresTypeAnnotation, pattern.span),

        //     Pattern::EnumVariant { inner_patterns: patterns, .. } => 
        //     Pattern::Tuple(tup_patterns) => todo!(),
        //     Pattern::Conditional { pattern, .. } => todo!(),
        //     Pattern::Literal(value) => todo!(),
        //     Pattern::PlacePointer { expr } => todo!(),
        // }
    }


    pub(super) fn mark_vars_in_pattern_as_const(&mut self, pattern: PatternId, const_val: TypeVarConstVal) {
        let span = self.ast.get_pattern_span(pattern);
        match self.ast.get_pattern(pattern) {
            Pattern::Binding { name, mutable } => {
                if let Some(var_id) = self.typed_ast.resolved_pattern_var.get(&pattern) {
                    // pattern var already exists!
                    self.typed_ast.get_var_mut(*var_id).const_val = const_val;
                } else {
                    // pattern var doesn't exist so make one
                    let var_id = self.define_variable(name, TypeId::ERROR, false, true, span, const_val);
                    self.typed_ast.resolved_pattern_var.insert(pattern, var_id);

                    if *mutable {
                        self.error(ErrType::TyperConstCantBeMutable, span);
                    }
                }
            },
            Pattern::Or(patterns) => {
                // all vars in an or-pattern must be the same, so only marking the first one is fine
                self.mark_vars_in_pattern_as_const(patterns[0], const_val);
            }
            Pattern::Tuple(ast_tuple_patterns) => {
                // if we have a const_val, then we need to destructure the const tuple to each pattern.
                if let TypeVarConstVal::Evaluated(RuntimeValue::Tup(elems)) = const_val {
                    assert_eq!(elems.len(), ast_tuple_patterns.len());

                    for (p, elem) in ast_tuple_patterns.iter().zip(elems) {
                        self.mark_vars_in_pattern_as_const(p.pattern, TypeVarConstVal::Evaluated(elem));
                    }
                }
                else {
                    for p in ast_tuple_patterns {
                        self.mark_vars_in_pattern_as_const(p.pattern, const_val.clone());
                    }
                }
            }
            Pattern::EnumVariant { name: _, attached_tuple } => {
                if let Some(tup) = attached_tuple {
                    self.mark_vars_in_pattern_as_const(*tup, const_val);
                }
            }
            Pattern::Conditional { pattern, cond: _ }
            | Pattern::Typed { pattern, typ: _ }
            | Pattern::TypeDestructor { typ: _, data: pattern }
            | Pattern::Not(pattern) => self.mark_vars_in_pattern_as_const(*pattern, const_val),
            
            Pattern::Wildcard | Pattern::CompareExpr(_) | Pattern::PlacePointer(_) => { /* no vars */ },
        }
    }
}