use std::collections::{HashMap, hash_map::Entry};

use crate::{
    ErrType, parsing::ast_structure::{MatchPattern, MatchPatternInfo, PatternSpace, TupleType, TypeKind, Value}, typing::{Typechecker, VarID, check_expressions::ExprContext}
};


#[derive(Clone, Copy)]
pub enum CheckPatternMode {
    NoInit,
    Set,
}

pub struct ExpectVarsDefined {
    // every piece of an Or-pattern needs to define the exact same variables.
    // the HashMap has the expected vars from the first Or-pattern in there.
    expected_vars: HashMap<String, VarID>,
    
    // if a pattern binds a variable that is not in the hashmap, its name gets added to here for a combined error message.
    bound_too_many_vars: Vec<String>
}

impl Typechecker<'_> {
    pub(super) fn check_match_pattern(
        &mut self,
        pattern: &mut MatchPatternInfo,
        expected_type: &TypeKind,
        check_pattern_mode: CheckPatternMode,
        expect_vars_defined: &mut Option<&mut ExpectVarsDefined>,
    ) {
        let mut steal_pattern_vars_defined = |other: &mut MatchPatternInfo| {
            // steal the other patterns stuff.
            pattern.vars_defined.extend(std::mem::take(&mut other.vars_defined));
        };

        match pattern.typ {
            TypeKind::ParserUnknown => pattern.typ = expected_type.clone(),
            TypeKind::Never => { self.error(ErrType::TyperPatternNeverType, pattern.span); },
            _ => {}
        }
        self.unify_types(&pattern.typ, expected_type, pattern.span);


        match &mut pattern.pattern {
            MatchPattern::Literal(lit) => {
                let lit_type = self.check_literal(lit);
                self.unify_types(expected_type, &lit_type, pattern.span);
                match lit {
                    Value::Bool(bool) => pattern.covered_cases.push(PatternSpace::Bool { bool: *bool }),
                    // cannot cover all float or string cases
                    Value::Num(_)
                    | Value::Str(_) => {}
                    _ => unreachable!("other values can't be reached")
                }
            }

            MatchPattern::Binding { name, mutable, var_id } => {
                // if an Or-pattern expects vars to be defined,
                // then it needs to make sure that the exact same variables with the same VarIDs are defined.
                if let Some(ExpectVarsDefined { expected_vars, bound_too_many_vars }) = expect_vars_defined {
                    match expected_vars.entry(name.clone()) {
                        Entry::Occupied(occ) => {
                            let removed_occ = occ.remove();
                            *var_id = Some(removed_occ);
                            let occ_var = self.var_lookup.get(&removed_occ).unwrap();
                            self.unify_types(&occ_var.typ.clone(), &pattern.typ, pattern.span);
                        }
                        Entry::Vacant(_) => {
                            // the var wasn't in the other Or-pattern => error
                            bound_too_many_vars.push(name.clone());
                        }
                    }
                }
                else {
                    // if it isn't in an or-pattern, just define it as normal
                    let is_init = !matches!(check_pattern_mode, CheckPatternMode::NoInit);
                    let var = self.define_variable(name.clone(), *mutable, is_init, pattern.typ.clone(), pattern.span);
                    *var_id = Some(var.var_id);
                    pattern.vars_defined.push((var.name, var.var_id));
                }

                // bindings can't fail, so any case is covered here now.
                pattern.covered_cases.push(PatternSpace::All);
            }

            MatchPattern::Array(inner_patterns) => {
                let inner_arr_typ = self.new_inference_type();
                self.unify_types(&TypeKind::Arr(Box::new(inner_arr_typ.clone())), &pattern.typ, pattern.span);
                
                for p in inner_patterns {
                    self.check_match_pattern(p, &inner_arr_typ, check_pattern_mode, expect_vars_defined);
                    steal_pattern_vars_defined(p);
                }
                // TODO array missing case stuff (for now its just NotCovered, so it requires a Wildcard pattern afterwards)
            }

            MatchPattern::Tuple(inner_patterns) => {
                let tuple_types: Vec<TupleType> = inner_patterns.iter()
                    .map(|p| TupleType { label: p.label.clone(), typ: self.new_inference_type()})
                    .collect();
                self.unify_types(&TypeKind::Tup(tuple_types.clone()), &pattern.typ, pattern.span);

                let mut tuple_covered_cases = Vec::new();

                for (p, tuple_typ) in inner_patterns.iter_mut().zip(tuple_types) {
                    self.check_match_pattern(&mut p.pattern, &tuple_typ.typ, check_pattern_mode, expect_vars_defined);
                    steal_pattern_vars_defined(&mut p.pattern);

                    tuple_covered_cases.push(std::mem::take(&mut p.pattern.covered_cases));
                }

                pattern.covered_cases = self.tuple_missing_cases_cartesian_product(&tuple_covered_cases);
            }

            MatchPattern::Wildcard => {
                pattern.covered_cases.push(PatternSpace::All);
            }

            MatchPattern::Or(inner_patterns) => {
                let (first_pattern, other_patterns) = inner_patterns.split_first_mut()
                    .expect("Parser makes sure that there are always be at least 2 patterns here");

                // check the first pattern normal
                self.check_match_pattern(first_pattern, &pattern.typ, check_pattern_mode, expect_vars_defined);
                steal_pattern_vars_defined(first_pattern);
                
                let first_pattern_vars: HashMap<String, VarID> = first_pattern.vars_defined.iter().cloned().collect();
                
                for p in other_patterns {
                    let mut expected_vars = ExpectVarsDefined {
                        expected_vars: first_pattern_vars.clone(),
                        bound_too_many_vars: Vec::new()
                    };
                    self.check_match_pattern(p, &pattern.typ, check_pattern_mode,
                    &mut Some(&mut expected_vars));
                    // the vars were already taken from the first or_pattern, so just trash these
                    p.vars_defined = Vec::new();
                    steal_pattern_vars_defined(p);

                    let vars_not_bound: Vec<String> = expected_vars.expected_vars.into_keys().collect();
                    if !vars_not_bound.is_empty() {
                        self.error(ErrType::TyperOrPatternDoesntBindVars { vars: vars_not_bound }, p.span);
                    }
                    if !expected_vars.bound_too_many_vars.is_empty() {
                        self.error(ErrType::TyperOrPatternBindsVarsTooMuch { vars: expected_vars.bound_too_many_vars }, p.span);
                    }

                    pattern.covered_cases.extend(std::mem::take(&mut p.covered_cases));
                }
            }

            MatchPattern::Conditional { pattern: p, body } => {
                self.check_match_pattern(p, &pattern.typ, check_pattern_mode, expect_vars_defined);
                steal_pattern_vars_defined(p);
                self.check_expression(body, &ExprContext::default());
                self.unify_types(&TypeKind::Bool, &body.typ, p.span);
                pattern.typ = p.typ.clone();

                // missing cases defaults to NotCovered here, which is correct
            }

            MatchPattern::EnumVariant { .. } => {
                todo!()
                // if path.len() != 1 { return AssignablePatternType {
                //     typ: self.add_error("Multi-segment paths in match patterns are not yet supported.".to_string()), has_place: false, vars: Vec::new()
                // } }
                
                // let enum_name = &path[0];

                // let enum_definition = match self.env.lookup_type(enum_name) {
                //     Some(DefinedTypeKind::Enum { inner_types }) => inner_types,
                //     Some(_) => { return self.add_error(format!("Type '{}' is not an enum.", enum_name)); }
                //     None => { return self.add_error(format!("Enum type '{}' not found.", enum_name)); }
                // };
                // let expected_enum_type = TypeKind::Enum { name: enum_name.clone() };
                
                // if let Some(expected_variant_params) = enum_definition.get(name) {
                //     if inner_patterns.len() != expected_variant_params.len() {
                //         return self.add_error(format!(
                //             "Enum variant '{}::{}' expects {} arguments. Found {}.", enum_name, name, expected_variant_params.len(), inner_patterns.len()
                //         ));
                //     }

                //     // idk what this does, is for later
                //     for (pattern_arg, def_param) in inner_patterns.iter_mut().zip(expected_variant_params.iter()) {
                //         if let AssignablePattern::Binding { typ: expected_type, .. } = def_param {
                //             self.check_binding_pattern(pattern_arg, define_pattern_vars);
                //         }
                //         else { unreachable!() }
                //     }

                // }
                // else { return self.add_error(format!("Enum '{}' has no variant named '{}'.", enum_name, name)); }
                // expected_enum_type
            }

            MatchPattern::PlacePointer { expr } => {
                let ctx = ExprContext::default();
                
                self.check_expression(expr, &ctx);
                match self.prune(&expr.typ, Some(pattern.span)) {
                    TypeKind::Pointer { mutable, inner, borrows_var } => {
                        self.unify_types(&inner, &pattern.typ, pattern.span);
                        
                        self.update_variable(borrows_var, pattern.span);
                    }
                    x => {
                        self.type_mismatch(
                            TypeKind::Pointer { mutable: true, inner: Box::new(TypeKind::ParserUnknown), borrows_var: None },
                            x,
                            pattern.span
                        );
                    }
                }                
                // PlacePointer can never fail, so any case is covered here now.
                pattern.covered_cases.push(PatternSpace::All);
            }
        }
    }




    pub(super) fn get_match_pattern_type(&mut self, pattern: &MatchPatternInfo) -> TypeKind {
        if pattern.typ != TypeKind::ParserUnknown {
            return pattern.typ.clone()
        }
        self.error(ErrType::TyperRequiresTypeAnnotation, pattern.span)
        // match &pattern.pattern {
        //     MatchPattern::Binding { .. }
        //     | MatchPattern::Wildcard
        //     | MatchPattern::Or(_)
        //     | MatchPattern::Array(_) => self.error(ErrType::TyperRequiresTypeAnnotation, pattern.span),

        //     MatchPattern::EnumVariant { inner_patterns: patterns, .. } => 
        //     MatchPattern::Tuple(tup_patterns) => todo!(),
        //     MatchPattern::Conditional { pattern, .. } => todo!(),
        //     MatchPattern::Literal(value) => todo!(),
        //     MatchPattern::PlacePointer { expr } => todo!(),
        // }
    }




    pub(super) fn convert_to_missing_cases(&self, covered_cases: &[PatternSpace]) -> Vec<PatternSpace> {
        let mut missing_cases = vec![PatternSpace::All];

        for covered in covered_cases {
            let mut new_missing_cases = Vec::new();

            for missing in missing_cases {
                new_missing_cases.extend(missing.subtract(covered));
            }
            missing_cases = new_missing_cases;
        }
        missing_cases
    }

    pub(super) fn tuple_missing_cases_cartesian_product(&self, missing_cases: &[Vec<PatternSpace>]) -> Vec<PatternSpace> {
        // with [[a, b], [c, d]]
        // this should generate [a, c], [a, d], [b, c], [b, d]
        let mut result = Vec::with_capacity(missing_cases.iter().map(|x| x.len()).product());
        result.push(Vec::new());  // start with one empty tuple

        for cases in missing_cases {
            let mut new_result = Vec::new();
            for existing in result {
                for case in cases {
                    let mut new_inner = existing.clone();
                    new_inner.push(case.clone());
                    new_result.push(new_inner);
                }
            }
            result = new_result;
        }

        result.into_iter().map(|inners| PatternSpace::Tup { inners }).collect()
    }
}



impl PatternSpace {
    fn subtract(&self, now_covered: &Self) -> Vec<Self> {
        let mut new_missing_cases = Vec::new();

        match (self, now_covered) {
            (_, Self::All) => {
                // anything - All = nothing
                // even All - All = nothing
                // so just add nothing here
            }

            (Self::All, Self::Bool { bool: covered_bool }) => {
                new_missing_cases.push(Self::Bool { bool: !covered_bool });
            }
            (Self::Bool { bool: missing_bool }, Self::Bool { bool: covered_bool }) => {
                if missing_bool != covered_bool {
                    // missing the same case again
                    new_missing_cases.push(self.clone());
                }
                // else: fully covered
            }

            (Self::All, Self::Tup { inners: covered_inners }) => {
                let all_tup_vec = Self::Tup { inners: vec![Self::All; covered_inners.len()] };
                new_missing_cases.extend(all_tup_vec.subtract(now_covered));
            }
            (Self::Tup { inners: missing_inners }, Self::Tup { inners: covered_inners }) => {
                // the generalized logic for this with missing_inners: (M1, M2, ..., Mn) and covered_inners (C1, C2, ..., Cn)
                // 1. (M1 - C1, M2, M3, ...)
                // 2. (M1 ∩ C1, M2 - C2, M3, ...)
                // 3. (M1 ∩ C1, M2 ∩ C2, M3 - C3, ...)

                // with missing_inners: (All, All, All) and covered_inners: (false, false, false)
                // this would generate: (true, All, All), (false, true, All), (false, false, true)

                let mut curr_vec = missing_inners.clone();

                assert_eq!(missing_inners.len(), covered_inners.len());
                for (i, (mi, ci)) in missing_inners.iter().zip(covered_inners).enumerate() {
                    // Calculate Mi - Ci (this can return multiple things)
                    for subtract in mi.subtract(ci) {
                        let mut new_inners = curr_vec.clone();
                        new_inners[i] = subtract;
                        new_missing_cases.push(Self::Tup { inners: new_inners });
                    }

                    // Calculate Mi ∩ Ci and put it in the curr_vec for the other cases.
                    if let Some(intersect) = mi.intersect(ci) {
                        curr_vec[i] = intersect;
                    } else {
                        // if the intersection didn't match (e.g. false ∩ true) we stop
                        // e.g. with missing_inners: (All, true, All) and covered_inners: (false, false, false)
                        // this would generate: (true, true, All), (false, true, All), nothing here anymore
                        break;
                    }
                }
            }
            (a, b) => unreachable!("pattern subtraction is not defined for {a:?} - {b:?}")
        }

        new_missing_cases
    }


    fn intersect(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::All, x) | (x, Self::All) => Some(x.clone()),

            (Self::Bool { bool: a }, Self::Bool { bool: b }) => {
                if a == b { Some(Self::Bool { bool: *a }) } else { None }
            }

            (Self::Tup { inners: a }, Self::Tup { inners: b }) => {
                a.iter()
                    .zip(b)
                    .map(|(a, b)| a.intersect(b))
                    .collect::<Option<Vec<_>>>()
                    .map(|inners| Self::Tup { inners })
            }

            _ => unreachable!("cannot intersect {self:?} and {other:?}")
        }
    }
}