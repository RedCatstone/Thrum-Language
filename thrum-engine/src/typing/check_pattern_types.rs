use std::{collections::{HashMap, hash_map::Entry}, rc::Rc};

use crate::{
    ErrType, parsing::ast_structure::{MatchPattern, MatchPatternInfo, TupleMatchPattern, TupleType, TypeKind}, typing::{Typechecker, VarID, check_expressions::ExprContext}
};

impl<'a> Typechecker<'a> {
    pub(super) fn check_binding_pattern(
        &mut self,
        pattern: &mut MatchPatternInfo,

        // every piece of an Or-pattern needs to define the exact same variables.
        // the HashMap has the expected vars from the first Or-pattern in there.
        // if this pattern binds a variable that is not in the hashmap, its name gets added to the Vec<String> for a combined error message.
        expect_vars_defined: &mut Option<(&mut HashMap<String, VarID>, &mut Vec<String>)>,
        define_pattern_vars: bool
    ) {
        let mut merge_pattern = |other: &mut MatchPatternInfo| {
            if other.has_place { pattern.has_place = true }
            if other.can_fail { pattern.can_fail = true }
    
            pattern.vars_defined.extend(std::mem::take(&mut other.vars_defined));
        };


        match &mut pattern.pattern {
            MatchPattern::Literal(lit) => {
                pattern.typ = self.check_literal(lit);
                pattern.can_fail = true;
            }

            MatchPattern::Binding { name, mutable, typ, var_id } => {
                if *typ == TypeKind::ParserUnknown {
                    // if no type is annotated create a new inference var
                    *typ = self.new_inference_type();
                }
                else if *typ == TypeKind::Never {
                    self.error(crate::ErrType::TyperPatternNeverType, pattern.span);
                }
                if define_pattern_vars {
                    // if an Or-pattern expects vars to be defined,
                    // then it needs to make sure that the exact same variables with the same VarIDs are defined.
                    if let Some((expect_map, err_vec)) = expect_vars_defined {
                        match expect_map.entry(name.to_string()) {
                            Entry::Occupied(occ) => {
                                let removed_occ = occ.remove();
                                let occ_var = self.var_lookup.get(&removed_occ).unwrap();
                                *var_id = Some(occ_var.var_id);
                                self.unify_types(&occ_var.typ.clone(), typ, pattern.span);
                            }
                            Entry::Vacant(_) => {
                                err_vec.push(name.clone());
                            }
                        }
                    }
                    else {
                        // if it isn't in an or-pattern, just define it normally
                        let var = self.define_variable(name.clone(), *mutable, typ.clone(), pattern.span);
                        *var_id = Some(var.var_id);
                        pattern.vars_defined.push((var.name, var.var_id));
                    }
                }
                pattern.typ = typ.clone();
            }

            MatchPattern::Array(inner_patterns) => {
                let mut arr_types = Vec::new();
                
                for p in inner_patterns {
                    self.check_binding_pattern(p, expect_vars_defined, define_pattern_vars);
                    merge_pattern(p);
                    arr_types.push(p.typ.clone());
                }
                let arr_type = self.unify_type_vec(&arr_types, pattern.span);
                pattern.typ = TypeKind::Arr(Box::new(arr_type));
            }

            MatchPattern::Tuple(inner_patterns) => {
                let mut tuple_types = Vec::new();

                for TupleMatchPattern { label, pattern: p } in inner_patterns {
                    self.check_binding_pattern(p, expect_vars_defined, define_pattern_vars);
                    merge_pattern(p);
                    tuple_types.push(TupleType { label: label.clone(), typ: p.typ.clone() });
                }
                pattern.typ = TypeKind::Tup(tuple_types);
            }

            MatchPattern::Wildcard => {
                pattern.typ = self.new_inference_type();
            }

            MatchPattern::Or(inner_patterns) => {
                let mut or_types = Vec::new();

                let Some((first_pattern, other_patterns)) = inner_patterns.split_first_mut() else {
                    unreachable!("Parser makes sure that this is impossible.")
                };

                // check the first pattern normal
                self.check_binding_pattern(first_pattern, expect_vars_defined, define_pattern_vars);
                let first_pattern_vars: HashMap<String, VarID> = std::mem::take(&mut first_pattern.vars_defined).into_iter().collect();
                merge_pattern(first_pattern);
                or_types.push(first_pattern.typ.clone());

                
                for p in other_patterns {
                    let mut vars_bound_too_much = Vec::new();

                    let mut first_pattern_vars_clone = first_pattern_vars.clone();
                    self.check_binding_pattern(p, &mut Some((&mut first_pattern_vars_clone, &mut vars_bound_too_much)), define_pattern_vars);
                    merge_pattern(p);
                    or_types.push(p.typ.clone());

                    let vars_not_bound: Vec<String> = first_pattern_vars_clone.into_keys().collect();
                    if !vars_not_bound.is_empty() {
                        self.error(ErrType::TyperOrPatternDoesntBindVars(vars_not_bound), p.span);
                    }
                    if !vars_bound_too_much.is_empty() {
                        self.error(ErrType::TyperOrPatternBindsVarsTooMuch(vars_bound_too_much), p.span);
                    }
                }

                pattern.typ = self.unify_type_vec(&or_types, pattern.span);
            }

            MatchPattern::Conditional { pattern: p, body } => {
                self.check_binding_pattern(p, expect_vars_defined, define_pattern_vars);
                merge_pattern(p);
                self.check_expression(Rc::get_mut(body).unwrap(), &ExprContext::default());
                self.unify_types(&TypeKind::Bool, &body.typ, p.span);
                pattern.can_fail = true;
                pattern.typ = p.typ.clone();
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

            MatchPattern::PlaceIdentifier { name, var_id } => {
                pattern.typ = self.use_variable(name, false, pattern.span, var_id);
                pattern.has_place = true;
            }

            MatchPattern::PlaceDeref { name, var_id } => {
                let typ = self.use_variable(name, false, pattern.span, var_id);
                pattern.typ = self.new_inference_type();
                self.unify_types(&TypeKind::MutPointer(Box::new(pattern.typ.clone())), &typ, pattern.span);
                pattern.has_place = true;
            }

            MatchPattern::PlaceIndex { left, index } => {
                self.check_expression(Rc::get_mut(left).unwrap(), &ExprContext::default());
                pattern.typ = self.new_inference_type();
                self.unify_types(&TypeKind::Arr(Box::new(pattern.typ.clone())), &left.typ, pattern.span);
                self.check_expression(Rc::get_mut(index).unwrap(), &ExprContext::default());
                self.unify_types(&TypeKind::Num, &index.typ, pattern.span);
                pattern.has_place = true;
                pattern.can_fail = true;
            }
        }
    }
}