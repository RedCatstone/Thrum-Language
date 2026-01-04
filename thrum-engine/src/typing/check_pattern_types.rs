use std::{collections::HashMap, rc::Rc};

use crate::{
    ErrType, parsing::ast_structure::{MatchPattern, PlaceExpr, TupleMatchPattern, TupleType, TypeKind}, typing::{AssignablePatternType, TypeChecker, check_expressions::ExprContext}
};

impl<'a> TypeChecker<'a> {
    pub(super) fn check_binding_pattern(&mut self, pattern: &mut MatchPattern, define_pattern_vars: bool) -> AssignablePatternType {
        match pattern {
            MatchPattern::Literal(lit) => {
                AssignablePatternType { typ: self.check_literal(lit), has_place: false, can_fail: true, vars: Vec::new() }
            }

            MatchPattern::Binding { name, typ } => {
                if *typ == TypeKind::ParserUnknown {
                    // if no type is annotated create a new inference var
                    *typ = self.new_inference_type();
                }
                else if *typ == TypeKind::Never {
                    self.error(crate::ErrType::TyperPatternNeverType);
                }
                if define_pattern_vars { self.checked_define_variable(name.clone(), typ.clone()); }
                AssignablePatternType { typ: typ.clone(), has_place: false, can_fail: false, vars: vec![(name.clone(), typ.clone())] }
            }

            MatchPattern::Array(elements) => {
                let mut arr_types = Vec::new();
                let mut has_place = false;
                let mut vars = Vec::new();
                
                for element in elements {
                    let pattern_type = self.check_binding_pattern(element, define_pattern_vars);
                    arr_types.push(pattern_type.typ);
                    vars.extend(pattern_type.vars);
                    if pattern_type.has_place { has_place = true; }
                }
                let arr_type = self.unify_type_vec(&arr_types);
                AssignablePatternType { typ: TypeKind::Arr(Box::new(arr_type)), has_place, can_fail: true, vars }
            }

            MatchPattern::Tuple(elements) => {
                let mut tuple_types = Vec::new();
                let mut has_place = false;
                let mut can_fail = false;
                let mut vars = Vec::new();

                for TupleMatchPattern { label, pattern } in elements {
                    let pattern_type = self.check_binding_pattern(pattern, define_pattern_vars);
                    tuple_types.push(TupleType { label: label.clone(), typ: pattern_type.typ });
                    vars.extend(pattern_type.vars);
                    if pattern_type.has_place { has_place = true; }
                    if pattern_type.can_fail { can_fail = true; }
                }
                AssignablePatternType { typ: TypeKind::Tup(tuple_types), has_place, can_fail, vars }
            }

            MatchPattern::Wildcard => {
                AssignablePatternType { typ: self.new_inference_type(), has_place: false, can_fail: false, vars: Vec::new() }
            }

            MatchPattern::Or(patterns) => {
                let mut or_types = Vec::new();
                let mut has_place = false;
                let mut can_fail = false;
                let mut vars = HashMap::new();

                for (i, pattern) in patterns.iter_mut().enumerate() {
                    let pattern_type = self.check_binding_pattern(pattern, define_pattern_vars);

                    // make sure that all or patterns define the same variables.
                    if i == 0 { vars = pattern_type.vars.into_iter().collect(); }
                    else {
                        // check if both or-match-patterns define the exact same variables.
                        if pattern_type.vars.len() != vars.len() {
                            let mut vars_not_bound = Vec::new();
                            for var in vars.keys() {
                                if pattern_type.vars.iter().all(|(pv_str, _)| pv_str != var) {
                                    vars_not_bound.push(var.to_string());
                                }
                            }
                            if !vars_not_bound.is_empty() { self.error(ErrType::TyperOrPatternDoesntBindVars(vars_not_bound)); }
                        }
                        let mut vars_bound_too_much = Vec::new();
                        for (pattern_var_str, pattern_var_type) in pattern_type.vars {
                            match vars.get(&pattern_var_str) {
                                Some(x) => self.unify_types(x, &pattern_var_type),
                                None => vars_bound_too_much.push(pattern_var_str)
                            }
                        }
                        if !vars_bound_too_much.is_empty() { self.error(ErrType::TyperOrPatternBindsVarsTooMuch(vars_bound_too_much)); }
                    }
                    or_types.push(pattern_type.typ);
                    if pattern_type.has_place { has_place = true; }
                    if pattern_type.can_fail { can_fail = true; }
                }
                let typ = self.unify_type_vec(&or_types);
                AssignablePatternType { typ, has_place, can_fail, vars: vars.into_iter().collect() }
            }

            MatchPattern::Conditional { pattern, body } => {
                let mut pattern_type = self.check_binding_pattern(pattern, define_pattern_vars);
                self.check_expression(Rc::get_mut(body).unwrap(), &ExprContext::default());
                self.unify_types(&TypeKind::Bool, &body.typ);
                pattern_type.can_fail = true;
                pattern_type
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

            MatchPattern::Place(PlaceExpr::Identifier(name)) => {
                AssignablePatternType { typ: self.check_identifier(name).clone(), has_place: true, can_fail: false, vars: Vec::new() }
            }

            MatchPattern::Place(PlaceExpr::Deref(name)) => {
                let typ = self.check_identifier(name);
                let inner_type = self.new_inference_type();
                self.unify_types(&TypeKind::MutPointer(Box::new(inner_type.clone())), &typ);
                AssignablePatternType { typ: inner_type, has_place: true, can_fail: false, vars: Vec::new() }
            }

            MatchPattern::Place(PlaceExpr::Index { left, index }) => {
                self.check_expression(Rc::get_mut(left).unwrap(), &ExprContext::default());
                let arr_type = self.new_inference_type();
                self.unify_types(&TypeKind::Arr(Box::new(arr_type.clone())), &left.typ);
                self.check_expression(Rc::get_mut(index).unwrap(), &ExprContext::default());
                self.unify_types(&TypeKind::Num, &index.typ);
                AssignablePatternType { typ: arr_type, has_place: true, can_fail: true, vars: Vec::new() }
            }
        }
    }
}