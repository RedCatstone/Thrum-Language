use crate::{
    lexing::tokens::TokenType, nativelib::{ThrumModule, ThrumType, get_native_lib}, parsing::{ast_structure::{DefinedTypeKind, Expr, MatchPattern, PlaceExpr, TupleElement, TupleMatchPattern, TupleType, TypeKind, TypedExpr, Value}, desugar}, pretty_printing::join_slice_to_string, typing::type_environment::TypecheckEnvironment
};
use std::{cell::RefCell, collections::{HashMap, HashSet}, fmt, rc::Rc};


mod type_environment;
mod inference;


// example:
// let x = []
// let y = x
// let z: str = x[1]
//
// x -> Inference(0) - (0 -> arr<inference<1>>)
// y -> Inference(2) - (2 -> arr<inference<1>>)
// z -> Inference(3) - (3 -> str) and figures the type of inference<1> out. now all types are fully known.



pub struct TypeCheckError {
    pub message: String
}
impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}



struct AssignablePatternType {
    typ: TypeKind,
    has_place: bool,
    can_fail: bool,
    vars: Vec<(String, TypeKind)>,
}

#[derive(Default, Clone)]
struct ExprContext {
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


#[derive(Default)]
pub struct TypeChecker {
    errors: Vec<TypeCheckError>,
    env: TypecheckEnvironment,
    library: ThrumModule,
    inference_id_lookup: HashMap<usize, TypeKind>,
    next_inference_id: usize,

    current_function_return_type: Option<TypeKind>,
    current_break_types: Vec<(String, TypeKind)>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let library = get_native_lib();
        let mut env = TypecheckEnvironment::new();
        Self::load_prelude_into_env(&library, &mut env);

        TypeChecker {
            library,
            env,
            ..Default::default()
        }
    }

    pub fn typecheck_program(program: &mut [TypedExpr]) -> Vec<TypeCheckError> {
        let mut type_checker = Self::new();
        
        // PASS 1: run type unification/constraint solving logic.
        type_checker.check_block(program, &ExprContext::default());

        // PASS 2: clean up (remove all TypeKind::Infered(id))
        type_checker.finalize_expressions(program);

        
        println!("inference map: {:?}", type_checker.inference_id_lookup);
        type_checker.errors
    }


    fn load_prelude_into_env(module: &ThrumModule, env: &mut TypecheckEnvironment) {
        for (name, value) in &module.values {
            if value.is_prelude {
                env.define_variable(name.clone(), value.typ.clone());
            }
        }
        // Recursion
        for sub_module in module.sub_modules.values() {
            Self::load_prelude_into_env(sub_module, env);
        }
    }

    fn error(&mut self, message: String) -> TypeKind {
        self.errors.push(TypeCheckError { message });
        TypeKind::TypeError
    }

    fn type_mismatch(&mut self, expected: &TypeKind, found: &TypeKind) {
        self.error(format!("Type mismatch. Expected {}, found {}.", expected, found));
    }


    fn check_expression(&mut self, expr: &mut TypedExpr, old_ctx: &ExprContext) {
        let mut ctx = ExprContext::default();
        let mut is_never = false;

        let inferred_type = match &mut expr.expression {
            Expr::Literal(val) => self.check_literal(val),

            Expr::Identifier { name } => self.check_identifier(name, false),

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
                    _ => { self.error(format!("Unsupported prefix operator: {:?}", operator)); TypeKind::TypeError }
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

            Expr::If { condition, consequence, alternative } => {
                self.check_expression(condition, &ctx.expect(TypeKind::Bool).allow_conditional_bindings());
    
                self.check_expression(consequence, &ctx);
                self.check_expression(alternative, &ctx.expect(consequence.typ.clone()));

                consequence.typ.clone()
            },

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
                    self.error(format!("Match expression does not cover all cases. Remaining cases: {:?}", cases_to_cover.unwrap()));
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
                    self.errors.push(TypeCheckError { message: "Failable pattern in let-expression. Use 'if case ...' or 'ensure case ...' instead.".to_string() });
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
                    self.error("Binding case-expressions are not allowed here.".to_string());
                }

                TypeKind::Bool
            },

            Expr::MutRef { expr } => {
                self.check_expression(expr, &ctx);
                match &mut expr.expression {
                    Expr::Identifier { name } => {
                        self.lookup_variable_mut(name);
                        TypeKind::MutPointer(Box::new(expr.typ.clone()))
                    }
                    _ => self.error("Cannot borrow non identifier as mut.".to_string())
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
                    .unwrap_or_else(|| self.error("'return' is only allowed inside functions.".to_string()));

                self.check_expression(ret, &ctx.expect(curr_return_type));

                TypeKind::Never
            },

            Expr::Break { expr, label } => {
                let curr_break_type = if self.current_break_types.is_empty() {
                    self.error("'break' is only allowed inside loops.".to_string())
                }
                else if let Some(break_label) = label {
                    // break with a label -> find the closest loop with that label
                    match self.current_break_types
                        .iter().rev()
                        .find(|(x_label, _typ)| x_label == break_label) {
                            Some((_label, typ)) => typ.clone(),
                            None => self.error(format!(
                                "could not find a loop to break to, with the label #{break_label} (available labels: {})",
                                join_slice_to_string(&self.current_break_types.iter().map(|x| "#".to_string() + &x.0).collect::<Vec<_>>(), ", ")
                            ))
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
                            self.error(format!(
                                "could not find a loop to break to, with the label #{continue_label} (available labels: {})",
                                join_slice_to_string(&self.current_break_types.iter().map(|x| "#".to_string() + &x.0).collect::<Vec<_>>(), ", ")
                            ));
                        }
                TypeKind::Never
            }

            Expr::Call { callee, arguments } => {
                self.check_expression(callee, &ctx);
    
                let mut param_types = Vec::new();
                for arg in arguments {
                    self.check_expression(arg, &ctx);
                    param_types.push(arg.typ.clone());
                }

                match &mut callee.typ {
                    TypeKind::Fn { param_types, return_type } => {
                        if param_types.len() != param_types.len() {
                            self.error(format!("Expected {} arguments, found {}.", param_types.len(), param_types.len()))
                        }
                        else {
                            for (param_type, arg_type) in param_types.iter().zip(param_types.iter()) {
                                self.unify_types(param_type, arg_type);
                            }
                            *return_type.clone()
                        }
                    }
                    TypeKind::Inference(id) => {
                        // TODO not the best code, but works for now
                        let return_type = self.new_inference_type();
                        let fn_type = TypeKind::Fn { param_types, return_type: Box::new(return_type.clone()) };
                        self.unify_types(&fn_type, &TypeKind::Inference(*id));
                        return_type
                    }
                    _ => self.error(format!("Cannot call a non-function type '{}'", callee.typ))
                }
            },

            
            Expr::MemberAccess { left, member, resolved_index } => {
                self.check_expression(left, &ctx);
                if let TypeKind::Tup(elements) = self.prune(&left.typ) {
                    let member_index = elements.iter().position(|elem| elem.label == *member);
                    *resolved_index = member_index;

                    match member_index {
                        Some(i) => elements[i].typ.clone(),
                        None => self.error(format!("member .{member} does not exist on tuple: {}", TypeKind::Tup(elements)))
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

            Expr::ParserTempTypeAnnotation(_) => self.error("Type annotations are not allowed here.".to_string()),
            Expr::While { .. } | Expr::Ensure { .. } => unreachable!("should be desugared already..."),


        };

        if inferred_type.is_never() { is_never = true }

        // special case just for ensure expressions
        if !is_never && (expr.typ.is_never()) {
            self.type_mismatch(&TypeKind::Never, &inferred_type);
        }

        expr.typ = if is_never { TypeKind::Never }
            else { self.prune(&inferred_type) };
        
        if let Some(expected) = &old_ctx.expected_type {
            self.unify_types(expected, &expr.typ);
        }
    }




    fn check_literal(&mut self, val: &mut Value) -> TypeKind {
        match val {
            Value::Num(_) => TypeKind::Num,
            Value::Str(_) => TypeKind::Str,
            Value::Bool(_) => TypeKind::Bool,
            _ => unreachable!() // other values are not used in the parser
        }
    }

    fn checked_define_variable(&mut self, name: String, typ: TypeKind) {
        if self.env.define_variable(name.clone(), typ) {
            self.error(format!("Name '{name}' is already defined in this scope."));
        }
    }
    fn checked_define_type(&mut self, name: String, typ: ThrumType) {
        if self.env.define_type(name.clone(), typ) {
            self.error(format!("Name '{name}' is already defined in this scope."));
        }
    }

    fn check_binding_pattern(&mut self, pattern: &mut MatchPattern, define_pattern_vars: bool) -> AssignablePatternType {
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
                    self.error("Type Never '!' is not allowed in binding patterns.".to_string());
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
                        if pattern_type.vars.len() != vars.len() {
                            self.error(format!("Pattern expects {} variables defined, found {}.", vars.len(), pattern_type.vars.len()));
                        }
                        for (pattern_var_str, pattern_var_type) in pattern_type.vars {
                            match vars.get(&pattern_var_str) {
                                Some(x) => { self.unify_types(x, &pattern_var_type); }
                                None => { self.error(format!("All or-assign-patterns must define the same variables. Found {}", pattern_var_str)); }
                            }
                        }
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
                AssignablePatternType { typ: self.check_identifier(name, true).clone(), has_place: true, can_fail: false, vars: Vec::new() }
            }

            MatchPattern::Place(PlaceExpr::Deref(name)) => {
                let typ = self.check_identifier(name, true);
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






    fn check_identifier(&mut self, name: &str, mutable: bool) -> TypeKind {
        let typ = match self.env.lookup_variable(name) {
            Some(x) => x,
            None => return self.error(format!("Undefined identifier: '{}'", name))
        };
        if mutable { self.lookup_variable_mut(name); }

        typ
    }


    pub fn lookup_variable_mut(&mut self, name: &str) {
        for scope in self.env.scopes.iter_mut().rev() {
            if let Some(scope_type) = scope.vars.get_mut(name) {
                if let Some(_already_borrowed_by) = &mut scope_type.mut_borrowed_by {
                    // self.errors.push(TypeCheckError {
                    //     message: format!("Cannot borrow {name} as mutable because it is already borrowed as mutable by {already_borrowed_by}.")
                    // });
                }
                scope_type.mut_borrowed_by = Some(name.to_string());
                return;
            }
        }
    }



    fn check_block(&mut self, body: &mut [TypedExpr], ctx: &ExprContext) -> TypeKind {
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
                    _ => self.error(format!("Cannot apply operator '{}' to type {}.", operator, unified_type))
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
                    _ => self.error(format!("Cannot compare types {}.", unified_type))
                }
            }

            // boolean operators
            TokenType::Ampersand | TokenType::Pipe => {
                self.unify_types(&TypeKind::Bool, left_type);
                self.unify_types(&TypeKind::Bool, right_type);
                TypeKind::Bool
            }
            _ => { self.error(format!("Unsupported infix operator: {:?}", operator)) }
        }
    }


    fn get_fn_type(&mut self, params: &mut [MatchPattern], return_type: &mut TypeKind, define_params: bool) -> TypeKind {
        let mut param_types = Vec::new();
        for param_pattern in params.iter_mut() {
            let pattern_type = self.check_binding_pattern(param_pattern, define_params);

            if pattern_type.has_place { self.error("Place patterns are not allowed in function parameters.".to_string()); }
            if pattern_type.can_fail { self.error("Failable patterns are not allowed in function parameters.".to_string()); }

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


    fn check_fn_expression(&mut self, params: &mut [MatchPattern], return_type: &mut TypeKind, body: &mut Rc<TypedExpr>, ctx: &ExprContext) -> TypeKind {
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
                    _ => return self.error("type path had 2 or more remaining segments.".to_string())
                }
            }

            // else check for consts/functions (e.g. io::print)
            else if let Some(module_val) = curr_module.values.get(segment) {
                if i == segments.len() - 1 {
                    return module_val.typ.clone()
                }
                else {
                    return self.error("value path too long.".to_string())
                }
            }

            return self.error(format!("segment {segment} could not be found..."));
        }

        self.error(format!("'{}' could not be found...", segments.join("::")))
    }











    fn finalize_expressions(&mut self, expressions: &mut [TypedExpr]) {
        // wrap self in a RefCell to allow multiple "borrows" that are checked at runtime.
        let self_cell = RefCell::new(self);

        desugar::loop_over_every_ast_node(
            expressions,

            |mut expr| {
                let mut self_borrow = self_cell.borrow_mut();
                self_borrow.finalize_type(&mut expr.typ);
                match &mut expr.expression {
                    Expr::FnDefinition { return_type, .. }
                    | Expr::Closure { return_type, .. } => {
                        self_borrow.finalize_type(return_type);
                    }

                    // Do nothing to other nodes
                    _ => { }
                }
                expr
            },

            |mut pattern| {
                match &mut pattern {
                    // finalize binding pattern types
                    MatchPattern::Binding { typ, .. } => {
                        self_cell.borrow_mut().finalize_type(typ);
                    }
                    // Do nothing to other patterns
                    _ => {}
                }
                pattern
            }
        );
    }

    fn finalize_type(&mut self, typ: &mut TypeKind) -> TypeKind {
        let mut pruned = self.prune(typ);
        let final_typ = match &mut pruned {
            TypeKind::Inference(id) => {
                self.inference_id_lookup.insert(*id, TypeKind::TypeError);
                self.error(format!("Cannot infer type {}.", pruned))
            }
            TypeKind::Arr(inner) => TypeKind::Arr(Box::new(self.finalize_type(inner))),
            TypeKind::Tup(elements) => TypeKind::Tup(
                elements
                .iter_mut()
                .map(|TupleType { label, typ }| TupleType { label: label.clone(), typ: self.finalize_type(typ) })
                .collect()
            ),
            TypeKind::Fn { param_types, return_type } => TypeKind::Fn {
                param_types: param_types.iter_mut().map(|t| self.finalize_type(t)).collect(),
                return_type: Box::new(self.finalize_type(return_type)),
            },
            _ => pruned,
        };
        *typ = final_typ;
        typ.clone()
    }
}