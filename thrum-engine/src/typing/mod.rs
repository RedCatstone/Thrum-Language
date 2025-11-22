use crate::{
    lexing::tokens::TokenType,
    nativelib::{ThrumModule, ThrumType, get_native_lib},
    parsing::{ast_structure::{AssignablePattern, DefinedTypeKind, EnumExpression, Expr, MatchArm, PlaceExpr, TypeKind, TypedExpr, Value}, desugar}, typing::type_environment::TypecheckEnvironment
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



pub struct TypeChecker {
    pub errors: Vec<TypeCheckError>,
    env: TypecheckEnvironment,
    library: ThrumModule,
    pub inference_id_lookup: HashMap<usize, TypeKind>,
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
            errors: Vec::new(),
            env,
            library,
            inference_id_lookup: HashMap::new(),
            next_inference_id: 0,
            current_function_return_type: None,
            current_break_types: Vec::new(),
        }
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


    pub fn check_program(&mut self, program: &mut Vec<TypedExpr>) {
        // PASS 1: run type unification/constraint solving logic.
        self.check_block(program);

        // PASS 2: clean up (remove all TypeKind::Infered(id))
        self.finalize_expressions(program);
    }



    fn check_expression(&mut self, expr: &mut TypedExpr) {
        let inferred_type = match &mut expr.expression {
            Expr::Literal(val) => self.check_literal(val),
            Expr::Identifier { name } => self.check_identifier(name, false),
            Expr::TemplateString(parts) => self.check_template_string(parts),
            Expr::Tuple(elements) => self.check_tuple(elements),
            Expr::Array(elements) => self.check_array(elements),
            Expr::Index { left, index } => self.check_index(left, index),

            Expr::Block(body) => self.check_block(body),
            Expr::Prefix { operator, right } => self.check_prefix(operator, right),
            Expr::Infix { left, operator, right } => {
                self.check_expression(left);
                self.check_expression(right);
                self.check_infix(&left.typ, operator, &right.typ)
            }
            Expr::If { condition, consequence, alternative } => self.check_if(condition, consequence, alternative),
            Expr::IfLet { pattern, value, consequence, alternative } => {
                self.check_if_let(pattern, value, consequence, alternative)
            }
            Expr::Match { match_value, arms: cases } => self.check_match(match_value, cases),
            Expr::While { condition, body } => self.check_while(condition, body),
            Expr::Loop { body } => self.check_loop(body),
            
            Expr::Assign { pattern, extra_operator, value, alternative } => {
                self.check_assign(pattern, extra_operator, value, alternative)
            }
            Expr::MutRef { expr } => {
                self.check_expression(expr);
                match &mut expr.expression {
                    Expr::Identifier { name } => {
                        self.lookup_variable_mut(name);
                        TypeKind::MutPointer(Box::new(expr.typ.clone()))
                    }
                    _ => self.error(format!("Cannot borrow non identifier as mut."))
                }
            }
            Expr::Deref { expr } => {
                self.check_expression(expr);
                let inner_typ = self.new_inference_type();
                self.unify_types(&TypeKind::MutPointer(Box::new(inner_typ.clone())), &expr.typ);
                inner_typ
            }

            Expr::FnDefinition { params, return_type, body, .. } => {
                self.check_fn_expression(params, return_type, body);
                TypeKind::Void
            }
            Expr::Closure { params, return_type: return_value, body } => {
                self.check_fn_expression(params, return_value, body)
            }
            Expr::Return(ret) => self.check_return(ret),
            Expr::Break { expr } => self.check_break(expr),
            Expr::Call { callee, arguments } => self.check_fn_call(callee, arguments),

            Expr::TypePath(segments) => self.check_path_expression(segments),
            Expr::MemberAccess { left, member } => self.check_member_acess_expression(left, member),
            Expr::EnumDefinition { name, enums } => self.check_enum_expression(name, enums),
            Expr::Void => { TypeKind::Void }

            Expr::ParserTempTypeAnnotation(_) => self.error(format!("Type annotations are not allowed here.")),
            Expr::ParserTempLetPattern(_) => self.error(format!("Let patterns are not allowed here.")),
        };

        expr.typ = self.prune(&inferred_type);
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

    fn check_binding_pattern(&mut self, pattern: &mut AssignablePattern, define_pattern_vars: bool) -> AssignablePatternType {
        match pattern {
            AssignablePattern::Literal(lit) => {
                AssignablePatternType { typ: self.check_literal(lit), has_place: false, can_fail: false, vars: Vec::new() }
            }

            AssignablePattern::Binding { name, typ } => {
                if *typ == TypeKind::ParserUnknown {
                    // if no type is annotated create a new inference var
                    *typ = self.new_inference_type();
                }
                else if *typ == TypeKind::Never {
                    self.error(format!("Type Never '!' is not allowed in binding patterns."));
                }
                if define_pattern_vars { self.checked_define_variable(name.clone(), typ.clone()); }
                AssignablePatternType { typ: typ.clone(), has_place: false, can_fail: false, vars: vec![(name.clone(), typ.clone())] }
            }

            AssignablePattern::Array(elements) => {
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

            AssignablePattern::Tuple(elements) => {
                let mut tuple_types = Vec::new();
                let mut has_place = false;
                let mut can_fail = false;
                let mut vars = Vec::new();

                for element in elements {
                    let pattern_type = self.check_binding_pattern(element, define_pattern_vars);
                    tuple_types.push(pattern_type.typ);
                    vars.extend(pattern_type.vars);
                    if pattern_type.has_place { has_place = true; }
                    if pattern_type.can_fail { can_fail = true; }
                }
                AssignablePatternType { typ: TypeKind::Tup(tuple_types), has_place, can_fail, vars }
            }

            AssignablePattern::Wildcard => {
                AssignablePatternType { typ: self.new_inference_type(), has_place: false, can_fail: false, vars: Vec::new() }
            }

            AssignablePattern::Or(patterns) => {
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

            AssignablePattern::Conditional { pattern, body } => {
                let mut typ = self.check_binding_pattern(pattern, define_pattern_vars);
                self.check_expression(Rc::get_mut(body).unwrap());
                self.unify_types(&TypeKind::Bool, &body.typ);
                typ.can_fail = true;
                typ
            }

            AssignablePattern::EnumVariant { path, name, inner_patterns } => {
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

            AssignablePattern::Place(PlaceExpr::Identifier(name)) => {
                AssignablePatternType { typ: self.check_identifier(name, true).clone(), has_place: true, can_fail: false, vars: Vec::new() }
            }

            AssignablePattern::Place(PlaceExpr::Deref(name)) => {
                let typ = self.check_identifier(name, true);
                let inner_type = self.new_inference_type();
                self.unify_types(&TypeKind::MutPointer(Box::new(inner_type.clone())), &typ);
                AssignablePatternType { typ: inner_type, has_place: true, can_fail: false, vars: Vec::new() }
            }

            AssignablePattern::Place(PlaceExpr::Index { left, index }) => {
                self.check_expression(Rc::get_mut(left).unwrap());
                let arr_type = self.new_inference_type();
                self.unify_types(&TypeKind::Arr(Box::new(arr_type.clone())), &left.typ);
                self.check_expression(Rc::get_mut(index).unwrap());
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
                if let Some(already_borrowed_by) = &mut scope_type.mut_borrowed_by {
                    // self.errors.push(TypeCheckError {
                    //     message: format!("Cannot borrow {name} as mutable because it is already borrowed as mutable by {already_borrowed_by}.")
                    // });
                }
                scope_type.mut_borrowed_by = Some(name.to_string());
                return;
            }
        }
    }



    fn check_block(&mut self, body: &mut Vec<TypedExpr>) -> TypeKind {
        self.env.enter_scope();

        // 1. define FnDefinitions
        for expr in body.iter_mut() {
            if let Expr::FnDefinition { name, params, return_type, .. } = &mut expr.expression {
                let fn_typ = self.get_fn_type(params, return_type, false);
                self.checked_define_variable(name.clone(), fn_typ);
            }
        }

        // normal pass
        let mut last_type = TypeKind::Void;
        for expr in body {
            self.check_expression(expr);
            if last_type != TypeKind::Never {
                last_type = expr.typ.clone();
            }
        }
        self.env.exit_scope();
        last_type
    }

    fn check_prefix(&mut self, operator: &TokenType, right: &mut TypedExpr) -> TypeKind{
        self.check_expression(right);
        match operator {
            TokenType::Exclamation => {
                self.unify_types(&TypeKind::Bool, &right.typ);
                right.typ.clone()
            }
            TokenType::Minus => {
                self.unify_types(&TypeKind::Num, &right.typ);
                right.typ.clone()
            }
            _ => { self.error(format!("Unsupported prefix operator: {:?}", operator)); TypeKind::TypeError }
        }
    }

    fn check_infix(&mut self, left_type: &TypeKind, operator: &TokenType, right_type: &TypeKind) -> TypeKind {
        match operator {
            // num/str operators
            TokenType::Plus | TokenType::Greater | TokenType::Less | TokenType::GreaterEqual | TokenType::LessEqual => {
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
            TokenType::EqualEqual | TokenType::NotEqual => {
                self.unify_types(left_type, right_type);
                let unified_type = self.prune(left_type);
                match unified_type {
                    TypeKind::Num | TypeKind::Str | TypeKind::Bool | TypeKind::Arr(_) | TypeKind::Tup(_) => TypeKind::Bool,
                    // let it propagate, it will be solved later.
                    TypeKind::Inference(_) => TypeKind::Bool,
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

    fn check_if(&mut self, condition: &mut TypedExpr, consequence: &mut TypedExpr, alternative: &mut Box<TypedExpr>) -> TypeKind {
        self.check_expression(condition);
        self.unify_types(&TypeKind::Bool, &condition.typ);
        
        self.check_expression(consequence);
        self.check_expression(alternative);
        
        self.unify_types(&consequence.typ, &alternative.typ);
        consequence.typ.clone()
    }

    fn check_if_let(&mut self, pattern: &mut AssignablePattern, value: &mut TypedExpr, consequence: &mut TypedExpr, alternative: &mut Box<TypedExpr>) -> TypeKind {
        self.env.enter_scope();
        let pattern_type = self.check_binding_pattern(pattern, true);
        self.check_expression(value);
        self.unify_types(&value.typ, &pattern_type.typ);
        if pattern_type.has_place { return self.error(format!("Place patterns are not allowed in if let expressions.")) }

        self.check_expression(consequence);
        self.env.exit_scope();

        self.check_expression(alternative);
        self.unify_types(&consequence.typ, &alternative.typ);
        consequence.typ.clone()
    }

    fn check_while(&mut self, condition: &mut TypedExpr, body: &mut TypedExpr) -> TypeKind {
        self.check_expression(condition);
        self.unify_types(&TypeKind::Bool, &condition.typ);

        self.current_break_types.push(("while".to_string(), TypeKind::Void));
        self.check_expression(body);
        self.current_break_types.pop().unwrap();
        TypeKind::Void
    }

    fn check_loop(&mut self, body: &mut TypedExpr) -> TypeKind {
        let loop_break_type = self.new_inference_type();
        self.current_break_types.push(("loop".to_string(), loop_break_type.clone()));
        self.check_expression(body);
        self.unify_types(&TypeKind::Void, &body.typ);
        self.current_break_types.pop().unwrap();
        loop_break_type
    }

    fn check_template_string(&mut self, parts: &mut Vec<TypedExpr>) -> TypeKind {
        for part in parts {
            self.check_expression(part);
        }
        TypeKind::Str
    }

    fn check_tuple(&mut self, elements: &mut Vec<TypedExpr>) -> TypeKind {
        let mut tuple_types = Vec::new();
        let mut is_never = false;

        for element in elements {
            self.check_expression(element);
            if self.prune(&element.typ) == TypeKind::Never {
                is_never = true;
            }
            tuple_types.push(element.typ.clone());
        }
        if is_never { TypeKind::Never }
        else { TypeKind::Tup(tuple_types) }
    }

    fn check_array(&mut self, elements: &mut Vec<TypedExpr>) -> TypeKind {
        let types: Vec<TypeKind> = elements.iter_mut().map(|element| { self.check_expression(element); element.typ.clone() }).collect();
        let arr_type = self.unify_type_vec(&types);
        if arr_type == TypeKind::Never { TypeKind::Never }
        else { TypeKind::Arr(Box::new(arr_type)) }
    }

    fn check_index(&mut self, left: &mut TypedExpr, index: &mut TypedExpr) -> TypeKind {
        self.check_expression(left);
        self.check_expression(index);
        self.unify_types(&TypeKind::Num, &index.typ);
        let element_type = self.new_inference_type();
        let expected_left_type = TypeKind::Arr(Box::new(element_type.clone()));
        self.unify_types(&left.typ, &expected_left_type);
        element_type
    }

    fn check_assign(&mut self, pattern: &mut AssignablePattern, extra_operator: &TokenType, value: &mut TypedExpr, alternative: &mut Option<Box<TypedExpr>>) -> TypeKind {
        self.check_expression(value);
        let pattern_type = self.check_binding_pattern(pattern, true);
        if *extra_operator == TokenType::Equal { self.unify_types(&pattern_type.typ, &value.typ); }
        else { self.check_infix(&pattern_type.typ, extra_operator, &value.typ); }
        
        if let Some(alt) = alternative {
            self.check_expression(alt);
            self.unify_types(&TypeKind::Never, &alt.typ);
        }

        TypeKind::Void
    }

    fn get_fn_type(&mut self, params: &mut Vec<AssignablePattern>, return_type: &mut TypeKind, define_params: bool) -> TypeKind {
        let mut param_types = Vec::new();
        for param_pattern in params.iter_mut() {
            let pattern_type = self.check_binding_pattern(param_pattern, define_params);

            if pattern_type.has_place { self.error(format!("Place patterns are not allowed in function parameters.")); }
            if pattern_type.can_fail { self.error(format!("Failable patterns are not allowed in function parameters.")); }

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


    fn check_fn_expression(&mut self, params: &mut Vec<AssignablePattern>, return_type: &mut TypeKind, body: &mut Rc<TypedExpr>) -> TypeKind {
        self.env.enter_scope();
        // check the fn_type in a new scope to also define the function parameters
        let fn_type = self.get_fn_type(params, return_type, true);

        // set the return context to this functions return type
        let previous_function_return_type = self.current_function_return_type.clone();
        self.current_function_return_type = Some(return_type.clone());

        let body_mut = Rc::get_mut(body).unwrap();
        self.check_expression(body_mut);
        self.unify_types(return_type, &body_mut.typ);

        // reset return context
        self.current_function_return_type = previous_function_return_type;

        self.env.exit_scope();

        fn_type
    }

    fn check_fn_call(&mut self, callee: &mut TypedExpr, arguments: &mut Vec<TypedExpr>) -> TypeKind {
        self.check_expression(callee);
        
        let mut arg_types = Vec::new();
        for arg in arguments {
            self.check_expression(arg);
            arg_types.push(arg.typ.clone());
        }

        match &mut callee.typ {
            TypeKind::Fn { param_types, return_type } => {
                if param_types.len() != arg_types.len() {
                    return self.error(format!("Expected {} arguments, found {}.", param_types.len(), arg_types.len()));
                }
                for (param_type, arg_type) in param_types.iter().zip(arg_types.iter()) {
                    self.unify_types(param_type, arg_type);
                }
                *return_type.clone()
            }
            TypeKind::Inference(id) => {
                // not the best code, but works for now
                let return_type = self.new_inference_type();
                let fn_type = TypeKind::Fn { param_types: arg_types, return_type: Box::new(return_type.clone()) };
                self.unify_types(&fn_type, &TypeKind::Inference(*id));
                return_type
            }
            _ => self.error(format!("Cannot call a non-function type '{}'", callee.typ))
        }
    }

    fn check_return(&mut self, return_expression: &mut TypedExpr) -> TypeKind {
        self.check_expression(return_expression);

        if let Some(x) = self.current_function_return_type.clone() {
            self.unify_types(&x, &return_expression.typ);
        }
        else { self.error(format!("'return' is only allowed inside functions.")); }

        TypeKind::Never
    }

    fn check_break(&mut self, expr: &mut TypedExpr) -> TypeKind {
        self.check_expression(expr);

        if let Some((label, typ)) = self.current_break_types.last().cloned() {
            self.unify_types(&typ, &expr.typ);
        }
        else { self.error(format!("'break' is only allowed inside loops.")); }
        TypeKind::Never
    }

    fn check_match(&mut self, match_value: &mut TypedExpr, arms: &mut Vec<MatchArm>) -> TypeKind {
        self.check_expression(match_value);

        let mut cases_to_cover = match self.prune(&match_value.typ) {
            TypeKind::Bool => Some(HashSet::from(["true".to_string(), "false".to_string()])),
            _ => None
        };
        
        let mut has_unfailable_arm = false;
        let mut arm_types = Vec::new();
        
        for arm in arms {
            self.env.enter_scope();
            match arm.pattern {
                AssignablePattern::Literal(Value::Bool(bool)) => {
                    cases_to_cover.as_mut().unwrap().remove(&bool.to_string());
                }
                _ => { }
            }
            let pattern_type = self.check_binding_pattern(&mut arm.pattern, true);
            self.unify_types(&match_value.typ, &pattern_type.typ);

            if !pattern_type.can_fail { has_unfailable_arm = true; }

            self.check_expression(&mut arm.body);
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
    }


    fn check_enum_expression(&mut self, name: &mut String, enums: &mut Vec<EnumExpression>) -> TypeKind {
        self.checked_define_type(name.clone(), ThrumType {
            typ: DefinedTypeKind::Enum {
                name: name.to_string(),
                inner_types: enums.into_iter()
                    .map(|x| (x.name.clone(), x.inner_types.clone()))
                    .collect()
            },
            values: HashMap::new()
        });
        TypeKind::Void
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
                    _ => return self.error(format!("type path had 2 or more remaining segments."))
                }
            }

            // else check for consts/functions (e.g. io::print)
            else if let Some(module_val) = curr_module.values.get(segment) {
                if i == segments.len() - 1 {
                    return module_val.typ.clone()
                }
                else {
                    return self.error(format!("value path too long."))
                }
            }

            return self.error(format!("segment {segment} could not be found..."));
        }

        self.error(format!("'{}' could not be found...", segments.join("::")))
    }


    fn check_member_acess_expression(&mut self, left: &mut TypedExpr, member: &mut String) -> TypeKind {
        todo!()
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

                    /* Do nothing to other nodes */
                    _ => { }
                }
                expr
            },

            |mut pattern| {
                match &mut pattern {
                    AssignablePattern::Binding { typ, .. } => {
                        self_cell.borrow_mut().finalize_type(typ);
                    }
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
            TypeKind::Tup(inners) => TypeKind::Tup(inners.iter_mut().map(|t| self.finalize_type(t)).collect()),
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