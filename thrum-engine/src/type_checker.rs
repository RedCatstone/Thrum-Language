use crate::{ast_structure::{BindingPattern, DefinedTypeKind, EnumExpression, Expr, MatchArm, MatchPattern, TypeKind, TypedExpr, Value}, nativelib::get_native_lib, pretty_printing::slice_to_string, tokens::TokenType};
use std::{collections::{HashMap, HashSet}, fmt, rc::Rc};



// example:
// let x = []
// let y = x
// let z: str = x[1]
//
// x gets assigned arr<Inference(x)<Unbound>>
// y gets assigned arr<Inference(x)<Unbound>>
// z gets assigned arr<Inference(x)<Bound<str>>> and figures the type of x out. now InferenceVar(x) gets changed to str everywhere.



#[derive(Default)]
struct TypeCheckScope {
    vars: HashMap<String, TypeKind>,
    types: HashMap<String, DefinedTypeKind>,
}

pub struct TypecheckEnvironment {
    scopes: Vec<TypeCheckScope>,
}
impl TypecheckEnvironment {
    pub fn new() -> Self { TypecheckEnvironment { scopes: vec![TypeCheckScope::default()] } }

    // e.g. for a block or function
    pub fn enter_scope(&mut self) { self.scopes.push(TypeCheckScope::default()); }
    pub fn exit_scope(&mut self) { self.scopes.pop(); }

    pub fn define_variable(&mut self, name: String, typ: TypeKind) -> bool {
        let already_exists = self.name_exists_already(&name);
        self.scopes.last_mut().unwrap().vars.insert(name, typ);
        already_exists
    }
    pub fn lookup_variable(&self, name: &str) -> Option<TypeKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.vars.get(name) {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn define_type(&mut self, name: String, typ: DefinedTypeKind) -> bool {
        let already_exists = self.name_exists_already(&name);
        self.scopes.last_mut().unwrap().types.insert(name, typ);
        already_exists
    }
    pub fn lookup_type(&mut self, name: &str) -> Option<DefinedTypeKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types.get(name) {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn name_exists_already(&mut self, name: &str) -> bool {
        self.lookup_variable(name).is_some() || self.lookup_type(name).is_some()
    }
}








pub struct TypeCheckError {
    pub message: String
}
impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}



pub struct TypeChecker {
    pub errors: Vec<TypeCheckError>,
    env: TypecheckEnvironment,
    inference_id_lookup: HashMap<usize, TypeKind>,
    next_inference_id: usize,

    current_function_return_type: Option<TypeKind>,
    can_break: bool,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = TypeChecker {
            errors: Vec::new(),
            env: TypecheckEnvironment::new(),
            inference_id_lookup: HashMap::new(),
            next_inference_id: 0,
            current_function_return_type: None,
            can_break: false,
        };

        let native_lib = get_native_lib();
        for (name, typ, _val) in native_lib {
            checker.env.define_variable(name, typ);
        }

        checker
    }

    fn add_error(&mut self, message: String) -> TypeKind {
        self.errors.push(TypeCheckError { message });
        TypeKind::TypeError
    }

    fn type_mismatch(&mut self, expected: &TypeKind, found: &TypeKind) {
        self.add_error(format!("Type mismatch. Expected {}, found {}.", expected, found));
    }

    fn new_inference_type(&mut self) -> TypeKind {
        self.next_inference_id += 1;
        TypeKind::Inference(self.next_inference_id)
    }

    fn prune(&mut self, typ: &TypeKind) -> TypeKind {
        if let TypeKind::Inference(id) = typ {
            if let Some(entry) = self.inference_id_lookup.get(&id) {
                let pruned = self.prune(&entry.clone());
                return pruned;
            }
        }
        typ.clone()
    }


    fn unify_types(&mut self, a: &TypeKind, b: &TypeKind) {
        let type_a = self.prune(a);
        let type_b = self.prune(b);
        
        match (&type_a, &type_b) {
            (TypeKind::Never, _) => { /* Do nothing */ }
            (_, TypeKind::Never) => { /* Do nothing */ }

            // if one is an inference variable, bind it to the other type.
            (TypeKind::Inference(id), _) => { self.inference_id_lookup.insert(*id, type_b.clone()); }
            (_, TypeKind::Inference(id)) => { self.inference_id_lookup.insert(*id, type_a.clone()); }

            (TypeKind::Arr(inner_a), TypeKind::Arr(inner_b)) => {
                self.unify_types(inner_a, inner_b);
            }
            (TypeKind::Tup(inners_a), TypeKind::Tup(inners_b)) => {
                if inners_a.len() == inners_b.len() {
                    for (ia, ib) in inners_a.iter().zip(inners_b.iter()) {
                        self.unify_types(ia, ib);
                    }
                }
                else { self.type_mismatch(&type_a, &type_b); }
            }

            _ if type_a != type_b => { self.type_mismatch(&type_a, &type_b); }

            // types match, do nothing
            _ => {}
        }
    }


    pub fn check_program(&mut self, program: &mut Vec<TypedExpr>) {
        // PASS 1: run type unification/constraint solving logic.
        self.check_block(program);

        // PASS 2: clean up (remove all TypeKind::Infered(id))
        self.finalize_ast(program);

        println!("inference map: {:?}", self.inference_id_lookup);
    }



    fn check_expression(&mut self, expr: &mut TypedExpr) {
        let inferred_type = match &mut expr.expression {
            Expr::Literal(val) => self.check_literal(val),
            Expr::Identifier(name) => self.check_identifier(name),
            Expr::TemplateString(parts) => self.check_template_string(parts),
            Expr::Tuple(elements) => self.check_tuple(elements),
            Expr::Array(elements) => self.check_array(elements),
            Expr::Index { left, index } => self.check_index(left, index),

            Expr::Let { pattern, value } => self.check_let(pattern, value),
            Expr::Block(body) => self.check_block(body),
            Expr::Prefix { operator, right } => self.check_prefix(operator, right),
            Expr::Infix { left, operator, right } => {
                self.check_expression(left);
                self.check_expression(right);
                self.check_infix(&left.typ, operator, &right.typ)
            }
            Expr::If { condition, consequence, alternative } => self.check_if(condition, consequence, alternative),
            Expr::While { condition, body } => self.check_while(condition, body),
            
            Expr::Assign { left, extra_operator: operator, right } => self.check_assign(left, operator, right),

            Expr::FnDefinition { params, return_type, body, .. } => {
                self.check_fn_expression(params, return_type, body);
                TypeKind::Void
            }
            Expr::Return(ret) => self.check_return(ret),
            Expr::Break => self.check_break(),
            Expr::Match { match_value, arms: cases } => self.check_match(match_value, cases),
            Expr::Call { callee, arguments } => self.check_fn_call(callee, arguments),
            Expr::PathedIdentifier(segments) => self.check_path_expression(segments),
            Expr::EnumDefinition { name, enums } => self.check_enum_expression(name, enums),

            Expr::ParserTempTypeAnnotation(_) => self.add_error(format!("Type annotations are not allowed here.")),
            _ => unreachable!("{:?}", expr)
        };

        expr.typ = self.prune(&inferred_type);
    }




    fn check_literal(&mut self, val: &mut Value) -> TypeKind {
        match val {
            Value::Num(_) => TypeKind::Num,
            Value::Str(_) => TypeKind::Str,
            Value::Bool(_) => TypeKind::Bool,
            
            Value::Closure { params, return_type: return_value, body } => {
                self.check_fn_expression(params, return_value, body)
            }
            _ => unreachable!() // other values are not used yet in the parser
        }
    }

    fn check_let(&mut self, pattern: &mut BindingPattern, val: &mut TypedExpr) -> TypeKind {
        self.check_expression(val);
        let binding_type = self.check_binding_pattern(pattern, true);
        self.unify_types(&binding_type, &val.typ);
        TypeKind::Void
    }

    fn checked_define_variable(&mut self, name: String, typ: TypeKind) {
        if self.env.define_variable(name.clone(), typ) {
            self.add_error(format!("Name '{name}' is already defined in this scope."));
        }
    }
    fn checked_define_type(&mut self, name: String, typ: DefinedTypeKind) {
        if self.env.define_type(name.clone(), typ) {
            self.add_error(format!("Name '{name}' is already defined in this scope."));
        }
    }

    fn check_binding_pattern(&mut self, pattern: &mut BindingPattern, define_pattern_vars: bool) -> TypeKind {
        match pattern {
            BindingPattern::NameAndType { name, typ } => {
                if *typ == TypeKind::ParserUnknown {
                    // if no type is annotaed create a new inference var
                    *typ = self.new_inference_type();
                }
                if define_pattern_vars { self.checked_define_variable(name.clone(), typ.clone()); }
                typ.clone()
            }
            BindingPattern::Array(elements) => {
                let arr_type = self.new_inference_type();
                
                for element in elements {
                    let element_typ = self.check_binding_pattern(element, define_pattern_vars);
                    self.unify_types(&element_typ, &arr_type);
                }
                TypeKind::Arr(Box::new(arr_type))
            }
            BindingPattern::Tuple(elements) => {
                let mut tuple_types = Vec::new();

                for element in elements {
                    let element_typ = self.check_binding_pattern(element, define_pattern_vars);
                    tuple_types.push(element_typ);
                }
                TypeKind::Tup(tuple_types)
            }
            BindingPattern::Wildcard => {
                self.new_inference_type()
            }
        }
    }

    fn check_identifier(&mut self, name: &str) -> TypeKind {
        self.env.lookup_variable(name).unwrap_or_else(|| {
            self.add_error(format!("Undefined identifier: '{}'", name));
            TypeKind::TypeError
        })
    }

    fn check_block(&mut self, body: &mut Vec<TypedExpr>) -> TypeKind {
        self.env.enter_scope();

        // pass 1: FnDefinitions
        for expr in body.iter_mut() {
            if let Expr::FnDefinition { name, params, return_type, .. } = &mut expr.expression {
                let fn_typ = self.get_fn_type(params, return_type, false);
                self.checked_define_variable(name.clone(), fn_typ);
            }
        }

        // pass 2: everything else
        let mut last_type = TypeKind::Void;
        for expr in body {
            self.check_expression(expr);
            last_type = expr.typ.clone();
        }
        self.env.exit_scope();
        last_type
    }

    fn check_prefix(&mut self, operator: &TokenType, right: &mut TypedExpr) -> TypeKind{
        self.check_expression(right);
        match operator {
            TokenType::Not => {
                self.unify_types(&TypeKind::Bool, &right.typ);
                right.typ.clone()
            }
            TokenType::Minus => {
                self.unify_types(&TypeKind::Num, &right.typ);
                right.typ.clone()
            }
            _ => { self.add_error(format!("Unsupported prefix operator: {:?}", operator)); TypeKind::TypeError }
        }
    }

    fn check_infix(&mut self, left_type: &TypeKind, operator: &TokenType, right_type: &TypeKind) -> TypeKind {
        match operator {
            // overloaded operators
            TokenType::Plus => {
                self.unify_types(left_type, right_type);
                let unified_type = self.prune(left_type);
                match unified_type {
                    TypeKind::Num | TypeKind::Str => unified_type,
                    // let it propagate, it will be solved later.
                    TypeKind::Inference(_) => unified_type,
                    _ => {
                        self.add_error(format!("Cannot apply operator '+' to type {}.", unified_type));
                        TypeKind::TypeError
                    }
                }
            }

            // numeric operators
            TokenType::Minus | TokenType::Star | TokenType::Slash | TokenType::Percent | TokenType::StarStar => {
                self.unify_types(left_type, &TypeKind::Num);
                self.unify_types(right_type, &TypeKind::Num);
                TypeKind::Num
            }

            // comparison operators
            TokenType::EqualEqual | TokenType::NotEqual | TokenType::Greater | TokenType::Less | TokenType::GreaterEqual | TokenType::LessEqual => {
                self.unify_types(left_type, right_type);
                TypeKind::Bool
            }

            // boolean operators
            TokenType::Ampersand | TokenType::Pipe => {
                self.unify_types(&TypeKind::Bool, left_type);
                self.unify_types(&TypeKind::Bool, right_type);
                TypeKind::Bool
            }
            _ => { self.add_error(format!("Unsupported infix operator: {:?}", operator)); TypeKind::TypeError }
        }
    }

    fn check_if(&mut self, condition: &mut TypedExpr, consequence: &mut TypedExpr, alternative: &mut Option<Box<TypedExpr>>) -> TypeKind {
        self.check_expression(condition);
        self.unify_types(&TypeKind::Bool, &condition.typ);
        
        self.check_expression(consequence);
        if let Some(alt) = alternative {
            self.check_expression(alt);

            self.unify_types(&consequence.typ, &alt.typ);
            return consequence.typ.clone();
        }
        TypeKind::Void
    }

    fn check_while(&mut self, condition: &mut TypedExpr, body: &mut TypedExpr) -> TypeKind {
        self.check_expression(condition);
        self.unify_types(&TypeKind::Bool, &condition.typ);

        let previous_can_break = self.can_break;
        self.can_break = true;
        self.check_expression(body);
        self.can_break = previous_can_break;
        TypeKind::Void
    }

    fn check_template_string(&mut self, parts: &mut Vec<TypedExpr>) -> TypeKind {
        for part in parts {
            self.check_expression(part);
        }
        TypeKind::Str
    }

    fn check_tuple(&mut self, elements: &mut Vec<TypedExpr>) -> TypeKind {
        let mut tuple_types = Vec::new();

        for element in elements {
            self.check_expression(element);
            tuple_types.push(element.typ.clone());
        }
        TypeKind::Tup(tuple_types)
    }

    fn check_array(&mut self, elements: &mut Vec<TypedExpr>) -> TypeKind {
        let arr_type = self.new_inference_type();

        for element in elements {
            self.check_expression(element);
            self.unify_types(&arr_type, &element.typ);
        }
        TypeKind::Arr(Box::new(arr_type))
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

    fn check_assign(&mut self, left: &mut TypedExpr, operator: &TokenType, right: &mut TypedExpr) -> TypeKind {
        self.check_expression(left);
        self.check_expression(right);
        if *operator == TokenType::Equal { self.unify_types(&left.typ, &right.typ); }
        else { self.check_infix(&left.typ, operator, &right.typ); }

        TypeKind::Void
    }

    fn get_fn_type(&mut self, params: &mut Vec<BindingPattern>, return_type: &mut TypeKind, define_params: bool) -> TypeKind {
        let mut param_types = Vec::new();
        for param_pattern in params.iter_mut() {
            param_types.push(self.check_binding_pattern(param_pattern, define_params));
        }
        if *return_type == TypeKind::ParserUnknown {
            *return_type = self.new_inference_type();
        }
        TypeKind::Fn {
            param_types,
            return_type: Box::new(return_type.clone()),
        }
    }


    fn check_fn_expression(&mut self, params: &mut Vec<BindingPattern>, return_type: &mut TypeKind, body: &mut Rc<TypedExpr>) -> TypeKind {
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
                    return self.add_error(format!("Expected {} arguments, found {}.", param_types.len(), arg_types.len()));
                }
                for (param_type, arg_type) in param_types.iter().zip(arg_types.iter()) {
                    self.unify_types(param_type, arg_type);
                }
                *return_type.clone()
            }
            _ => self.add_error(format!("Cannot call a non-function type '{}'", callee.typ))
        }
    }

    fn check_return(&mut self, return_expression: &mut TypedExpr) -> TypeKind {
        self.check_expression(return_expression);

        if let Some(x) = self.current_function_return_type.clone() {
            self.unify_types(&x, &return_expression.typ);
        }
        else { self.add_error(format!("'return' are only allowed inside functions.")); }

        TypeKind::Never
    }

    fn check_break(&mut self) -> TypeKind {
        if !self.can_break { self.add_error(format!("'break' is only allowed inside loops.")); }
        TypeKind::Never
    }

    fn check_match(&mut self, match_value: &mut TypedExpr, arms: &mut Vec<MatchArm>) -> TypeKind {
        self.check_expression(match_value);

        let mut cases_to_cover = match self.prune(&match_value.typ) {
            TypeKind::Bool => Some(HashSet::from(["true".to_string(), "false".to_string()])),
            _ => None
        };
        
        let mut has_wildcard = false;
        let arm_type = self.new_inference_type();
        
        for arm in arms {
            if arm.extra_condition.is_none() {
                match arm.pattern {
                    MatchPattern::Wildcard => has_wildcard = true,
                    MatchPattern::Literal(Value::Bool(bool)) => {
                        if let Some(x) = &mut cases_to_cover { x.remove(&bool.to_string()); }
                    }
                    _ => { }
                }
            }
            let pattern_type = self.check_match_pattern(&mut arm.pattern);
            self.unify_types(&match_value.typ, &pattern_type);
            
            if let Some(x) = &mut arm.extra_condition {
                self.check_expression(x);
                self.unify_types(&TypeKind::Bool, &x.typ);
            }
            self.check_expression(&mut arm.body);
            self.unify_types(&arm_type, &arm.body.typ);
        }

        if !has_wildcard && match &cases_to_cover {
            None => false,
            Some(remaining_cases) => !remaining_cases.is_empty()
        } {
            self.add_error(format!("Match expression does not cover all cases. Remaining cases: {:?}", cases_to_cover.unwrap()));
        }

        arm_type
    }

    fn check_match_pattern(&mut self, pattern: &mut MatchPattern) -> TypeKind {
        match pattern {
            MatchPattern::Literal(lit) => self.check_literal(lit),
            MatchPattern::Wildcard => { self.new_inference_type() }
            MatchPattern::Binding(pattern) => self.check_binding_pattern(pattern, true),
            MatchPattern::Array(elements) => {
                let arr_type = self.new_inference_type();
                for element in elements {
                    let element_typ = self.check_match_pattern(element);
                    self.unify_types(&arr_type, &element_typ);
                }
                TypeKind::Arr(Box::new(arr_type))
            }
            MatchPattern::Tuple(elements) => {
                let mut tuple_types = Vec::new();
                for element in elements {
                    tuple_types.push(self.check_match_pattern(element));
                }
                TypeKind::Tup(tuple_types)
            }
            MatchPattern::EnumVariant { path, name, inner_patterns } => {
                if path.len() != 1 { return self.add_error("Multi-segment paths in match patterns are not yet supported.".to_string()) }
                
                let enum_name = &path[0];

                let enum_definition = match self.env.lookup_type(enum_name) {
                    Some(DefinedTypeKind::Enum { inner_types }) => inner_types,
                    Some(_) => { return self.add_error(format!("Type '{}' is not an enum.", enum_name)); }
                    None => { return self.add_error(format!("Enum type '{}' not found.", enum_name)); }
                };
                let expected_enum_type = TypeKind::Enum { name: enum_name.clone() };
                
                if let Some(expected_variant_params) = enum_definition.get(name) {
                    if inner_patterns.len() != expected_variant_params.len() {
                        return self.add_error(format!(
                            "Enum variant '{}::{}' expects {} arguments. Found {}.", enum_name, name, expected_variant_params.len(), inner_patterns.len()
                        ));
                    }

                    // idk what this does, is for later
                    for (pattern_arg, def_param) in inner_patterns.iter_mut().zip(expected_variant_params.iter()) {
                        if let BindingPattern::NameAndType { typ: expected_type, .. } = def_param {
                            self.check_match_pattern(pattern_arg);
                        }
                        else { unreachable!() }
                    }

                }
                else { return self.add_error(format!("Enum '{}' has no variant named '{}'.", enum_name, name)); }
                expected_enum_type
            }
            MatchPattern::Or(patterns) => {
                let patterns_typ = self.new_inference_type();
                for pattern in patterns {
                    let pattern_typ = self.check_match_pattern(pattern);
                    self.unify_types(&patterns_typ, &pattern_typ);
                }
                patterns_typ
            }
        }
    }


    fn check_enum_expression(&mut self, name: &mut String, enums: &mut Vec<EnumExpression>) -> TypeKind {
        self.checked_define_type(name.clone(), DefinedTypeKind::Enum {
            inner_types: enums.into_iter()
                .map(|x| (x.name.clone(), x.inner_types.clone()))
                .collect()
        });
        TypeKind::Void
    }


    fn check_path_expression(&mut self, segments: &[String]) -> TypeKind {
        // Look up the first segment as a defined type (e.g., an Enum name)
        let first_segment = &segments[0];

        if let Some(typ) = self.env.lookup_type(first_segment) {
            match typ {
                DefinedTypeKind::Enum { inner_types } => {
                    let enum_variation_name = &segments[1];
                    if let Some(enum_variation) = inner_types.get(enum_variation_name) {
                        return TypeKind::Enum { name: first_segment.clone() };
                    }
                    else { return self.add_error(format!("Enum {} doesn't have a variation named {}.", first_segment, enum_variation_name)) }
                }
            }
        }
        else if let Some(var) = self.env.lookup_variable(first_segment) {
            return var.clone();
        }
        else { return self.add_error(format!("Could not find {}.", first_segment)); }
    }













    fn finalize_ast(&mut self, expressions: &mut [TypedExpr]) {
        let mut worklist: Vec<&mut TypedExpr> = expressions.iter_mut().collect();
        while let Some(expr) = worklist.pop() {
            self.finalize_type_recursively(&mut expr.typ);
            match &mut expr.expression {
                Expr::Let { value, pattern, .. } => {
                    self.finalize_pattern_type(pattern); // Also finalize patterns!
                    worklist.push(value);
                },
                Expr::Block(x) | Expr::Array(x) | Expr::Tuple(x) | Expr::TemplateString(x) => {
                    worklist.extend(x);
                },
                Expr::Index { left, index, .. } => { worklist.push(left); worklist.push(index); },
                Expr::Prefix { right: x, .. } => {
                    worklist.push(x);
                }
                Expr::Infix { left, right, .. } | Expr::Assign { left, right, .. } => {
                    worklist.push(left);
                    worklist.push(right);
                },
                Expr::If { condition, consequence, alternative } => {
                    worklist.push(condition);
                    worklist.push(consequence);
                    if let Some(alt) = alternative { worklist.push(alt); }
                },
                Expr::While { condition, body } => {
                    worklist.push(condition);
                    worklist.push(body);
                }
                Expr::Call { callee: function, arguments } => {
                    worklist.push(function);
                    worklist.extend(arguments);
                }
                Expr::FnDefinition { params, return_type, body, .. } => {
                    for param in params { self.finalize_pattern_type(param); }
                    self.finalize_type_recursively(return_type);
                    worklist.push(Rc::get_mut(body).unwrap());
                }
                Expr::Return(return_expression) => {
                    worklist.push(return_expression)
                }
                Expr::Match { match_value, arms: cases } => {
                    worklist.push(match_value);
                    for case in cases {
                        if let Some(x) = &mut case.extra_condition { worklist.push(x); }
                        worklist.push(&mut case.body);
                    }
                }
                Expr::Literal(Value::Closure { params, return_type, body }) => {
                    for param in params { self.finalize_pattern_type(param); }
                    self.finalize_type_recursively(return_type);
                    worklist.push(Rc::get_mut(body).unwrap());
                }
                Expr::Literal(_) | Expr::Identifier(_) | Expr::PathedIdentifier(_) | Expr::EnumDefinition{..} | Expr::Break => { }  // types should already be finalized
                Expr::ParserTempTypeAnnotation(_) => { } // already errored in check_expression()
            }
        }
    }

    fn finalize_type_recursively(&mut self, typ: &mut TypeKind) -> TypeKind {
        let mut pruned = self.prune(typ);
        let final_typ = match &mut pruned {
            TypeKind::Inference(id) => {
                self.inference_id_lookup.insert(*id, TypeKind::TypeError);
                self.add_error(format!("Cannot infer type {}.", pruned))
            }
            TypeKind::Arr(inner) => TypeKind::Arr(Box::new(self.finalize_type_recursively(inner))),
            TypeKind::Tup(inners) => TypeKind::Tup(inners.iter_mut().map(|t| self.finalize_type_recursively(t)).collect()),
            TypeKind::Fn { param_types, return_type } => TypeKind::Fn {
                param_types: param_types.iter_mut().map(|t| self.finalize_type_recursively(t)).collect(),
                return_type: Box::new(self.finalize_type_recursively(return_type)),
            },
            _ => pruned,
        };
        *typ = final_typ;
        typ.clone()
    }

    fn finalize_pattern_type(&mut self, pattern: &mut BindingPattern) {
         match pattern {
            BindingPattern::NameAndType { typ, .. } => {
                *typ = self.finalize_type_recursively(typ);
            }
            BindingPattern::Array(elements) | BindingPattern::Tuple(elements) => {
                for element in elements {
                    self.finalize_pattern_type(element);
                }
            }
            BindingPattern::Wildcard => {}
        }
    }
}