use crate::{ast_structure::{BindingPattern, DefinedTypeKind, EnumExpression, Expr, LiteralValue, MatchArm, MatchPattern, TypeKind, TypedExpr}, tokens::TokenType};
use std::{collections::{HashMap, HashSet}, fmt};



// example:
// let x = []
// let y = x
// let z: str = x[1]
//
// x gets assigned arr<Inference(x)<Unbound>>
// y gets assigned arr<Inference(x)<Unbound>>
// z gets assigned arr<Inference(x)<Bound<str>>> and figures the type of x out. now InferenceVar(x) gets changed to str everywhere.



#[derive(Default)]
struct TypeScope {
    vars: HashMap<String, TypeKind>,
    types: HashMap<String, DefinedTypeKind>,
}

pub struct Environment {
    scopes: Vec<TypeScope>,
}
impl Environment {
    pub fn new() -> Self {
        Environment { scopes: vec![TypeScope::default()] }
    }

    // e.g. for a block or function
    pub fn enter_scope(&mut self) { self.scopes.push(TypeScope::default()); }
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








#[derive(Debug)]
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
    env: Environment,
    inference_id_lookup: HashMap<usize, TypeKind>,
    next_inference_id: usize,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker { errors: Vec::new(), env: Environment::new(), inference_id_lookup: HashMap::new(), next_inference_id: 0 }
    }

    fn add_error(&mut self, message: String) -> TypeKind {
        self.errors.push(TypeCheckError { message });
        TypeKind::Error
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
            Expr::Infix { left, operator, right } => self.check_infix(left, operator, right),
            Expr::If { condition, consequence, alternative } => self.check_if(condition, consequence, alternative),
            
            Expr::Assign { left, operator, right } => self.check_assign(left, operator, right),

            Expr::FnDefinition { function, .. } => {
                if let Expr::Fn { params, return_value, body } = &mut function.expression {
                    function.typ = self.check_fn_expression(params, return_value, body, Some(function.typ.clone()));
                    TypeKind::Void
                } else { unreachable!() }
            }
            Expr::Fn { params, return_value, body } => self.check_fn_expression(params, return_value, body, None),
            Expr::Match { match_value, cases } => self.check_match(match_value, cases),
            Expr::Call { callee: function, arguments } => self.check_fn_call(function, arguments),
            Expr::TypePath(segments) => self.check_path_expression(segments),
            Expr::EnumDefinition { name, enums } => self.check_enum_expression(name, enums),
            _ => unreachable!("{:?}", expr)
        };

        expr.typ = inferred_type;
    }




    fn check_literal(&mut self, val: &LiteralValue) -> TypeKind {
        match val {
            LiteralValue::Number(_) => TypeKind::Num,
            LiteralValue::String(_) => TypeKind::Str,
            LiteralValue::Bool(_) => TypeKind::Bool,
        }
    }

    fn check_let(&mut self, pattern: &mut BindingPattern, val: &mut TypedExpr) -> TypeKind {
        self.check_expression(val);
        self.check_binding_pattern(pattern, &val.typ);
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

    fn check_binding_pattern(&mut self, pattern: &mut BindingPattern, value_type: &TypeKind) {
        match pattern {
            BindingPattern::NameAndType { name, typ } => {
                if *typ == TypeKind::ParserUnknown {
                    // if no type is annotaed create a new inference var
                    let new_inference_type = self.new_inference_type();
                    self.checked_define_variable(name.clone(), new_inference_type.clone());
                    self.unify_types(&new_inference_type, value_type);
                    *typ = new_inference_type;
                }
                else {
                    // type was annotated -> unify them
                    self.unify_types(typ, value_type);
                    self.checked_define_variable(name.clone(), typ.clone());
                }
            }
            _ => {
                self.add_error(format!("Unsupported binding pattern type: {:?}", pattern));
            }
        }
    }

    fn check_identifier(&mut self, name: &str) -> TypeKind {
        self.env.lookup_variable(name).unwrap_or_else(|| {
            self.add_error(format!("Undefined identifier: '{}'", name));
            TypeKind::Error
        })
    }

    fn check_block(&mut self, body: &mut Vec<TypedExpr>) -> TypeKind {
        self.env.enter_scope();

        // pass 1: FnDefinitions
        for expr in body.iter_mut() {
            if let Expr::FnDefinition { name, function } = &mut expr.expression {
                if let Expr::Fn { params, return_value, .. } = &mut function.expression {
                    function.typ = self.check_fn_type(params, return_value);
                } else { unreachable!() }
                self.checked_define_variable(name.clone(), function.typ.clone());
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
            _ => { self.add_error(format!("Unsupported prefix operator: {:?}", operator)); TypeKind::Error }
        }
    }

    fn check_infix(&mut self, left: &mut TypedExpr, operator: &TokenType, right: &mut TypedExpr) -> TypeKind {
        self.check_expression(left);
        self.check_expression(right);

        match operator {
            // overloaded operators
            TokenType::Plus => {
                self.unify_types(&left.typ, &right.typ);
                let unified_type = self.prune(&left.typ);
                match unified_type {
                    TypeKind::Num | TypeKind::Str => unified_type,
                    // let it propagate, it will be solved later.
                    TypeKind::Inference(_) => unified_type,
                    _ => {
                        self.add_error(format!("Cannot apply operator '+' to type {}.", unified_type));
                        TypeKind::Error
                    }
                }
            }

            // numeric operators
            TokenType::Minus | TokenType::Star | TokenType::Slash | TokenType::Percent | TokenType::StarStar => {
                self.unify_types(&left.typ, &TypeKind::Num);
                self.unify_types(&right.typ, &TypeKind::Num);
                TypeKind::Num
            }

            // comparison operators
            TokenType::EqualEqual | TokenType::NotEqual | TokenType::Greater | TokenType::Less | TokenType::GreaterEqual | TokenType::LessEqual => {
                self.unify_types(&left.typ, &right.typ);
                TypeKind::Bool
            }
            _ => { self.add_error(format!("Unsupported infix operator: {:?}", operator)); TypeKind::Error }
        }
    }

    fn check_if(&mut self, condition: &mut TypedExpr, consequence: &mut TypedExpr, alternative: &mut Option<Box<TypedExpr>>) -> TypeKind {
        self.check_expression(condition);
        self.unify_types(&TypeKind::Bool, &condition.typ);
        
        self.check_expression(consequence);
        if let Some(alt) = alternative {
            self.check_expression(alt);

            if consequence.typ == alt.typ { return consequence.typ.clone() }
        }
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

        match &left.expression {
            Expr::Identifier(_) => {},
            _ => panic!("not supported yet")
        }

        self.unify_types(&left.typ, &right.typ);
        TypeKind::Void
    }

    fn check_fn_type(&mut self, params: &mut Vec<BindingPattern>, return_value: &mut Option<BindingPattern>) -> TypeKind {
        let mut param_types = Vec::new();
        for param_pattern in params.iter_mut() {
            match param_pattern {
                BindingPattern::NameAndType { typ, .. } => {
                    *typ = self.new_inference_type();
                    param_types.push(typ.clone())
                }
                _ => panic!("unsupported")
            }
        }
        let return_type = if let Some(ret_val) = return_value {
            match ret_val {
                BindingPattern::NameAndType { typ, .. } => {
                    *typ = self.new_inference_type();
                    Box::new(typ.clone())
                }
                _ => panic!("unsupported")
            }
        } else { Box::new(self.new_inference_type()) };
        TypeKind::Fn {
            param_types,
            return_type,
        }
    }


    fn check_fn_expression(
        &mut self, params: &mut Vec<BindingPattern>, return_value: &mut Option<BindingPattern>, body: &mut TypedExpr, already_typed: Option<TypeKind>
    ) -> TypeKind {
        let fn_type = match already_typed {
            Some(x) => x, // already typed in check_block_expression()
            None => self.check_fn_type(params, return_value)
        };

        self.env.enter_scope();
        for param in params.iter_mut() {
            match param {
                BindingPattern::NameAndType { name, typ } => {
                    self.checked_define_variable(name.clone(), typ.clone());
                }
                _ => panic!("unsupported")
            }
        }
        self.check_expression(body);
        if let Some(ret_val) = return_value {
            if let BindingPattern::NameAndType { typ, .. } = ret_val {
                self.unify_types(typ, &body.typ);
            }
            else { unreachable!() }
        }
        self.env.exit_scope();

        // unify return value
        if let TypeKind::Fn { return_type, .. } = fn_type.clone() {
            self.unify_types(&return_type, &body.typ);
        } else { unreachable!() }

        fn_type
    }

    fn check_fn_call(&mut self, function: &mut TypedExpr, arguments: &mut Vec<TypedExpr>) -> TypeKind {
        self.check_expression(function);
        
        let mut arg_types = Vec::new();
        for arg in arguments {
            self.check_expression(arg);
            arg_types.push(arg.typ.clone());
        }

        match &mut function.typ {
            TypeKind::Fn { param_types, return_type } => {
                if param_types.len() != arg_types.len() {
                    self.add_error(format!("Expected {} arguments, found {}.", param_types.len(), arg_types.len()));
                    return TypeKind::Error;
                }
                for (param_type, arg_type) in param_types.iter().zip(arg_types.iter()) {
                    self.unify_types(arg_type, param_type);
                }

                // function returns nothing -> Void expression
                *return_type.clone()
            }
            _ => {
                self.add_error(format!("Cannot call a non-function type '{}'", function.typ));
                TypeKind::Error
            }
        }
    }

    fn check_match(&mut self, match_value: &mut TypedExpr, arms: &mut Vec<MatchArm>) -> TypeKind {
        self.check_expression(match_value);

        let mut cases_to_cover = match match_value.typ {
            TypeKind::Bool => Some(HashSet::from(["true".to_string(), "false".to_string()])),
            _ => None
        };
        
        let mut has_wildcard = false;
        let arm_type = self.new_inference_type();
        
        for arm in arms {
            match arm.pattern {
                MatchPattern::Wildcard if arm.extra_condition.is_none() => has_wildcard = true,
                MatchPattern::Literal(LiteralValue::Bool(bool)) if arm.extra_condition.is_none() => {
                    if let Some(x) = &mut cases_to_cover { x.remove(&bool.to_string()); }
                    self.check_match_pattern(&mut arm.pattern, &match_value.typ);
                }
                _ => self.check_match_pattern(&mut arm.pattern, &match_value.typ)
            }
            
            if let Some(x) = &mut arm.extra_condition {
                self.check_expression(x);
                self.unify_types(&TypeKind::Bool, &x.typ);
            }
            self.check_expression(&mut arm.body);
            self.unify_types(&arm_type, &arm.body.typ);
        }

        if !has_wildcard && !(if let Some(remaining_cases) = cases_to_cover { remaining_cases.is_empty() } else { false })
        { self.add_error(format!("Match expression does not cover all cases.")); }

        arm_type
    }

    fn check_match_pattern(&mut self, pattern: &mut MatchPattern, value_type: &TypeKind) {
        match pattern {
            MatchPattern::Literal(lit) => {
                let lit_type = self.check_literal(lit);
                self.unify_types(value_type, &lit_type);
            }
            MatchPattern::Binding(pattern) => self.check_binding_pattern(pattern, value_type),
            MatchPattern::EnumVariant { path, name, inner_patterns } => {
                if path.len() != 1 { self.add_error("Multi-segment paths in match patterns are not yet supported.".to_string()); return; }
                
                let enum_name = &path[0];

                let enum_definition = match self.env.lookup_type(enum_name) {
                    Some(DefinedTypeKind::Enum { inner_types }) => inner_types,
                    Some(_) => { self.add_error(format!("Type '{}' is not an enum.", enum_name)); return; }
                    None => { self.add_error(format!("Enum type '{}' not found.", enum_name)); return; }
                };
                let expected_enum_type = TypeKind::Enum { name: enum_name.clone() };
                self.unify_types(value_type, &expected_enum_type);
                
                if let Some(expected_variant_params) = enum_definition.get(name) {
                    if inner_patterns.len() != expected_variant_params.len() {
                        self.add_error(format!(
                            "Enum variant '{}::{}' expects {} arguments. Found {}.", enum_name, name, expected_variant_params.len(), inner_patterns.len()
                        ));
                        return;
                    }

                    for (pattern_arg, def_param) in inner_patterns.iter_mut().zip(expected_variant_params.iter()) {
                        if let BindingPattern::NameAndType { typ: expected_type, .. } = def_param {
                            self.check_match_pattern(pattern_arg, expected_type);
                        }
                        else { unreachable!() }
                    }

                }
                else { self.add_error(format!("Enum '{}' has no variant named '{}'.", enum_name, name)); }
            }
            _ => {
                self.add_error(format!("Unsupported match pattern type: {:?}", pattern));
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


    fn check_path_expression(&mut self, segments: &mut Vec<String>) -> TypeKind {
        if segments.is_empty() {
            self.add_error("Path expression cannot be empty.".to_string());
            return TypeKind::Error;
        }

        // Look up the first segment as a defined type (e.g., an Enum name)
        let first_segment = &segments[0];
        let type_def = match self.env.lookup_type(first_segment) {
            Some(def) => def,
            None => {
                self.add_error(format!("Type or module '{}' not found.", first_segment));
                return TypeKind::Error;
            }
        };

        if segments.len() == 1 { unreachable!() }
        if segments.len() > 2 {
            self.add_error("Nested paths (e.g., A::B::C) are not yet supported.".to_string());
            return TypeKind::Error;
        }

        let second_segment = &segments[1];

        match type_def {
            DefinedTypeKind::Enum { inner_types } => {
                if let Some(variant_params) = inner_types.get(second_segment) {
                    if variant_params.is_empty() {
                        TypeKind::Enum { name: first_segment.clone() }
                    } else {
                        // It's a variant with arguments, so it acts like a constructor function.
                        // We build a `TypeKind::Fn` on the fly for it.
                        let param_types: Vec<TypeKind> = variant_params.iter().map(|x| {
                            if let BindingPattern::NameAndType { typ, .. } = x { typ.clone() }
                            else { self.add_error("Unsupported binding pattern in enum definition.".to_string()) }
                        }).collect();

                        TypeKind::Fn {
                            param_types,
                            return_type: Box::new(TypeKind::Enum { name: first_segment.clone() }),
                        }
                    }
                } else { self.add_error(format!("Enum '{}' does not have a variant named '{}'.", first_segment, second_segment)) }
            }
            _ => panic!("structs/modules not yet supported...")
        }
    }













    fn finalize_ast(&mut self, expressions: &mut [TypedExpr]) {
        let mut worklist: Vec<&mut TypedExpr> = expressions.iter_mut().collect();
        while let Some(expr) = worklist.pop() {
            self.finalize_node(expr);
            let expr_expression_str = format!("{:?}", expr.expression);
            match &mut expr.expression {
                Expr::Let { value, pattern, .. } => {
                    self.finalize_pattern_type(pattern); // Also finalize patterns!
                    worklist.push(value);
                },
                Expr::Block(body) => worklist.extend(body),
                Expr::Array(e) | Expr::Tuple(e) | Expr::TemplateString(e) => {
                    worklist.extend(e);
                },
                Expr::Index { left, index, .. } => { worklist.push(left); worklist.push(index); },
                Expr::Prefix { right, .. } => worklist.push(right),
                Expr::Infix { left, right, .. } | Expr::Assign { left, right, .. } => {
                    worklist.push(left);
                    worklist.push(right);
                },
                Expr::If { condition, consequence, alternative, .. } => {
                    worklist.push(condition);
                    worklist.push(consequence);
                    if let Some(alt) = alternative { worklist.push(alt); }
                },
                Expr::Fn { params, return_value, body } => {
                    params.iter_mut().for_each(|x| self.finalize_pattern_type(x));
                    if let Some(ret_val) = return_value { self.finalize_pattern_type(ret_val); }
                    worklist.push(body);
                }
                Expr::Call { callee: function, arguments } => {
                    worklist.push(function);
                    worklist.extend(arguments);
                }
                Expr::FnDefinition { function, .. } => {
                    worklist.push(function);
                }
                Expr::Match { match_value, cases } => {
                    worklist.push(match_value);
                    for case in cases {
                        if let Some(x) = &mut case.extra_condition { worklist.push(x); }
                        worklist.push(&mut case.body);
                    }
                }
                Expr::Literal(_) | Expr::Identifier(_) | Expr::TypePath(_) | Expr::EnumDefinition{..} => {}  // types should already be finalized
                _ => panic!("unhandled in finalize_ast: {}", expr_expression_str)
            }
        }
    }

    fn finalize_node(&mut self, expr: &mut TypedExpr) {
        expr.typ = self.finalize_type_recursively(&expr.typ);
    }

    fn finalize_type_recursively(&mut self, typ: &TypeKind) -> TypeKind {
        let pruned = self.prune(typ);
        match &pruned {
            TypeKind::Inference(id) => {
                self.add_error(format!("Cannot infer type ?T{}.", id));
                self.inference_id_lookup.insert(*id, TypeKind::Error);
                TypeKind::Error
            }
            TypeKind::Arr(inner) => TypeKind::Arr(Box::new(self.finalize_type_recursively(inner))),
            TypeKind::Tup(inners) => TypeKind::Tup(inners.iter().map(|t| self.finalize_type_recursively(t)).collect()),
            TypeKind::Fn { param_types, return_type } => TypeKind::Fn {
                param_types: param_types.iter().map(|t| self.finalize_type_recursively(t)).collect(),
                return_type: Box::new(self.finalize_type_recursively(return_type)),
            },
            // ... Struct/Fn ...
            _ => pruned,
        }
    }

    fn finalize_pattern_type(&mut self, pattern: &mut BindingPattern) {
         match pattern {
            BindingPattern::NameAndType { typ, .. } => {
                *typ = self.finalize_type_recursively(typ);
            }
            // ... Array/Tuple ...
            _ => {}
        }
    }
}