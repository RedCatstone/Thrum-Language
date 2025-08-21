use crate::{ast_structure::{Expression, ExpressionPattern, LiteralValue, TypedExpression}, tokens::TokenType};
use std::{collections::HashMap, fmt};





#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Num,
    Str,
    Bool,

    Arr(Box<TypeKind>),
    Tup(Vec<TypeKind>),
    Fn {
        param_types: Vec<TypeKind>,
        return_type: Box<TypeKind>,
    },
    Struct {
        name: String,
        inner_types: Vec<TypeKind>,
    },


    Inference(usize),
    
    // 'let', 'FnDefinition', empty block, sometimes if statement
    Void,

    // parser puts this type everywhere at first.
    ParserUnknown,

    Error,
}


// example:
// let x = []
// let y = x
// let z: str = x[1]
//
// x gets assigned arr<Inference(x)<Unbound>>
// y gets assigned arr<Inference(x)<Unbound>>
// z gets assigned arr<Inference(x)<Bound<str>>> and figures the type of x out. now InferenceVar(x) gets changed to str everywhere.





impl TypeKind {
    pub fn from_str(str: &str, inner_types: Vec<TypeKind>) -> TypeKind {
        match str {
            "num" => TypeKind::Num,
            "str" => TypeKind::Str,
            "bool" => TypeKind::Bool,
            "tup" => TypeKind::Tup(inner_types),
            "arr" => {
                if inner_types.len() == 1 {
                    TypeKind::Arr(Box::new(inner_types.into_iter().next().unwrap()))
                } else {
                    panic!("NOOO!!! invalid types for arr! need 1 inner type!");
                }
            }
            &_ => TypeKind::Struct { name: str.to_string(), inner_types }
        }
    }
}













type Scope = HashMap<String, TypeKind>;

pub struct Environment {
    scopes: Vec<Scope>,
}
impl Environment {
    pub fn new() -> Self {
        Environment { scopes: vec![Scope::new()] }
    }

    // e.g. for a block or function
    pub fn enter_scope(&mut self) { self.scopes.push(Scope::new()); }
    pub fn exit_scope(&mut self) { self.scopes.pop(); }

    pub fn define_identifier(&mut self, name: String, typ: TypeKind) {
        self.scopes.last_mut().unwrap().insert(name, typ);
    }

    pub fn lookup_identifier(&self, name: &str) -> Option<TypeKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
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

    fn add_error(&mut self, message: String) {
        self.errors.push(TypeCheckError { message });
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


    pub fn check_program(&mut self, program: &mut Vec<TypedExpression>) {
        // PASS 1: run type unification/constraint solving logic.
        self.check_block(program);

        // PASS 2: clean up (remove all TypeKind::Infered(id))
        self.finalize_ast(program);

        println!("inference map: {:?}", self.inference_id_lookup);
    }



    fn check_expression(&mut self, expr: &mut TypedExpression) {
        let inferred_type = match &mut expr.expression {
            Expression::Literal(val) => self.check_literal(val),
            Expression::Identifier(name) => self.check_identifier(name),
            Expression::TemplateString(parts) => self.check_template_string(parts),
            Expression::Tuple(elements) => self.check_tuple(elements),
            Expression::Array(elements) => self.check_array(elements),
            Expression::Index { left, index } => self.check_index(left, index),

            Expression::Let { pattern, value } => self.check_let(pattern, value),
            Expression::Block(body) => self.check_block(body),
            Expression::Prefix { operator, right } => self.check_prefix(operator, right),
            Expression::Infix { left, operator, right } => self.check_infix(left, operator, right),
            Expression::If { condition, consequence, alternative } => self.check_if(condition, consequence, alternative),
            
            Expression::Assign { left, operator, right } => self.check_assign(left, operator, right),

            Expression::FnDefinition { function, .. } => {
                if let Expression::Fn { params, return_value, body } = &mut function.expression {
                    function.typ = self.check_fn_expression(params, return_value, body, Some(function.typ.clone()));
                    TypeKind::Void
                } else { unreachable!() }
            }
            Expression::Fn { params, return_value, body } => self.check_fn_expression(params, return_value, body, None),
            Expression::FnCall { function, arguments, .. } => self.check_fn_call(function, arguments),
            // Expression::CurlyNew { name, params, .. } => self.check_curly_new(name, params),
            // Expression::Match { matcher, cases, .. } => self.check_match(matcher, cases),
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

    fn check_let(&mut self, pattern: &mut ExpressionPattern, val: &mut TypedExpression) -> TypeKind {
        self.check_expression(val);
        self.check_expression_pattern(pattern, &val.typ);
        TypeKind::Void
    }

    fn check_expression_pattern(&mut self, pattern: &mut ExpressionPattern, value_type: &TypeKind) {
        match pattern {
            ExpressionPattern::NameAndType { name, typ } => {
                if *typ == TypeKind::ParserUnknown {
                    // if no type is annotaed create a new inference var
                    let new_inference_type = self.new_inference_type();
                    self.env.define_identifier(name.clone(), new_inference_type.clone());
                    self.unify_types(&new_inference_type, value_type);
                    *typ = new_inference_type;
                }
                else {
                    // type was annotated -> unify them
                    self.unify_types(typ, value_type);
                    self.env.define_identifier(name.clone(), typ.clone());
                }
            }
            _ => {
                self.add_error(format!("Unsupported pattern type: {:?}", pattern));
            }
        }
    }

    fn check_identifier(&mut self, name: &str) -> TypeKind {
        self.env.lookup_identifier(name).unwrap_or_else(|| {
            self.add_error(format!("Undefined identifier: '{}'", name));
            TypeKind::Error
        })
    }

    fn check_block(&mut self, body: &mut Vec<TypedExpression>) -> TypeKind {
        self.env.enter_scope();

        // pass 1: FnDefinitions
        for expr in body.iter_mut() {
            if let Expression::FnDefinition { name, function } = &mut expr.expression {
                if let Expression::Fn { params, return_value, .. } = &mut function.expression {
                    function.typ = self.check_fn_type(params, return_value);
                } else { unreachable!() }
                self.env.define_identifier(name.clone(), function.typ.clone());
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

    fn check_prefix(&mut self, operator: &TokenType, right: &mut TypedExpression) -> TypeKind{
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

    fn check_infix(&mut self, left: &mut TypedExpression, operator: &TokenType, right: &mut TypedExpression) -> TypeKind {
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

    fn check_if(&mut self, condition: &mut TypedExpression, consequence: &mut TypedExpression, alternative: &mut Option<Box<TypedExpression>>) -> TypeKind {
        self.check_expression(condition);
        self.unify_types(&TypeKind::Bool, &condition.typ);
        
        self.check_expression(consequence);
        if let Some(alt) = alternative {
            self.check_expression(alt);

            if consequence.typ == alt.typ { return consequence.typ.clone() }
        }
        TypeKind::Void
    }

    fn check_template_string(&mut self, parts: &mut Vec<TypedExpression>) -> TypeKind {
        for part in parts {
            self.check_expression(part);
        }
        TypeKind::Str
    }

    fn check_tuple(&mut self, elements: &mut Vec<TypedExpression>) -> TypeKind {
        let mut tuple_types = Vec::new();

        for element in elements {
            self.check_expression(element);
            tuple_types.push(element.typ.clone());
        }
        TypeKind::Tup(tuple_types)
    }

    fn check_array(&mut self, elements: &mut Vec<TypedExpression>) -> TypeKind {
        let arr_type = self.new_inference_type();

        for element in elements {
            self.check_expression(element);
            self.unify_types(&arr_type, &element.typ);
        }
        TypeKind::Arr(Box::new(arr_type))
    }

    fn check_index(&mut self, left: &mut TypedExpression, index: &mut TypedExpression) -> TypeKind {
        self.check_expression(left);
        self.check_expression(index);
        self.unify_types(&TypeKind::Num, &index.typ);
        let element_type = self.new_inference_type();
        let expected_left_type = TypeKind::Arr(Box::new(element_type.clone()));
        self.unify_types(&left.typ, &expected_left_type);
        element_type
    }

    fn check_assign(&mut self, left: &mut TypedExpression, operator: &TokenType, right: &mut TypedExpression) -> TypeKind {
        self.check_expression(left);
        self.check_expression(right);

        match &left.expression {
            Expression::Identifier(_) => {},
            _ => panic!("not supported yet")
        }

        self.unify_types(&left.typ, &right.typ);
        TypeKind::Void
    }

    fn check_fn_type(&mut self, params: &mut Vec<ExpressionPattern>, return_value: &mut Option<ExpressionPattern>) -> TypeKind {
        let mut param_types = Vec::new();
        for param_pattern in params.iter_mut() {
            match param_pattern {
                ExpressionPattern::NameAndType { typ, .. } => {
                    *typ = self.new_inference_type();
                    param_types.push(typ.clone())
                }
                _ => panic!("unsupported")
            }
        }
        let return_type = if let Some(ret_val) = return_value {
            match ret_val {
                ExpressionPattern::NameAndType { typ, .. } => {
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
        &mut self, params: &mut Vec<ExpressionPattern>, return_value: &mut Option<ExpressionPattern>, body: &mut TypedExpression, already_typed: Option<TypeKind>
    ) -> TypeKind {
        let fn_type = match already_typed {
            Some(x) => x, // already typed in check_block_expression()
            None => self.check_fn_type(params, return_value)
        };

        self.env.enter_scope();
        for param in params.iter_mut() {
            match param {
                ExpressionPattern::NameAndType { name, typ } => {
                    self.env.define_identifier(name.clone(), typ.clone());
                }
                _ => panic!("unsupported")
            }
        }
        self.check_expression(body);
        if let Some(ret_val) = return_value {
            if let ExpressionPattern::NameAndType { typ, .. } = ret_val {
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

    fn check_fn_call(&mut self, function: &mut TypedExpression, arguments: &mut Vec<TypedExpression>) -> TypeKind {
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









    fn finalize_ast(&mut self, expressions: &mut [TypedExpression]) {
        let mut worklist: Vec<&mut TypedExpression> = expressions.iter_mut().collect();
        while let Some(expr) = worklist.pop() {
            self.finalize_node(expr);
            match &mut expr.expression {
                Expression::Let { value, pattern, .. } => {
                    self.finalize_pattern_type(pattern); // Also finalize patterns!
                    worklist.push(value);
                },
                Expression::Block(body) => worklist.extend(body),
                Expression::Array(e) | Expression::Tuple(e) | Expression::TemplateString(e) => {
                    worklist.extend(e);
                },
                Expression::Index { left, index, .. } => { worklist.push(left); worklist.push(index); },
                Expression::Prefix { right, .. } => worklist.push(right),
                Expression::Infix { left, right, .. } | Expression::Assign { left, right, .. } => {
                    worklist.push(left);
                    worklist.push(right);
                },
                Expression::If { condition, consequence, alternative, .. } => {
                    worklist.push(condition);
                    worklist.push(consequence);
                    if let Some(alt) = alternative { worklist.push(alt); }
                },
                Expression::Fn { params, return_value, body } => {
                    params.iter_mut().for_each(|x| self.finalize_pattern_type(x));
                    if let Some(ret_val) = return_value { self.finalize_pattern_type(ret_val); }
                    worklist.push(body);
                }
                Expression::FnCall { function, arguments } => {
                    worklist.push(function);
                    worklist.extend(arguments);
                }
                Expression::FnDefinition { function, .. } => {
                    worklist.push(function);
                }

                Expression::Literal(_) | Expression::Identifier(_) => {}
                _ => panic!("unhandled...")
            }
        }
    }

    fn finalize_node(&mut self, expr: &mut TypedExpression) {
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

    fn finalize_pattern_type(&mut self, pattern: &mut ExpressionPattern) {
         match pattern {
            ExpressionPattern::NameAndType { typ, .. } => {
                *typ = self.finalize_type_recursively(typ);
            }
            // ... Array/Tuple ...
            _ => {}
        }
    }
}