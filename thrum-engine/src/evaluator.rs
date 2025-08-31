use core::fmt;
use std::{collections::HashMap, rc::Rc};

use crate::{ast_structure::{BindingPattern, Expr, MatchArm, MatchPattern, TypeKind, TypedExpr, Value}, nativelib::get_native_lib, tokens::TokenType};

#[derive(Debug, Clone)]
pub struct RuntimeEnvironment {
    scopes: Vec<HashMap<String, Value>>,
}

impl RuntimeEnvironment {
    pub fn new() -> Self { RuntimeEnvironment { scopes: vec![HashMap::new()] } }

    pub fn enter_scope(&mut self) { self.scopes.push(HashMap::new()); }
    pub fn exit_scope(&mut self) { self.scopes.pop(); }

    pub fn define_var(&mut self, name: String, value: Value) {
        self.scopes.last_mut().unwrap().insert(name, value);
    }
    pub fn lookup_var(&self, name: &str) -> Value {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return value.clone();
            }
        }
        unreachable!("tried to lookup {name}, but couldn't find it... (this should be handled in the typechecker already)")
    }
    pub fn lookup_var_mut<'a>(&'a mut self, name: &str) -> &'a mut Value {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return value;
            }
        }
        unreachable!("tried to lookup {name}, but couldn't find it... (this should be handled in the typechecker already)")
    }
}






pub struct RuntimeError {
    pub message: String
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub enum PropagateValue {
    Error(RuntimeError),
    Return(Value),
    Break,
}


pub struct Executor {
    env: RuntimeEnvironment,
}

impl Executor {
    pub fn new() -> Self {
        let mut exe = Executor { env: RuntimeEnvironment::new() };

        let native_lib = get_native_lib();
        for (name, _typ, val) in native_lib {
            exe.env.define_var(name, val);
        }

        exe
    }

    pub fn execute_program(&mut self, program: &[TypedExpr]) -> Result<Value, RuntimeError> {
        match self.eval_block_expression(program) {
            Ok(val) => Ok(val),
            Err(err) => {
                let PropagateValue::Error(runtime_error) = err else { unreachable!("top level return") };
                Err(runtime_error)
            }
        }
    }

    pub fn new_error(&mut self, message: String) -> PropagateValue {
        PropagateValue::Error(RuntimeError { message })
    }


    fn eval_expression(&mut self, expr: &TypedExpr) -> Result<Value, PropagateValue> {
        match &expr.expression {
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Array(arr) => {
                let mut results = Vec::with_capacity(arr.len());
                for element_expr in arr {
                    results.push(self.eval_expression(element_expr)?);
                }
                Ok(Value::Arr(results))
            }
            Expr::Tuple(tup) => {
                let mut results = Vec::with_capacity(tup.len());
                for element_expr in tup {
                    results.push(self.eval_expression(element_expr)?);
                }
                Ok(Value::Tup(results))
            }

            Expr::Identifier(name) => Ok(self.env.lookup_var(name)),

            Expr::Block(body) => self.eval_block_expression(body),
            
            Expr::Infix { left, operator, right } => {
                let left_val = self.eval_expression(left)?;
                let right_val = self.eval_expression(right)?;
                eval_infix(left_val, operator, right_val)
            }

            Expr::Prefix { operator, right } => {
                let right_val = self.eval_expression(right)?;
                eval_prefix(operator, right_val)
            }

            Expr::Assign { left, extra_operator, right } => {
                let new_value = self.eval_expression(right)?;
                match &left.expression {
                    Expr::Identifier(name) => {
                        let var_to_modify = self.env.lookup_var_mut(&name);
                        *var_to_modify = match extra_operator {
                            TokenType::Equal => new_value,
                            _ => eval_infix(var_to_modify.clone(), extra_operator, new_value)?,
                        };
                    }
                    _ => panic!("not supported yet")
                }
                Ok(Value::Void)
            }

            Expr::Let { pattern, value } => {
                let val_to_bind = self.eval_expression(value)?;
                self.eval_binding_pattern(pattern, val_to_bind)
            }

            Expr::If { condition, consequence, alternative } => {
                let condition_val = self.eval_expression(condition)?;
                match condition_val {
                    Value::Bool(true) => self.eval_expression(consequence),
                    Value::Bool(false) => {
                        if let Some(x) = alternative { self.eval_expression(x) }
                        else { Ok(Value::Void) }
                    }
                    _ => unreachable!()
                }
            }

            Expr::While { condition, body } => {
                loop {
                    match self.eval_expression(condition)? {
                        Value::Bool(true) => {
                            match self.eval_expression(body) {
                                Err(PropagateValue::Break) => break,
                                others => others
                            }?;
                        },
                        Value::Bool(false) => { break; }
                        _ => unreachable!()
                    }
                }
                Ok(Value::Void)
            }

            Expr::TemplateString(parts) => {
                let mut str = String::new();
                for part in parts {
                    let part_val = self.eval_expression(part)?;
                    let part_str = self.literal_to_string(part_val)?;
                    str.push_str(&part_str);
                }
                Ok(Value::Str(str))
            }

            Expr::Index { left, index } => {
                let left_val = self.eval_expression(left)?;
                if let Value::Num(index_val) = self.eval_expression(index)? {
                    if index_val.fract() > 0.0 {
                        return Err(self.new_error(format!("Cannot index arr with a non-integer number: {}", index_val)))
                    }
                    match left_val {
                        Value::Arr(arr) => {
                            if index_val.is_sign_positive() {
                                let corrected_index = index_val as usize;
                                match arr.get(corrected_index) {
                                    Some(x) => Ok(x.clone()),
                                    None => Err(self.new_error(format!("Index {corrected_index} is out of bounds for arr of length {}.", arr.len())))
                                }
                            }
                            else {
                                let x = -index_val as usize;
                                if x > arr.len() {
                                    Err(self.new_error(format!("Index {index_val} is out of bounds for arr of length {}.", arr.len())))
                                }
                                else {
                                    Ok(arr[arr.len() - x].clone())
                                }
                            }
                        }
                        _ => unreachable!()
                    }
                }
                else { unreachable!() }
            }
            

            Expr::Match { match_value, arms: cases } => {
                let match_val = self.eval_expression(match_value)?;
                
                for MatchArm { pattern, extra_condition, body } in cases {
                    self.env.enter_scope();
                    let pattern_matches = self.check_match_pattern(pattern, &match_val)?;

                    if pattern_matches && match extra_condition {
                        None => true,
                        Some(x) => {
                            if let Value::Bool(bool) = self.eval_expression(x)? { bool }
                            else { unreachable!() }
                        }
                    } {
                        // both pattern and extra condition match!
                        let body_val = self.eval_expression(body);
                        self.env.exit_scope();
                        return body_val;
                    }
                    self.env.exit_scope();
                }
                unreachable!("No match pattern matched...");
            }


            Expr::FnDefinition { name, params, return_type, body } => {
                self.env.define_var(name.clone(), Value::Closure { params: params.clone(), return_type: return_type.clone(), body: body.clone() });
                Ok(Value::Void)
            }

            Expr::Return(ret) => {
                // using the Err() variant to propagate the return value all the way up to the function it belongs to.
                let return_value = self.eval_expression(ret)?;
                Err(PropagateValue::Return(return_value))
            }
            Expr::Break => {
                Err(PropagateValue::Break)
            }


            Expr::Call { callee, arguments } => {
                let callee_val = self.eval_expression(callee)?;

                let mut args_values = Vec::new();
                for arg_expr in arguments {
                    args_values.push(self.eval_expression(arg_expr)?);
                }

                match callee_val {
                    Value::NativeFn(native_fn) => {
                        // it's a built-in Rust function (like `print`).
                        native_fn(&mut args_values).map_err(|e| self.new_error(e))
                    }
                    Value::Closure { params, body, .. } => {
                        self.env.enter_scope();
                        for (param_pattern, arg_value) in params.iter().zip(args_values.into_iter()) {
                            self.eval_binding_pattern(param_pattern, arg_value)?;
                        }
                        let result = match self.eval_expression(&body) {
                            Err(PropagateValue::Return(x)) => Ok(x),
                            others => others
                        };
                        self.env.exit_scope();
                        result
                    }
                    _ => unreachable!("tried to call a non-function value.")
                }
            }

            Expr::PathedIdentifier(segments) => {
                let first_segment = &segments[0];

                if segments.len() == 1 {
                    return Ok(self.env.lookup_var(first_segment));
                }

                panic!("enums not implemented")
            }

            _ => Err(self.new_error(format!("Execution for {:?} is not yet implemented.", expr.expression)))
        }
    }

    fn eval_block_expression(&mut self, body: &[TypedExpr]) -> Result<Value, PropagateValue> {
        self.env.enter_scope();
        let mut last_val = Value::Void;
        for statement in body {
            last_val = self.eval_expression(statement)?;
        }
        self.env.exit_scope();
        Ok(last_val)
    }


    fn eval_binding_pattern(&mut self, pattern: &BindingPattern, val_to_bind: Value) -> Result<Value, PropagateValue> {
        match pattern {
            BindingPattern::NameAndType { name, .. } => self.env.define_var(name.clone(), val_to_bind),
            BindingPattern::Array(bind_elements) => {
                if let Value::Arr(val_elements) = val_to_bind {
                    if bind_elements.len() > val_elements.len() { return Err(
                        self.new_error(format!("Array does not have enough elements to bind. Needs {}, found: {}", bind_elements.len(), val_elements.len()))
                    )}
                    for (bind_element, val_element) in bind_elements.iter().zip(val_elements) {
                        self.eval_binding_pattern(bind_element, val_element)?;
                    }
                }
                else { unreachable!() }
            }
            BindingPattern::Tuple(bind_elements) => {
                if let Value::Tup(val_elements) = val_to_bind {
                    for (bind_element, val_element) in bind_elements.iter().zip(val_elements) {
                        self.eval_binding_pattern(bind_element, val_element)?;
                    }
                }
                else { unreachable!() }
            }
            BindingPattern::Wildcard => { }
        }
        Ok(Value::Void)
    }

    fn check_match_pattern(&mut self, pattern: &MatchPattern, match_val: &Value) -> Result<bool, PropagateValue> {
        match pattern {
            MatchPattern::Wildcard => Ok(true),
            MatchPattern::Literal(lit) => Ok(*lit == *match_val),
            MatchPattern::Binding(bind) => { self.eval_binding_pattern(bind, match_val.clone())?; Ok(true) }
            MatchPattern::Array(arr) => {
                if let Value::Arr(match_arr) = match_val { 
                    if arr.len() != match_arr.len() { Ok(false) }
                    else {
                        for (arr_element, match_arr_element) in arr.iter().zip(match_arr) {
                            let matched = self.check_match_pattern(arr_element, match_arr_element)?;
                            if !matched {
                                return Ok(false);
                            }
                        }
                        Ok(true)
                    }
                }
                else { unreachable!() }
            }
            MatchPattern::Tuple(tup) => {
                if let Value::Tup(match_tup) = match_val {
                    for (tup_element, match_tup_element) in tup.iter().zip(match_tup) {
                        let matched = self.check_match_pattern(tup_element, match_tup_element)?;
                        if !matched { return Ok(false); }
                    }
                    Ok(true)
                }
                else { unreachable!() }
            }
            MatchPattern::EnumVariant { path, name, inner_patterns } => {
                unreachable!()
            }
            MatchPattern::Or(patterns) => {
                for pattern in patterns {
                    let matched = self.check_match_pattern(pattern, match_val)?;
                    if matched { return Ok(true); }
                }
                Ok(false)
            }
        }
    }


    fn literal_to_string(&mut self, literal: Value) -> Result<String, PropagateValue> {
        match literal {
            Value::Num(num) => Ok(num.to_string()),
            Value::Str(str) => Ok(str),
            Value::Bool(bool) => Ok(bool.to_string()),

            Value::Arr(arr) => {
                let str_results: Result<Vec<String>, PropagateValue> = arr.into_iter().map(|x| self.literal_to_string(x)).collect();
                Ok(String::from("[") + &str_results?.join(", ") + "]")
            }
            Value::Tup(tup) => {
                let str_results: Result<Vec<String>, PropagateValue> = tup.into_iter().map(|x| self.literal_to_string(x)).collect();
                Ok(String::from("(") + &str_results?.join(", ") + ")")
            }
            _ => Err(self.new_error(format!("Literal {:?} cannot be converted into a string.", literal)))
        }
    }
}

fn eval_infix(left: Value, operator: &TokenType, right: Value) -> Result<Value, PropagateValue> {
    match (left, right) {
        (Value::Num(l), Value::Num(r)) => match operator {
            TokenType::Plus => Ok(Value::Num(l + r)),
            TokenType::Minus => Ok(Value::Num(l - r)),
            TokenType::Star => Ok(Value::Num(l * r)),
            TokenType::Slash => Ok(Value::Num(l / r)),
            TokenType::Percent => Ok(Value::Num(l % r)),
            TokenType::StarStar => Ok(Value::Num(l.powf(r))),
            TokenType::EqualEqual => Ok(Value::Bool(l == r)),
            TokenType::NotEqual => Ok(Value::Bool(l != r)),
            TokenType::LessEqual => Ok(Value::Bool(l <= r)),
            TokenType::Less => Ok(Value::Bool(l < r)),
            TokenType::GreaterEqual => Ok(Value::Bool(l >= r)),
            TokenType::Greater => Ok(Value::Bool(l > r)),
            _ => unreachable!("Unsupported operator {} for type num", operator)
        },
        (Value::Str(l), Value::Str(r)) => match operator {
            TokenType::Plus => Ok(Value::Str(l + &r)),
            TokenType::EqualEqual => Ok(Value::Bool(l == r)),
            TokenType::NotEqual => Ok(Value::Bool(l != r)),
            _ => unreachable!("Unsupported operator {} for type str", operator)
        }
        (Value::Bool(l), Value::Bool(r)) => match operator {
            TokenType::Ampersand => Ok(Value::Bool(l && r)),
            TokenType::Pipe => Ok(Value::Bool(l || r)),
            _ => unreachable!("Unsupported operator {} for type bool", operator)
        }
        (left, right) => unreachable!("Mismatched types for infix operation: ({left}, {right})")
    }
}

fn eval_prefix(operator: &TokenType, right: Value) -> Result<Value, PropagateValue> {
    match right {
        Value::Bool(bool) => match operator {
            TokenType::Not => Ok(Value::Bool(!bool)),
            _ => unreachable!("Unsupported operator {} for type bool", operator)
        }
        Value::Num(num) => match operator {
            TokenType::Minus => Ok(Value::Num(-num)),
            _ => unreachable!("Unsupported operator {} for type num", operator)
        }
        _ => unreachable!("Mismatched types for prefix operation")
    }
}