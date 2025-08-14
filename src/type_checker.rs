use crate::ast_structure::{Expression, ExpressionPattern, LiteralValue};
use std::{collections::HashMap, fmt};






#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub nullable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    // Primitives
    Num,
    Str,
    Bool,

    // Complex Types
    Arr(Box<Type>),
    Tup(Vec<Type>),
    Fn {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        name: String,
        inner_types: Vec<Type>,
    },
    
    // Type of statements like 'let' or an empty block
    Void,

    // A special type for when we can't figure out the type,
    // to prevent cascading errors. Also used for the 'null' literal
    // before its type is inferred.
    Unknown,
}
impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Num => write!(f, "num"),
            TypeKind::Str => write!(f, "str"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::Unknown => write!(f, "unknown"),
            TypeKind::Arr(t) => write!(f, "arr<{}>", t),
            TypeKind::Tup(ts) => {
                let s: Vec<String> = ts.iter().map(|t| t.to_string()).collect();
                write!(f, "({})", s.join(", "))
            }
            TypeKind::Fn { param_types, return_type } => {
                let p: Vec<String> = param_types.iter().map(|t| t.to_string()).collect();
                write!(f, "fn({}) -> {}", p.join(", "), return_type)
            }
            TypeKind::Struct { name, inner_types } => {
                if inner_types.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let i: Vec<String> = inner_types.iter().map(|t| t.to_string()).collect();
                    write!(f, "{}<{}>", name, i.join(", "))
                }
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.kind, if self.nullable { "?" } else { "" })
    }
}


impl Type {
    // --- Type Constructors ---
    pub fn new(kind: TypeKind, nullable: bool) -> Self { Type { kind, nullable } }
    pub fn number() -> Self { Type::new(TypeKind::Num, false) }
    pub fn string() -> Self { Type::new(TypeKind::Str, false) }
    pub fn bool() -> Self { Type::new(TypeKind::Bool, false) }
    pub fn void() -> Self { Type::new(TypeKind::Void, false) }
    pub fn unknown() -> Self { Type::new(TypeKind::Unknown, false) }
    pub fn null_type() -> Self { Type::new(TypeKind::Unknown, true) }
    pub fn to_nullable(mut self) -> Self { self.nullable = true; self }

    pub fn is_assignable_from(&self, other: &Type) -> bool {
        // nullable type T? can be assigned 'null'
        if self.nullable && other.kind == TypeKind::Unknown && other.nullable { return true; }

        // non-nullable type T cannot be assigned a nullable type T?
        if !self.nullable && other.nullable { return false;  }

        // allow assignment to/from unknown to reduce cascading errors.
        if self.kind == TypeKind::Unknown || other.kind == TypeKind::Unknown { return true; }
        
        // the underlying kinds must be equal.
        self.kind == other.kind && self.nullable == other.nullable
    }

    pub fn from_str(str: &str, inner_types: Vec<Type>) -> Type {
        match str {
            "num" => Type::number(),
            "str" => Type::string(),
            "bool" => Type::bool(),
            "null" => Type::null_type(),
            &_ => Type::new(TypeKind::Struct { name: str.to_string(), inner_types: inner_types }, false)
        }
    }
}








type Scope = HashMap<String, Type>;

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

    pub fn define_identifier(&mut self, name: String, t: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, t);
        }
        else { unreachable!() }
    }

    pub fn lookup_identifier(&self, name: &str) -> Option<Type> {
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
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker { errors: Vec::new(), env: Environment::new() }
    }

    fn add_error(&mut self, message: String) {
        self.errors.push(TypeCheckError { message });
    }

    fn type_mismatch(&mut self, expected: &Type, found: &Type) {
        self.add_error(format!("Type mismatch. Expected {}, found {}.", expected, found));
    }

    pub fn check_program(&mut self, program: &mut Vec<Expression>) {
        self.check_block(program);
    }

    fn check_expression(&mut self, expr: &mut Expression) -> Type {
        let inferred_type = match expr {
            Expression::Literal(val) => self.check_literal(val),
            Expression::Let { pattern, value } => self.check_let(pattern, value),
            Expression::Identifier(name) => self.check_identifier(name),
            Expression::Block { body, .. } => self.check_block(body),
            // Expression::Prefix { operator, right, .. } => self.check_prefix(operator, right),
            // Expression::Infix { left, operator, right, .. } => self.check_infix(left, operator, right),
            // Expression::TemplateString(parts) => self.check_template_string(parts),
            // Expression::Call { function, params, .. } => self.check_call(function, params),
            // Expression::Index { left, index, .. } => self.check_index(left, index),
            // Expression::CurlyNew { name, params, .. } => self.check_curly_new(name, params),
            // Expression::If { condition, consequence, alternative, .. } => self.check_if(condition, consequence, alternative),
            // Expression::Match { matcher, cases, .. } => self.check_match(matcher, cases),
            // Expression::Fn { params, body, .. } => self.check_fn(params, body),
            // Expression::FnDefinition { name, function } => self.check_fn_definition(name, function),
            // Expression::Tuple(elements) => self.check_tuple(elements),
            // Expression::Array(elements) => self.check_array(elements),
            Expression::ParserTempPattern { .. } => { unreachable!() }
            _ => unreachable!()
        };

        self.update_node_type(expr, inferred_type.clone());
        inferred_type
    }

    fn update_node_type(&mut self, expr: &mut Expression, new_type: Type) {
        let node_type = match expr {
            Expression::Block { typ, .. } | Expression::Prefix { typ, .. } |
            Expression::Infix { typ, .. } | Expression::Call { typ, .. } |
            Expression::Index { typ, .. } | Expression::CurlyNew { typ, .. } |
            Expression::If { typ, .. } | Expression::Match { typ, .. } |
            Expression::Fn { typ, .. } => typ,
            _ => unreachable!(),
        };

        if node_type.is_assignable_from(&new_type) { *node_type = new_type; }
        else { self.type_mismatch(node_type, &new_type); }
    }




    fn check_literal(&mut self, val: &LiteralValue) -> Type {
        match val {
            LiteralValue::Number(_) => Type::number(),
            LiteralValue::String(_) => Type::string(),
            LiteralValue::Bool(_) => Type::bool(),
            LiteralValue::Null => Type::null_type(),
        }
    }

    fn check_let(&mut self, pattern: &mut ExpressionPattern, val: &mut Expression) -> Type {
        let value_type = self.check_expression(val);
        self.check_expression_pattern(pattern, &value_type);
        Type::void()
    }

    fn check_expression_pattern(&mut self, pattern: &mut ExpressionPattern, value_type: &Type) {
        match pattern {
            ExpressionPattern::NameAndType { name, typ } => {
                if typ.kind == TypeKind::Unknown {
                    *typ = value_type.clone();
                }
                else if !typ.is_assignable_from(value_type) {
                    self.type_mismatch(typ, value_type);
                }
                self.env.define_identifier(name.clone(), value_type.clone());
            }
            _ => {
                self.add_error(format!("Unsupported pattern type: {:?}", pattern));
            }
        }
    }

    fn check_identifier(&mut self, name: &str) -> Type {
        self.env.lookup_identifier(name).unwrap_or_else(|| {
            self.add_error(format!("Undefined identifier: '{}'", name));
            Type::unknown()
        })
    }

    fn check_block(&mut self, body: &mut Vec<Expression>) -> Type {
        self.env.enter_scope();
        let mut last_type = Type::void();
        for expr in body {
            last_type = self.check_expression(expr);
        }
        self.env.exit_scope();
        last_type
    }
}