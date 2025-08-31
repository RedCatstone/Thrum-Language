use std::{collections::HashMap, rc::Rc};

use crate::{nativelib::NativeFn, tokens::TokenType};


pub struct TypedExpr {
    pub typ: TypeKind,
    pub expression: Expr,
}
impl From<Expr> for TypedExpr {
    fn from(expression: Expr) -> Self {
        TypedExpr {
            typ: TypeKind::ParserUnknown,
            expression,
        }
    }
}



// Everything is an expression.
#[derive(Debug)]
pub enum Expr {
    // Primary expressions
    Identifier(String),
    Literal(Value),
    
    Let {  // let x = 2
        pattern: BindingPattern,
        value: Box<TypedExpr>,
    },

    Assign {  // x = 2
        left: Box<TypedExpr>,
        extra_operator: TokenType,
        right: Box<TypedExpr>,
    },

    // { ... }
    Block(Vec<TypedExpr>),

    // Operator expressions
    Prefix {  // !a
        operator: TokenType,
        right: Box<TypedExpr>,
    },
    Infix {  // a + b
        left: Box<TypedExpr>,
        operator: TokenType,
        right: Box<TypedExpr>,
    },

    // "a{b}c" -> [Literal("a"), Identifier("b"), Literal("c")]
    TemplateString(Vec<TypedExpr>),
    
    // Option::Some
    PathedIdentifier(Vec<String>),

    Call {  // x(1, 2)
        callee: Box<TypedExpr>,
        arguments: Vec<TypedExpr>,
    },

    Index {  // arr[1]
        left: Box<TypedExpr>,
        index: Box<TypedExpr>,
    },

    If {  // if (true) ... else ...
        condition: Box<TypedExpr>,
        consequence: Box<TypedExpr>,
        alternative: Option<Box<TypedExpr>>,
    },

    While {
        condition: Box<TypedExpr>,
        body: Box<TypedExpr>,
    },

    Match {  // match response { 2 -> "success", _ -> "nope." }
        match_value: Box<TypedExpr>,
        arms: Vec<MatchArm>,
    },

    EnumDefinition {
        name: String,
        enums: Vec<EnumExpression>,
    },

    FnDefinition {  // fn square(x: num) -> x**2
        name: String,
        params: Vec<BindingPattern>,
        return_type: TypeKind,
        body: Rc<TypedExpr>,
    },

    Return(Box<TypedExpr>),
    Break,

    Tuple(Vec<TypedExpr>),  // (1, 2)
    Array(Vec<TypedExpr>),  // [1, 2]


    ParserTempTypeAnnotation(BindingPattern),
}

// type signature for a native Rust function that can be called by thrum.
// for example `print()`

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),

    // for evaluating the tree
    Arr(Vec<Value>),
    Tup(Vec<Value>),
    Closure {  // x -> x**2
        params: Vec<BindingPattern>,
        return_type: TypeKind,
        body: Rc<TypedExpr>,
    },
    NativeFn(NativeFn),
    Void,
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(l), Value::Num(r)) => l == r,
            (Value::Str(l), Value::Str(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Arr(l), Value::Arr(r)) => l == r,
            (Value::Tup(l), Value::Tup(r)) => l == r,
            _ => false
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum BindingPattern {
    NameAndType {  // x: num
        name: String,
        typ: TypeKind,
        // default: Option<Box<TypedExpression>>,
    },
    Wildcard,
    Array(Vec<BindingPattern>),
    Tuple(Vec<BindingPattern>),
}

#[derive(Debug)]
pub enum MatchPattern {
    Literal(Value),
    Wildcard,
    Binding(BindingPattern),
    Array(Vec<MatchPattern>),
    Tuple(Vec<MatchPattern>),
    EnumVariant {
        path: Vec<String>, // std::Option
        name: String,   // Some
        inner_patterns: Vec<MatchPattern>,
    },
    Or(Vec<MatchPattern>),
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub extra_condition: Option<TypedExpr>,
    pub body: TypedExpr,
}

#[derive(Debug)]
pub struct EnumExpression {
    pub name: String,
    pub inner_types: Vec<BindingPattern>,
}







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
    Enum {
        name: String,
    },

    Inference(usize),
    TypeError,
    
    // 'let', 'FnDefinition', empty block, sometimes if statement
    Void,
    // return for example, type NEVER gets hit.
    Never,

    // parser puts this type everywhere at first. should not exist anymore after typecheck.
    ParserUnknown,

}

#[derive(Clone)]
pub enum DefinedTypeKind {
    Enum {
        inner_types: HashMap<String, Vec<BindingPattern>>,
    }
}