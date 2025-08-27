use std::collections::HashMap;

use crate::{tokens::TokenType};


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
    Literal(LiteralValue),
    
    Let {  // let x = 2
        pattern: BindingPattern,
        value: Box<TypedExpr>,
    },

    Assign {
        left: Box<TypedExpr>,
        operator: TokenType,
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
    TypePath(Vec<String>),

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

    Match {  // match response { 2 -> "success", _ -> "nope." }
        match_value: Box<TypedExpr>,
        cases: Vec<MatchArm>,
    },

    EnumDefinition {
        name: String,
        enums: Vec<EnumExpression>,
    },

    Fn {  // x -> x**2
        params: Vec<BindingPattern>,
        return_value: Option<BindingPattern>,
        body: Box<TypedExpr>,
    },

    FnDefinition {  // fn square(x: num) -> x**2
        name: String,
        function: Box<TypedExpr>,
    },

    Tuple(Vec<TypedExpr>),  // (1, 2)
    Array(Vec<TypedExpr>),  // [1, 2]


    ParserTempPattern(BindingPattern),
}

#[derive(Debug)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Bool(bool),
}


#[derive(Clone, PartialEq)]
pub enum BindingPattern {
    NameAndType {  // x: num
        name: String,
        typ: TypeKind,
        // default: Option<Box<TypedExpression>>,
    },

    Array(Vec<BindingPattern>),
    Tuple(Vec<BindingPattern>),
}


pub enum MatchPattern {
    Literal(LiteralValue),
    Wildcard,
    Binding(BindingPattern),
    Array(Vec<MatchPattern>),
    Tuple(Vec<MatchPattern>),
    EnumVariant {
        path: Vec<String>, // std::Option
        name: String,   // Some
        inner_patterns: Vec<MatchPattern>,
    },
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
    
    // 'let', 'FnDefinition', empty block, sometimes if statement
    Void,

    // parser puts this type everywhere at first.
    ParserUnknown,

    Error,
}

#[derive(Clone)]
pub enum DefinedTypeKind {
    Enum {
        inner_types: HashMap<String, Vec<BindingPattern>>,
    }
}