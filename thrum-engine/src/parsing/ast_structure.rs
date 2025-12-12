use std::{collections::HashMap, rc::Rc};

use crate::{lexing::tokens::TokenType, nativelib::NativeFn};


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
impl From<Expr> for Box<TypedExpr> {
    fn from(expr: Expr) -> Self {
        Box::new(expr.into())
    }
}



// Everything is an expression.
#[derive(Debug)]
pub enum Expr {
    // Primary expressions
    Literal(Value),
    Identifier {
        name: String,
    },

    Assign {  // x = 2  or  let x = 2
        pattern: Box<MatchPattern>,
        extra_operator: TokenType,
        value: Option<Box<TypedExpr>>,
    },

    Case {
        pattern: Box<MatchPattern>,
        value: Box<TypedExpr>,
    },

    // { ... }
    Block(Vec<TypedExpr>),

    // Operator expressions
    Prefix {  // !a
        operator: TokenType,
        right: Box<TypedExpr>,
    },
    Infix {  // a + b
        operator: TokenType,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },

    // "a{b}c" -> [Literal("a"), Identifier("b"), Literal("c")]
    TemplateString(Vec<TypedExpr>),
    Tuple(Vec<TypedExpr>),  // (1, 2)
    Array(Vec<TypedExpr>),  // [1, 2]

    // x^
    MutRef {
        expr: Box<TypedExpr>,
    },
    Deref {
        expr: Box<TypedExpr>,
    },
    
    // arr.len
    MemberAccess {
        left: Box<TypedExpr>,
        member: String,
    },
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
        alternative: Box<TypedExpr>,  // void if not present
    },
    
    Match {  // match response { 2 -> "success", _ -> "nope." }
        match_value: Box<TypedExpr>,
        arms: Vec<MatchArm>,
    },

    // sugar
    While {
        condition: Box<TypedExpr>,
        body: Box<TypedExpr>,
        label: String,
    },

    Loop {
        body: Box<TypedExpr>,
        label: String,
    },

    EnumDefinition {
        name: String,
        enums: Vec<EnumExpression>,
    },

    FnDefinition {  // fn square(x: num) -> x**2
        name: String,
        params: Vec<MatchPattern>,
        return_type: TypeKind,
        body: Rc<TypedExpr>,
    },

    Closure {  // x -> x**2
        params: Vec<MatchPattern>,
        return_type: TypeKind,
        body: Rc<TypedExpr>,
    },

    Return(Box<TypedExpr>),
    Break {
        label: Option<String>,
        expr: Box<TypedExpr>,
    },
    Continue {
        label: Option<String>,
    },

    // Semicolons are void expressions.
    Void,

    ParserTempTypeAnnotation(MatchPattern),
}

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),

    // for evaluating the tree
    Arr(Rc<Vec<Value>>),
    Tup(Rc<Vec<Value>>),
    ValueStackPointer(usize),
    NativeFn(NativeFn),
    Closure {
        chunk_index: usize,
    },

    // for functions that return nothing
    Void,

    // for empty local slots in the vm
    // i could also use <void> here, but i wonna be more clear
    Empty,
}
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Num(l), Value::Num(r)) => l.partial_cmp(r),
            (Value::Str(l), Value::Str(r)) => l.partial_cmp(r),
            (Value::Bool(l), Value::Bool(r)) => l.partial_cmp(r),
            (Value::Arr(l), Value::Arr(r)) => l.partial_cmp(r),
            (Value::Tup(l), Value::Tup(r)) => l.partial_cmp(r),
            (Value::Void, Value::Void) => Some(std::cmp::Ordering::Equal),
            (l, r) => panic!("Cannot compare {l} with {r}"),
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool { self.partial_cmp(other) == Some(std::cmp::Ordering::Equal) }
}


#[derive(Debug, Clone)]
pub enum MatchPattern {
    Binding {  // x: num
        name: String,
        typ: TypeKind,
    },
    Wildcard,  // _
    Or(Vec<MatchPattern>),
    Array(Vec<MatchPattern>),  // [...]
    Tuple(Vec<MatchPattern>),  // (...)
    EnumVariant {
        path: Vec<String>, // std::Option
        name: String,   // Some
        inner_patterns: Vec<MatchPattern>,
    },
    Literal(Value),
    Conditional {
        pattern: Box<MatchPattern>,
        body: Rc<TypedExpr>,
    },

    Place(PlaceExpr),
}

#[derive(Debug, Clone)]
pub enum PlaceExpr {
    Identifier(String),
    Deref(String),
    Index { left: Rc<TypedExpr>, index: Rc<TypedExpr> },
}



#[derive(Debug)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: TypedExpr,
}

#[derive(Debug)]
pub struct EnumExpression {
    pub name: String,
    pub inner_types: Vec<MatchPattern>,
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

    MutPointer(Box<TypeKind>),

    Inference(usize),
    TypeError,
    
    // 'let', 'FnDefinition', empty block, sometimes if statement
    Void,
    // return for example, type NEVER gets hit.
    Never,

    // parser puts this type everywhere at first. should not exist anymore after typecheck.
    ParserUnknown,

}
impl TypeKind {
    pub fn is_never(&self) -> bool {
        *self == Self::Never
    }
}

#[derive(Clone)]
pub enum DefinedTypeKind {
    Enum {
        name: String,
        inner_types: HashMap<String, Vec<MatchPattern>>,
    },

    Native(TypeKind),
}

impl DefinedTypeKind {
    pub fn to_typekind(self) -> TypeKind {
        match self {
            Self::Native(typ) => typ,
            Self::Enum { name, .. } => TypeKind::Enum { name },
        }
    }
}