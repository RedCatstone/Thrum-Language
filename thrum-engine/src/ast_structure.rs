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
    
    Let {  // let x = 2
        pattern: AssignablePattern,
        value: Box<TypedExpr>,
        alternative: Option<Box<TypedExpr>>,
    },

    Assign {  // x = 2
        left: Box<AssignablePattern>,
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
    Tuple(Vec<TypedExpr>),  // (1, 2)
    Array(Vec<TypedExpr>),  // [1, 2]

    // x^
    MutRef {
        expr: Box<TypedExpr>,
    },
    Deref {
        expr: Box<TypedExpr>,
    },
    
    // Option::Some
    Path(Vec<String>),

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
    
    // sugar
    IfLet {  // if let [x, 2] = [1, 2] { ... }
        pattern: AssignablePattern,
        value: Box<TypedExpr>,
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
    },

    Loop {
        body: Box<TypedExpr>,
    },

    EnumDefinition {
        name: String,
        enums: Vec<EnumExpression>,
    },

    FnDefinition {  // fn square(x: num) -> x**2
        name: String,
        params: Vec<AssignablePattern>,
        return_type: TypeKind,
        body: Rc<TypedExpr>,
    },

    Closure {  // x -> x**2
        params: Vec<AssignablePattern>,
        return_type: TypeKind,
        body: Rc<TypedExpr>,
    },

    Return(Box<TypedExpr>),
    Break {
        expr: Box<TypedExpr>,
    },

    // Semicolons are void expressions.
    Void,

    ParserTempTypeAnnotation(AssignablePattern),
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
            (l, r) => panic!("Cannot compare {l} with {r}"),
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool { self.partial_cmp(other) == Some(std::cmp::Ordering::Equal) }
}


#[derive(Debug, Clone)]
pub enum AssignablePattern {
    Binding {  // x: num
        name: String,
        typ: TypeKind,
    },
    Wildcard,  // _
    Array(Vec<AssignablePattern>),  // [...]
    Tuple(Vec<AssignablePattern>),  // (...)
    EnumVariant {
        path: Vec<String>, // std::Option
        name: String,   // Some
        inner_patterns: Vec<AssignablePattern>,
    },
    Literal(Value),
    Or(Vec<AssignablePattern>),
    Conditional {
        pattern: Box<AssignablePattern>,
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
    pub pattern: AssignablePattern,
    pub body: TypedExpr,
}

#[derive(Debug)]
pub struct EnumExpression {
    pub name: String,
    pub inner_types: Vec<AssignablePattern>,
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

#[derive(Clone)]
pub enum DefinedTypeKind {
    Enum {
        inner_types: HashMap<String, Vec<AssignablePattern>>,
    }
}