use std::{cmp::{self, min}, collections::HashMap, rc::Rc};

use strum_macros::IntoStaticStr;

use crate::{lexing::tokens::TokenType, nativelib::NativeFn, typing::TypeID};

pub struct ExprInfo {
    pub expression: Expr,
    pub typ: TypeKind,

    // where its located in the file, for errors
    pub span: Span,
}
impl Expr {
    pub fn to_info(self, span: Span) -> ExprInfo {
        self.to_info_with_type(span, TypeKind::ParserUnknown)
    }
    pub fn to_info_with_type(self, span: Span, typ: TypeKind) -> ExprInfo {
        ExprInfo { expression: self, span, typ }
    }
}


#[derive(Clone, Copy, Debug, Default)]
pub struct Span {
    pub line: usize,
    pub byte_offset: usize,
    pub length: usize,
}
impl Span {
    pub fn merge(self, other: Span) -> Span {
        match self.byte_offset.cmp(&other.byte_offset) {
            cmp::Ordering::Less => Span {
                line: self.line,
                byte_offset: self.byte_offset,
                length: (other.byte_offset + other.length) - self.byte_offset
            },
            cmp::Ordering::Greater => other.merge(self),
            cmp::Ordering::Equal => unreachable!("tried to merge 2 spans with the same byte_offset")
        }
    }
    pub fn to_0_width_right(self) -> Span {
        Span {
            line: self.line,
            byte_offset: self.byte_offset + self.length,
            length: 0
        }
    }
}



// Everything is an expression.
#[derive(Debug, IntoStaticStr)]
pub enum Expr {
    // Primary expressions
    Literal(Value),
    Identifier {
        name: String,
    },

    Assign {  // x = 2  or  let x = 2
        pattern: Box<MatchPattern>,
        extra_operator: TokenType,
        value: Option<Box<ExprInfo>>,
    },

    Case {
        pattern: Box<MatchPattern>,
        value: Box<ExprInfo>,
    },

    // { ... }
    Block(Vec<ExprInfo>),

    // Operator expressions
    Prefix {  // !a
        operator: TokenType,
        right: Box<ExprInfo>,
    },
    Infix {  // a + b
        operator: TokenType,
        left: Box<ExprInfo>,
        right: Box<ExprInfo>,
    },

    // "a{b}c" -> [Literal("a"), Identifier("b"), Literal("c")]
    TemplateString(Vec<ExprInfo>),
    Tuple(Vec<TupleElement>),  // (1, 2)
    Array(Vec<ExprInfo>),  // [1, 2]

    // x^
    MutRef {
        expr: Box<ExprInfo>,
    },
    Deref {
        expr: Box<ExprInfo>,
    },
    
    // arr.len
    MemberAccess {
        left: Box<ExprInfo>,
        member: String,
        resolved_index: Option<usize>,
    },
    // Option::Some
    TypePath(Vec<String>),

    Call {  // x(1, 2)
        callee: Box<ExprInfo>,
        arguments: Vec<ExprInfo>,
    },

    Index {  // arr[1]
        left: Box<ExprInfo>,
        index: Box<ExprInfo>,
    },

    If {  // if true { ... } else ...
        condition: Box<ExprInfo>,
        then: Box<ExprInfo>,
        alt: Box<ExprInfo>,  // void if not present
    },
    
    Ensure {  // ensure true else { ... }
        condition: Box<ExprInfo>,
        alt: Box<ExprInfo>,
        then: Box<ExprInfo>
    },
    
    Match {  // match response { 2 -> "success", _ -> "nope." }
        match_value: Box<ExprInfo>,
        arms: Vec<MatchArm>,
    },

    // sugar
    While {
        condition: Box<ExprInfo>,
        body: Box<ExprInfo>,
        label: String,
    },

    Loop {
        body: Box<ExprInfo>,
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
        body: Rc<ExprInfo>,
    },

    Closure {  // x -> x**2
        params: Vec<MatchPattern>,
        return_type: TypeKind,
        body: Rc<ExprInfo>,
    },

    Return(Box<ExprInfo>),
    Break {
        label: Option<String>,
        expr: Box<ExprInfo>,
    },
    Continue {
        label: Option<String>,
    },

    // Semicolons are void expressions.
    Void,
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
    Tuple(Vec<TupleMatchPattern>),  // (...)
    EnumVariant {
        path: Vec<String>, // std::Option
        name: String,   // Some
        inner_patterns: Vec<MatchPattern>,
    },
    Literal(Value),
    Conditional {
        pattern: Box<MatchPattern>,
        body: Rc<ExprInfo>,
    },

    Place(PlaceExpr),
}
#[derive(Debug)]
pub struct TupleElement {
    pub label: String,
    pub expr: ExprInfo,
}
#[derive(Debug, Clone)]
pub struct TupleMatchPattern {
    pub label: String,
    pub pattern: MatchPattern,
}
#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub label: String,
    pub typ: TypeKind,
}

#[derive(Debug, Clone)]
pub enum PlaceExpr {
    Identifier(String),
    Deref(String),
    Index { left: Rc<ExprInfo>, index: Rc<ExprInfo> },
}



#[derive(Debug)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: ExprInfo,
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
    Tup(Vec<TupleType>),
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

    Inference(TypeID),
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

    pub fn prune(&self, type_lookup: &HashMap<TypeID, TypeKind>) -> TypeKind {
        if let TypeKind::Inference(id) = self
            && let Some(entry) = type_lookup.get(id) {
                let pruned = entry.prune(&type_lookup);
                // type_lookup.insert(*id, pruned.clone());
                pruned
            }
        else { self.clone() }
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