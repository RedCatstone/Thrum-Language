use std::{cmp, collections::HashMap, rc::Rc};

use strum_macros::IntoStaticStr;

use crate::{lexing::tokens::{TokenSpan, TokenType}, nativelib::NativeFn, typing::{TypeID, VarID}};


// this is the main struct that builds the AST (Abstract Syntax Tree)
#[derive(Debug)]
pub struct ExprInfo {
    // the actualy expression info, this is a very long enum.
    pub expression: Expr,

    // starts out as ParserUnknown, but later gets filled in by the typechecker.
    pub typ: TypeKind,

    // where its located in the source code, for better errors
    pub span: Span,
}
impl Expr {
    pub fn to_info(self, span: Span) -> ExprInfo {
        ExprInfo { expression: self, span, typ: TypeKind::ParserUnknown }
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
        // |----------| (span self)
        // 219029812813 + (12321 * 1259812895)
        //                 |----------------| (span other)
        // merged span:
        // |--------------------------------|
        let start_byte = cmp::min(self.byte_offset, other.byte_offset);
        let end_byte = cmp::max(self.byte_offset + self.length, other.byte_offset + other.length);
        Span {
            line: cmp::min(self.line, other.line),
            byte_offset: start_byte,
            length: end_byte - start_byte,
        }
    }
    pub fn to_0_width_right(self) -> Span {
        Span {
            line: self.line,
            byte_offset: self.byte_offset + self.length,
            length: 0
        }
    }
    pub fn invalid() -> Span {
        Span { line: usize::MAX, byte_offset: usize::MAX, length: usize::MAX }
    }
}



// Everything is an expression.
#[derive(Debug, IntoStaticStr)]
pub enum Expr {
    // Primary expressions
    Literal(Value),
    Identifier {
        name: String,
        var_id: Option<VarID>,
    },

    Assign {  // x = 2  or  let x = 2
        pattern: Box<MatchPatternInfo>,
        extra_operator: TokenSpan,
        value: Option<Box<ExprInfo>>,
    },

    Case {  // case ?x = queue.pop()
        pattern: Box<MatchPatternInfo>,
        value: Box<ExprInfo>,
    },

    // { ... }
    Block {
        exprs: Vec<ExprInfo>,
        label: Option<String>,
        drops_vars: Vec<VarID>,
    },

    // Operator expressions
    Prefix {  // !a
        operator: TokenType,
        right: Box<ExprInfo>,
    },
    Infix {  // a + b
        operator: TokenSpan,
        left: Box<ExprInfo>,
        right: Box<ExprInfo>,
    },

    // "a{b}c" -> [Literal("a"), Identifier("b"), Literal("c")]
    TemplateString(Vec<ExprInfo>),
    Tuple(Vec<TupleElement>),  // (1, 2)
    Array(Vec<ExprInfo>),  // [1, 2]

    
    MutRef {  // x^
        expr: Box<ExprInfo>,
    },
    Deref {  // *x
        expr: Box<ExprInfo>,
    },
    

    MemberAccess { // arr.len
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

    // sugar for a normal loop
    While {  // while true { ... }
        condition: Box<ExprInfo>,
        body: Box<ExprInfo>,
        label: String,
    },

    Loop {  // loop { ... }
        body: Box<ExprInfo>,
        label: String,
    },

    EnumDefinition {  // enum Color { Red, Blue, Green(data) }
        name: String,
        enums: Vec<EnumExpression>,
    },

    FnDefinition {  // fn square(x: num) -> { x**2 }
        name: String,
        var_id: Option<VarID>,
        params: Vec<MatchPatternInfo>,
        return_type: TypeKind,
        body: Rc<ExprInfo>,
    },

    Closure {  // |x -> x**2
        params: Vec<MatchPatternInfo>,
        return_type: TypeKind,
        body: Rc<ExprInfo>,
    },

    // return ...
    Return(Box<ExprInfo>),
    // break #label ...
    Break {
        label: Option<String>,
        expr: Box<ExprInfo>,
    },
    // continue #label
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
    // i could also use <void> here, but i want to be more clear
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
pub struct MatchPatternInfo {
    pub pattern: MatchPattern,
    pub typ: TypeKind,
    pub span: Span,
    pub has_place: bool,
    pub can_fail: bool,

    // only the outermost pattern has stuff in this Vec.
    pub vars_defined: Vec<(String, VarID)>,
}
impl MatchPattern {
    pub fn to_info(self, span: Span) -> MatchPatternInfo {
        MatchPatternInfo { pattern: self, span, typ: TypeKind::ParserUnknown, has_place: false, can_fail: false, vars_defined: Vec::new() }
    }
}


#[derive(Debug, Clone, IntoStaticStr)]
pub enum MatchPattern {
    Binding {  // x: num
        name: String,
        mutable: bool,
        typ: TypeKind,
        var_id: Option<VarID>,
    },
    Wildcard,  // _
    Or(Vec<MatchPatternInfo>),
    Array(Vec<MatchPatternInfo>),  // [...]
    Tuple(Vec<TupleMatchPattern>),  // (...)
    EnumVariant {
        path: Vec<String>, // std::Option
        name: String,   // Some
        inner_patterns: Vec<MatchPatternInfo>,
    },
    Literal(Value),
    Conditional {
        pattern: Box<MatchPatternInfo>,
        body: Rc<ExprInfo>,
    },

    PlaceIdentifier {
        name: String,
        var_id: Option<VarID>
    },
    PlaceDeref {
        name: String,
        var_id: Option<VarID>
    },
    PlaceIndex {
        left: Rc<ExprInfo>,
        index: Rc<ExprInfo>
    }
}



#[derive(Debug)]
pub struct TupleElement {
    pub label: String,
    pub expr: ExprInfo,
}
#[derive(Debug, Clone)]
pub struct TupleMatchPattern {
    pub label: String,
    pub pattern: MatchPatternInfo,
}
#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub label: String,
    pub typ: TypeKind,
}



#[derive(Debug)]
pub struct MatchArm {
    pub pattern: MatchPatternInfo,
    pub body: ExprInfo,
}

#[derive(Debug)]
pub struct EnumExpression {
    pub name: String,
    pub inner_types: Vec<MatchPatternInfo>,
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
                entry.prune(type_lookup)
                // type_lookup.insert(*id, pruned.clone());
            }
        else { self.clone() }
    }
}

#[derive(Clone)]
pub enum DefinedTypeKind {
    Enum {
        name: String,
        inner_types: HashMap<String, Vec<MatchPatternInfo>>,
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