use std::{cmp, collections::HashMap};

use strum_macros::IntoStaticStr;

use crate::{lexing::tokens::{TokenSpan, TokenType}, nativelib::NativeFn, typing::{TypeID, VarID}};


// this is the main struct that builds the AST (Abstract Syntax Tree)
#[derive(Debug, Clone)]
pub struct ExprInfo {
    // the actualy expression info, this is a very long enum.
    pub expression: Expr,

    // starts out as ParserUnknown, but later gets filled in by the typechecker.
    pub typ: TypeKind,

    // where its located in the source code, for better errors
    pub span: Span,
}
impl Expr {
    pub const fn to_info(self, span: Span) -> ExprInfo {
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
    pub fn merge(self, other: Self) -> Self {
        // |----------| (span self)
        // 219029812813 + (12321 * 1259812895)
        //                 |----------------| (span other)
        // merged span:
        // |--------------------------------|
        let start_byte = cmp::min(self.byte_offset, other.byte_offset);
        let end_byte = cmp::max(self.byte_offset + self.length, other.byte_offset + other.length);
        Self {
            line: cmp::min(self.line, other.line),
            byte_offset: start_byte,
            length: end_byte - start_byte,
        }
    }
    pub const fn to_0_width_right(self) -> Self {
        Self {
            line: self.line,
            byte_offset: self.byte_offset + self.length,
            length: 0
        }
    }
    pub const fn invalid() -> Self {
        Self { line: usize::MAX, byte_offset: usize::MAX, length: usize::MAX }
    }
}



// Everything is an expression.
#[derive(Debug, IntoStaticStr, Clone)]
pub enum Expr {
    // Primary expressions
    Literal(Value),
    IdentifierRef {
        name: String,
        mutable: bool,
        var_id: Option<VarID>,
    },

    Assign {  // x = 2  or  let x = 2
        pattern: Box<MatchPatternInfo>,
        extra_operator: Option<TokenType>,
        op_span: Span,
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


    Move {  // x^
        expr: Box<ExprInfo>,
        auto_clone: bool,
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
        variants: Vec<EnumExpression>,
    },

    FnDefinition {  // fn square(x: num) -> { x**2 }
        name: String,
        var_id: Option<VarID>,
        params: Vec<MatchPatternInfo>,
        return_type_annotation: TypeKindInfo,
        body: Box<ExprInfo>,
    },

    Closure {  // |x -> x**2
        params: Vec<MatchPatternInfo>,
        return_type_annotation: TypeKindInfo,
        body: Box<ExprInfo>,
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
    Arr(Vec<Self>),
    Tup(Vec<Self>),

    // raw unsafe pointer for the vm
    // this SHOULD be fully safe, since the borrow checker checked all the lifetimes.
    ValuePointer(*mut Self),
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
            (Self::Num(l), Self::Num(r)) => l.partial_cmp(r),
            (Self::Str(l), Self::Str(r)) => l.partial_cmp(r),
            (Self::Bool(l), Self::Bool(r)) => l.partial_cmp(r),
            (Self::Arr(l), Self::Arr(r)) => l.partial_cmp(r),
            (Self::Tup(l), Self::Tup(r)) => l.partial_cmp(r),
            (Self::Void, Self::Void) => Some(std::cmp::Ordering::Equal),
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

    // only the outermost pattern has stuff in these.
    pub vars_defined: Vec<(String, VarID)>,
    pub covered_cases: Vec<PatternSpace>,
}
impl MatchPattern {
    pub const fn to_info(self, span: Span) -> MatchPatternInfo {
        MatchPatternInfo {
            pattern: self,
            typ: TypeKind::ParserUnknown,
            span,
            vars_defined: Vec::new(),
            covered_cases: Vec::new(),
        }
    }
}


#[derive(Debug, IntoStaticStr, Clone)]
pub enum MatchPattern {
    Binding {  // x: num
        name: String,
        mutable: bool,
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
        body: ExprInfo,
    },

    // PlaceIdentifier {
    //     name: String,
    //     var_id: Option<VarID>
    // },
    PlacePointer {
        expr: ExprInfo
    },
}


#[derive(Debug, Clone)]
pub enum PatternSpace {
    // Num { from: f64, to: f64 },
    Bool { bool: bool },

    // if i cover (false, false) it will be [true, All], [false, true]
    // if i then cover (_, true) it will be [true, false] (from first missingcase, the second one results in no case)
    Tup { inners: Vec<Self> },

    // EnumVariant { name: String, attached_tuple: Box<PatternSpace/*::Tup */> },

    // this represents ALL cases
    // if we are missing ALL and a wildcard subtracts ALL from that -> empty (covered)
    All,
}



#[derive(Debug, Clone)]
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



#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: MatchPatternInfo,
    pub body: ExprInfo,
}

#[derive(Debug, Clone)]
pub struct EnumExpression {
    pub variant_name: String,
    pub attached_tuple: TypeKindInfo,
}







#[derive(Debug, Clone)]
pub struct TypeKindInfo {
    pub typ: TypeKind,
    pub span: Span
}




#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Num,
    Str,
    Bool,

    Arr(Box<Self>),
    Tup(Vec<TupleType>),
    Fn {
        param_types: Vec<Self>,
        return_type: Box<Self>,
    },

    Pointer {
        mutable: bool,
        inner: Box<Self>,
        borrows_var: Option<VarID>
    },

    // type Point = { x: u32, y: u32 }
    CustomType {
        name: String,
        generic_types: Vec<Self>,
    },

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
    pub fn from_custom_type(self) -> Self {
        let Self::CustomType { name, generic_types } = self
        else { unreachable!("this function should only be called with TypeKind::CustomStruct. ({self})") };

        match name.as_str() {
            "num" => Self::Num,
            "bool" => Self::Bool,
            "str" => Self::Str,
            "void" => Self::Void,
            "never" => Self::Never,
            "arr" => {
                assert!(generic_types.len() == 1, "arr type has to have 1 generic. (this panic definitely needs fixing later)");
                Self::Arr(Box::new(generic_types[0].clone()))
            }
            _ => Self::CustomType {
                name,
                generic_types
            }
        }
    }

    pub fn is_never(&self) -> bool {
        *self == Self::Never
    }

    pub fn prune(&self, type_lookup: &HashMap<TypeID, Self>) -> Self {
        if let Self::Inference(id) = self
            && let Some(entry) = type_lookup.get(id) {
                entry.prune(type_lookup)
                // type_lookup.insert(*id, pruned.clone());
            }
        else { self.clone() }
    }

    pub fn is_auto_clone(&self) -> bool {
        match self {
            Self::Num
            | Self::Bool
            | Self::Pointer { .. }
            | Self::Fn { .. } => true,

            Self::Str
            | Self::Arr(..)
            | Self::Tup(..)
            | Self::CustomType { .. }
            | Self::Void
            | Self::Never
            | Self::TypeError => false,

            Self::Inference(..)
            | Self::ParserUnknown => unreachable!("is_auto_clone() should not be called with type {self}")
        }
    }
}

#[derive(Debug)]
pub enum DefinedTypeKind {
    Enum {
        name: String,
        variants: Vec<EnumExpression>
    },

    Native(TypeKind),
}