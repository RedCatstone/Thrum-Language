use crate::{tokens::TokenType, type_checker::TypeKind};


pub struct TypedExpression {
    pub typ: TypeKind,
    pub expression: Expression,
}
impl From<Expression> for TypedExpression {
    fn from(expression: Expression) -> Self {
        TypedExpression {
            typ: TypeKind::ParserUnknown,
            expression,
        }
    }
}



// Everything is an expression.
#[derive(Debug)]
pub enum Expression {
    // Primary expressions
    Identifier(String),
    Literal(LiteralValue),
    
    Let {  // let x = 2
        pattern: ExpressionPattern,
        value: Box<TypedExpression>,
    },

    Assign {
        left: Box<TypedExpression>,
        operator: TokenType,
        right: Box<TypedExpression>,
    },

    // { ... }
    Block(Vec<TypedExpression>),

    // Operator expressions
    Prefix {  // !a
        operator: TokenType,
        right: Box<TypedExpression>,
    },
    Infix {  // a + b
        left: Box<TypedExpression>,
        operator: TokenType,
        right: Box<TypedExpression>,
    },

    // "a{b}c" -> [Literal("a"), Identifier("b"), Literal("c")]
    TemplateString(Vec<TypedExpression>),
    
    FnCall {  // x(1, 2)
        function: Box<TypedExpression>,
        arguments: Vec<TypedExpression>,
    },

    Index {  // arr[1]
        left: Box<TypedExpression>,
        index: Box<TypedExpression>,
    },

    CurlyNew {  // dict { 1, 2 }
        name: String,
        params: Vec<TypedExpression>,
    },

    If {  // if (true) ... else ...
        condition: Box<TypedExpression>,
        consequence: Box<TypedExpression>,
        alternative: Option<Box<TypedExpression>>,
    },

    Match {  // match response { 2 -> "success", _ -> "nope." }
        matcher: Box<TypedExpression>,
        cases: Vec<(TypedExpression, TypedExpression)>,
    },

    Fn {  // x -> x**2
        params: Vec<ExpressionPattern>,
        return_value: Option<ExpressionPattern>,
        body: Box<TypedExpression>,
    },

    FnDefinition {  // fn square(x: num) -> x**2
        name: String,
        function: Box<TypedExpression>,
    },

    Tuple(Vec<TypedExpression>),  // (1, 2)
    Array(Vec<TypedExpression>),  // [1, 2]


    ParserTempPattern(ExpressionPattern),
}

#[derive(Debug)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Bool(bool),
}



pub enum ExpressionPattern {
    NameAndType {  // x: num
        name: String,
        typ: TypeKind,
        // default: Option<Box<TypedExpression>>,
    },

    Array(Vec<ExpressionPattern>),
    Tuple(Vec<ExpressionPattern>),
}