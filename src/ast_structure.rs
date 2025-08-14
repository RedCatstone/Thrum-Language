use crate::{tokens::TokenType, type_checker::Type};


// Everything is an expression.
#[derive(Debug)]
pub enum Expression {
    // Primary expressions
    Identifier(String),
    Literal(LiteralValue),
    
    Let {  // let x = 2
        pattern: ExpressionPattern,
        value: Box<Expression>,
    },

    // { ... }
    Block {
        body: Vec<Expression>,
        typ: Type,
    },

    // Operator expressions
    Prefix {  // !a
        operator: TokenType,
        right: Box<Expression>,
        typ: Type,
    },
    Infix {  // a + b
        left: Box<Expression>,
        operator: TokenType,
        right: Box<Expression>,
        typ: Type,
    },

    // "a{b}c" -> [Literal("a"), Identifier("b"), Literal("c")]
    TemplateString(Vec<Expression>),
    
    Call {  // x(1, 2)
        function: Box<Expression>,
        params: Vec<Expression>,
        typ: Type,
    },

    Index {  // arr[1]
        left: Box<Expression>,
        index: Box<Expression>,
        typ: Type,
    },

    CurlyNew {  // dict { 1, 2 }
        name: String,
        params: Vec<Expression>,
        typ: Type,
    },

    If {  // if (true) ... else ...
        condition: Box<Expression>,
        consequence: Box<Expression>,
        alternative: Option<Box<Expression>>,
        typ: Type,
    },

    Match {  // match response { 2 -> "success", _ -> "nope." }
        matcher: Box<Expression>,
        cases: Vec<(Expression, Expression)>,
        typ: Type,
    },

    Fn {  // (x: num) -> x**2
        params: Vec<ExpressionPattern>,
        body: Box<Expression>,
        typ: Type,
    },

    FnDefinition {  // fn square(x: num) -> x**2
        name: String,
        function: Box<Expression>,
    },

    Tuple(Vec<Expression>),  // (1, 2)
    Array(Vec<Expression>),  // [1, 2]


    ParserTempPattern(ExpressionPattern),
}

#[derive(Debug)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
}



#[derive(Debug)]
pub enum ExpressionPattern {
    NameAndType {  // x: num
        name: String,
        typ: Type,
    },

    Array(Vec<ExpressionPattern>),
    Tuple(Vec<ExpressionPattern>),
}