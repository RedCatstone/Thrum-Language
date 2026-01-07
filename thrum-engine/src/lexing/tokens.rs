use crate::parsing::ast_structure::Span;

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    // Basic
    LeftParen, RightParen,      // ( )
    LeftBracket, RightBracket,  // [ ]
    LeftBrace, RightBrace,      // { }
    Comma, Semicolon,           // , ;
    Colon, ColonColon,          // : ::

    // Operators
    Equal,                      // =
    Plus, PlusEqual,            // + +=
    Minus, MinusEqual,          // - -=
    Star, StarEqual,            // * *=
    StarStar, StarStarEqual,    // ** **=
    Slash, SlashEqual,          // / /=
    Percent, PercentEqual,      // % %=
    Quest, QuestDot, QuestEqual,// ? ?. ?=

    // Bitwise
    BitNot, BitNotEqual,        // ~ ~=
    BitAnd, BitAndEqual,        // ~& ~&=
    BitOr, BitOrEqual,          // ~| ~|=
    BitXor, BitXorEqual,        // ~^ ~^=
    LeftShift, LeftShiftEqual,  // ~< ~<=
    RightShift, RightShiftEqual,// ~> ~>=
    
    // Logical
    Ampersand, Pipe,            // & |
    EqualEqual,                 // ==
    Exclamation, NotEqual,              // ! !=
    Less, LessEqual,            // < <=
    Greater, GreaterEqual,      // > >=
    
    // Advanced
    RightArrow,                 // ->
    PipeGreater, Caret,         // |> ^
    DotDot, DotDotLess,         // .. ..<
    DotDotDot,                  // ...
    Hashtag,
    Dot(String),                //

    // Literals
    Identifier(String),
    Number(f64),
    StringStart, StringEnd, StringFrag(String),
    Bool(bool),
    Null,

    // Keywords
    If, Else,
    For, In, While, Loop,
    Break, Continue,
    Fn, Return,
    Let, Const, Case, Ensure,
    Mut,
    Struct, Enum,
    Import, From, As,
    Match,

    EndOfFile
}

#[derive(Clone, Debug)]
pub struct TokenSpan {
    pub token: TokenType,

    // where its located in the file, for errors
    pub span: Span,
}
impl TokenSpan {
    pub const END_TOKEN: TokenSpan = TokenSpan {
        token: TokenType::EndOfFile,
        span: Span {
            line: usize::MAX,
            byte_offset: usize::MAX,
            length: 0,
        }
    };
}


pub fn get_keyword(identifier: &str) -> Option<TokenType> {
    match identifier {
        "if" => Some(TokenType::If), "else" => Some(TokenType::Else),
        "and" => Some(TokenType::Ampersand), "or" => Some(TokenType::Pipe),
        "for" => Some(TokenType::For), "in" => Some(TokenType::In), "while" => Some(TokenType::While), "loop" => Some(TokenType::Loop),
        "break" => Some(TokenType::Break), "continue" => Some(TokenType::Continue),
        "fn" => Some(TokenType::Fn), "return" => Some(TokenType::Return),
        "let" => Some(TokenType::Let), "const" => Some(TokenType::Const), "case" => Some(TokenType::Case), "ensure" => Some(TokenType::Ensure),
        "mut" => Some(TokenType::Mut),
        "struct" => Some(TokenType::Struct), "enum" => Some(TokenType::Enum),
        "match" => Some(TokenType::Match),
        "import" => Some(TokenType::Import), "from" => Some(TokenType::From), "as" => Some(TokenType::As),

        "true" => Some(TokenType::Bool(true)), "false" => Some(TokenType::Bool(false)),
        "null" => Some(TokenType::Null),
        _ => None
    }
}