#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    // Basic
    LeftParen, RightParen,      // ( )
    LeftBracket, RightBracket,  // [ ]
    LeftBrace, RightBrace,      // { }
    Comma, Dot, Semicolon,      // , . ;
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
    Let, Const, Case,
    Mut,
    Struct, Enum,
    Import, From, As,
    Match,

    EndOfFile
}

#[derive(Clone, Debug)]
pub struct LexerToken {
    pub token_type: TokenType,
    pub byte_start: usize,
    pub line: usize,
    pub length: usize,
}


pub fn get_keyword(identifier: &str) -> Option<TokenType> {
    match identifier {
        "if" => Some(TokenType::If), "else" => Some(TokenType::Else),
        "and" => Some(TokenType::Ampersand), "or" => Some(TokenType::Pipe),
        "for" => Some(TokenType::For), "in" => Some(TokenType::In), "while" => Some(TokenType::While), "loop" => Some(TokenType::Loop),
        "break" => Some(TokenType::Break), "continue" => Some(TokenType::Continue),
        "fn" => Some(TokenType::Fn), "return" => Some(TokenType::Return),
        "let" => Some(TokenType::Let), "const" => Some(TokenType::Const), "case" => Some(TokenType::Case),
        "mut" => Some(TokenType::Mut),
        "struct" => Some(TokenType::Struct), "enum" => Some(TokenType::Enum),
        "match" => Some(TokenType::Match),
        "import" => Some(TokenType::Import), "from" => Some(TokenType::From), "as" => Some(TokenType::As),

        "true" => Some(TokenType::Bool(true)), "false" => Some(TokenType::Bool(false)),
        "null" => Some(TokenType::Null),
        _ => None
    }
}