#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    // Basic
    LeftParen, RightParen,      // ( )
    LeftBrace, RightBrace,      // { }
    LeftBracket, RightBracket,  // [ ]
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
    Quest, QuestDot,            // ?
    QuestQuest, QuestQuestEqual,// ?? ??=
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
    Not, NotEqual,              // ! !=
    Less, LessEqual,            // < <=
    Greater, GreaterEqual,      // > >=
    
    // Advanced
    RightArrow,                 // ->
    PipeGreater, Caret,         // |> ^
    DotDot, DotDotLess,         // .. ..<
    DotDotDot,                  // ...

    // Literals
    Identifier(String),
    Number(f64),
    StringStart, StringEnd, StringFrag(String),
    Bool(bool),
    Null,

    // Keywords
    If, Else,
    For, In, While,
    Break, Continue,
    Fn, Return,
    Let, Const,
    Struct, Enum,
    Import, From, As,
    Match,

    EndOfFile
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
}


pub fn get_keyword(identifier: &str) -> Option<TokenType> {
    match identifier {
        "if" => Some(TokenType::If), "else" => Some(TokenType::Else),
        "and" => Some(TokenType::Ampersand), "or" => Some(TokenType::Pipe),
        "for" => Some(TokenType::For), "in" => Some(TokenType::In), "while" => Some(TokenType::While),
        "break" => Some(TokenType::Break), "continue" => Some(TokenType::Continue),
        "fn" => Some(TokenType::Fn), "return" => Some(TokenType::Return),
        "let" => Some(TokenType::Let), "const" => Some(TokenType::Const),
        "struct" => Some(TokenType::Struct), "enum" => Some(TokenType::Enum),
        "match" => Some(TokenType::Match),
        "import" => Some(TokenType::Import), "from" => Some(TokenType::From), "as" => Some(TokenType::As),

        "true" => Some(TokenType::Bool(true)), "false" => Some(TokenType::Bool(false)),
        "null" => Some(TokenType::Null),
        _ => None
    }
}