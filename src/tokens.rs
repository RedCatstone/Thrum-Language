use std::collections::HashMap;
use once_cell::sync::Lazy;

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    // Basic
    LeftParen, RightParen,      // ( )
    LeftBrace, RightBrace,      // { }
    LeftBracket, RightBracket,  // [ ]
    Comma, Dot, Semicolon,      // , . ;
    Colon,                      // :

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
    LeftShift, LeftShiftEqual,  // << <<=
    RightShift, RightShiftEqual,// >> >>=
    
    // Logical
    And, Or,                    // & |
    EqualEqual,                 // ==
    Not, NotEqual,              // ! !=
    Less, LessEqual,            // < <=
    Greater, GreaterEqual,      // > >=
    
    // Advanced
    RightArrow,                 // ->
    PipeGreater, Caret,         // |> ^
    DotDot, DotDotEqual,        // .. ..=
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

    Eof
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
}


pub static KEYWORDS: Lazy<HashMap<String, TokenType>> = Lazy::new(|| {
    let keywords_data: &[(&str, TokenType)] = &[
        ("if", TokenType::If), ("else", TokenType::Else),
        ("and", TokenType::And), ("or", TokenType::Or),
        ("for", TokenType::For), ("in", TokenType::In), ("while", TokenType::While),
        ("break", TokenType::Break), ("continue", TokenType::Continue),
        ("fn", TokenType::Fn), ("return", TokenType::Return),
        ("let", TokenType::Let), ("const", TokenType::Const),
        ("struct", TokenType::Struct), ("enum", TokenType::Enum),
        ("match", TokenType::Match),
        ("import", TokenType::Import), ("from", TokenType::From), ("as", TokenType::As),

        ("true", TokenType::Bool(true)), ("false", TokenType::Bool(false)),
        ("null", TokenType::Null),
    ];
    HashMap::from_iter(keywords_data.iter().map(|(k, t)| (k.to_string(), t.clone())))
});