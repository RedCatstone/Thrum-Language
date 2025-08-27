use std::fmt;
use crate::{ast_structure::{BindingPattern, Expr, MatchPattern, TypedExpr}, tokens::{Token, TokenType}, ast_structure::TypeKind};




impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Literals with data
            TokenType::Identifier(s) => write!(f, "{}", s),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::StringFrag(s) => write!(f, "\"{}\"", s),
            TokenType::Bool(b) => write!(f, "{}", b),
            TokenType::Null => write!(f, "null"),

            // Basic punctuation
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Colon => write!(f, ":"),
            TokenType::ColonColon => write!(f, "::"),

            // Operators
            TokenType::Equal => write!(f, "="),
            TokenType::Plus => write!(f, "+"),
            TokenType::PlusEqual => write!(f, "+="),
            TokenType::Minus => write!(f, "-"),
            TokenType::MinusEqual => write!(f, "-="),
            TokenType::Star => write!(f, "*"),
            TokenType::StarEqual => write!(f, "*="),
            TokenType::StarStar => write!(f, "**"),
            TokenType::StarStarEqual => write!(f, "**="),
            TokenType::Slash => write!(f, "/"),
            TokenType::SlashEqual => write!(f, "/="),
            TokenType::Percent => write!(f, "%"),
            TokenType::PercentEqual => write!(f, "%="),
            TokenType::Quest => write!(f, "?"),
            TokenType::QuestDot => write!(f, "?."),
            TokenType::QuestQuest => write!(f, "??"),
            TokenType::QuestQuestEqual => write!(f, "??="),
            
            // Bitwise
            TokenType::BitNot => write!(f, "~!"),
            TokenType::BitNotEqual => write!(f, "~!="),
            TokenType::BitAnd => write!(f, "~&"),
            TokenType::BitAndEqual => write!(f, "~&="),
            TokenType::BitOr => write!(f, "~|"),
            TokenType::BitOrEqual => write!(f, "~|="),
            TokenType::BitXor => write!(f, "~^"),
            TokenType::BitXorEqual => write!(f, "~^="),
            TokenType::LeftShift => write!(f, "~<"),
            TokenType::LeftShiftEqual => write!(f, "~<="),
            TokenType::RightShift => write!(f, "~>"),
            TokenType::RightShiftEqual => write!(f, "~>="),
            
            // Logical
            TokenType::And => write!(f, "&"),
            TokenType::Or => write!(f, "|"),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::Not => write!(f, "!"),
            TokenType::NotEqual => write!(f, "!="),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            
            // Advanced
            TokenType::RightArrow => write!(f, "->"),
            TokenType::PipeGreater => write!(f, "|>"),
            TokenType::Caret => write!(f, "^"),
            TokenType::DotDot => write!(f, ".."),
            TokenType::DotDotLess => write!(f, "..<"),
            TokenType::DotDotDot => write!(f, "..."),

            // String parts (descriptive)
            TokenType::StringStart => write!(f, "<StringStart>"),
            TokenType::StringEnd => write!(f, "<StringEnd>"),

            // Keywords
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::For => write!(f, "for"),
            TokenType::In => write!(f, "in"),
            TokenType::While => write!(f, "while"),
            TokenType::Break => write!(f, "break"),
            TokenType::Continue => write!(f, "continue"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::Return => write!(f, "return"),
            TokenType::Let => write!(f, "let"),
            TokenType::Const => write!(f, "const"),
            TokenType::Struct => write!(f, "struct"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::Import => write!(f, "import"),
            TokenType::From => write!(f, "from"),
            TokenType::As => write!(f, "as"),
            TokenType::Match => write!(f, "match"),

            TokenType::EndOfFile => write!(f, "<EndOfFile>"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_type)
    }
}






impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Num => write!(f, "num"),
            TypeKind::Str => write!(f, "str"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::ParserUnknown => write!(f, "unknown"),
            TypeKind::Error => write!(f, "error"),
            TypeKind::Arr(typ) => write!(f, "arr<{}>", typ),
            TypeKind::Tup(types) => write!(f, "({})", types.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")),
            TypeKind::Fn { param_types, return_type } => {
                write!(f, "fn<({}) -> {:?}>", param_types.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "), return_type)
            }
            TypeKind::Struct { name, inner_types } => {
                if inner_types.is_empty() {
                    write!(f, "struct({})", name)
                } else {
                    write!(f, "struct({}<{}>)", name, inner_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
                }
            }
            TypeKind::Enum { name } => {
                write!(f, "enum({})", name)
            }
            TypeKind::Inference(id) => write!(f, "inference<{}>", id),
        }
    }
}

impl fmt::Debug for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            format_recursive(self, f, 0, "", true)
        } else {
            write!(f, "{:?}[{}]", self.expression, self.typ) // Compact format for `{:?}`
        }
    }
}

// The recursive workhorse for pretty-printing.
fn format_recursive(eat: &TypedExpr, f: &mut fmt::Formatter, indent: usize, prefix: &str, is_last: bool) -> fmt::Result {
    let i = "  ".repeat(indent);
    let branch = if indent > 0 { if is_last { "└─ " } else { "├─ " } } else { "" };
    let type_info = format!("[{}]", eat.typ);

    match &eat.expression {
        Expr::Literal(val) => writeln!(f, "{i}{branch}{prefix}Literal({val:?}) {type_info}")?,
        Expr::Identifier(name) => writeln!(f, "{i}{branch}{prefix}Identifier(\"{name}\") {type_info}")?,
        
        Expr::Let { pattern, value } => {
            writeln!(f, "{i}{branch}{prefix}Let (pattern: {pattern:?}) {type_info}")?;
            format_recursive(value, f, indent + 1, "value: ", true)?;
        }
        Expr::Block(body) => {
            writeln!(f, "{i}{branch}{prefix}Block {type_info}")?;
            let len = body.len();
            for (idx, expr) in body.iter().enumerate() {
                format_recursive(expr, f, indent + 1, "", idx == len - 1)?;
            }
        }
        Expr::Infix { left, operator, right } => {
            writeln!(f, "{i}{branch}{prefix}Infix({operator}) {type_info}")?;
            format_recursive(left, f, indent + 1, "left: ", false)?;
            format_recursive(right, f, indent + 1, "right: ", true)?;
        }
        Expr::Prefix { operator, right } => {
            writeln!(f, "{i}{branch}{prefix}Prefix({operator}) {type_info}")?;
            format_recursive(right, f, indent + 1, "right: ", true)?;
        }
        Expr::Call { callee: function, arguments } => {
            writeln!(f, "{i}{branch}{prefix}Call {type_info}")?;
            let len = arguments.len();
            format_recursive(function, f, indent + 1, "func: ", len == 0)?;
            for (idx, arg) in arguments.iter().enumerate() {
                format_recursive(arg, f, indent + 1, "arg: ", idx == len - 1)?;
            }
        }
        Expr::If { condition, consequence, alternative } => {
            writeln!(f, "{i}{branch}{prefix}If {type_info}")?;
            let has_alt = alternative.is_some();
            format_recursive(condition, f, indent + 1, "cond: ", false)?;
            format_recursive(consequence, f, indent + 1, "then: ", !has_alt)?;
            if let Some(alt) = alternative {
                format_recursive(alt, f, indent + 1, "else: ", true)?;
            }
        }
        Expr::Fn { params, body, .. } => {
            let params_str = params.iter().map(|p| format!("{p:?}")).collect::<Vec<_>>().join(", ");
            writeln!(f, "{i}{branch}{prefix}Fn (params: [{params_str}]) {type_info}")?;
            format_recursive(body, f, indent + 1, "body: ", true)?;
        }
        Expr::FnDefinition { name, function } => {
            writeln!(f, "{i}{branch}{prefix}FnDefinition (name: \"{name}\") {type_info}")?;
            format_recursive(function, f, indent + 1, "func: ", true)?;
        }
        Expr::Array(elements) | Expr::Tuple(elements) => {
            let name = if matches!(eat.expression, Expr::Array(_)) { "Array" } else { "Tuple" };
            writeln!(f, "{i}{branch}{prefix}{name} {type_info}")?;
            let len = elements.len();
            for (idx, el) in elements.iter().enumerate() {
                format_recursive(el, f, indent + 1, "", idx == len - 1)?;
            }
        }
        // Fallback for any other expression types
        _ => writeln!(f, "{i}{branch}{prefix}{:?} {type_info}", eat.expression)?,
    }
    Ok(())
}

// Custom Debug impl for patterns to make them print cleanly
impl fmt::Debug for BindingPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BindingPattern::NameAndType { name, typ } => {
                if *typ == TypeKind::ParserUnknown { write!(f, "{}", name) }
                else { write!(f, "{}: {}", name, typ) }
            }
            BindingPattern::Array(patterns) => write!(f, "[{:?}]", patterns),
            BindingPattern::Tuple(patterns) => write!(f, "({:?})", patterns),
        }
    }
}

impl fmt::Debug for MatchPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MatchPattern::Literal(value) =>  write!(f, "{:?}", value),
            MatchPattern::Wildcard => write!(f, "_"),
            MatchPattern::Binding(pattern) => write!(f, "{:?}", pattern),
            MatchPattern::Array(patterns) => write!(f, "[{:?}]", patterns),
            MatchPattern::Tuple(patterns) => write!(f, "({:?})", patterns),
            MatchPattern::EnumVariant { path, name, inner_patterns } => {
                write!(f, "{:?}::{}({:?})", path, name, inner_patterns)
            }
        }
    }
}