use std::fmt;
use crate::{
    ast_structure::{AssignablePattern, Expr, PlaceExpr, TypeKind, TypedExpr, Value},
    to_bytecode::{BytecodeChunk, OpCode},
    tokens::{LexerToken, TokenType},
    vm::{CallFrame, VM}
};




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
            TokenType::QuestEqual => write!(f, "?="),
            
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
            TokenType::Ampersand => write!(f, "&"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::Exclamation => write!(f, "!"),
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
            TokenType::Loop => write!(f, "loop"),
            TokenType::Break => write!(f, "break"),
            TokenType::Continue => write!(f, "continue"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::Return => write!(f, "return"),
            TokenType::Let => write!(f, "let"),
            TokenType::Const => write!(f, "const"),
            TokenType::Mut => write!(f, "mut"),
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

impl fmt::Display for LexerToken {
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
            TypeKind::TypeError => write!(f, "error"),
            TypeKind::Arr(typ) => write!(f, "arr<{}>", typ),
            TypeKind::Tup(types) => write!(f, "({})", join_slice_to_string(types, ", ")),
            TypeKind::Fn { param_types, return_type } => {
                write!(f, "fn<({}) -> {}>", join_slice_to_string(param_types, ", "), return_type)
            }
            TypeKind::Struct { name, inner_types } => {
                if inner_types.is_empty() {
                    write!(f, "struct({})", name)
                } else {
                    write!(f, "struct({}<{}>)", name, join_slice_to_string(inner_types, ", "))
                }
            }
            TypeKind::Enum { name } => write!(f, "enum({})", name),
            TypeKind::MutPointer(x) => write!(f, "mut {}", x),
            TypeKind::Inference(id) => write!(f, "?{}", id),
            TypeKind::Never => write!(f, "never"),
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
        Expr::Identifier { name } => writeln!(f, "{i}{branch}{prefix}Identifier(\"{name}\") {type_info}")?,
        
        Expr::Assign { pattern, extra_operator, value, alternative } => {
            writeln!(f, "{i}{branch}{prefix}Assign (pattern: {pattern}) ({extra_operator}) {type_info}")?;
            format_recursive(value, f, indent + 1, "value: ", true)?;
            if let Some(alt) = alternative {
                format_recursive(alt, f, indent + 1, "alternative: ", true)?;
            }
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
            format_recursive(function, f, indent + 1, "func: ", arguments.len() == 0)?;
            for (i, arg) in arguments.iter().enumerate() {
                format_recursive(arg, f, indent + 1, "arg: ", i == arguments.len() - 1)?;
            }
        }
        Expr::If { condition, consequence, alternative } => {
            writeln!(f, "{i}{branch}{prefix}If {type_info}")?;
            format_recursive(condition, f, indent + 1, "cond: ", false)?;
            format_recursive(consequence, f, indent + 1, "then: ", false)?;
            format_recursive(alternative, f, indent + 1, "else: ", true)?;
        }
        Expr::Match { match_value, arms } => {
            writeln!(f, "{i}{branch}{prefix}Match {type_info}")?;
            format_recursive(match_value, f, indent + 1, "match value: ", false)?;
            for (i, arm) in arms.iter().enumerate() {
                format_recursive(&arm.body, f, indent + 1, &format!("pattern: {:?} arm: ", arm.pattern), i == arms.len() - 1)?;
            }
        }
        Expr::Array(elements) | Expr::Tuple(elements) => {
            let name = if matches!(eat.expression, Expr::Array(_)) { "Array" } else { "Tuple" };
            writeln!(f, "{i}{branch}{prefix}{name} {type_info}")?;
            let len = elements.len();
            for (idx, el) in elements.iter().enumerate() {
                format_recursive(el, f, indent + 1, "", idx == len - 1)?;
            }
        }
        Expr::Loop { body } => {
            writeln!(f, "{i}{branch}{prefix}Loop {type_info}")?;
            format_recursive(body, f, indent + 1, "body: ", true)?;
        }
        Expr::FnDefinition { name, params, return_type, body } => {
            writeln!(f, "{i}{branch}{prefix}FnDefintion({name}) -> {return_type} {type_info}")?;
            for param in params {
                writeln!(f, "{i}{branch}{prefix}param: {param}")?;
            }
            format_recursive(body, f, indent + 1, "body: ", true)?;
        }
        // Fallback for any other expression types
        _ => writeln!(f, "{i}{branch}{prefix}{:?} {type_info}", eat.expression)?,
    }
    Ok(())
}

// Custom Debug impl for patterns to make them print cleanly
impl fmt::Display for AssignablePattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignablePattern::Literal(value) =>  write!(f, "{}", value),
            AssignablePattern::Binding { name, typ } => write!(f, "{}: {}", name, typ),
            AssignablePattern::Array(patterns) => write!(f, "[{}]", join_slice_to_string(patterns, ", ")),
            AssignablePattern::Tuple(patterns) => write!(f, "({})", join_slice_to_string(patterns, ", ")),
            AssignablePattern::Or(patterns) => write!(f, "{}", join_slice_to_string(patterns, " | ")),
            AssignablePattern::EnumVariant { path, name, inner_patterns } => {
                write!(f, "{}::{}({})",
                    path.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "),
                    name,
                    inner_patterns.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")
                )
            }
            AssignablePattern::Wildcard => write!(f, "_"),
            AssignablePattern::Place(place_expr) => write!(f, "place({})", place_expr),
            AssignablePattern::Conditional { pattern, body } => {
                write!(f, "{} if ({:?})", pattern, body)
            }
        }
    }
}
impl fmt::Display for PlaceExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlaceExpr::Identifier(name) => write!(f, "{}", name),
            PlaceExpr::Deref(name) => write!(f, "({})^", name),
            PlaceExpr::Index { left, index } => write!(f, "{:?}[{:?}]", left, index)
        }
    }
}





impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(x) => write!(f, "{}", x),
            Value::Str(x) => write!(f, "\"{}\"", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Arr(x) => write!(f, "[{}]", join_slice_to_string(x, ", ")),
            Value::Tup(x) => write!(f, "({})", join_slice_to_string(x, ", ")),
            Value::ValueStackPointer(i) => write!(f, "mut<{}>", i),
            Value::Closure { chunk_index } => write!(f, "closure<{}>", chunk_index),
            Value::NativeFn(x) => write!(f, "{:?}", x),
            Value::Void => write!(f, "<void>"),
            Value::Empty => write!(f, "<empty>"),
        }
    }
}






impl fmt::Display for BytecodeChunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn how_many_operands_for_op_code(op: &OpCode) -> usize {
            match op {
                OpCode::ArrUnpackCheckJump | OpCode::LocalsFree => 2,
                OpCode::ConstGet | OpCode::LocalGet | OpCode::LocalSet | OpCode::StrTemplate | OpCode::ArrCreate
                | OpCode::TupCreate | OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpBack | OpCode::CallFn
                | OpCode::MakePointer => 1,
                _ => 0,
            }
        }
        let mut strings = Vec::new();

        let mut frame = CallFrame::default();
        loop {
            let op_code = VM::read_next_instruction(&mut frame, &self);
            let mut opnums = Vec::new();

            for _ in 0..how_many_operands_for_op_code(&op_code) {
                if frame.ip >= self.codes.len() - 1 {
                    panic!("Incorrect printing? {op_code:?}, {opnums:?} \nOpCodes - [{}],\nConstants - [{}]\n",
                        strings.join(", "), join_slice_to_string(&self.constants, ", ")
                    )
                }
                opnums.push(VM::read_next_opnum(&mut frame, &self))
            }

            if opnums.is_empty() { strings.push(format!("{:?}", op_code)); }
            else { strings.push(format!("{:?}({})", op_code, join_slice_to_string(&opnums, ", "))); }

            // if we are at the end
            if frame.ip >= self.codes.len() - 1 {
                break;
            }
        }
        write!(f, "OpCodes - [{}],\nConstants - [{}]", strings.join(", "), join_slice_to_string(&self.constants, ", "))
    }
}









pub fn join_slice_to_string<T: ToString>(vec: &[T], join: &str) -> String {
    vec.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(join)
}