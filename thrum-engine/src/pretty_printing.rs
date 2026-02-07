use std::fmt::{self, Write};
use crate::{
    Program, ProgramError, lexing::tokens::{TokenSpan, TokenType}, parsing::ast_structure::{Expr, ExprInfo, MatchPattern, MatchPatternInfo, PatternSpace, TupleMatchPattern, TupleType, TypeKind, Value}, vm_compiling::{BytecodeChunk, OpCode}, vm_evaluating::{CallFrame, VM}
};




impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Literals with data
            Self::Identifier(s) => write!(f, "ident<{s}>"),
            Self::Number(n) => write!(f, "{n}"),
            Self::StringFrag(s) => write!(f, "\"{s}\""),
            Self::Dot(s) => write!(f, ".{s}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Null => write!(f, "null"),

            // Basic punctuation
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBracket => write!(f, "["),
            Self::RightBracket => write!(f, "]"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::ColonColon => write!(f, "::"),

            // Operators
            Self::Equal { extra_operator } => {
                if let Some(extra_op) = extra_operator { write!(f, "{extra_op}=") }
                else { write!(f, "=") }
            }
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::StarStar => write!(f, "**"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Quest => write!(f, "?"),
            Self::QuestDot => write!(f, "?."),
            
            // Bitwise
            Self::BitNot => write!(f, "~!"),
            Self::BitAnd => write!(f, "~&"),
            Self::BitOr => write!(f, "~|"),
            Self::BitXor => write!(f, "~^"),
            Self::LeftShift => write!(f, "~<"),
            Self::RightShift => write!(f, "~>>"),
            
            // Logical
            Self::Ampersand => write!(f, "&"),
            Self::Pipe => write!(f, "|"),
            Self::EqualEqual => write!(f, "=="),
            Self::Exclamation => write!(f, "!"),
            Self::NotEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            
            // Advanced
            Self::RightArrow => write!(f, "->"),
            Self::TildeArrow => write!(f, "~>"),
            Self::PipeGreater => write!(f, "|>"),
            Self::Caret => write!(f, "^"),
            Self::DotDot => write!(f, ".."),
            Self::DotDotLess => write!(f, "..<"),
            Self::DotDotDot => write!(f, "..."),
            Self::Hashtag=> write!(f, "#"),

            // String parts (descriptive)
            Self::StringStart => write!(f, "<StringStart>"),
            Self::StringEnd => write!(f, "<StringEnd>"),

            // Keywords
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::For => write!(f, "for"),
            Self::In => write!(f, "in"),
            Self::While => write!(f, "while"),
            Self::Loop => write!(f, "loop"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Fn => write!(f, "fn"),
            Self::Return => write!(f, "return"),
            Self::Let => write!(f, "let"),
            Self::Const => write!(f, "const"),
            Self::Case => write!(f, "case"),
            Self::Ensure => write!(f, "ensure"),
            Self::Mut => write!(f, "mut"),
            Self::Ref => write!(f, "ref"),
            Self::Own => write!(f, "own"),
            Self::Struct => write!(f, "struct"),
            Self::Enum => write!(f, "enum"),
            Self::Import => write!(f, "import"),
            Self::From => write!(f, "from"),
            Self::As => write!(f, "as"),
            Self::Match => write!(f, "match"),

            Self::EndOfFile => write!(f, "<EndOfFile>"),
        }
    }
}

impl fmt::Display for TokenSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}[{}]", self.span.line, self.span.length, self.token)
    }
}





#[allow(clippy::useless_format)]
pub fn format_program_error(err: &ProgramError, program: &Program) -> String {
    let err_type_msg = format!("{}", err.typ);

    // print the error message
    let mut output_str = format!("ERROR: {err_type_msg}\n");

    if err.length > usize::MAX / 2 || err.line > usize::MAX / 2 { return format!("Error (couldn't print where): {err_type_msg}") }

    let err_start = err.byte_offset;
    let err_end = err.byte_offset + err.length;
    let mut prefix = "  ";
    let line_number_width = program.line_starts_lookup.len().to_string().len();

    for line_index in err.line.. {
        if line_index > program.line_starts_lookup.len() { unreachable!(); }
        
        let line_start = program.line_starts_lookup[line_index - 1];
        let line_end = if line_index < program.line_starts_lookup.len() {
                program.line_starts_lookup[line_index]
            } else { program.source_code.len() };

        let mut add_line = |line_number: bool, msg: &str, pfx: &str| {
            writeln!(output_str, "{:>width$} |{pfx}{msg}",
                if line_number { line_index.to_string() } else { String::new() },
                width = line_number_width
            ).unwrap();
        };
        add_line(true, program.source_code[line_start..line_end].trim_end(), prefix);

        let err_starts_before_this_line = err_start < line_start;
        let err_ends_after_this_line = err_end > line_end;

        // underlining logic
        // ^^^^^^^^^^^
        match (err_starts_before_this_line, err_ends_after_this_line) {
            // single-line error, easiest case
            //             ^^^^^
            (false, false) => {
                add_line(false, &format!("{}{}", " ".repeat(err_start - line_start), "^".repeat(err_end - err_start)), prefix);
                break;
            }
            // multi line errors
            (true, false) => {
                add_line(false, &format!("{}", "^".repeat(err_end - line_start)), "|_");
                break;
            }
            (false, true) => {
                add_line(false, &format!("{}{}", "_".repeat(err_start - line_start), "^".repeat(line_end - err_start)), " _");
                prefix = "| ";
            }
            (true, true) => { },
        }
    }

    output_str
}




impl fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.lexer_tokens.is_empty() {
            write!(f, "LEXER TOKENS: [\n{}\n]", join_slice_to_string(&self.lexer_tokens, ", "))?;
        }
        if let Some(expr) = &self.ast {
            writeln!(f, "AST:")?;
            expr.format_recursive(f, 0, "", true)?;
        }
        if !self.type_lookup.is_empty() {
            write!(f, "TYPE LOOKUP \n{:?}", self.type_lookup)?;
        }


        Ok(())
    }
}




impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num => write!(f, "num"),
            Self::Str => write!(f, "str"),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::ParserUnknown => write!(f, "unknown"),
            Self::TypeError => write!(f, "error"),
            Self::Arr(typ) => write!(f, "arr<{typ}>"),
            Self::Tup(elements) => write!(f, "({})", join_slice_to_string(elements, ", ")),
            Self::Fn { param_types, return_type } => {
                write!(f, "fn<({}) -> {return_type}>", join_slice_to_string(param_types, ", "))
            }
            Self::CustomType { name, generic_types } => {
                write!(f, "CustomType:{name}<{}>", join_slice_to_string(generic_types, ", "))
            }
            Self::Pointer { inner: typ, mutable, borrows_var } => {
                write!(f, "&{}{borrows_var:?} {typ}", if *mutable { "mut " } else { "" })
            }
            Self::Inference(id) => write!(f, "?{id:?}"),
            Self::Never => write!(f, "never"),
        }
    }
}


impl fmt::Display for TupleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".{} = {:?}", self.label, self.typ)
    }
}
impl fmt::Display for TupleMatchPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".{} = {:?}", self.label, self.pattern)
    }
}






impl ExprInfo {    
    fn format_recursive(&self, f: &mut fmt::Formatter, ind: usize, prefix: &str, is_last: bool) -> fmt::Result {
        write!(f, "{}{} {}{} [{}]",
            "  ".repeat(ind),
            if ind == 0 { "" } else if is_last { "└─" } else { "├─" },
            if prefix.is_empty() { String::new() } else { format!("{prefix}: ") },
            Into::<&str>::into(&self.expression),
            self.typ,
        )?;

        match &self.expression {
            Expr::Literal(val) => writeln!(f, " - {val:?}")?,
            Expr::IdentifierRef { name, mutable, var_id } => writeln!(f, " - {}\"{name}\" ({var_id:?})", if *mutable {"mut "} else {""})?,

            Expr::Assign { pattern, extra_operator, value, .. } => {
                writeln!(f, " - {} pattern: {pattern}", TokenType::Equal { extra_operator: extra_operator.as_ref().map(|op| Box::new(op.clone())) })?;
                if let Some(val) = value {
                    val.format_recursive(f, ind + 1, "value", true)?;
                }
            }
            Expr::Case { pattern, value } => {
                writeln!(f, " - {pattern}")?;
                value.format_recursive(f, ind + 1, "value", true)?;
            }
            Expr::Block { exprs, label, drops_vars } => {
                writeln!(f, " - #{label:?} drops: {}", join_slice_to_debug_string(drops_vars, ", "))?;
                for (i, expr) in exprs.iter().enumerate() {
                    expr.format_recursive(f, ind + 1, "", i == exprs.len()-1)?;
                }
            }
            Expr::Infix { operator, left, right } => {
                writeln!(f, " - {operator}")?;
                left.format_recursive(f, ind + 1, "left", false)?;
                right.format_recursive(f, ind + 1, "right", true)?;
            }
            Expr::Prefix { operator, right } => {
                writeln!(f, " - {operator}")?;
                right.format_recursive(f, ind + 1, "right", true)?;
            }
            Expr::Call { callee, arguments } => {
                writeln!(f)?;
                callee.format_recursive(f, ind + 1, "func", false)?;
                for (i, arg) in arguments.iter().enumerate() {
                    arg.format_recursive(f, ind + 1, "arg", i == arguments.len()-1)?;
                }
            }
            Expr::If { condition, then, alt } => {
                writeln!(f)?;
                condition.format_recursive(f, ind + 1, "cond", false)?;
                then.format_recursive(f, ind + 1, "then", false)?;
                alt.format_recursive(f, ind + 1, "else", true)?;
            }
            Expr::Match { match_value, arms } => {
                writeln!(f)?;
                match_value.format_recursive(f, ind + 1, "match value", false)?;
                for (i, arm) in arms.iter().enumerate() {
                    arm.body.format_recursive(f, ind + 1, &format!("pattern: {:?} arm", arm.pattern), i == arms.len()-1)?;
                }
            }
            Expr::Array(elements) | Expr::TemplateString(elements) => {
                writeln!(f)?;
                for (i, el) in elements.iter().enumerate() {
                    el.format_recursive(f, ind + 1, "", i == elements.len()-1)?;
                }
            }
            Expr::Tuple(elements) => {
                writeln!(f)?;
                for (i, el) in elements.iter().enumerate() {
                    el.expr.format_recursive(f, ind + 1, &format!(".{}", el.label), i == elements.len()-1)?;
                }
            }
            Expr::Loop { body, label } => {
                writeln!(f, " - #{label}")?;
                body.format_recursive(f, ind + 1, "body", true)?;
            }
            Expr::FnDefinition { name, var_id, params, return_type_annotation, body } => {
                writeln!(f, " - {name} {} {var_id:?}", return_type_annotation.typ)?;
                for param in params {
                    writeln!(f, "{}    - param: {param}", "  ".repeat(ind))?;
                }
                body.format_recursive(f, ind + 1, "body", true)?;
            }
            Expr::Closure { params, return_type_annotation, body } => {
                writeln!(f, " -> {}", return_type_annotation.typ)?;
                for param in params {
                    writeln!(f, " - {param}")?;
                }
                body.format_recursive(f, ind + 1, "body", true)?;
            }
            Expr::Move { expr, auto_clone: just_a_clone } => {
                writeln!(f, "clone: {just_a_clone}")?;
                expr.format_recursive(f, ind + 1, "expr", true)?;
            }
            Expr::MemberAccess { left, member, resolved_index } => {
                writeln!(f, " - .{member} (idx: {})", resolved_index.map_or(String::new(), |x| x.to_string()))?;
                left.format_recursive(f, ind + 1, "object", true)?;
            }
            Expr::TypePath(segments) => {
                writeln!(f, " - {}", segments.join("::"))?;
            }
            Expr::Index { left, index } => {
                writeln!(f)?;
                left.format_recursive(f, ind + 1, "arr", false)?;
                index.format_recursive(f, ind + 1, "idx", true)?;
            }
            Expr::Ensure { condition, alt, then } => {
                writeln!(f)?;
                condition.format_recursive(f, ind + 1, "cond", false)?;
                alt.format_recursive(f, ind + 1, "else", false)?;
                then.format_recursive(f, ind + 1, "then", true)?;
            }
            Expr::While { condition, body, label } => {
                writeln!(f, " - #{label}")?;
                condition.format_recursive(f, ind + 1, "cond", false)?;
                body.format_recursive(f, ind + 1, "body", true)?;
            }
            Expr::EnumDefinition { name, variants: enums } => {
                writeln!(f, " - {name}")?;
                for (i, e) in enums.iter().enumerate() {
                    write!(f, "{}{}", 
                        "  ".repeat(ind + 1),
                        if i == enums.len() - 1 { "└─" } else { "├─" }
                    )?;
                    writeln!(f, " {e:?}")?; 
                }
            }
            Expr::Return(expr) => {
                writeln!(f)?;
                expr.format_recursive(f, ind + 1, "val", true)?;
            }
            Expr::Break { label, expr } => {
                writeln!(f, " - #{}", label.clone().map_or("none".to_string(), |x| x))?;
                expr.format_recursive(f, ind + 1, "val", true)?;
            }
            Expr::Continue { label } => {
                writeln!(f, " - #{}", label.clone().map_or("none".to_string(), |x| x))?;
            }

            Expr::Void => writeln!(f)?,
        }
        Ok(())
    }
}

// Custom Debug impl for patterns to make them print cleanly
impl fmt::Display for MatchPatternInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} [{}] - ",
            Into::<&str>::into(&self.pattern),
            self.typ,
        )?;
        match &self.pattern {
            MatchPattern::Literal(value) =>  write!(f, "{value}"),
            MatchPattern::Binding { name, mutable, var_id } => {
                if *mutable { write!(f, "mut ")? }
                write!(f, "{name} ({var_id:?})")
            }
            MatchPattern::Or(patterns) => write!(f, "{}", join_slice_to_string(patterns, " | ")),
            MatchPattern::Array(patterns) => write!(f, "[{}]", join_slice_to_string(patterns, ", ")),
            MatchPattern::Tuple(patterns) => write!(f, "({})", join_slice_to_string(patterns, ", ")),
            MatchPattern::EnumVariant { path, name, inner_patterns } => {
                write!(f, "{}::{}({})",
                    path.join(", "),
                    name,
                    inner_patterns.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")
                )
            }
            MatchPattern::Wildcard => write!(f, "_"),
            MatchPattern::Conditional { pattern, body } => {
                write!(f, "{pattern} if ({body:?})")
            }
            MatchPattern::PlacePointer { expr } => {
                writeln!(f)?;
                expr.format_recursive(f, 7, "", true)
            }
        }
    }
}




impl fmt::Display for PatternSpace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool { bool } => write!(f, "{bool}"),
            Self::Tup { inners } => write!(f, "({})", join_slice_to_string(inners, ", ")),
            Self::All => write!(f, "_"),
        }
    }
}





impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(x) => write!(f, "{x}"),
            Self::Str(x) => write!(f, "\"{x}\""),
            Self::Bool(x) => write!(f, "{x}"),
            Self::Arr(x) => write!(f, "[{}]", join_slice_to_string(x, ", ")),
            Self::Tup(x) => write!(f, "({})", join_slice_to_string(x, ", ")),
            Self::ValuePointer(p) => write!(f, "pointer<{p:?}>"),
            Self::Closure { chunk_index } => write!(f, "closure<{chunk_index}>"),
            Self::NativeFn(x) => write!(f, "{x:?}"),
            Self::Void => write!(f, "<void>"),
            Self::Empty => write!(f, "<empty>"),
        }
    }
}






impl fmt::Display for BytecodeChunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const fn how_many_operands_for_op_code(op: &OpCode) -> usize {
            match op {
                OpCode::ArrUnpackCheckJump | OpCode::LocalsFree => 2,
                OpCode::ConstGetRef | OpCode::PointerGetClone | OpCode::LocalSet | OpCode::StrTemplate | OpCode::ArrCreate | OpCode::ArrGet | OpCode::TupGet
                | OpCode::TupCreate | OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpBack | OpCode::CallFn
                 => 1,
                _ => 0,
            }
        }
        let mut strings = Vec::new();

        let mut frame = CallFrame::default();
        loop {
            let op_code = VM::read_next_instruction(&mut frame, self);
            let mut opnums = Vec::new();

            for _ in 0..how_many_operands_for_op_code(&op_code) {
                assert!(frame.ip < self.codes.len() - 1,
                    "Incorrect printing? {op_code:?}, {opnums:?} \nOpCodes - [{}],\nConstants - [{}]\n",
                    strings.join(", "), join_slice_to_string(&self.constants, ", ")
                );
                opnums.push(VM::read_next_opnum(&mut frame, self));
            }

            if opnums.is_empty() { strings.push(format!("{op_code:?}")); }
            else { strings.push(format!("{op_code:?}({})", join_slice_to_string(&opnums, ", "))); }

            // if we are at the end
            if frame.ip >= self.codes.len() - 1 {
                break;
            }
        }
        write!(f, "OpCodes - [{}],\nConstants - [{}]", strings.join(", "), join_slice_to_string(&self.constants, ", "))
    }
}









pub fn join_slice_to_string<T: fmt::Display>(vec: &[T], join: &str) -> String {
    vec.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(join)
}
pub fn join_slice_to_debug_string<T: fmt::Debug>(vec: &[T], join: &str) -> String {
    vec.iter().map(|x| format!("{x:?}")).collect::<Vec<_>>().join(join)
}