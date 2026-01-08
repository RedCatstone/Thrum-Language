use std::fmt;
use crate::{
    ErrType, Program, ProgramError, lexing::tokens::{TokenSpan, TokenType}, parsing::ast_structure::{Expr, ExprInfo, MatchPattern, MatchPatternInfo, TupleElement, TupleMatchPattern, TupleType, TypeKind, Value}, vm_compiling::{BytecodeChunk, OpCode}, vm_evaluating::{CallFrame, VM}
};




impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Literals with data
            TokenType::Identifier(s) => write!(f, "ident<{}>", s),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::StringFrag(s) => write!(f, "\"{}\"", s),
            TokenType::Dot(s) => write!(f, ".{}", s),
            TokenType::Bool(b) => write!(f, "{}", b),
            TokenType::Null => write!(f, "null"),

            // Basic punctuation
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
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
            TokenType::Hashtag=> write!(f, "#"),

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
            TokenType::Case => write!(f, "case"),
            TokenType::Ensure => write!(f, "ensure"),
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

impl fmt::Display for TokenSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}[{}]", self.span.line, self.span.length, self.token)
    }
}





#[allow(clippy::useless_format)]
pub fn format_program_error(err: &ProgramError, program: &Program) -> String {
    let err_type_msg = match &err.typ {
        ErrType::LexerTilda => format!("Unexpected character '~'. Did you mean '~!' (Bitwise Not)?"),
        ErrType::LexerUnexpectedCharacter(c) => format!("Unexpected character '{}'.", c),
        ErrType::LexerUnterminatedString => format!("Unterminated string."),
        ErrType::LexerNumberParseError(text) => format!("Could not parse number '{}'.", text),

        ErrType::ParserExpectToken(expected, err_msg, found) => format!(
            "Expected '{expected}' {err_msg}. Found '{found}' instead."
        ),
        ErrType::ParserUnexpectedExpression => format!("Unexpected expression start."),
        ErrType::ParserExpectedAnExpression => format!("Expected an expression."),
        ErrType::ParserUnexpectedPathToken => format!("Incorrect '::'-path Syntax."),
        ErrType::ParserPatternTemplateString => format!("Template strings are not allowed in match patterns."),
        ErrType::ParserPatternInvalidSyntax => format!("Invalid syntax in match pattern."),
        ErrType::ParserLabelsHaveToBeOnSameLine => format!("Labels have to be on the same line with the labeled thing."),

        ErrType::TyperMismatch(expected, found) => format!("Expected type: {}, found: {}",
            expected.prune(&program.type_lookup), found.prune(&program.type_lookup)
        ),
        ErrType::TyperNameAlreadyDefined(name) => format!("Name {name} is already defined in this scope."),
        ErrType::TyperUndefinedIdentifier(name) => format!("Undefined identifier: {name}"),
        ErrType::TyperCantInferType(typ) => format!("Can't infer type {}", typ.prune(&program.type_lookup)),
        ErrType::TyperPatternDoesntCoverAllCases(remaining) => format!("Pattern doesn't cover all cases. Remaining cases: {}", remaining.join(", ")),
        ErrType::TyperFailableLetPattern => format!("Failable pattern in let-expression. Use 'if case ...' or 'ensure case ...' instead."),
        ErrType::TyperFnParamPlacePatterns => format!("Place patterns are not allowed in function parameters."),
        ErrType::TyperFailableFnParamPatterns => format!("Failable patterns are not allowed in function parameters."),
        ErrType::TyperInvalidBindingCaseExpr => format!("Case-expressions that bind variables aren't allowed here."),
        ErrType::TyperBreakOutsideLoop => format!("break is not allowed outside of loops."),
        ErrType::TyperUndefinedLoopLabel(label, available) => format!("could not find the label #{label}. Current labels in scope: {}",
            available.join(", ")
        ),
        ErrType::TyperTooManyArguments(expected, found) => format!("Expected {} arguments, found {}.", expected, found),
        ErrType::TyperCantCallNonFnType(typ) => format!("Can't call a non-function type: {}.", typ.prune(&program.type_lookup)),
        ErrType::TyperTupleDoesntHaveMember(tup, member) => format!("member .{member} does not exist on tuple: {}", tup.prune(&program.type_lookup)),
        ErrType::TyperInvalidOperatorOnType(op, typ) => format!("The operator {op} is not defined on type {typ}."),
        ErrType::TyperPatternNeverType => format!("{} is not allowed in patterns.", TypeKind::Never),
        ErrType::TyperOrPatternBindsVarsTooMuch(vars) => format!("All or-patterns must bind the same variables. This pattern binds {}.",
            join_slice_to_string(vars, ", ")
        ),
        ErrType::TyperOrPatternDoesntBindVars(vars) => format!("All or-patterns must bind the same variables. This pattern doesn't bind {}.",
            join_slice_to_string(vars, ", ")
        ),
        ErrType::TyperPatternVarBoundTwice(vars) => format!("Pattern binds {} twice.", join_slice_to_string(vars, ", ")),
        ErrType::TyperVarIsntDeclaredMut(var) => format!("Variable ({var:?}) cannot be assigned twice, because it isn't mutable."),
        ErrType::TyperCantUseUninitializedVar(var) => format!("Can't use ({var:?}) because it isn't initialized yet."),
        ErrType::TyperCantUseMaybeInitializedVar(var) => format!("Can't use ({var:?}) because it isn't initialized in every possible branch."),


        // this case should not be used, every error should have its own entry in this enum!
        ErrType::DefaultString(s) => format!("{}", s),
    };

    // print the error message
    let mut output_str = format!("ERROR: {}\n", err_type_msg);

    if err.length > usize::MAX / 2 { return format!("Error (couldn't print where): {}", err_type_msg) }

    let err_start = err.byte_offset;
    let err_end = err.byte_offset + err.length;
    
    let mut prefix = "  ";

    for line_index in err.line.. {
        if line_index > program.line_starts_lookup.len() { break; }
        
        let line_start_byte = program.line_starts_lookup[line_index - 1];
        let line_end_byte = if line_index < program.line_starts_lookup.len() {
                program.line_starts_lookup[line_index]
            } else {
                program.source_code.len()
            };

        output_str += &format!("{prefix}{}\n", program.source_code[line_start_byte..line_end_byte].trim_end());

        let err_starts_before_this_line = err_start < line_start_byte;
        let err_ends_after_this_line = err_end > line_end_byte;

        match (err_starts_before_this_line, err_ends_after_this_line) {
            // single-line error, easiest case
            (false, false) => {
                output_str += &format!("{prefix}{}{}\n",
                    " ".repeat(err_start - line_start_byte),
                    "^".repeat(err_end - err_start),
                );
                break;
            }
            // multi line errors
            (true, false) => {
                prefix = "|_";
                output_str += &format!("{prefix}{}\n",
                    "^".repeat(err_end - line_start_byte),
                );
                break;
            }
            (false, true) => {
                prefix = " _";
                output_str += &format!("{prefix}{}{}\n",
                    "_".repeat(err_start - line_start_byte),
                    "^".repeat(line_end_byte - err_start),
                );
                prefix = "| ";
            }
            (true, true) => { },
        }
    }

    output_str
}




impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.lexer_tokens.is_empty() {
            write!(f, "LEXER TOKENS: [\n{}\n]", join_slice_to_string(&self.lexer_tokens, ", "))?;
        }
        if let Some(expr) = &self.ast {
            writeln!(f, "AST:")?;
            expr.format_recursive(f, 0, "", true)?
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
            TypeKind::Num => write!(f, "num"),
            TypeKind::Str => write!(f, "str"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::ParserUnknown => write!(f, "unknown"),
            TypeKind::TypeError => write!(f, "error"),
            TypeKind::Arr(typ) => write!(f, "arr<{}>", typ),
            TypeKind::Tup(elements) => write!(f, "({})", join_slice_to_string(elements, ", ")),
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
            TypeKind::Inference(id) => write!(f, "?{:?}", id),
            TypeKind::Never => write!(f, "never"),
        }
    }
}

impl fmt::Display for TupleElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".{} = ", self.label)?;
        self.expr.format_recursive(f, 0, "", true)
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
            if prefix.is_empty() { "".to_string() } else { format!("{prefix}: ") },
            Into::<&str>::into(&self.expression),
            self.typ,
        )?;

        match &self.expression {
            Expr::Literal(val) => writeln!(f, " - {val:?}")?,
            Expr::Identifier { name, var_id: resolved_var_id } => writeln!(f, " - \"{name}\" ({resolved_var_id:?})")?,

            Expr::Assign { pattern, extra_operator, value } => {
                writeln!(f, " - {extra_operator} (pattern: {pattern})")?;
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
            Expr::Array(elements) => {
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
            Expr::FnDefinition { name, var_id, params, return_type, body } => {
                writeln!(f, " - {name} {return_type} {var_id:?}")?;
                for param in params {
                    writeln!(f, "{}    - param: {param}", "  ".repeat(ind))?;
                }
                body.format_recursive(f, ind + 1, "body", true)?;
            }
            Expr::Closure { params, return_type, body } => {
                writeln!(f, " -> {return_type}")?;
                for param in params {
                    writeln!(f, " - {param}")?;
                }
                body.format_recursive(f, ind + 1, "body", true)?;
            }
            Expr::TemplateString(elements) => {
                writeln!(f)?;
                for (i, el) in elements.iter().enumerate() {
                    el.format_recursive(f, ind + 1, "", i == elements.len() - 1)?;
                }
            }
            Expr::MutRef { expr } | Expr::Deref { expr } => {
                writeln!(f)?;
                expr.format_recursive(f, ind + 1, "expr", true)?;
            }
            Expr::MemberAccess { left, member, resolved_index } => {
                writeln!(f, " - .{member} (idx: {})", resolved_index.map_or("".to_string(), |x| x.to_string()))?;
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
            Expr::EnumDefinition { name, enums } => {
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
            MatchPattern::Literal(value) =>  write!(f, "{}", value),
            MatchPattern::Binding { name, mutable, typ, var_id } => {
                if *mutable { write!(f, "mut ")? }
                write!(f, "{}: {} ({var_id:?})", name, typ)
            }
            MatchPattern::Or(patterns) => write!(f, "{}", join_slice_to_string(patterns, " | ")),
            MatchPattern::Array(patterns) => write!(f, "[{}]", join_slice_to_string(patterns, ", ")),
            MatchPattern::Tuple(patterns) => write!(f, "({})", join_slice_to_string(patterns, ", ")),
            MatchPattern::EnumVariant { path, name, inner_patterns } => {
                write!(f, "{}::{}({})",
                    path.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "),
                    name,
                    inner_patterns.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")
                )
            }
            MatchPattern::Wildcard => write!(f, "_"),
            MatchPattern::Conditional { pattern, body } => {
                write!(f, "{} if ({:?})", pattern, body)
            }
            MatchPattern::PlaceIdentifier { name, var_id } => write!(f, "{} ({var_id:?})", name),
            MatchPattern::PlaceDeref { name, var_id } => write!(f, "({})^ ({var_id:?})", name),
            MatchPattern::PlaceIndex { left, index } => write!(f, "{:?}[{:?}]", left, index),
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
                OpCode::ConstGet | OpCode::LocalGet | OpCode::LocalSet | OpCode::StrTemplate | OpCode::ArrCreate | OpCode::ArrGet | OpCode::TupGet
                | OpCode::TupCreate | OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpBack | OpCode::CallFn
                | OpCode::MakePointer => 1,
                _ => 0,
            }
        }
        let mut strings = Vec::new();

        let mut frame = CallFrame::default();
        loop {
            let op_code = VM::read_next_instruction(&mut frame, self);
            let mut opnums = Vec::new();

            for _ in 0..how_many_operands_for_op_code(&op_code) {
                if frame.ip >= self.codes.len() - 1 {
                    panic!("Incorrect printing? {op_code:?}, {opnums:?} \nOpCodes - [{}],\nConstants - [{}]\n",
                        strings.join(", "), join_slice_to_string(&self.constants, ", ")
                    )
                }
                opnums.push(VM::read_next_opnum(&mut frame, self))
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









pub fn join_slice_to_string<T: fmt::Display>(vec: &[T], join: &str) -> String {
    vec.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(join)
}
pub fn join_slice_to_debug_string<T: fmt::Debug>(vec: &[T], join: &str) -> String {
    vec.iter().map(|x| format!("{:?}", x)).collect::<Vec<_>>().join(join)
}