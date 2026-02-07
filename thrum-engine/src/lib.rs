#![allow(clippy::option_if_let_else, clippy::equatable_if_let, clippy::or_fun_call, clippy::must_use_candidate)]

use std::{collections::HashMap, time::Instant};

use thiserror::Error;

use crate::{lexing::tokens::{TokenSpan, TokenType}, parsing::ast_structure::{ExprInfo, PatternSpace, TypeKind, Value}, pretty_printing::{format_program_error, join_slice_to_string}, typing::{TypeID, VarID}, vm_compiling::{BytecodeChunk, Compiler}, vm_evaluating::VM};

pub mod lexing;
pub mod parsing;
pub mod typing;
pub mod pretty_printing;
pub mod nativelib;
pub mod vm_compiling;
pub mod vm_evaluating;




#[derive(Debug, PartialEq)]
pub enum CodeResultError {
    LexerError,
    ParserError,
    TypecheckError,
    RuntimeError,
}


pub struct ProgramError {
    pub line: usize,
    pub byte_offset: usize,
    pub length: usize,
    pub typ: ErrType,
}

#[derive(Error, Debug)]
pub enum ErrType {
    #[error("Unexpected character '~'. Did you mean '~!' (Bitwise Not)?")]
    LexerTilde,
    #[error("Unexpected character '{c}'.")]
    LexerUnexpectedCharacter { c: char },
    #[error("Unterminated string.")]
    LexerUnterminatedString,
    #[error("Could not parse number '{text}'.")]
    LexerNumberParseError { text: String },

    #[error("Expected '{expected}' {err_msg}. Found '{found}' instead.")]
    ParserExpectToken { expected: TokenType, err_msg: String, found: TokenType },
    #[error("Unexpected expression start.")]
    ParserUnexpectedExpression,
    #[error("Expected an expression.")]
    ParserExpectedAnExpression,
    #[error("Incorrect '::'-path Syntax.")]
    ParserUnexpectedPathToken,
    #[error("Template strings are not allowed in match patterns.")]
    ParserPatternTemplateString,
    #[error("Invalid syntax in match pattern.")]
    ParserPatternInvalidSyntax,
    #[error("Labels have to be on the same line with the labeled thing.")]
    ParserLabelsHaveToBeOnSameLine,

    #[error("Expected type: {}, found: {}", expected, found)]
    TyperMismatch { expected: TypeKind, found: TypeKind },
    #[error("Name {name} is already defined in this scope.")]
    TyperNameAlreadyDefined { name: String },
    #[error("Undefined identifier: {name}")]
    TyperUndefinedIdentifier { name: String },
    #[error("Can't infer type {}", typ)]
    TyperCantInferType { typ: TypeKind },
    #[error("Type {} must be known at this point.", typ)]
    TyperTypeMustBeKnownHere { typ: TypeKind },
    #[error("Pattern doesn't cover all cases. Missing cases: {}", join_slice_to_string(remaining, ", "))]
    TyperPatternDoesntCoverAllCases { remaining: Vec<PatternSpace> },
    #[error("Pattern can't be reached.")]
    TyperPatternCantBeReached,
    #[error("Failable pattern in let-expression. Missing cases: {}", join_slice_to_string(remaining, ", "))]
    TyperFailableLetPattern { remaining: Vec<PatternSpace> },
    #[error("Failable patterns are not allowed in function parameters.")]
    TyperFailableFnParamPatterns,
    #[error("Requires type annotation.")]
    TyperRequiresTypeAnnotation,
    #[error("Case-expressions that bind variables aren't allowed here.")]
    TyperInvalidBindingCaseExpr,
    #[error("break is not allowed outside of loops.")]
    TyperBreakOutsideLoop,
    #[error("could not find the label #{label}. Current labels in scope: {}", available.join(", "))]
    TyperUndefinedLoopLabel { label: String, available: Vec<String> },
    #[error("Expected {} arguments, found {}.", expected, found)]
    TyperTooManyArguments { expected: usize, found: usize },
    #[error("Can't call a non-function type: {}.", typ)]
    TyperCantCallNonFnType { typ: TypeKind },
    #[error("member .{member} does not exist on tuple: {}", tup)]
    TyperTupleDoesntHaveMember { tup: TypeKind, member: String },
    #[error("Infix operation {type_a} {op} {type_b} is not defined.")]
    TyperInvalidOperatorOnType { op: TokenType, type_a: TypeKind, type_b: TypeKind },
    #[error("{} is not allowed in patterns.", TypeKind::Never)]
    TyperPatternNeverType,
    #[error("All or-patterns must bind the same variables. This pattern binds {}.", join_slice_to_string(vars, ", "))]
    TyperOrPatternBindsVarsTooMuch { vars: Vec<String> },
    #[error("All or-patterns must bind the same variables. This pattern doesn't bind {}.", join_slice_to_string(vars, ", "))]
    TyperOrPatternDoesntBindVars { vars: Vec<String> },
    #[error("Pattern binds {} twice.", join_slice_to_string(vars, ", "))]
    TyperPatternVarBoundTwice { vars: Vec<String> },
    #[error("Variable ({var:?}) cannot be re-assigned, because it isn't declared mutable.")]
    TyperVarIsntDeclaredMut { var: VarID },
    #[error("Can't use ({var:?}) because it isn't initialized yet.")]
    TyperCantUseUninitializedVar { var: VarID },
    #[error("Can't use ({var:?}) because it isn't initialized in every possible branch.")]
    TyperCantUseMaybeInitializedVar { var: VarID },
    #[error("Can't use ({var:?}) because it was moved.")]
    TyperCantUseMovedVar { var: VarID },
    #[error("Can't dereference non-pointer type: {typ}")]
    TyperCantDerefNonPointerType { typ: TypeKind },
    #[error("Can't borrow because already borrowed mutably.")]
    TyperCantBorrowBecauseAlreadyBorrowedMut,
    #[error("Can't borrow mutably because already borrowed.")]
    TyperCantBorrowMutBecauseAlreadyBorrowed,
    #[error("Can't borrow mutably because already borrowed mutably.")]
    TyperCantBorrowMutBecauseAlreadyBorrowedMut,


    // this case should not be used, every error should have its own entry in this enum!
    #[error("{0}")]
    DefaultString(String),
}


#[derive(Default)]
pub struct Program<'a> {
    source_code: &'a str,
    // v
    // Lexing
    // v
    lexer_tokens: Vec<TokenSpan>,
    // v
    // Parsing
    // v
    ast: Option<ExprInfo>,
    // v
    // Compiling
    // v
    compiled_bytecode: Vec<BytecodeChunk>,

    // this tells me at which byte line 3 starts for example
    line_starts_lookup: Vec<usize>,

    // extra data the type_checker adds
    type_lookup: HashMap<TypeID, TypeKind>,

    errors: Vec<ProgramError>,
}
impl Program<'_> {
    pub fn stage_complete(&mut self, stage: &str) -> bool {
        println!("--- {stage} Stage Complete! ---");

        let print_stages = ["Lexing", "Parsing", /* "Desugar after Parsing", */ "Typechecking", /* "Compiling" */];

        if print_stages.contains(&stage) {
            println!("{self}\n");
        }

        if self.errors.is_empty() { true } else {
            println!("--- {stage} Errors ---\n{}", self.errors.iter().map(|e| format_program_error(e, self) + "\n").collect::<String>());
            false
        }
    }
}









pub fn run_code(source_code: &str) -> Result<Value, CodeResultError> {
    let mut program = Program { source_code, ..Default::default() };

    lexing::tokenize_code(&mut program);
    if !program.stage_complete("Lexing") {
        return Err(CodeResultError::LexerError)
    }

    parsing::parse_program(&mut program);
    if !program.stage_complete("Parsing") {
        return Err(CodeResultError::ParserError)
    }

    parsing::desugar::desugar_after_parsing(&mut program);
    if !program.stage_complete("Desugar after Parsing") {
        unreachable!()
    }

    typing::typecheck_program(&mut program);
    if !program.stage_complete("Typechecking") {
        return Err(CodeResultError::TypecheckError)
    }

    Compiler::compile_program(&mut program);
    program.stage_complete("Compiling");


    println!("\n--- Execution ---");
    let mut vm = VM::new();
    vm.load_bytecodes(program.compiled_bytecode);
    let time_took = Instant::now();

    let result = unsafe { vm.run(cfg!(debug_assertions)) };
    match result {
        Ok(()) => {
            println!("\n--- Execution Successfull ({:?}) ---", time_took.elapsed());
            println!("{}", join_slice_to_string(&vm.value_stack, ", "));
        }
        Err(err) => {
            println!("\n--- Runtime Error ({:?}) ---", time_took.elapsed());
            println!("{err}");
            return Err(CodeResultError::RuntimeError);
        }
    }

    Ok(vm.value_stack[0].clone())
}