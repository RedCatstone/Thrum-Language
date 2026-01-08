use std::{collections::HashMap, time::Instant};

use crate::{lexing::tokens::{TokenSpan, TokenType}, parsing::ast_structure::{ExprInfo, TypeKind, Value}, pretty_printing::{format_program_error, join_slice_to_string}, typing::{TypeID, VarID}, vm_compiling::{BytecodeChunk, Compiler}, vm_evaluating::VM};

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


pub enum ErrType {
    LexerTilda,
    LexerUnexpectedCharacter(char),
    LexerUnterminatedString,
    LexerNumberParseError(String),

    ParserExpectToken(TokenType, String, TokenType),
    ParserUnexpectedExpression,
    ParserExpectedAnExpression,
    ParserUnexpectedPathToken,
    ParserPatternInvalidSyntax,
    ParserPatternTemplateString,
    ParserLabelsHaveToBeOnSameLine,

    TyperMismatch(TypeKind, TypeKind),
    TyperNameAlreadyDefined(String),
    TyperUndefinedIdentifier(String),
    TyperCantInferType(TypeKind),
    TyperPatternDoesntCoverAllCases(Vec<String>),
    TyperFailableLetPattern,
    TyperFailableFnParamPatterns,
    TyperFnParamPlacePatterns,
    TyperInvalidBindingCaseExpr,
    TyperBreakOutsideLoop,
    TyperUndefinedLoopLabel(String, Vec<String>),
    TyperTooManyArguments(usize, usize),
    TyperCantCallNonFnType(TypeKind),
    TyperTupleDoesntHaveMember(TypeKind, String),
    TyperInvalidOperatorOnType(TokenType, TypeKind),
    TyperPatternNeverType,
    TyperOrPatternBindsVarsTooMuch(Vec<String>),
    TyperOrPatternDoesntBindVars(Vec<String>),
    TyperPatternVarBoundTwice(Vec<String>),
    TyperVarIsntDeclaredMut(VarID),
    TyperCantUseUninitializedVar(VarID),
    TyperCantUseMaybeInitializedVar(VarID),

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
impl<'a> Program<'a> {
    pub fn stage_complete(&mut self, stage: &str) -> bool {
        println!("--- {stage} Stage Complete! ---");

        let print_stages = ["Lexing", "Parsing", /* "Desugar after Parsing", */ "Typechecking", /* "Compiling" */];

        if print_stages.contains(&stage) {
            println!("{}\n", self);
        }

        if !self.errors.is_empty() {
            println!("--- {stage} Errors ---\n{}", self.errors.iter().map(|e| format_program_error(e, self) + "\n").collect::<String>());
            false
        }
        else { true }
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
    match vm.run(cfg!(debug_assertions)) {
        Ok(()) => {
            println!("\n--- Execution Successfull ({:?}) ---", time_took.elapsed());
            println!("{}", join_slice_to_string(&vm.value_stack, ", "));
        }
        Err(err) => {
            println!("\n--- Runtime Error ({:?}) ---", time_took.elapsed());
            println!("{}", err);
            return Err(CodeResultError::RuntimeError);
        }
    }

    Ok(vm.value_stack[0].clone())
}