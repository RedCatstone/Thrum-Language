use std::time::Instant;

use crate::{parsing::ast_structure::Value, pretty_printing::join_slice_to_string, vm_compiling::Compiler, vm_evaluating::VM};

pub mod lexing;
pub mod parsing;
pub mod typing;
pub mod pretty_printing;
pub mod nativelib;
pub mod vm_compiling;
pub mod vm_evaluating;





pub fn run_code(source: &str) -> Result<Value, ()> {
    // tokenizing
    let (tokens, lexer_errors) = lexing::Lexer::tokenize_code(source);
    println!("\n--- Lexer Tokens --- \n{:?}", tokens);
    if !lexer_errors.is_empty() {
        println!("\n--- Lexer Errors ---\n{}", join_slice_to_string(&lexer_errors, "\n"));
        return Err(());
    }

    // AST parsing
    let (mut program, parser_errors) = parsing::Parser::parse_program(tokens);
    println!("\n--- Parsed Program ---{:#?}", program);
    if !parser_errors.is_empty() {
        println!("\n--- Parser Errors ---\n{}", join_slice_to_string(&parser_errors, "\n"));
        return Err(());
    }


    parsing::desugar::desugar(&mut program);
    println!("\n--- Desugared ---{:#?}", program);


    // Type checking!
    println!("--- Typed AST ---");
    let type_errors = typing::TypeChecker::typecheck_program(&mut program);
    println!("{:#?}", program);
    if !type_errors.is_empty() {
        println!("--- Type Errors ---\n{}", join_slice_to_string(&type_errors, "\n"));
        return Err(());
    }
    else { println!("\n--- Type Check Passed ---"); }



    // evaluating!
    // println!("--- Executing Program ---");
    // let mut evaluator = ast_walker::Executor::new();
    // match evaluator.execute_program(&program) {
    //     Ok(result) => {
    //         println!("--- Execution Successfull ---");
    //         println!("{:?}", result);
    //     }
    //     Err(err) => {
    //         println!("--- Runtime Errors ---");
    //         println!("{}", err);
    //     }
    // }


    // to bytecode
    let bytecode_chunks = Compiler::compile_program(&program);
    println!("\n--- Compiled to Bytecode ---");
    println!("{:?}", bytecode_chunks);
    println!("{}", join_slice_to_string(&bytecode_chunks, "\n\n"));


    // execute bytecode!
    println!("\n--- Execution ---");
    let mut vm = VM::new();
    vm.load_bytecodes(bytecode_chunks);
    let time_took = Instant::now();
    match vm.run(cfg!(debug_assertions)) {
        Ok(()) => {
            println!("\n--- Execution Successfull ({:?}) ---", time_took.elapsed());
            println!("{}", join_slice_to_string(&vm.value_stack, ", "));
        }
        Err(err) => {
            println!("\n--- Runtime Error ({:?}) ---", time_took.elapsed());
            println!("{}", err);
            return Err(());
        }
    }

    Ok(vm.value_stack[0].clone())
}