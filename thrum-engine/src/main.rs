use std::env;
use std::process;
use std::time::Instant;

use crate::pretty_printing::join_slice_to_string;
use crate::to_bytecode::Compiler;
use crate::vm::VM;

mod tokens;
mod lexer;
mod ast_structure;
mod parser;
mod desugar;
mod type_checker;
// mod ast_walker;
mod pretty_printing;
mod nativelib;
mod to_bytecode;
mod vm;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: thrum <file>");
        process::exit(64);
    }

    let file_path = &args[1];

    println!("\n\n\n--- START ({}) ---", file_path);

    // tokenizing
    let (tokens, lexer_errors) = lexer::tokenize_file(file_path).unwrap_or_else(|e| {
        eprintln!("Application error: {}", e);
        process::exit(1);
    });
    println!("\n--- Lexer Tokens --- \n{:?}", tokens);
    if !lexer_errors.is_empty() {
        println!("\n--- Lexer Errors ---\n{}", join_slice_to_string(&lexer_errors, "\n"));
        process::exit(1);
    }

    // AST parsing
    let mut parser = parser::Parser::new(tokens);
    let mut program = parser.parse_program();
    println!("\n--- Parsed Program ---{:#?}", program);
    if !parser.errors.is_empty() {
        println!("\n--- Parser Errors ---\n{}", join_slice_to_string(&parser.errors, "\n"));
        process::exit(1);
    }
    drop(parser);


    desugar::desugar(&mut program);
    println!("\n--- Desugared ---\n");


    // Type checking!
    let mut type_checker = type_checker::TypeChecker::new();
    type_checker.check_program(&mut program);

    println!("--- Typed AST ---");
    println!("inference map: {:?}", type_checker.inference_id_lookup);
    println!("{:#?}", program);
    if !type_checker.errors.is_empty() {
        println!("--- Type Errors ---\n{}", join_slice_to_string(&type_checker.errors, "\n"));
        process::exit(1);
    }
    else { println!("\n--- Type Check Passed ---"); }
    drop(type_checker);



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
        }
    }
}