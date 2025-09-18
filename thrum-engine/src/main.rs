use std::env;
use std::process;

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
    let tokens = lexer::tokenize_file(file_path).unwrap_or_else(|e| {
        eprintln!("Application error: {}", e);
        process::exit(1);
    });

    // AST parsing
    let mut parser = parser::Parser::new(tokens);
    let mut program = parser.parse_program();
    if !parser.errors.is_empty() {
        println!("{:#?}", program);
        println!("--- Parser Errors ---\n{:?}", parser.errors);
        process::exit(1);
    }
    else {
        println!("\n--- Parsed Program ---");
        println!("{:#?}", program);
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
        println!("--- Type Errors ---");
        for error in type_checker.errors {
            println!("{}", error);
        }
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
    let mut compiler = Compiler::new();
    let bytecode_chunks = compiler.compile_program(&program);
    println!("\n--- Compiled to Bytecode ---");
    println!("{:?}", bytecode_chunks);
    println!("{}", join_slice_to_string(&bytecode_chunks, "\n\n"));


    // execute bytecode!
    println!("\n--- Execution ---");
    let mut vm = VM::new();
    vm.load_bytecodes(bytecode_chunks);
    match vm.run(cfg!(debug_assertions)) {
        Ok(()) => {
            println!("\n--- Execution Successfull ---");
            println!("{}", join_slice_to_string(&vm.value_stack, ", "));
        }
        Err(err) => {
            println!("\n--- Runtime Error ---");
            println!("{}", err);
        }
    }
}