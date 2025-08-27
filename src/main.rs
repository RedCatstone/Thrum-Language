use std::env;
use std::process;

mod tokens;
mod lexer;
mod ast_structure;
mod parser;
mod type_checker;

mod pretty_printing;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: thrum <file>");
        process::exit(64);
    }

    let file_path = &args[1];


    // tokenizing
    let tokens = lexer::tokenize_file(file_path).unwrap_or_else(|e| {
        eprintln!("Application error: {}", e);
        process::exit(1);
    });

    // AST parsing
    let mut parser = parser::Parser::new(tokens);
    let mut program = parser.parse_program();
    
    println!("--- AST ---\n{:#?}", program);
    if !parser.errors.is_empty() {
        println!("--- Parser Errors ---\n{:?}", parser.errors);
        process::exit(1);
    }
    drop(parser);


    // Type checking!
    let mut type_checker = type_checker::TypeChecker::new();
    type_checker.check_program(&mut program);

    println!("--- Typed AST ---\n{:#?}", program);
    if !type_checker.errors.is_empty() {
        println!("--- Type Errors ---");
        for error in type_checker.errors {
            println!("{}", error);
        }
        process::exit(1);
    }
    else { println!("--- Type Check Passed ---"); }
}