use std::{env, fs};

use thrum_engine::run_code;


fn main() -> Result<(), ()> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: thrum <file>");
        return Err(());
    }

    let file_path = &args[1];
    let file_source_code = fs::read_to_string(file_path).expect("Could not read file...");

    println!("\n\n\n--- START ({}) ---", file_path);
    run_code(&file_source_code).unwrap();

    Ok(())
}