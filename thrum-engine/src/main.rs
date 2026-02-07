use std::{env, fs};
use thrum_engine::run_code;



fn main() -> Result<(), ()> {
    // unsafe { std::env::set_var("RUST_BACKTRACE", "1") }
    
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: thrum <file>");
        return Err(());
    }

    let file_path = &args[1];
    let file_source_code = fs::read_to_string(file_path).expect("Could not read file...");

    println!("\n\n\n--- START ({file_path}) ---");
    _ = run_code(&file_source_code);

    Ok(())
}