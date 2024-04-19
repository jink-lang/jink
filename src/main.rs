use std::fs;
use std::env;

mod compiler;

fn main() {
    // Get args input
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: jink <file.jk>");
        return;
    }

    if let Err(err) = fs::metadata(&args[1]) {
        println!("Error: {}", err);
        return;
    }

    let code = fs::read_to_string(&args[1])
        .expect("Failed to read file.");

    let verbose = args.len() > 2 && args[2] == "-v";
    compiler::compile(code, verbose);
}