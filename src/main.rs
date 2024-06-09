use inkwell::context::Context;
use jink::TokenTypes;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;

mod compiler;
mod interpreter;
use compiler::builder::CodeGen;
use compiler::lexer::Lexer;
use compiler::parser::Parser;
use interpreter::simulator::Simulator;

fn main() {
  // Get args input
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 || args.contains(&"-h".to_string()) {
    println!("Usage: jink <file.jk>");
    println!("Options:");
    println!("  -v: Verbose output");
    println!("  -i: Interpret code");
    println!("  -o: Output to an executable");
    println!("  -r: Run the executable after compilation");
    println!("  -l: Compile just to LLVM IR");
    println!("  -h: Display this help message");
    return;
  }

  if let Err(err) = fs::metadata(&args[1]) {
    println!("Error: {}", err);
    return;
  }

  // Collect flags
  let verbose = args.contains(&"-v".to_string());
  let interpret = args.contains(&"-i".to_string());
  let out = args.contains(&"-o".to_string());
  let run_after_out = args.contains(&"-r".to_string());
  let out_llvm = args.contains(&"-l".to_string());

  // Remove flags
  let args: Vec<String> = args.iter().map(|arg| arg.to_string()).filter(|arg| !arg.starts_with("-")).collect();

  let code = fs::read_to_string(&args[1])
    .expect("Failed to read file.");

  let mut lexer = Lexer::new();
  let lexed = lexer.lex(code.clone(), false);
  if verbose {
    println!("Tokens:");
    let mut cur = 0;
    let mut line: Vec<String> = vec![];
    for token in lexed.clone().iter() {
      if token.of_type == TokenTypes::Newline { continue; }

      if !line.is_empty() {
        if cur != token.line {
          println!("{:4} | {}", cur, line.join(" "));
          line.clear();
        } else {
          // line.push(token.value.as_ref().unwrap().to_owned());
          line.push(token.of_type.to_string());
          continue;
        }
      }

      cur = token.line;
      // line.push(token.value.as_ref().unwrap().to_owned());
      line.push(token.of_type.to_string());

      if cur == lexed.len() as i32 {
        println!("{:4} | {}", cur, line.join(" "));
      }
    }
  }

  let mut parser = Parser::new();
  let parsed = parser.parse(code.clone(), verbose, false);
  if let Err(err) = parsed {
    println!("{}", err);
    return;
  }

  // if verbose {
  //   println!("AST:");
  //   println!("{:?}", parsed.as_ref().unwrap());
  // }

  if interpret {
    let mut simulator = Simulator::new();
    let simulated = simulator.simulate(code.clone(), parsed.unwrap(), verbose);
    if let Err(err) = simulated {
      println!("{}", err);
      return;
    }
    println!("{:?}", simulated);
  } else {
    let context = Context::create();
    let mut builder = CodeGen::new(&context);

    let do_execute = !out && !out_llvm;

    let main = builder.build(
      code.clone(),
      parsed.unwrap(),
      verbose,
      do_execute
    );
    if let Err(err) = main {
      println!("{}", err);
      return;
    }
    if do_execute { return; }

    // Put LLVM IR to file
    let mut file = File::create("output.ll").unwrap();
    file.write_all(builder.module.print_to_string().to_bytes()).unwrap();
    if out_llvm {
      println!("Outputted LLVM IR to output.ll");
      return;
    }

    // Run system command to compile the LLVM IR
    #[cfg(target_os = "windows")]
    let output = std::process::Command::new("clang")
      .arg("-o")
      .arg("output.exe")
      .arg("output.ll")
      .output()
      .expect("Failed to compile LLVM IR");
    #[cfg(not(target_os = "windows"))]
    let output = std::process::Command::new("clang")
      .arg("-o")
      .arg("output")
      .arg("output.ll")
      .output()
      .expect("Failed to compile LLVM IR");

    // Delete the LLVM IR file
    std::fs::remove_file("output.ll").unwrap();

    if !output.status.success() {
      println!("Error: {}", String::from_utf8_lossy(&output.stderr));
      return;
    }

    if run_after_out {
      let output = std::process::Command::new("./output")
        .output()
        .expect("Failed to run executable");

      if !output.status.success() && String::from_utf8_lossy(&output.stderr) != "" {
        println!("Error: {}", String::from_utf8_lossy(&output.stderr));
      } else {
        println!("{}", String::from_utf8_lossy(&output.stdout));
      }
    }
  }
}