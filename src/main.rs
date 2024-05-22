use inkwell::context::Context;
use jink::TokenTypes;
use std::env;
use std::fs;

mod compiler;
mod interpreter;
use compiler::builder::CodeGen;
use compiler::lexer::Lexer;
use compiler::parser::Parser;
use interpreter::simulator::Simulator;

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

  let verbose = args.contains(&"-v".to_string());

  if verbose {
    // println!("Using code:");
    // for (i, line) in code.lines().enumerate() {
    //   println!("{:4} | {}", i + 1, line);
    // }
  }

  let mut lexer = Lexer::new();
  let lexed = lexer.lex(code.clone(), false);
  if verbose {
    println!("Tokens:");
    let mut cur = 0;
    let mut line: Vec<String> = vec![];
    for token in lexed.clone().iter() {
      if token.of_type == TokenTypes::Newline { continue; }

      if line.len() > 0 as usize {
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

  if args.contains(&"-i".to_string()) {
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
    let built = builder.build(code.clone(), parsed.unwrap(), verbose);
    if let Err(err) = built {
      println!("{}", err);
      return;
    }
  }
}