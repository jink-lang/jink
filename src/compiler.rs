mod lexer;
mod parser;
use lexer::Lexer;
use parser::Parser;
use jink::TokenTypes;

pub fn compile(code: String, verbose: bool) {
  if verbose {
    println!("Compiling code:");
    for (i, line) in code.lines().enumerate() {
      println!("{:4} | {}", i + 1, line);
    }
  }

  let mut lexer = Lexer::new();
  let lexed = lexer.lex(code.clone(), false);
  if verbose {
    println!("Tokens:");
    let mut cur = 0;
    let mut line: Vec<String> = vec![];
    for (_i, token) in lexed.clone().iter().enumerate() {
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
  let parsed = parser.parse(code.clone(), verbose);
  if let Err(err) = parsed {
    println!("{}", err);
    return;
  }

  println!("AST:");
  println!("{:?}", parsed.unwrap());
}