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

  let mut lexer = Lexer {
    code: String::new(),
    pos: 0,
    line: 1,
    line_pos: 0,
    code_end: 0,
    tokens: vec![]
  };

  let lexed = lexer.lex(code);
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

  let mut parser = Parser::new(lexed);
  let parsed = parser.parse(verbose);
  println!("AST:");
  println!("{:?}", parsed);
}