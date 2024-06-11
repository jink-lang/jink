use std::iter::Peekable;
use std::slice::Iter;
use std::any::type_name;
use std::convert::TryFrom;
use jink::Token;
use jink::TokenTypes;
use jink::OPERATORS;
use jink::KEYWORDS;

fn _type_of<T>(_: T) -> &'static str {
  type_name::<T>()
}

pub struct Lexer {
  pub code: String,
  pub pos: i32,
  pub line: i32,
  pub line_pos: i32,
  pub code_end: i32,
  pub tokens: Vec<Token>,
  pub testing: bool
}

impl Lexer {
  pub fn new() -> Self {
    Lexer {
      code: String::new(),
      pos: 0,
      // Windows and Unix have different line positions for some reason
      #[cfg(target_os = "windows")]
      line: 1,
      #[cfg(not(target_os = "windows"))]
      line: 2,
      line_pos: 0,
      code_end: 0,
      tokens: vec![],
      testing: false
    }
  }

  pub fn lex(&mut self, code: String, testing: bool) -> Vec<Token> {
    self.code = code;
    let code = self.code.chars().collect::<Vec<char>>();
    self.code_end = code.len() as i32;
    self.testing = testing;

    #[cfg(not(target_os = "windows"))]
    if self.testing { self.line -= 1; }

    let mut iter = code.iter().peekable();
    return self.parse_tokens(&mut iter);
  }

  fn add_token(&mut self, of_type: TokenTypes, value: Option<String>, line: i32, start_pos: Option<i32>, end_pos: Option<i32>) {
    if self.testing {
      self.tokens.push(Token { of_type, value, line, ..Default::default() });
    } else {
      self.tokens.push(Token { of_type, value, line, start_pos, end_pos });
    }
  }

  fn parse_tokens(&mut self, iter: &mut Peekable<Iter<char>>) -> Vec<Token> {
    while self.pos < self.code_end {
      // Increment positions
      self.pos += 1;
      self.line_pos += 1;

      let char = iter.next();
      if char.is_none() {
        self.add_token(TokenTypes::EOF, None, self.line, None, None);
        break;
      }

      // Ignore escaped newlines
      if char.unwrap() == &'\\' {
        if iter.next().unwrap() == &'\n' {
          self.pos += 1;
          self.line += 1;
          self.line_pos = 0;
        }
        continue;
      }

      // Ignore whitespace
      if char.unwrap().is_whitespace() {
        if char.unwrap() == &'\n' {
          self.line += 1;
          self.line_pos = 0;
          self.add_token(TokenTypes::Newline, Some("\n".to_string()), self.line, Some(self.line_pos), Some(self.line_pos));
        } else if char.unwrap() == &'\r' {
          continue;
        }
        continue;
      }

      // Ignore comments
      else if char.unwrap() == &'/' {
        if ['/', '*'].contains(iter.peek().unwrap()) {
          self.process_comment(iter);
        } else {
          self.parse_operator(iter, char.unwrap());
        }

      } else if char.unwrap().is_numeric() || (*char.unwrap() == '.' && iter.peek().unwrap().is_numeric()) {
        self.parse_number(iter, char.unwrap());

      } else if char.unwrap().is_alphabetic() || ['$', '_'].contains(char.unwrap()) {
        self.parse_identifier(iter, char.unwrap());

      } else if OPERATORS.contains(&&*char.unwrap().to_string()) {
        self.parse_operator(iter, char.unwrap());

      } else {
        match char.unwrap() {
          '(' => { self.add_token(TokenTypes::LParen, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          ')' => { self.add_token(TokenTypes::RParen, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          '[' => { self.add_token(TokenTypes::LBracket, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          ']' => { self.add_token(TokenTypes::RBracket, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          '{' => { self.add_token(TokenTypes::LBrace, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          '}' => { self.add_token(TokenTypes::RBrace, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          ';' => { self.add_token(TokenTypes::Semicolon, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          ':' => { self.add_token(TokenTypes::Colon, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          ',' => { self.add_token(TokenTypes::Comma, Some(char.unwrap().to_string()), self.line, Some(self.line_pos - 1), Some(self.line_pos - 1)); },
          '\'' | '"' => self.parse_string(iter, char.unwrap()),
          _ => self.add_token(TokenTypes::EOF, None, self.line, Some(self.line_pos), Some(self.line_pos))
        };
      }
    }

    if self.tokens.last().is_none() || self.tokens.last().unwrap().of_type != TokenTypes::EOF {
      self.add_token(TokenTypes::EOF, None, self.line, Some(self.line_pos), Some(self.line_pos));
    }

    return self.tokens.clone();
  }

  fn parse_identifier(&mut self, iter: &mut Peekable<Iter<char>>, init: &char) {
    // Prep string to hold identifier
    let mut identifier: String = init.to_string();
  
    // Build identifier
    while iter.peek().is_some() && (iter.peek().unwrap().is_alphabetic() || iter.peek().unwrap() == &&'_') {
      identifier.push(*iter.next().unwrap());
      self.pos += 1;
      self.line_pos += 1;
    }

    // If identifier is a keyword
    if KEYWORDS.contains(&&*identifier) {
      self.add_token(TokenTypes::Keyword, Some(identifier.clone()), self.line, Some(self.line_pos - identifier.len() as i32), Some(self.line_pos));
    } else {
      self.add_token(TokenTypes::Identifier, Some(identifier.clone()), self.line, Some(self.line_pos - identifier.len() as i32), Some(self.line_pos));
    }
  }

  fn process_comment(&mut self, iter: &mut Peekable<Iter<char>>) {
    let start_pos = self.line_pos;
    let start_line = self.line;

    // Get comment type
    let char = iter.next().unwrap();
    self.pos += 1;
    self.line_pos += 1;

    // Single-line comments
    if *char == '/' {
      while iter.peek().is_some() && !['\r', '\n'].contains(iter.peek().unwrap()) {
        iter.next();
        self.pos += 1;
        self.line_pos += 1;
      }

    // Block comments
    } else {
      let mut cur = iter.next();
      self.pos += 1;
      self.line_pos += 1;

      while cur.is_some() && iter.peek().is_some() && *cur.unwrap() != '*' && *iter.peek().unwrap() != &'/' {
        cur = iter.next();
        self.pos += 1;

        if *cur.unwrap() == '\n' {
          self.line += 1;
          self.line_pos = 0;
        } else {
          self.line_pos += 1;
        }
      }

      let end = iter.next();
      self.pos += 1;
      self.line_pos += 1;

      if end.is_none() || *end.unwrap() != '/' {
        panic!("Block comment not closed at {}:{}\n  {}\n  {}",
          start_line, start_pos, self.code.lines().nth(usize::try_from(start_line - 1).unwrap()).unwrap(), " ".repeat(usize::try_from(start_pos - 1).unwrap()) + "^"
        );
      }
    }
  }

  fn parse_operator(&mut self, iter: &mut Peekable<Iter<char>>, init: &char) {
    // Start building operator
    let mut operator = String::from(*init);
    let start = self.line_pos;

    // While we are still working with the operator
    while iter.peek().is_some() && OPERATORS.contains(&&(operator.clone() + &iter.peek().unwrap().to_string()).as_str()) {
      operator.push(*iter.next().unwrap());
      self.pos += 1;
      self.line_pos += 1;
    }

    if !OPERATORS.contains(&&*operator) {
      panic!("Invalid operator at {}:{}\n  {}\n  {}",
        self.line, start, self.code.lines().nth(usize::try_from(self.line - 1).unwrap()).unwrap(), " ".repeat(usize::try_from(start - 1).unwrap()) + "^"
      );
    }

    self.add_token(TokenTypes::Operator, Some(operator.clone()), self.line, Some(start - 1), Some(self.line_pos));
  }

  fn parse_string(&mut self, iter: &mut Peekable<Iter<char>>, char: &char) {
    let mut string = String::new();
    let mut end = false;
    let start_line = self.line;
    let start_line_pos = self.line_pos;

    while iter.peek().is_some() {
      // Closing the string
      if iter.peek().unwrap() == &char {
        end = true;
        iter.next();
        self.pos += 1;
        self.line_pos += 1;
        break;
      }

      // Handle escaped characters
      if iter.peek().unwrap() == &&'\\' {
        iter.next();
        self.pos += 1;
        self.line_pos += 1;

        // Get escaped character
        let escaped = iter.next();

        // Literal newline
        if escaped.unwrap() == &'n' {
          string.push('\n');
          self.pos += 1;
          self.line_pos = 1;

        // Escaped newline
        } else if ['\r', '\n'].contains(escaped.unwrap()) {
          self.pos += 1;
          self.line += 1;
          self.line_pos = 0;

          if escaped.unwrap() == &'\r' && iter.peek().unwrap() == &&'\n' {
            iter.next();
          }

        // All other escapes
        } else {
          string.push(*escaped.unwrap());
          self.pos += 1;
          self.line_pos += 1;
        }

      // Break without closing on unescaped newline
      } else if iter.peek().unwrap() == &&'\n' {
        break;

      // Add to string
      } else {
        string.push(*iter.next().unwrap());
        self.pos += 1;
        self.line_pos += 1;
      }
    }

    // String was not properly enclosed
    if !end {
      panic!("A string was not properly enclosed at {}:{}\n  {}\n  {}",
        start_line, start_line_pos, self.code.lines().nth(usize::try_from(start_line - 1).unwrap()).unwrap(), " ".repeat(usize::try_from(start_line_pos - 1).unwrap()) + "^"
      );
    }

    self.add_token(TokenTypes::String, Some(string.clone()), self.line, Some(self.line_pos - string.len() as i32), Some(self.line_pos));
  }

  fn parse_number(&mut self, iter: &mut Peekable<Iter<char>>, init: &char) {
    let mut number = String::from(*init);
    let start = self.line_pos;

    // While we are still working with the number
    while iter.peek().is_some() && (iter.peek().unwrap().is_numeric() || iter.peek().unwrap() == &&'.') {
      // Push next to number string
      number.push(*iter.next().unwrap());
      self.pos += 1;
      self.line_pos += 1;
    }

    if number.matches(".").count() > 1 {
      panic!("Invalid number at {}:{}\n  {}\n  {}",
        self.line, start, self.code.lines().nth(usize::try_from(self.line - 1).unwrap()).unwrap(), " ".repeat(usize::try_from(start - 1).unwrap()) + "^"
      );
    }

    self.add_token(TokenTypes::Number, Some(number.clone()), self.line, Some(self.line_pos - number.len() as i32), Some(self.line_pos));
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lex_number() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("123.456".to_string(), true), vec![
      Token { of_type: TokenTypes::Number, value: Some("123.456".to_string()), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 1, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_string() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("\"Hello, world!\"".to_string(), true), vec![
      Token { of_type: TokenTypes::String, value: Some("Hello, world!".to_string()), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 1, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_assignments() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("let a = 1;
    const name = \"Jink\"
    type Number = int;".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("=")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("1")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 1, ..Default::default() },

      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("const")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("name")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("=")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::String, value: Some(String::from("Jink")), line: 2, ..Default::default() },

      // Type alias
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("type")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("Number")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("=")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("int")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 3, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_conditional() {
    let mut lexer = Lexer::new();

    assert_eq!(lexer.lex("if (a == 1) {
      return a;
    } else {
      return b;
    }".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("if")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("==")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("1")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LBrace, value: Some(String::from("{")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::RBrace, value: Some(String::from("}")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("else")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::LBrace, value: Some(String::from("{")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 4, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 4, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 4, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 4, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 5, ..Default::default() },
      Token { of_type: TokenTypes::RBrace, value: Some(String::from("}")), line: 5, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 5, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_function_call() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("print(\"Hello, world!\")".to_string(), true), vec![
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("print")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::String, value: Some(String::from("Hello, world!")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 1, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_function_def() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("fun add(let a, let b) {
      return a + b;
    }".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("fun")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("add")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Comma, value: Some(String::from(",")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LBrace, value: Some(String::from("{")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("+")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::RBrace, value: Some(String::from("}")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 3, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_function_def_inline() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("fun sub(let a, let b) return a - b;".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("fun")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("sub")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Comma, value: Some(String::from(",")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("-")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 1, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_function_with_defaults() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("fun pow(let a: 1, let b: 2, let c: 3) {
      return a ^ b ^ c;
    }".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("fun")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("pow")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Colon, value: Some(String::from(":")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("1")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Comma, value: Some(String::from(",")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Colon, value: Some(String::from(":")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("2")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Comma, value: Some(String::from(",")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("c")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Colon, value: Some(String::from(":")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("3")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LBrace, value: Some(String::from("{")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("^")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("^")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("c")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::RBrace, value: Some(String::from("}")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 3, ..Default::default() }
    ]);
  }


  #[test]
  fn test_lex_function_with_return_type() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("fun add(int a, int b) -> int {
      return a + b;
    }".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("fun")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("add")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("int")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Comma, value: Some(String::from(",")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("int")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("->")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("int")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LBrace, value: Some(String::from("{")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("+")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::RBrace, value: Some(String::from("}")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 3, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_function_with_inline_conditional() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("fun are_even(int a, int b) -> int {
      if (a % 2 == 0 && b % 2 == 0) return true
      else return false
    }".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("fun")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("are_even")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("int")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Comma, value: Some(String::from(",")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("int")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("->")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("int")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::LBrace, value: Some(String::from("{")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("if")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::LParen, value: Some(String::from("(")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("%")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("2")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("==")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("0")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("&&")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("%")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("2")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("==")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("0")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::RParen, value: Some(String::from(")")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("true")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("else")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("return")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("false")), line: 3, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 4, ..Default::default() },
      Token { of_type: TokenTypes::RBrace, value: Some(String::from("}")), line: 4, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 4, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_newline_inside_string() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("\"Hello,\\nworld!\"".to_string(), true), vec![
      Token { of_type: TokenTypes::String, value: Some(String::from("Hello,\nworld!")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 1, ..Default::default() }
    ]);
  }

  #[test]
  fn test_lex_ignore_windows_newline_char() {
    let mut lexer = Lexer::new();
    assert_eq!(lexer.lex("let a = 1;\r\nlet b = 2;".to_string(), true), vec![
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("a")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("=")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("1")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 1, ..Default::default() },
      Token { of_type: TokenTypes::Newline, value: Some(String::from("\n")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Keyword, value: Some(String::from("let")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Identifier, value: Some(String::from("b")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Operator, value: Some(String::from("=")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Number, value: Some(String::from("2")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::Semicolon, value: Some(String::from(";")), line: 2, ..Default::default() },
      Token { of_type: TokenTypes::EOF, value: None, line: 2, ..Default::default() }
    ]);
  }
}