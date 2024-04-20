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
  pub pos: usize,
  pub line: i32,
  pub line_pos: i32,
  pub code_end: usize,
  pub tokens: Vec<Token>
}

impl Lexer {
  pub fn lex(&mut self, code: String) -> Vec<Token> {
    self.code = code;
    let code = self.code.chars().collect::<Vec<char>>();
    self.code_end = code.len();
    let mut iter = code.iter().peekable();

    self.parse_tokens(&mut iter)
  }

  fn parse_tokens(&mut self, iter: &mut Peekable<Iter<char>>) -> Vec<Token> {
    while self.pos < self.code_end {
      // Increment positions
      self.pos += 1;
      self.line_pos += 1;

      let char = iter.next();
      if char.is_none() {
        self.tokens.push(Token { of_type: TokenTypes::EOF, value: None, line: self.line });
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
          self.tokens.push(Token { of_type: TokenTypes::Newline, value: Some("\n".to_string()), line: self.line });
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
        let cur = match char.unwrap() {
          '(' => Token { of_type: TokenTypes::LParen, value: Some(char.unwrap().to_string()), line: self.line },
          ')' => Token { of_type: TokenTypes::RParen, value: Some(char.unwrap().to_string()), line: self.line },
          '[' => Token { of_type: TokenTypes::LBracket, value: Some(char.unwrap().to_string()), line: self.line },
          ']' => Token { of_type: TokenTypes::RBracket, value: Some(char.unwrap().to_string()), line: self.line },
          '{' => Token { of_type: TokenTypes::LBrace, value: Some(char.unwrap().to_string()), line: self.line },
          '}' => Token { of_type: TokenTypes::RBrace, value: Some(char.unwrap().to_string()), line: self.line },
          ';' => Token { of_type: TokenTypes::Semicolon, value: Some(char.unwrap().to_string()), line: self.line },
          ':' => Token { of_type: TokenTypes::Colon, value: Some(char.unwrap().to_string()), line: self.line },
          ',' => Token { of_type: TokenTypes::Comma, value: Some(char.unwrap().to_string()), line: self.line },
          '\'' | '"' => self.parse_string(iter, char.unwrap()),
          _ => Token { of_type: TokenTypes::EOF, value: None, line: self.line }
        };
        self.tokens.push(cur);
      }
    }

    if self.tokens.last().unwrap().of_type != TokenTypes::EOF {
      self.tokens.push(Token { of_type: TokenTypes::EOF, value: None, line: self.line });
    }

    return self.tokens.clone();
  }

  fn parse_identifier(&mut self, iter: &mut Peekable<Iter<char>>, init: &char) {
    // Prep string to hold identifier
    let mut identifier = String::new();
  
    // Hold current char
    let mut char: Option<&char> = Some(init);

    while iter.peek().is_some() && (char.unwrap().is_alphabetic() || *char.unwrap() == '_') {
      // Add char to identifier
      identifier.push(*char.unwrap());

      // Check next character
      if iter.peek().is_some() && (iter.peek().unwrap().is_alphabetic() || *iter.peek().unwrap() == &'_') {
        char = iter.next();
        self.pos += 1;
        self.line_pos += 1;
      } else {
        break;
      }
    }

    // If identifier is a keyword
    if KEYWORDS.contains(&&*identifier) {
      self.tokens.push(Token { of_type: TokenTypes::Keyword, value: Some(identifier), line: self.line });
    } else {
      self.tokens.push(Token { of_type: TokenTypes::Identifier, value: Some(identifier), line: self.line });
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

    // Jink only makes use of two-char operators..fortunately
    // Check if we have a two-char operator on our hands and add it
    if OPERATORS.contains(&&*String::from([String::from(*init), iter.peek().unwrap().to_string()].join(""))) {
      operator.push(*iter.next().unwrap());
      self.pos += 1;
      self.line_pos += 1;
    }

    if !OPERATORS.contains(&&*operator) {
      panic!("Invalid operator at {}:{}\n  {}\n  {}",
        self.line, start, self.code.lines().nth(usize::try_from(self.line - 1).unwrap()).unwrap(), " ".repeat(usize::try_from(start - 1).unwrap()) + "^"
      );
    }

    self.tokens.push(Token { of_type: TokenTypes::Operator, value: Some(operator), line: self.line });
  }

  fn parse_string(&mut self, iter: &mut Peekable<Iter<char>>, char: &char) -> Token {
    let mut string = String::new();
    let mut end = false;
    let start = self.line_pos;

    let mut cur = iter.next();
    self.pos += 1;
    self.line_pos += 1;

    while cur.is_some() {
      // Closing the string
      if cur.unwrap() == char {
        end = true;
        break;
      }

      // Handle escaped characters
      if *cur.unwrap() == '\\' {
        let escaped = iter.next();
        self.pos += 1;
        self.line_pos += 1;

        // Handle case of newlines
        if ['\n', 'n'].contains(escaped.unwrap()) {
          println!("Making sure newlines escape correctly in strings.");
          string.push('\n');
          self.line += 1;
          self.line_pos = 0;

        // All other escapes
        } else {
          string.push(*escaped.unwrap());
          cur = iter.next();
          self.pos += 1;
          self.line_pos += 1;
        }

      // Add to string
      } else {
        string.push(*cur.unwrap());
        cur = iter.next();
        self.pos += 1;
        self.line_pos += 1;
      }
    }

    // String was not properly enclosed
    if cur.is_none() && !end {
      panic!("A string was not properly enclosed at {}:{}\n  {}\n  {}",
        self.line, start, self.code.lines().nth(usize::try_from(self.line - 1).unwrap()).unwrap(), " ".repeat(usize::try_from(start - 1).unwrap()) + "^"
      );
    }

    Token { of_type: TokenTypes::String, value: Some(string), line: self.line }
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

    self.tokens.push(Token { of_type: TokenTypes::Number, value: Some(number), line: self.line });
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lex_number() {
    let mut lexer = Lexer {
      code: String::new(),
      pos: 0,
      line: 1,
      line_pos: 0,
      code_end: 0,
      tokens: vec![]
    };

    assert_eq!(lexer.lex("123.456".to_string()), vec![
      Token { of_type: TokenTypes::Number, value: Some("123.456".to_string()), line: 1 },
      Token { of_type: TokenTypes::EOF, value: None, line: 1 }
    ]);
  }

  #[test]
  fn test_lex_string() {
    let mut lexer = Lexer {
      code: String::new(),
      pos: 0,
      line: 1,
      line_pos: 0,
      code_end: 0,
      tokens: vec![]
    };

    assert_eq!(lexer.lex("\"Hello, world!\"".to_string()), vec![
      Token { of_type: TokenTypes::String, value: Some("Hello, world!".to_string()), line: 1 },
      Token { of_type: TokenTypes::EOF, value: None, line: 1 }
    ]);
  }

  #[test]
  fn test_lex_function() {
    let mut lexer = Lexer {
      code: String::new(),
      pos: 0,
      line: 1,
      line_pos: 0,
      code_end: 0,
      tokens: vec![]
    };

    assert_eq!(lexer.lex("fun add(a, b) { return a + b; }".to_string()), vec![
      Token { of_type: TokenTypes::Keyword, value: Some("fun".to_string()), line: 1 },
      Token { of_type: TokenTypes::Identifier, value: Some("add".to_string()), line: 1 },
      Token { of_type: TokenTypes::LParen, value: Some("(".to_string()), line: 1 },
      Token { of_type: TokenTypes::Identifier, value: Some("a".to_string()), line: 1 },
      Token { of_type: TokenTypes::Comma, value: Some(",".to_string()), line: 1 },
      Token { of_type: TokenTypes::Identifier, value: Some("b".to_string()), line: 1 },
      Token { of_type: TokenTypes::RParen, value: Some(")".to_string()), line: 1 },
      Token { of_type: TokenTypes::LBrace, value: Some("{".to_string()), line: 1 },
      Token { of_type: TokenTypes::Keyword, value: Some("return".to_string()), line: 1 },
      Token { of_type: TokenTypes::Identifier, value: Some("a".to_string()), line: 1 },
      Token { of_type: TokenTypes::Operator, value: Some("+".to_string()), line: 1 },
      Token { of_type: TokenTypes::Identifier, value: Some("b".to_string()), line: 1 },
      Token { of_type: TokenTypes::Semicolon, value: Some(";".to_string()), line: 1 },
      Token { of_type: TokenTypes::RBrace, value: Some("}".to_string()), line: 1 },
      Token { of_type: TokenTypes::EOF, value: None, line: 1 }
    ]);
  }  
}