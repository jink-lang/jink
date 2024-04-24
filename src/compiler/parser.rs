use core::panic;

use jink::FutureIter;
use jink::Token;
use jink::TokenTypes;
use jink::Operator;
use jink::Name;
use jink::Literals;
use jink::Expression;
use jink::Type;

use super::lexer::Lexer;

pub struct Parser {
  pub code: String,
  pub iter: FutureIter,
  // pub pos: usize,
  // pub tok_end: usize,
  pub ast: Vec<Expression>
}

impl Parser {
  pub fn new() -> Self {
    Parser {
      code: String::new(),
      iter: FutureIter::new(vec![]),
      // pos: 0,
      // tok_end: iterator.input.len(),
      ast: Vec::new(),
    }
  }

  // Build AST
  pub fn parse(&mut self, code: String, _verbose: bool) -> Vec<Expression> {
    let tokens = Lexer::new().lex(code.clone(), false);
    let iterator = FutureIter::new(tokens);
    self.code = code;
    self.iter = iterator;

    let mut ast: Vec<Expression> = Vec::new();
    while self.iter.current.is_some() {
      self.skip_newlines(None);
      let parsed: Expression = self.parse_top();

      // End of file
      if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
        ast.push(parsed);
        return ast;
      }

      // Distinguish expressions in the main scope
      self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false);
      ast.push(parsed);
    }

    return ast;
  }

  // Removes expected token, given a type or a multiple types
  fn consume(&mut self, tokens: &[TokenTypes], soft: bool) -> Token {
    let current = self.iter.next().unwrap();

    // Soft consume doesn't error if the token isn't found
    if soft { return current; }

    if !tokens.contains(&current.of_type) {
      let formatted = tokens.iter().map(|t| format!("{:?}", t)).collect::<Vec<String>>().join(" or ");
      panic!("Expected {}, got {} on line {}.", formatted, current.clone().of_type, current.line);
    }

    return current;
  }

  // Skip all newlines or a specific amount
  fn skip_newlines(&mut self, count: Option<i16>) {
    let mut c: i16 = count.unwrap_or(-1);
    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Newline && c != 0 {
      if c > 0 { c -= 1; }
      self.iter.next();
    }
  }

  fn parse_top(&mut self) -> Expression {
    let init = self.iter.current.as_ref().clone();

    if init.is_none() || init.unwrap().of_type == TokenTypes::EOF {
      return Expression::Literal(Literals::EOF);

    } else if init.unwrap().of_type != TokenTypes::Keyword {
      return self.parse_expression(0);

    } else if ["let", "const"].contains(&init.unwrap().value.as_ref().unwrap().as_str()) {
      let assignment = self.iter.next().unwrap();
      let identifier = self.consume(&[TokenTypes::Identifier], false);

      if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "="
        || [TokenTypes::Newline, TokenTypes::Semicolon].contains(&self.iter.current.as_ref().unwrap().of_type) {
        return self.parse_assignment(Some(&assignment), identifier);
      } else {
        panic!("Expected \"=\", got {} on line {}.",
          self.iter.current.as_ref().unwrap().of_type,
          self.iter.current.as_ref().unwrap().line
        );
      }

    } else if init.unwrap().value.as_ref().unwrap() == "import" {
      return self.parse_module();

    } else if init.unwrap().value.as_ref().unwrap() == "fun" {
      self.iter.next();
      return self.parse_function();

    } else if init.unwrap().value.as_ref().unwrap() == "return" {
      return self.parse_return();

    } else if init.unwrap().value.as_ref().unwrap() == "if" {
      return self.parse_conditional();

    } else if init.unwrap().value.as_ref().unwrap() == "cls" {
      return self.parse_class();

    } else if init.unwrap().value.as_ref().unwrap() == "type" {
      self.iter.next();
      return self.parse_type();

    } else if init.unwrap().value.as_ref().unwrap() == "null" {
      self.iter.next();
      return Expression::Literal(Literals::Null);

    } else {
      panic!("Unexpected token {:?} on line {}.", init.unwrap().of_type, init.unwrap().line);
    }

    //   self.ast
  }

  fn parse_expression(&mut self, precedence: i8) -> Expression {
    let mut left = self.parse_primary();

    while self.iter.current.is_some()
      && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
      && self.get_precedence(self.iter.current.as_ref().unwrap().clone()) >= precedence {

      let operator = self.iter.next().unwrap();
      if ["++", "--"].contains(&&operator.value.as_ref().unwrap().as_str()) {
        return Expression::UnaryOperator(
          Operator(String::from(operator.value.as_ref().unwrap().to_owned() + ":post")),
          Box::new(left)
        );
      }

      let mut next_precedence = self.get_precedence(operator.clone());
      if self.is_left_associative(operator.clone()) { next_precedence += 1; }

      let right = self.parse_expression(next_precedence);
      left = Expression::BinaryOperator(
        Operator((*operator.to_owned().value.unwrap()).to_string()),
        Box::new(left),
        Box::new(right)
      );
    }

    // if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Semicolon {
    //   self.consume(&[TokenTypes::Semicolon], false);
    // }

    left
  }

  fn parse_primary(&mut self) -> Expression {
    self.skip_newlines(None);

    if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
      panic!("Unexpected end of file.");
    }

    // Unary operators
    if self.is_unary_operator(&self.iter.current.as_ref().unwrap().clone()) {
      let operator = self.iter.next().unwrap();

      if ![TokenTypes::Identifier, TokenTypes::Number, TokenTypes::LParen].contains(&self.iter.current.as_ref().unwrap().of_type) {
        panic!("Unexpected token {:?} on line {}.",
          self.iter.current.as_ref().unwrap().value.as_ref().unwrap(),
          self.iter.current.as_ref().unwrap().line
        );
      }

      if ["+", "-", "!"].contains(&operator.clone().value.unwrap().as_str()) {
        let value = self.parse_primary();
        return Expression::UnaryOperator(Operator(operator.clone().value.unwrap()), Box::new(value));
      }

      let precedence = self.get_precedence(operator.clone());
      let value = self.parse_expression(precedence);
      return Expression::UnaryOperator(
        Operator(operator.clone().to_string()),
        Box::new(value)
      );

    // Parentheses
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
      self.consume(&[TokenTypes::LParen], false);
      let expr = self.parse_expression(0);
      self.consume(&[TokenTypes::RParen], false);
      return expr;

    // Arrays
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LBracket {
      self.consume(&[TokenTypes::LBracket], false);
      let arr = self.parse_array();
      self.consume(&[TokenTypes::RBracket], false);
      return arr;

    // Objects
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LBrace {
      return self.parse_object(None);

    // Numbers
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Number {
      let cur = self.iter.next().unwrap();
      if cur.clone().value.unwrap().contains(".") {
        return Expression::Literal(Literals::FloatingPoint(cur.value.unwrap().parse::<f64>().unwrap()));
      } else {
        return Expression::Literal(Literals::Integer(cur.value.unwrap().parse::<i64>().unwrap()));
      }

    // Strings
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::String {
      return Expression::Literal(Literals::String(self.iter.next().unwrap().value.unwrap().to_owned()));

    // Identifiers
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
      let ident = self.iter.next().unwrap();

      // TODO: Object property
      // if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Dot {

      // EOF, return ident literal
      if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
        return Expression::Literal(Literals::Identifier(Name(String::from(ident.value.unwrap())), None));

      // Call
      } else if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
        return self.parse_call(ident);

      // Assignment
      } else if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "=" {
        return self.parse_assignment(None, ident);

      // Typed assignment
      } else if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
        // We have a type, get the identifier 
        let name = self.iter.next().unwrap();
        return self.parse_assignment(Some(&ident), name);

      // Literal
      } else {
        return Expression::Literal(
          Literals::Identifier(Name(String::from(ident.value.unwrap())), None)
        );
      }

    // Keywords
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword {
      let keyword = self.iter.next().unwrap();

      // Calls
      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
        return self.parse_call(keyword);

      // Booleans
      } else if ["true", "false"].contains(&keyword.value.as_ref().unwrap().as_str()) {
        return Expression::Literal(Literals::Boolean(keyword.value.as_ref().unwrap() == "true"));

      // Nulls
      } else if keyword.value.as_ref().unwrap() == "null" {
        return Expression::Literal(Literals::Null);

      } else {
        panic!("Unexpected keyword {:?} on line {}.", keyword.value.clone().unwrap(), keyword.line);
      }

    } else {
      panic!("Unexpected token {:?} on line {}.",
        self.iter.current.as_ref().unwrap().value.as_ref().unwrap(),
        self.iter.current.as_ref().unwrap().line
      );
    }

  }

  fn is_unary_operator(&mut self, token: &Token) -> bool {
    if token.of_type != TokenTypes::Operator { return false; }
    return [
      "+", "-", "++", "--", "!"
    ].contains(&token.value.as_ref().unwrap().as_str());
  }

  fn is_left_associative(&mut self, operator: Token) -> bool {
    return ![
      "++", "--", "+=", "-=", "*=", "/=", "//=", "%=",
      ">", "<", ">=", "<=", "==", "!=",
      "=", "!", "+", "-", "->", ".", "&&", "||",
    ].contains(&operator.value.unwrap().as_str());
  }

  fn get_precedence(&mut self, operator: Token) -> i8 {
    match operator.value.as_ref().unwrap().as_str() {
      "+=" | "-=" | "*=" | "/=" | "//=" | "%=" => 1,
      "||" => 2,
      "&&" => 3,
      "^" => 4,
      "==" | "!=" => 5,
      "+" | "-" => 6,
      "*" | "/" | "//" | "%" => 7,
      _ => 0
    }
  }

  fn parse_assignment(&mut self, _type: Option<&Token>, identifier: Token) -> Expression {
    // Get type if it exists
    let mut assignment_type: Option<Type> = None;
    if _type.is_some() {
      assignment_type = Some(Type(_type.unwrap().value.as_ref().unwrap().to_owned()));
    }

    let assignment: Expression;

    // TODO: Handle comma case (multiple assignments at once)
    if [TokenTypes::Newline, TokenTypes::Semicolon].contains(&self.iter.current.as_ref().unwrap().of_type) {
      assignment = Expression::Assignment(
        assignment_type,
        Box::new(Literals::Identifier(Name(String::from(identifier.value.unwrap())), None)),
        None
      );
    } else if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "=" {
      self.iter.next();
      assignment = Expression::Assignment(
        assignment_type,
        Box::new(Literals::Identifier(Name(String::from(identifier.value.unwrap())), None)),
        Some(Box::new(self.parse_expression(0)))
      );
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
      assignment = Expression::Assignment(
        assignment_type,
        Box::new(Literals::Identifier(Name(String::from(identifier.value.unwrap())), None)),
        None
      );
    } else {
      assignment = Expression::Assignment(
        assignment_type,
        Box::new(Literals::Identifier(Name(String::from(identifier.value.unwrap())), None)),
        Some(Box::new(self.parse_expression(0)))
      );
    }

    return assignment;
  }

  fn parse_array(&mut self) -> Expression {
    let mut list = vec![];
    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBracket {
      list.push(self.parse_primary());
      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Comma {
        self.iter.next();
      } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBracket {
        break;
      } else {
        panic!("Expected \",\" or \"]\", got {:?} on line {}.",
          self.iter.current.as_ref().unwrap().value.as_ref().unwrap(),
          self.iter.current.as_ref().unwrap().line
        );
      }
    }

    return Expression::Array(Box::new(list));
  }

  fn parse_type(&mut self) -> Expression {
    let ident = self.consume(&[TokenTypes::Identifier], false).clone();

    // Expect =
    if !self.iter.current.is_some()
      || self.iter.current.as_ref().unwrap().of_type != TokenTypes::Operator
      || self.iter.current.as_ref().unwrap().value.as_ref().unwrap() != "=" {
      panic!("Expected \"=\", got {:?} on line {}.", ident.value.as_ref().unwrap(), ident.line);
    }
    self.iter.next();

    // If type alias
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
      let t = self.iter.next().unwrap();
      return Expression::TypeDef(
        Literals::Identifier(Name(String::from(ident.value.unwrap())), None),
        Box::new(Literals::Identifier(Name(String::from(t.value.unwrap())), None))
      );

    // If typedef / struct
    } else {
      return self.parse_object(Some(ident));
    }

  }

  fn parse_object(&mut self, type_tok: Option<Token>) -> Expression {
    self.consume(&[TokenTypes::LBrace], false);
    let mut obj: Vec<Literals> = vec![];

    let init = self.iter.current.as_ref().unwrap().clone();

    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
      self.skip_newlines(None);
      let key = self.consume(&[TokenTypes::Identifier], false);
      self.consume(&[TokenTypes::Colon], false);

      // object
      if type_tok.is_none() {
        let value = self.parse_expression(0);
        obj.push(Literals::ObjectProperty(
          Some(Name(key.value.unwrap())),
          Box::new(value)
        ));

      // typedef / struct
      } else {
        let val = self.consume(&[TokenTypes::Identifier], false);
        obj.push(Literals::ObjectProperty(
          Some(Name(key.value.unwrap())),
          Box::new(Expression::Literal(
            Literals::Identifier(Name(val.value.unwrap()), None)
          ))
        ));
      }

      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Comma {
        self.iter.next();
        continue;
      }

      self.skip_newlines(None);
    }

    // End of construction
    if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBrace {
      self.consume(&[TokenTypes::RBrace], false);

      // object
      if type_tok.is_none() {
        return Expression::Literal(Literals::Object(Box::new(obj)));

      // typedef / struct
      } else {
        return Expression::TypeDef(
          Literals::Identifier(Name(String::from(type_tok.unwrap().value.unwrap())), None),
          Box::new(Literals::Object(Box::new(obj)))
        );
      }
    } else {
      panic!("Expected \",\" or \"}}\", got {:?} following object definition on line {}.",
        init.value.as_ref().unwrap(), init.line
      );
    }
  }

  fn parse_call(&mut self, identifier: Token) -> Expression {
    let args = self.parse_args_params("args");
    return Expression::Call(Name(identifier.value.unwrap().to_owned()), Box::new(args));
  }

  fn parse_function(&mut self) -> Expression {
    let identifier = self.consume(&[TokenTypes::Identifier], false);
    let params = self.parse_args_params("params");

    // Get return type
    let mut return_type: Option<Token> = None;
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
      && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "->" {
      self.iter.next();
      // Identifier for now
      return_type = Some(self.consume(&[TokenTypes::Identifier], false));
    }

    let body = self.parse_block();

    if return_type.is_some() {
      return Expression::Function(
        Name(identifier.value.unwrap().to_owned()),
        Some(Literals::Identifier(Name(return_type.unwrap().value.unwrap()), None)),
        Some(Box::new(params)),
        Some(Box::new(body))
      );
    } else {
      return Expression::Function(
        Name(identifier.value.unwrap().to_owned()),
        None,
        Some(Box::new(params)),
        Some(Box::new(body))
      )
    }
  }

  fn parse_args_params(&mut self, parse_type: &str) -> Vec<Expression> {
    self.consume(&[TokenTypes::LParen], false);
    let mut list: Vec<Expression> = vec![];

    // Skip by one newline if there is one
    self.skip_newlines(Some(1));

    // Function parameters
    if parse_type == "params" {
      while self.iter.current.is_some() {
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
          self.consume(&[TokenTypes::RParen], false);
          break;
        } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBrace {
          break;
        }

        // Type identifier, let or const
        let cur = self.iter.next().unwrap();
        if cur.of_type == TokenTypes::Identifier || (
          cur.of_type == TokenTypes::Keyword && ["let", "const"].contains(&&cur.value.as_ref().unwrap().as_str())
        ) {
          let ident = self.consume(&[TokenTypes::Identifier], false);

          // Close out function params
          if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
            list.push(Expression::FunctionParam(
              Type(String::from(cur.value.unwrap())),
              Literals::Identifier(Name(String::from(ident.value.unwrap())), None),
              None
            ));

          // Expect colon for param defaults
          // fun num(int a: 1) {}
          // Operator check here in case we make colon an operator
          } else if [TokenTypes::Colon, TokenTypes::Operator].contains(&self.iter.current.as_ref().unwrap().of_type)
            && [":"].contains(&self.iter.current.as_ref().unwrap().value.as_ref().unwrap().as_str()) {

            if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == ":" {
              self.iter.next();
              let default = self.parse_expression(0);
              list.push(Expression::FunctionParam(
                Type(String::from(cur.value.as_ref().unwrap())),
                Literals::Identifier(Name(String::from(ident.value.as_ref().unwrap())), None),
                Some(Box::new(default))
              ));
            }

            // We have comma, continue to next param
            if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "," {
              self.iter.next();
              // Skip by one newline if there is one
              self.skip_newlines(Some(1));
              continue;
            }

          // Expect comma for multiple params without defaults
          // fun add(int a, int b) {}
          } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Comma {
            self.iter.next();

            // Skip by one newline if there is one
            self.skip_newlines(Some(1));

            list.push(Expression::FunctionParam(
              Type(String::from(cur.value.as_ref().unwrap())),
              Literals::Identifier(Name(String::from(ident.value.as_ref().unwrap())), None),
              None
            ));

          } else {
            panic!("Expected \")\", \",\" or \":\", got {:?} on line {}.",
              self.iter.current.as_ref().unwrap().value.as_ref().unwrap(),
              self.iter.current.as_ref().unwrap().line
            );
          }
        } else {
          panic!("Expected type, \"let\" or \"const\", got {:?} on line {}.", cur.of_type, cur.line);
        }
      }

    // Call arguments
    } else {
      while self.iter.current.is_some() {
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
          self.consume(&[TokenTypes::RParen], false);
          break;
        }
        list.push(self.parse_top());
        // Multiple arguments, continue
        if [TokenTypes::Comma, TokenTypes::Newline].contains(&self.iter.current.as_ref().unwrap().of_type) {
          self.consume(&[TokenTypes::Comma, TokenTypes::Newline], true);
        // End of arguments, break
        } else {
          self.consume(&[TokenTypes::RParen], false);
          break;
        }
      }
    }

    return list;
  }

  fn parse_return(&mut self) -> Expression {
    self.iter.next();
    let ret: Expression;
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Semicolon {
      // self.iter.next();
      ret = Expression::Return(Box::new(Expression::Literal(Literals::Null)));
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Newline {
      ret = Expression::Return(Box::new(Expression::Literal(Literals::Null)));
    } else {
      ret = Expression::Return(Box::new(self.parse_expression(0)));
    }

    return ret;
  }

  fn parse_conditional(&mut self) -> Expression {
    let init = self.iter.next().unwrap();

    // Parse else first because it is unlike if/elseif
    if init.value.as_ref().unwrap() == "else" {
      return Expression::Conditional(
        Type(String::from("else")),
        None,
        Some(Box::new(self.parse_block())),
        None
      );
    }

    let mut else_body = vec![];
    self.consume(&[TokenTypes::LParen], false);
    let expr = self.parse_expression(0);
    self.consume(&[TokenTypes::RParen], false);
    let body = self.parse_block();

    // If an else case is next
    if self.iter.current.is_some()
      && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword
      && ["elseif", "else"].contains(&self.iter.current.as_ref().unwrap().value.as_ref().unwrap().as_str()) {
      else_body.push(self.parse_conditional());
    }

    return Expression::Conditional(
      Type(String::from(init.value.unwrap())),
      Some(Box::new(expr)),
      Some(Box::new(body)),
      Some(Box::new(else_body))
    )
  }

  // TODO: Move actual block parsing to its own func (see block parsing for classes)
  // and rename this to parse function block bc it clearly caters just to functions
  // and then change the call to this from function calls because oops that makes no sense
  fn parse_block(&mut self) -> Vec<Expression> {
    let mut body: Vec<Expression> = Vec::new();

    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LBrace {
      self.consume(&[TokenTypes::LBrace], false);
      self.skip_newlines(None);
      while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
        body.push(self.parse_top());
        // Break if early end of block
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBrace {
          break;
        }
        self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false);
        self.skip_newlines(None);
      }
      if self.iter.current.is_none() {
        panic!("Expected '}}', got '{:?}' on line {}.", self.iter.current.as_ref().unwrap().of_type, self.iter.current.as_ref().unwrap().line);
      } else {
        self.consume(&[TokenTypes::RBrace], false);
      }

    // # One or two lined
    // # ex: fun say_hi() return print("Hi")
    } else {
      let init = self.iter.current.as_ref().unwrap().clone();
      // Should be no more than one newline before expression
      self.skip_newlines(Some(1));
      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Newline {
        panic!("Empty function body on line {}.", init.line);
      }
      body.push(self.parse_top());
      self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false);
    }

    return body;
  }

  fn parse_class(&mut self) -> Expression {
    self.iter.next();
    let ident = self.consume(&[TokenTypes::Identifier], false);

    // Specifying parent classes
    let mut parents: Option<Vec<Name>> = None;
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
      self.iter.next();

      // cls Child(Parent, OtherParent) = {}
      while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RParen {
        if self.iter.current.is_none() {
          panic!("Unexpected end of file."); // clean up
        }
        let name = self.consume(&[TokenTypes::Identifier], false);
        // Add name to list of parents or start new list
        if let Some(ref mut vec) = parents {
          vec.push(Name(name.value.unwrap()));
        } else {
          parents = Some(vec![Name(name.value.unwrap())]);
        }

        // Expect right paren or comma for additional parent(s)
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
          break;
        } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Comma {
          self.iter.next();
        }
      }
      self.consume(&[TokenTypes::RParen], false);
    }

    if self.iter.current.as_ref().unwrap().of_type != TokenTypes::Operator
      || self.iter.current.as_ref().unwrap().value.as_ref().unwrap() != "=" {
      panic!("Expected class assignment, got {:?} on line {}.",
        self.iter.current.as_ref().unwrap().of_type,
        self.iter.current.as_ref().unwrap().line
      );
    } else {
      self.iter.next();
    }

    // Class body
    self.consume(&[TokenTypes::LBrace], false);
    let mut body: Vec<Expression> = vec![];
    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
      self.skip_newlines(None);
      body.push(self.parse_top());
      self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false);
      self.skip_newlines(None);
    }
    self.consume(&[TokenTypes::RBrace], false);

    if parents.is_some() {
      return Expression::Class(
        Name(ident.value.unwrap()),
        parents,
        Some(Box::new(body))
      );
    } else {
      return Expression::Class(
        Name(ident.value.unwrap()),
        None,
        Some(Box::new(body))
      );
    }
  }

  fn parse_import_name_or_alias(&mut self, has_name: Option<Token>) -> (Name, Option<Name>) {
    let name: Token;
    if has_name.is_none()  {
      name = self.consume(&[TokenTypes::Identifier], false);
    } else {
      name = has_name.unwrap();
    }

    // Has alias
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword
      && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "as" {
      self.iter.next();
      let alias = self.consume(&[TokenTypes::Identifier], false);
      return (Name(name.value.unwrap()), Some(Name(alias.value.unwrap())));

    // No alias
    } else {
      return (Name(name.value.unwrap()), None);
    }
  }

  fn parse_import_dot_index(&mut self) -> Vec<Token> {
    let mut names = vec![];

    // Get initial name
    names.push(self.consume(&[TokenTypes::Identifier], false));

    while self.iter.current.is_some() && ![
      TokenTypes::LBrace, TokenTypes::Semicolon, TokenTypes::Newline, TokenTypes::Keyword
    ].contains(&self.iter.current.as_ref().unwrap().of_type) {
      // Expect "."
      if self.iter.current.as_ref().unwrap().of_type != TokenTypes::Operator
        || self.iter.current.as_ref().unwrap().value.as_ref().unwrap() != "." {
        panic!("Expected \".\", got {:?} on line {:?}",
          self.iter.current.as_ref().unwrap().of_type,
          self.iter.current.as_ref().unwrap().line
        );

      } else {
        // Get rid of .
        self.iter.next();

        // If wildcard import
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
          && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "*" {
          names.push(self.iter.next().unwrap());
          break;
        }

        // Get name
        names.push(self.consume(&[TokenTypes::Identifier], false));
      }
    }

    return names;
  }

  // Rules to capture
  // import module;
  // import module.abc.def;
  // import module.abc.*;
  // import module.abc as xyz;
  // import from module.abc { def as xyz, ghi as jkl, mno };
  fn parse_module(&mut self) -> Expression {
    self.iter.next();

    // No "from"
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
      let index = self.parse_import_dot_index();

      // No index, importing whole module - check for alias and return
      if index.iter().len() == 1 {
        let (name, alias) = self.parse_import_name_or_alias(Some(index[0].clone()));

        // No alias
        if alias.is_none() {
          return Expression::Module(vec![Name(String::from(index[0].value.as_ref().unwrap()))], None);
        }

        // Alias
        return Expression::Module(
          vec![Name(String::from(index[0].value.as_ref().unwrap()))],
          Some(vec![(name, alias)])
        );
      }

      // Has index //

      // If last name in index has an alias
      let (name, alias) = self.parse_import_name_or_alias(Some(index[index.iter().len() - 1].clone()));

      // No alias
      if alias.is_none() {
        return Expression::Module(
          index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>(),
          None
        );
      }

      // Alias
      return Expression::Module(
        index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>(),
        Some(vec![(name, alias)])
      );

    // Has "from"
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword
      && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "from" {
      self.iter.next();
      let index = self.parse_import_dot_index();

      // Expect grouping import
      self.consume(&[TokenTypes::LBrace], false);
      self.skip_newlines(Some(1));
      let mut names = vec![];

      while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
        let (name, alias) = self.parse_import_name_or_alias(None);
        names.push((name, alias));
        self.skip_newlines(Some(1));

        // Expect comma or end of import
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Comma {
          self.iter.next();
          self.skip_newlines(Some(1));
          continue;
        } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBrace {
          break;
        } else {
          panic!("Expected \",\" or \"}}\", got {:?} on line {}.",
            self.iter.current.as_ref().unwrap().value.as_ref().unwrap(),
            self.iter.current.as_ref().unwrap().line
          );
        }
      }
      self.consume(&[TokenTypes::RBrace], false);

      return Expression::Module(
        index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>(),
        Some(names)
      );

    // No "from" or identifier
    } else {
      panic!("Expected module or \"from\", got {:?} on line {}.",
        self.iter.current.as_ref().unwrap().value.as_ref().unwrap(),
        self.iter.current.as_ref().unwrap().line
      );
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_assignments() {
    let ast = Parser::new().parse("let a = 1;
    const name = \"Jink\"
    type Number = int;".to_string(), false);
    assert_eq!(ast, vec![
      Expression::Assignment(
        Some(Type(String::from("let"))),
        Box::new(Literals::Identifier(Name(String::from("a")), None)),
        Some(Box::new(Expression::Literal(Literals::Integer(1)))
      )),
      Expression::Assignment(
        Some(Type(String::from("const"))),
        Box::new(Literals::Identifier(Name(String::from("name")), None)),
        Some(Box::new(Expression::Literal(Literals::String(String::from("Jink"))))
      )),
      Expression::TypeDef(
        Literals::Identifier(Name(String::from("Number")), None),
        Box::new(Literals::Identifier(Name(String::from("int")), None))
      ),
      Expression::Literal(Literals::EOF)
    ]);
  }

  #[test]
  fn test_parse_conditional() {
    let ast = Parser::new().parse("if (a == 1) {
      return a;
    } else {
      return b;
    }".to_string(), false);
    assert_eq!(ast[0], Expression::Conditional(
      Type(String::from("if")),
      Some(Box::new(Expression::BinaryOperator(
        Operator(String::from("==")),
        Box::new(Expression::Literal(Literals::Identifier(Name(String::from("a")), None))),
        Box::new(Expression::Literal(Literals::Integer(1)))
      ))),
      Some(Box::new(vec![
        Expression::Return(Box::new(Expression::Literal(Literals::Identifier(Name(String::from("a")), None))))
      ])),
      Some(Box::new(vec![
        Expression::Conditional(
          Type(String::from("else")),
          None,
          Some(Box::new(vec![
            Expression::Return(Box::new(Expression::Literal(Literals::Identifier(Name(String::from("b")), None))))
          ])),
          None
        )
      ]))
    ));
  }

  #[test]
  fn test_parse_function_call() {
    let ast = Parser::new().parse("print(\"Hello, world!\");".to_string(), false);
    assert_eq!(ast[0], Expression::Call(
      Name(String::from("print")),
      Box::new(vec![Expression::Literal(Literals::String(String::from("Hello, world!")))])
    ));
  }

  #[test]
  fn test_parse_function_def() {
    let ast = Parser::new().parse("fun add(let a, let b) {
      return a + b;
    }".to_string(), false);
    assert_eq!(ast[0], Expression::Function(
      Name(String::from("add")),
      None,
      Some(Box::new(vec![
        Expression::FunctionParam(
          Type(String::from("let")),
          Literals::Identifier(Name(String::from("a")), None),
          None
        ),
        Expression::FunctionParam(
          Type(String::from("let")),
          Literals::Identifier(Name(String::from("b")), None),
          None
        )
      ])),
      Some(Box::new(vec![
        Expression::Return(Box::new(Expression::BinaryOperator(
          Operator(String::from("+")),
          Box::new(Expression::Literal(Literals::Identifier(Name(String::from("a")), None))),
          Box::new(Expression::Literal(Literals::Identifier(Name(String::from("b")), None)))
        )))
      ]))
    ));
  }

  #[test]
  fn test_parse_function_def_inline() {
    let ast = Parser::new().parse("fun sub(let a, let b) return a - b;".to_string(), false);
    assert_eq!(ast[0], Expression::Function(
      Name(String::from("sub")),
      None,
      Some(Box::new(vec![
        Expression::FunctionParam(
          Type(String::from("let")),
          Literals::Identifier(Name(String::from("a")), None),
          None
        ),
        Expression::FunctionParam(
          Type(String::from("let")),
          Literals::Identifier(Name(String::from("b")), None),
          None
        )
      ])),
      Some(Box::new(vec![
        Expression::Return(Box::new(Expression::BinaryOperator(
          Operator(String::from("-")),
          Box::new(Expression::Literal(Literals::Identifier(Name(String::from("a")), None))),
          Box::new(Expression::Literal(Literals::Identifier(Name(String::from("b")), None)))
        )))
      ]))
    ));
  }

  #[test]
  fn test_parse_function_with_defaults() {
    let ast = Parser::new().parse("fun pow(let a: 1, let b: 2, let c: 3) {
      return a ^ b ^ c;
    }".to_string(), false);
    assert_eq!(ast[0], Expression::Function(
      Name(String::from("pow")),
      None,
      Some(Box::new(vec![
        Expression::FunctionParam(
          Type(String::from("let")),
          Literals::Identifier(Name(String::from("a")), None),
          Some(Box::new(Expression::Literal(Literals::Integer(1))))
        ),
        Expression::FunctionParam(
          Type(String::from("let")),
          Literals::Identifier(Name(String::from("b")), None),
          Some(Box::new(Expression::Literal(Literals::Integer(2))))
        ),
        Expression::FunctionParam(
          Type(String::from("let")),
          Literals::Identifier(Name(String::from("c")), None),
          Some(Box::new(Expression::Literal(Literals::Integer(3))))
        )
      ])),
      Some(Box::new(vec![
        Expression::Return(Box::new(Expression::BinaryOperator(
          Operator(String::from("^")),
          Box::new(Expression::BinaryOperator(
            Operator(String::from("^")),
            Box::new(Expression::Literal(Literals::Identifier(Name(String::from("a")), None))),
            Box::new(Expression::Literal(Literals::Identifier(Name(String::from("b")), None)))
          )),
          Box::new(Expression::Literal(Literals::Identifier(Name(String::from("c")), None)))
        )))
      ]))
    ));
  }

  #[test]
  fn test_parse_function_def_with_return_type() {
    let ast = Parser::new().parse("fun add(int a, int b) -> int {
      return a + b;
    }".to_string(), false);
    assert_eq!(ast[0], Expression::Function(
      Name(String::from("add")),
      Some(Literals::Identifier(Name(String::from("int")), None)),
      Some(Box::new(vec![
        Expression::FunctionParam(
          Type(String::from("int")),
          Literals::Identifier(Name(String::from("a")), None),
          None
        ),
        Expression::FunctionParam(
          Type(String::from("int")),
          Literals::Identifier(Name(String::from("b")), None),
          None
        )
      ])),
      Some(Box::new(vec![
        Expression::Return(Box::new(Expression::BinaryOperator(
          Operator(String::from("+")),
          Box::new(Expression::Literal(Literals::Identifier(Name(String::from("a")), None))),
          Box::new(Expression::Literal(Literals::Identifier(Name(String::from("b")), None)))
        )))
      ]))
    ));
  }

  #[test]
  fn test_parse_function_def_with_inline_conditional() {
    let ast = Parser::new().parse("fun are_even(int a, int b) -> int {
      if (a % 2 == 0 && b % 2 == 0) return true
      else return false
    }".to_string(), false);
    assert_eq!(ast[0], Expression::Function(
      Name(String::from("are_even")),
      Some(Literals::Identifier(Name(String::from("int")), None)),
      Some(Box::new(vec![
        Expression::FunctionParam(
          Type(String::from("int")),
          Literals::Identifier(Name(String::from("a")), None),
          None
        ),
        Expression::FunctionParam(
          Type(String::from("int")),
          Literals::Identifier(Name(String::from("b")), None),
          None
        )
      ])),
      Some(Box::new(vec![
        Expression::Conditional(
          Type(String::from("if")),
          Some(Box::new(
            Expression::BinaryOperator(
              Operator(String::from("&&")),
              Box::new(Expression::BinaryOperator(
                Operator(String::from("==")),
                Box::new(Expression::BinaryOperator(
                  Operator(String::from("%")),
                  Box::new(Expression::Literal(Literals::Identifier(Name(String::from("a")), None))),
                  Box::new(Expression::Literal(Literals::Integer(2)))
                )),
                Box::new(Expression::Literal(Literals::Integer(0)))
              )),
              Box::new(Expression::BinaryOperator(
                Operator(String::from("==")),
                Box::new(Expression::BinaryOperator(
                  Operator(String::from("%")),
                  Box::new(Expression::Literal(Literals::Identifier(Name(String::from("b")), None))),
                  Box::new(Expression::Literal(Literals::Integer(2)))
                )),
                Box::new(Expression::Literal(Literals::Integer(0)))
              ))
            )
          )),
          Some(Box::new(vec![
            Expression::Return(Box::new(Expression::Literal(Literals::Boolean(true))))
          ])),
          Some(Box::new(vec![
            Expression::Conditional(
              Type(String::from("else")),
              None,
              Some(Box::new(vec![
                Expression::Return(Box::new(Expression::Literal(Literals::Boolean(false))))
              ])),
              None
            )
          ]))
        )
      ]))
    ));
  }
}