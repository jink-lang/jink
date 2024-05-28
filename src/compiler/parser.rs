use jink::Error;
use jink::FutureIter;
use jink::Token;
use jink::TokenTypes;
use jink::Operator;
use jink::Name;
use jink::Literals;
use jink::Expression;
use jink::Expr;
use jink::Type;

use super::lexer::Lexer;

pub struct Parser {
  pub code: String,
  pub iter: FutureIter,
  pub testing: bool,
}

impl Parser {
  pub fn new() -> Self {
    Parser {
      code: String::new(),
      iter: FutureIter::new(vec![]),
      testing: false,
    }
  }

  // Build AST
  pub fn parse(&mut self, code: String, _verbose: bool, testing: bool) -> Result<Vec<Expression>, Error> {
    let tokens = Lexer::new().lex(code.clone(), false);
    let iterator = FutureIter::new(tokens);
    self.code = code;
    self.iter = iterator;
    self.testing = testing;

    let mut ast: Vec<Expression> = Vec::new();
    while self.iter.current.is_some() {
      self.skip_newlines(None);
      let parsed: Result<Expression, Error> = self.parse_top();

      // Error
      if let Err(err) = parsed {
        return Err(err);
      }

      // Validate top level expressions
      if parsed.as_ref().unwrap().expr != Expr::Literal(Literals::EOF) {
        match parsed.as_ref().unwrap().expr {
          Expr::Function(_, _, _, _) => {},
          Expr::Call(_, _) => {},
          Expr::TypeDef(_, _) => {},
          Expr::Class(_, _, _) => {},
          Expr::Module(_, _) => {},
          Expr::Assignment(_, _, _) => {},
          Expr::Conditional(_, _, _, _) => {},
          Expr::UnaryOperator(_, _) => {},
          Expr::BinaryOperator(_, _, _) => {},
          Expr::ForLoop(_, _, _) => {},
          _ => {
            return Err(Error::new(
              Error::UnexpectedExpression,
              Some(self.iter.current.as_ref().unwrap().clone()),
              self.code.lines().nth((parsed.as_ref().unwrap().first_line.unwrap() - 1) as usize).unwrap(),
              parsed.as_ref().unwrap().first_pos,
              parsed.as_ref().unwrap().last_line,
              "Unexpected expression".to_string()
            ));
          }
        }
      }

      // End of file
      if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
        ast.push(parsed.unwrap());
        return Ok(ast);
      }

      // Distinguish expressions in the main scope
      self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false)?;
      ast.push(parsed.unwrap());
    }

    return Ok(ast);
  }

  fn get_expr(&self, expr: Expr, first_line: Option<i32>, first_pos: Option<i32>, last_line: Option<i32>) -> Expression {
    if self.testing {
      return Expression { expr, first_line: None, first_pos: None, last_line: None };
    } else {
      return Expression { expr, first_line, first_pos, last_line };
    }
  }

  // Removes expected token, given a type or a multiple types
  fn consume(&mut self, tokens: &[TokenTypes], soft: bool) -> Result<Token, Error> {
    let current = self.iter.next().unwrap();

    // Soft consume doesn't error if the token isn't found
    if soft { return Ok(current); }

    if !tokens.contains(&current.of_type) {
      let formatted = tokens.iter().map(|t| format!("{:?}", t)).collect::<Vec<String>>().join(" or ");
      let line = match self.code.lines().nth((current.line - 1) as usize) {
        Some(line) => line,
        None => self.code.lines().nth((current.line - 2) as usize).unwrap()
      };
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(current.clone()),
        line,
        current.start_pos,
        current.end_pos,
        format!("Expected {}, got", formatted)
      ));
    }

    return Ok(current);
  }

  /// Skip all newlines or a specific amount
  /// Returns true if any newlines were skipped
  fn skip_newlines(&mut self, count: Option<i16>) -> bool {
    let mut c: i16 = count.unwrap_or(-1);
    let mut ret = false;
    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Newline && c != 0 {
      if c > 0 { c -= 1; }
      self.iter.next();
      ret = true;
    }
    return ret;
  }

  fn parse_top(&mut self) -> Result<Expression, Error> {
    let init = self.iter.current.as_ref().clone();

    if init.is_none() {
      return Ok(self.get_expr(Expr::Literal(Literals::EOF), None, None, None));
    } else if init.unwrap().of_type == TokenTypes::EOF {
      return Ok(self.get_expr(Expr::Literal(Literals::EOF),
        Some(init.unwrap().line), init.unwrap().start_pos, Some(init.unwrap().line)
      ));

    } else if init.unwrap().of_type != TokenTypes::Keyword {
      return self.parse_expression(0);

    } else if ["let", "const"].contains(&init.unwrap().value.as_ref().unwrap().as_str()) {
      let assignment = self.iter.next().unwrap();
      let identifier = self.consume(&[TokenTypes::Identifier], false);

      if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "="
        || [TokenTypes::Newline, TokenTypes::Semicolon].contains(&self.iter.current.as_ref().unwrap().of_type) {
        return self.parse_assignment(Some(&assignment), identifier?);
      } else {
        return Err(Error::new(
          Error::UnexpectedToken,
          Some(self.iter.current.as_ref().unwrap().clone()),
          self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
          self.iter.current.as_ref().unwrap().start_pos,
          self.iter.current.as_ref().unwrap().end_pos,
          "Expected \"=\", got".to_string()
        ));
      }

    } else if init.unwrap().value.as_ref().unwrap() == "import" {
      return self.parse_module();

    } else if init.unwrap().value.as_ref().unwrap() == "fun" {
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
    
    } else if init.unwrap().value.as_ref().unwrap() == "for" {
      return self.parse_for_loop();

    } else {
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(init.unwrap().clone()),
        self.code.lines().nth((init.unwrap().line - 1) as usize).unwrap(),
        init.unwrap().start_pos,
        init.unwrap().end_pos,
        "Unexpected token".to_string()
      ));
    }
  }

  fn parse_expression(&mut self, precedence: i8) -> Result<Expression, Error> {
    let init = self.iter.current.as_ref().unwrap().clone();
    let mut left = self.parse_primary();

    while self.iter.current.is_some()
      && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
      && self.get_precedence(self.iter.current.as_ref().unwrap().clone()) >= precedence {

      let operator = self.iter.next().unwrap();
      if ["++", "--"].contains(&&operator.value.as_ref().unwrap().as_str()) {
        return Ok(self.get_expr(Expr::UnaryOperator(
          Operator(String::from(operator.value.as_ref().unwrap().to_owned() + ":post")),
          Box::new(left?)
        ), Some(init.line), init.start_pos, Some(operator.line)));
      }

      let mut next_precedence = self.get_precedence(operator.clone());
      if self.is_left_associative(operator.clone()) { next_precedence += 1; }

      let right = self.parse_expression(next_precedence);
      left = Ok(self.get_expr(Expr::BinaryOperator(
        Operator((*operator.to_owned().value.unwrap()).to_string()),
        Box::new(left?),
        Box::new(right.as_ref().unwrap().to_owned())
      ), Some(init.line), init.start_pos, right.unwrap().last_line));
    }

    // if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Semicolon {
    //   self.consume(&[TokenTypes::Semicolon], false);
    // }

    return left;
  }

  fn parse_primary(&mut self) -> Result<Expression, Error> {
    self.skip_newlines(None);

    if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
      return Err(Error::new(
        Error::UnexpectedEOF,
        None,
        "",
        Some(0),
        Some(0),
        "Unexpected end of file.".to_string()
      ));
    }

    // Unary operators
    if self.is_unary_operator(&self.iter.current.as_ref().unwrap().clone()) {
      let operator = self.iter.next().unwrap();

      if ![TokenTypes::Identifier, TokenTypes::Number, TokenTypes::LParen].contains(&self.iter.current.as_ref().unwrap().of_type) {
        return Err(Error::new(
          Error::UnexpectedToken,
          Some(self.iter.current.as_ref().unwrap().clone()),
          self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
          self.iter.current.as_ref().unwrap().start_pos,
          self.iter.current.as_ref().unwrap().end_pos,
          "Unexpected token".to_string()
        ));
      }

      if ["+", "-", "!", "..."].contains(&operator.clone().value.unwrap().as_str()) {
        let value = self.parse_primary().unwrap();
        return Ok(self.get_expr(
          Expr::UnaryOperator(Operator(operator.clone().value.unwrap()), Box::new(value.clone())),
          Some(operator.line), operator.start_pos, value.last_line
        ));

      } else if ["++", "--"].contains(&operator.clone().value.unwrap().as_str()) {
        let value = self.parse_primary().unwrap();
        return Ok(self.get_expr(
          Expr::UnaryOperator(Operator(operator.clone().value.unwrap() + ":pre"), Box::new(value.clone())),
          Some(operator.line), operator.start_pos, value.last_line
        ));
      }

      let precedence = self.get_precedence(operator.clone());
      let value = self.parse_expression(precedence);
      return Ok(self.get_expr(Expr::UnaryOperator(
        Operator(operator.clone().to_string()),
        Box::new(value.as_ref().unwrap().to_owned())
      ), Some(operator.line), operator.start_pos, value.unwrap().last_line));

    // Parentheses
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
      self.consume(&[TokenTypes::LParen], false)?;
      let expr = self.parse_expression(0);
      self.consume(&[TokenTypes::RParen], false)?;
      return Ok(expr?);

    // Arrays
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LBracket {
      self.consume(&[TokenTypes::LBracket], false)?;
      let arr = self.parse_array();
      self.consume(&[TokenTypes::RBracket], false)?;
      return Ok(arr?);

    // Objects
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LBrace {
      return self.parse_object(None);

    // Numbers
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Number {
      let cur = self.iter.next().unwrap();
      if cur.clone().value.unwrap().contains(".") {
        return Ok(self.get_expr(Expr::Literal(Literals::FloatingPoint(cur.value.unwrap().parse::<f64>().unwrap())),
          Some(cur.line), cur.start_pos, Some(cur.line)
        ));
      } else {
        return Ok(self.get_expr(Expr::Literal(Literals::Integer(cur.value.unwrap().parse::<i64>().unwrap())),
          Some(cur.line), cur.start_pos, Some(cur.line)
        ));
      }

    // Strings
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::String {
      let cur = self.iter.next();
      return Ok(self.get_expr(Expr::Literal(Literals::String(cur.as_ref().unwrap().value.as_ref().unwrap().to_owned())),
        Some(cur.as_ref().unwrap().line),
        Some(cur.as_ref().unwrap().start_pos.unwrap() - 2), // backtrack quote and starting pos
        Some(cur.as_ref().unwrap().line)
      ));

    // Identifiers
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
      let ident = self.iter.next().unwrap();

      // TODO: Object property
      // if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Dot {

      // EOF, return ident literal
      if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
        return Ok(self.get_expr(Expr::Literal(Literals::Identifier(Name(String::from(ident.value.unwrap())), None)),
          Some(ident.line), ident.start_pos, Some(ident.line)
        ));

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
        return Ok(self.get_expr(Expr::Literal(
          Literals::Identifier(Name(String::from(ident.value.unwrap())), None)
        ), Some(ident.line), ident.start_pos, Some(ident.line)));
      }

    // Keywords
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword {
      let keyword = self.iter.next().unwrap();

      // Calls
      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
        return self.parse_call(keyword);

      // Booleans
      } else if ["true", "false"].contains(&keyword.value.as_ref().unwrap().as_str()) {
        return Ok(self.get_expr(Expr::Literal(Literals::Boolean(keyword.value.as_ref().unwrap() == "true")),
          Some(keyword.line), keyword.start_pos, Some(keyword.line)
        ));

      // Nulls
      } else if keyword.value.as_ref().unwrap() == "null" {
        return Ok(self.get_expr(Expr::Literal(Literals::Null),
          Some(keyword.line), keyword.start_pos, Some(keyword.line)
        ));

      } else {
        return Err(Error::new(
          Error::UnexpectedToken,
          Some(keyword.clone()),
          self.code.lines().nth((keyword.line - 1) as usize).unwrap(),
          keyword.start_pos,
          keyword.end_pos,
          "Unexpected keyword ".to_string() + keyword.value.as_ref().unwrap()
        ));
      }

    } else {
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(self.iter.current.as_ref().unwrap().clone()),
        self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
        self.iter.current.as_ref().unwrap().start_pos,
        self.iter.current.as_ref().unwrap().end_pos,
        "Unexpected token.".to_string()
      ));
    }

  }

  fn is_unary_operator(&mut self, token: &Token) -> bool {
    if token.of_type != TokenTypes::Operator { return false; }
    return [
      "+", "-", "++", "--", "!", "..."
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

  fn parse_assignment(&mut self, _type: Option<&Token>, identifier: Token) -> Result<Expression, Error> {
    // Get type if it exists
    let mut assignment_type: Option<Type> = None;
    if _type.is_some() {
      assignment_type = Some(Type(_type.unwrap().value.as_ref().unwrap().to_owned()));
    }

    let assignment: Expression;

    // TODO: Handle comma case (multiple assignments at once)
    if [TokenTypes::Newline, TokenTypes::Semicolon].contains(&self.iter.current.as_ref().unwrap().of_type) {
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Literals::Identifier(Name(String::from(identifier.value.unwrap())), None),
        None
      ), Some(identifier.line), identifier.start_pos, Some(identifier.line));
    } else if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "=" {
      self.iter.next();
      let assign = self.parse_expression(0)?;
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Literals::Identifier(Name(String::from(identifier.value.unwrap())), None),
        Some(Box::new(assign.clone()))
      ), Some(identifier.line), identifier.start_pos, assign.last_line);
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Literals::Identifier(Name(String::from(identifier.value.unwrap())), None),
        None
      ), Some(identifier.line), identifier.start_pos, Some(identifier.line));
    } else {
      let assign = self.parse_expression(0)?;
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Literals::Identifier(Name(String::from(identifier.value.unwrap())), None),
        Some(Box::new(assign.clone()))
      ), Some(identifier.line), identifier.start_pos, assign.last_line);
    }

    return Ok(assignment);
  }

  fn parse_array(&mut self) -> Result<Expression, Error> {
    let mut list = vec![];
    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBracket {
      list.push(self.parse_primary()?);
      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Comma {
        self.iter.next();
      } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBracket {
        break;
      } else {
        return Err(Error::new(
          Error::UnexpectedToken,
          Some(self.iter.current.as_ref().unwrap().clone()),
          self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
          self.iter.current.as_ref().unwrap().start_pos,
          self.iter.current.as_ref().unwrap().end_pos,
          "Expected \",\" or \"]\", got".to_string()
        ));
      }
    }

    return Ok(self.get_expr(Expr::Array(Box::new(list)),
      Some(self.iter.current.as_ref().unwrap().line),
      self.iter.current.as_ref().unwrap().start_pos,
      Some(self.iter.current.as_ref().unwrap().line)
    ));
  }

  fn parse_type(&mut self) -> Result<Expression, Error> {
    let ident = self.consume(&[TokenTypes::Identifier], false)?.clone();

    // Expect =
    if self.iter.current.is_none()
      || self.iter.current.as_ref().unwrap().of_type != TokenTypes::Operator
      || self.iter.current.as_ref().unwrap().value.as_ref().unwrap() != "=" {
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(self.iter.current.as_ref().unwrap().clone()),
        self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
        self.iter.current.as_ref().unwrap().start_pos,
        self.iter.current.as_ref().unwrap().end_pos,
        "Expected \"=\", got".to_string()
      ));
    }
    self.iter.next();

    // If type alias
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
      let t = self.iter.next().unwrap();
      return Ok(self.get_expr(Expr::TypeDef(
        Literals::Identifier(Name(String::from(ident.value.unwrap())), None),
        Box::new(Literals::Identifier(Name(String::from(t.value.unwrap())), None))
      ), Some(ident.line), ident.start_pos, Some(t.line)));

    // If typedef / struct
    } else {
      return self.parse_object(Some(ident));
    }
  }

  fn parse_object(&mut self, type_tok: Option<Token>) -> Result<Expression, Error> {
    self.consume(&[TokenTypes::LBrace], false)?;
    let mut obj: Vec<Literals> = vec![];

    let init = self.iter.current.as_ref().unwrap().clone();

    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
      self.skip_newlines(None);
      let key = self.consume(&[TokenTypes::Identifier], false)?;
      self.consume(&[TokenTypes::Colon], false)?;

      // object
      if type_tok.is_none() {
        let value = self.parse_expression(0)?;
        obj.push(Literals::ObjectProperty(
          Some(Name(key.value.unwrap())),
          Box::new(value)
        ));

      // typedef / struct
      } else {
        let val = self.consume(&[TokenTypes::Identifier], false)?;
        obj.push(Literals::ObjectProperty(
          Some(Name(key.value.unwrap())),
          Box::new(self.get_expr(
            Expr::Literal(Literals::Identifier(Name(val.value.unwrap()), None)),
            Some(val.line), val.start_pos, Some(val.line)
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
      self.consume(&[TokenTypes::RBrace], false)?;

      // object
      if type_tok.is_none() {
        return Ok(self.get_expr(Expr::Literal(Literals::Object(Box::new(obj))),
          Some(init.line), init.start_pos, Some(init.line)
        ));

      // typedef / struct
      } else {
        return Ok(self.get_expr(Expr::TypeDef(
          Literals::Identifier(Name(String::from(type_tok.as_ref().unwrap().value.as_ref().unwrap())), None),
          Box::new(Literals::Object(Box::new(obj)))
        ),
        Some(type_tok.as_ref().unwrap().line),
        type_tok.as_ref().unwrap().start_pos,
        Some(self.iter.current.as_ref().unwrap().line)));
      }
    } else {
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(init.clone()),
        self.code.lines().nth((init.line - 1) as usize).unwrap(),
        init.start_pos,
        init.end_pos,
        "Expected \",\" or \"}}\", got".to_string()
      ));
    }
  }

  fn parse_for_loop(&mut self) -> Result<Expression, Error> {
    let init = self.iter.next(); // Consume "for"
    
    self.consume(&[TokenTypes::LParen], false)?;

    // Expect "let" keyword
    self.expect_keyword("let")?;

    let loop_variable = if let Some(token) = self.iter.current.clone() {
      if token.of_type == TokenTypes::Identifier {
        let identifier_token = self.iter.next().unwrap();
        Ok(Expression {
          expr: Expr::Literal(Literals::String(identifier_token.value.unwrap())),
          first_line: Some(identifier_token.line),
          first_pos: identifier_token.start_pos,
          last_line: identifier_token.end_pos,
        })
      } else {
        eprintln!("Debug: Expected identifier, got {:?}", token);
        Err(Error::new(
          Error::UnexpectedToken,
          Some(token.clone()),
          self.code.lines().nth((token.line - 1) as usize).unwrap(),
          token.start_pos,
          token.end_pos,
          "Expected identifier after \"let\", got".to_string(),
        ))
      }
  } else {
      Err(Error::new(
        Error::UnexpectedEOF,
        None,
        "",
        Some(0),
        Some(0),
        "Expected identifier after \"let\", got end of file.".to_string(),
      ))
    }?;

    // Expect "in" keyword
    self.expect_keyword("in")?;

    if let Some(token) = &self.iter.current {
      println!("Current token: {:?}", token);
    }

    let iterable = self.parse_expression(0)?;

    self.consume(&[TokenTypes::RParen], false)?;

    let body = self.parse_loop_block()?;

    return Ok(Expression {
      expr: Expr::ForLoop(
        Box::new(loop_variable),
        Box::new(iterable),
        Some(body),
      ),
      first_line: Some(init.as_ref().unwrap().line),
      first_pos: Some(init.as_ref().unwrap().start_pos.unwrap()),
      last_line: Some(init.unwrap().line),
    });
  }

  fn expect_keyword(&mut self, keyword: &str) -> Result<(), Error> {
    if let Some(token) = &self.iter.current {
        if token.of_type == TokenTypes::Keyword && token.value.as_ref().map_or(false, |v| v == keyword) {
          self.iter.next(); // Consume keyword
          Ok(())
        } else {
          eprintln!("Debug: Expected keyword \"{}\", got {:?}", keyword, token);
          Err(Error::new(
            Error::UnexpectedToken,
            Some(token.clone()),
            self.code.lines().nth((token.line - 1) as usize).unwrap(),
            token.start_pos,
            token.end_pos,
            format!("Expected keyword \"{}\", got", keyword),
          ))
        }
    } else {
      Err(Error::new(
        Error::UnexpectedEOF,
        None,
        "",
        Some(0),
        Some(0),
        format!("Expected keyword \"{}\", got end of file.", keyword),
      ))
    }
  }

  fn parse_call(&mut self, identifier: Token) -> Result<Expression, Error> {
    let args = self.parse_args_params("args")?;
    return Ok(self.get_expr(Expr::Call(Name(identifier.value.unwrap().to_owned()), Box::new(args)),
      Some(identifier.line),
      identifier.start_pos,
      Some(self.iter.current.as_ref().unwrap().line)
    ));
  }

  fn parse_function(&mut self) -> Result<Expression, Error> {
    let init = self.iter.next(); // "fun"
    let identifier = self.consume(&[TokenTypes::Identifier], false)?;
    let params = self.parse_args_params("params")?;

    // Get return type
    let mut return_type: Option<Token> = None;
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
      && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "->" {
      self.iter.next();
      // Identifier for now
      return_type = Some(self.consume(&[TokenTypes::Identifier], false)?);
    }

    let body = self.parse_func_block()?;

    if return_type.is_some() {
      return Ok(self.get_expr(
        Expr::Function(
          Name(identifier.value.unwrap().to_owned()),
          Some(Literals::Identifier(Name(return_type.unwrap().value.unwrap()), None)),
          Some(Box::new(params)),
          Some(Box::new(body))
        ),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(self.iter.current.as_ref().unwrap().line)
      ));
    } else {
      return Ok(self.get_expr(
        Expr::Function(
          Name(identifier.value.unwrap().to_owned()),
          None,
          Some(Box::new(params)),
          Some(Box::new(body))
        ),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(self.iter.current.as_ref().unwrap().line)
      ));
    }
  }

  fn parse_args_params(&mut self, parse_type: &str) -> Result<Vec<Expression>, Error> {
    self.consume(&[TokenTypes::LParen], false)?;
    let mut list: Vec<Expression> = vec![];

    // Skip by one newline if there is one
    self.skip_newlines(Some(1));

    // Function parameters
    if parse_type == "params" {
      while self.iter.current.is_some() {
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
          self.consume(&[TokenTypes::RParen], false)?;
          break;
        } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBrace {
          break;
        }

        // Type identifier, let or const
        let cur = self.iter.next().unwrap();
        if cur.of_type == TokenTypes::Identifier || (
          cur.of_type == TokenTypes::Keyword && ["let", "const"].contains(&cur.value.as_ref().unwrap().as_str())
        ) {

          // Spread operator
          let mut spread = false;
          let mut spread_op: Option<Token> = None;
          if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
            && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "..." {
            spread = true;
            spread_op = self.iter.next();
          }

          // If let, this is an identifier
          // If constant, this could be a type or ident - ex:
          // fun add(const int a, const b: 10) {}
          //               ^            ^
          let ident_or_ty = self.consume(&[TokenTypes::Identifier], false)?;
          let mut const_ident: Option<Token> = None;
          if cur.value.as_ref().unwrap() == "const" {

            // If spread operator was consumed and this is a typed constant, it was invalid
            if spread && [TokenTypes::Identifier].contains(&self.iter.current.as_ref().unwrap().of_type) {
              return Err(Error::new(
                Error::UnexpectedToken,
                Some(spread_op.as_ref().unwrap().clone()),
                self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
                spread_op.as_ref().unwrap().start_pos,
                spread_op.as_ref().unwrap().end_pos,
                "Unexpected token".to_string()
              ));
            } else if !spread {
              // Check for `fun print(const array_type ...args: []) {}`
              if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
                && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "..." {
                self.iter.next();
                spread = true;
              }
            }

            // If current token is an identifier, ident_or_ty is the type
            // Otherwise, ident_or_ty is the identifier
            if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
              const_ident = Some(self.consume(&[TokenTypes::Identifier], false)?);
            }
          }

          let (ty, is_const, ident) = if cur.value.as_ref().unwrap() == "const" {
            // Typed constant - `const int a`
            if const_ident.is_some() {
              (
                Some(Type(String::from(ident_or_ty.value.unwrap()))),
                true,
                Literals::Identifier(Name(String::from(const_ident.as_ref().unwrap().value.as_ref().unwrap())), None)
              )
            // Regular constant - `const a`
            } else {
              (
                Some(Type(String::from(cur.value.unwrap()))),
                true,
                Literals::Identifier(Name(String::from(ident_or_ty.value.unwrap())), None)
              )
            }
          // Not a constant - `let a`, `int b`
          } else {
            (
              Some(Type(String::from(cur.value.unwrap()))),
              false,
              Literals::Identifier(Name(String::from(ident_or_ty.value.unwrap())), None)
            )
          };

          // Close out function params
          if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
            list.push(self.get_expr(Expr::FunctionParam(ty, is_const, ident, None, spread),
              Some(cur.line),
              cur.start_pos,
              Some(cur.line)
            ));

          // Expect colon for param defaults
          // fun num(int a: 1) {}
          // Operator check here in case we make colon an operator
          } else if [TokenTypes::Colon, TokenTypes::Operator].contains(&self.iter.current.as_ref().unwrap().of_type)
            && [":"].contains(&self.iter.current.as_ref().unwrap().value.as_ref().unwrap().as_str()) {

            if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == ":" {
              self.iter.next();
              let default = self.parse_expression(0)?;
              list.push(self.get_expr(Expr::FunctionParam(ty, is_const, ident,  Some(Box::new(default.clone())), spread),
                Some(cur.line),
                cur.start_pos,
                default.last_line
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

            list.push(self.get_expr(Expr::FunctionParam(ty, is_const, ident, None, spread),
              Some(cur.line),
              cur.start_pos,
              Some(cur.line)
            ));

          } else {
            return Err(Error::new(
              Error::UnexpectedToken,
              Some(self.iter.current.as_ref().unwrap().clone()),
              self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
              self.iter.current.as_ref().unwrap().start_pos,
              self.iter.current.as_ref().unwrap().end_pos,
              "Expected \")\", \",\" or \":\", got".to_string()
            ));
          }
        } else {
          return Err(Error::new(
            Error::UnexpectedToken,
            Some(cur.clone()),
            self.code.lines().nth((cur.line - 1) as usize).unwrap(),
            cur.start_pos,
            cur.end_pos,
            "Expected type, \"let\" or \"const\", got".to_string()
          ));
        }
      }

    // Call arguments
    } else {
      while self.iter.current.is_some() {
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
          self.consume(&[TokenTypes::RParen], false)?;
          break;
        }
        list.push(self.parse_top()?);
        // Multiple arguments, continue
        if [TokenTypes::Comma, TokenTypes::Newline].contains(&self.iter.current.as_ref().unwrap().of_type) {
          self.consume(&[TokenTypes::Comma, TokenTypes::Newline], true)?;
        // End of arguments, break
        } else {
          self.consume(&[TokenTypes::RParen], false)?;
          break;
        }
      }
    }

    return Ok(list);
  }

  fn parse_return(&mut self) -> Result<Expression, Error> {
    let init = self.iter.next(); // "return"
    let ret: Expression;
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Semicolon {
      // self.iter.next();
      ret = self.get_expr(Expr::Return(
        Box::new(self.get_expr(Expr::Literal(Literals::Null),
          Some(self.iter.current.as_ref().unwrap().line),
          self.iter.current.as_ref().unwrap().start_pos,
          Some(self.iter.current.as_ref().unwrap().line)
        ))),
        Some(init.as_ref().unwrap().line),
        init.as_ref().unwrap().start_pos,
        Some(init.unwrap().line)
      );
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Newline {
      ret = self.get_expr(Expr::Return(
        Box::new(self.get_expr(Expr::Literal(Literals::Null),
          Some(self.iter.current.as_ref().unwrap().line),
          self.iter.current.as_ref().unwrap().start_pos,
          Some(self.iter.current.as_ref().unwrap().line)
        ))),
        Some(init.as_ref().unwrap().line),
        init.as_ref().unwrap().start_pos,
        Some(init.unwrap().line)
      );
    } else {
      let expr = self.parse_expression(0)?;
      ret = self.get_expr(Expr::Return(Box::new(expr.clone())),
        Some(init.as_ref().unwrap().line),
        init.as_ref().unwrap().start_pos,
        expr.last_line
      );
    }

    return Ok(ret);
  }

  fn parse_conditional(&mut self) -> Result<Expression, Error> {
    let init = self.iter.next().unwrap(); // "if", "else", "elseif"

    // Parse else first because it is unlike if/elseif
    if init.value.as_ref().unwrap() == "else" {
      let block = self.parse_func_block()?;
      return Ok(self.get_expr(Expr::Conditional(
        Type(String::from("else")),
        None,
        Some(Box::new(block)),
        None
      ), Some(init.line), init.start_pos, Some(self.iter.current.as_ref().unwrap().line)));
    }

    let mut else_body = vec![];
    self.consume(&[TokenTypes::LParen], false)?;
    let expr = self.parse_expression(0)?;
    self.consume(&[TokenTypes::RParen], false)?;
    let body = self.parse_func_block()?;

    // If an else case is next. Handles the two following grammars:
    // if (true) return 1
    // else return 2
    // &
    // if (true) return 1
    // return 2
    if (
      self.iter.current.is_some()
      && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword
      && ["elseif", "else"].contains(&self.iter.current.as_ref().unwrap().value.as_ref().unwrap().as_str())
    ) || (
      self.iter.peek().is_some()
      && self.iter.peek().unwrap().of_type == TokenTypes::Keyword
      && ["elseif", "else"].contains(&self.iter.peek().unwrap().value.as_ref().unwrap().as_str())
    ) {
      self.skip_newlines(Some(1));
      else_body.push(self.parse_conditional()?);
    }

    return Ok(self.get_expr(Expr::Conditional(
      Type(String::from(init.value.unwrap())),
      Some(Box::new(expr)),
      Some(Box::new(body)),
      Some(Box::new(else_body))
    ), Some(init.line), init.start_pos, Some(self.iter.current.as_ref().unwrap().line)));
  }

  // TODO: Move actual block parsing to its own func (see block parsing for classes)
  // and rename this to parse function block bc it clearly caters just to functions
  // and then change the call to this from function calls because oops that makes no sense
  fn parse_func_block(&mut self) -> Result<Vec<Expression>, Error> {
    let mut body: Vec<Expression> = Vec::new();

    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LBrace {
      self.consume(&[TokenTypes::LBrace], false)?;
      self.skip_newlines(None);
      while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
        body.push(self.parse_top()?);
        // Break if early end of block
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBrace {
          break;
        }
        self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false)?;
        self.skip_newlines(None);
      }
      if self.iter.current.is_none() {
        return Err(Error::new(
          Error::UnexpectedEOF,
          None,
          "",
          Some(0),
          Some(0),
          "Expected \"}}\", got end of file.".to_string()
        ));
      } else {
        self.consume(&[TokenTypes::RBrace], false)?;
      }

    // # One or two lined
    // # ex: fun say_hi() return print("Hi")
    } else {
      let init = self.iter.current.as_ref().unwrap().clone();
      // Should be no more than one newline before expression
      let skipped = self.skip_newlines(Some(1));
      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Newline {
        return Err(Error::new(
          Error::EmptyFunctionBody,
          Some(init.clone()),
          &(self.code.lines().nth((init.line - 2) as usize).unwrap().to_owned() + "\n" + self.code.lines().nth((init.line - 1) as usize).unwrap()),
          init.start_pos,
          init.end_pos,
          "Empty function body.".to_string()
        ));
      } else if [TokenTypes::Semicolon, TokenTypes::EOF].contains(&self.iter.current.as_ref().unwrap().of_type) {
        return Err(Error::new(
          Error::EmptyFunctionBody,
          Some(init.clone()),
          &(self.code.lines().nth((init.line - if skipped { 2 } else { 1 }) as usize).unwrap().to_owned()),
          init.start_pos,
          init.end_pos,
          "Empty function body.".to_string()
        ));
      }
      body.push(self.parse_top()?);
    }

    return Ok(body);
  }

  fn parse_loop_block(&mut self) -> Result<Vec<Expression>, Error> {
    let mut body: Vec<Expression> = Vec::new();

    // Skip newlines before the opening brace
    self.skip_newlines(None);

    self.consume(&[TokenTypes::LBrace], false)?;

    // Skip newlines after opening brace
    self.skip_newlines(None);

    while let Some(token) = &self.iter.current {
      if token.of_type == TokenTypes::RBrace {
        self.iter.next();
        break;
      }

      let statement = self.parse_expression(0)?;
      body.push(statement);

      self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false)?;
      
      // Skip newlines after each statement
      self.skip_newlines(None);
    }

    if self.iter.current.is_none() {
      return Err(Error::new(
        Error::UnexpectedEOF,
        None,
        "",
        Some(0),
        Some(0),
        "Expected '}' to end for loop block, got end of file.".into(),
      ));
    }

    return Ok(body);
  }

  fn parse_class(&mut self) -> Result<Expression, Error> {
    let init = self.iter.next();
    let ident = self.consume(&[TokenTypes::Identifier], false)?;

    // Specifying parent classes
    let mut parents: Option<Vec<Name>> = None;
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
      self.iter.next();

      // cls Child(Parent, OtherParent) = {}
      while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RParen {
        let name = self.consume(&[TokenTypes::Identifier], false)?;
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
      self.consume(&[TokenTypes::RParen], false)?;
    }

    if self.iter.current.as_ref().unwrap().of_type != TokenTypes::Operator
      || self.iter.current.as_ref().unwrap().value.as_ref().unwrap() != "=" {
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(self.iter.current.as_ref().unwrap().clone()),
        self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
        self.iter.current.as_ref().unwrap().start_pos,
        self.iter.current.as_ref().unwrap().end_pos,
        "Expected \"=\", got".to_string()
      ));
    } else {
      self.iter.next();
    }

    // Class body
    self.consume(&[TokenTypes::LBrace], false)?;
    let mut body: Vec<Expression> = vec![];
    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
      self.skip_newlines(None);
      body.push(self.parse_top()?);
      self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false)?;
      self.skip_newlines(None);
    }
    let end = self.consume(&[TokenTypes::RBrace], false)?;

    if parents.is_some() {
      return Ok(self.get_expr(Expr::Class(
        Name(ident.value.unwrap()),
        parents,
        Some(Box::new(body))
      ), Some(init.as_ref().unwrap().line), init.unwrap().start_pos, Some(end.line)));
    } else {
      return Ok(self.get_expr(Expr::Class(
        Name(ident.value.unwrap()),
        None,
        Some(Box::new(body))
      ), Some(init.as_ref().unwrap().line), init.unwrap().start_pos, Some(end.line)));
    }
  }

  fn parse_import_name_or_alias(&mut self, has_name: Option<Token>) -> Result<(Name, Option<Name>), Error> {
    let name: Token;
    if has_name.is_none()  {
      name = self.consume(&[TokenTypes::Identifier], false)?;
    } else {
      name = has_name.unwrap();
    }

    // Has alias
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword
      && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "as" {
      self.iter.next();
      let alias = self.consume(&[TokenTypes::Identifier], false)?;
      return Ok((Name(name.value.unwrap()), Some(Name(alias.value.unwrap()))));

    // No alias
    } else {
      return Ok((Name(name.value.unwrap()), None));
    }
  }

  fn parse_import_dot_index(&mut self) -> Result<Vec<Token>, Error> {
    let mut names = vec![];

    // Get initial name
    names.push(self.consume(&[TokenTypes::Identifier], false)?);

    while self.iter.current.is_some() && ![
      TokenTypes::LBrace, TokenTypes::Semicolon, TokenTypes::Newline, TokenTypes::Keyword
    ].contains(&self.iter.current.as_ref().unwrap().of_type) {
      // Expect "."
      if self.iter.current.as_ref().unwrap().of_type != TokenTypes::Operator
        || self.iter.current.as_ref().unwrap().value.as_ref().unwrap() != "." {
        return Err(Error::new(
          Error::UnexpectedToken,
          Some(self.iter.current.as_ref().unwrap().clone()),
          self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
          self.iter.current.as_ref().unwrap().start_pos,
          self.iter.current.as_ref().unwrap().end_pos,
          "Expected \".\", got".to_string()
        ));

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
        names.push(self.consume(&[TokenTypes::Identifier], false)?);
      }
    }

    return Ok(names);
  }

  // Rules to capture
  // import module;
  // import module.abc.def;
  // import module.abc.*;
  // import module.abc as xyz;
  // import from module.abc { def as xyz, ghi as jkl, mno };
  fn parse_module(&mut self) -> Result<Expression, Error> {
    let init = self.iter.next(); // "import"

    // No "from"
    if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
      let index = self.parse_import_dot_index()?;

      // No index, importing whole module - check for alias and return
      if index.iter().len() == 1 {
        let (name, alias) = self.parse_import_name_or_alias(Some(index[0].clone()))?;

        // No alias
        if alias.is_none() {
          return Ok(self.get_expr(Expr::Module(vec![Name(String::from(index[0].value.as_ref().unwrap()))], None),
            Some(init.as_ref().unwrap().line),
            init.unwrap().start_pos,
            Some(index[0].line)
          ));
        }

        // Alias
        return Ok(self.get_expr(Expr::Module(
          vec![Name(String::from(index[0].value.as_ref().unwrap()))],
          Some(vec![(name, alias)])
        ), Some(init.as_ref().unwrap().line), init.unwrap().start_pos, Some(index[0].line)));
      }

      // Has index //

      // If last name in index has an alias
      let (name, alias) = self.parse_import_name_or_alias(Some(index[index.iter().len() - 1].clone()))?;

      // No alias
      if alias.is_none() {
        return Ok(
          self.get_expr(Expr::Module(
            index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>(),
            None
          ),
          Some(init.as_ref().unwrap().line),
          init.unwrap().start_pos,
          Some(index.last().unwrap().line)
        ));
      }

      // Alias
      return Ok(
        self.get_expr(Expr::Module(
          index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>(),
          Some(vec![(name, alias)])
        ),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(index.last().unwrap().line)
      ));

    // Has "from"
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Keyword
      && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "from" {
      self.iter.next();
      let index = self.parse_import_dot_index()?;

      // Expect grouping import
      self.consume(&[TokenTypes::LBrace], false)?;
      self.skip_newlines(Some(1));
      let mut names = vec![];

      while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBrace {
        let (name, alias) = self.parse_import_name_or_alias(None)?;
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
          return Err(Error::new(
            Error::UnexpectedToken,
            Some(self.iter.current.as_ref().unwrap().clone()),
            self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
            self.iter.current.as_ref().unwrap().start_pos,
            self.iter.current.as_ref().unwrap().end_pos,
            "Expected \",\" or \"}}\", got".to_string()
          ));
        }
      }
      let end = self.consume(&[TokenTypes::RBrace], false)?;

      return Ok(
        self.get_expr(Expr::Module(
          index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>(),
          Some(names)
        ),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(end.line)
      ));

    // No "from" or identifier
    } else {
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(self.iter.current.as_ref().unwrap().clone()),
        self.code.lines().nth((self.iter.current.as_ref().unwrap().line - 1) as usize).unwrap(),
        self.iter.current.as_ref().unwrap().start_pos,
        self.iter.current.as_ref().unwrap().end_pos,
        "Expected Module or \"from\", got".to_string()
      ));
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_assignments() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("let a = 1;
    const name = \"Jink\"
    type Number = int;".to_string(), false, true)?;
    return Ok(assert_eq!(ast, vec![
      parser.get_expr(Expr::Assignment(
        Some(Type(String::from("let"))),
        Literals::Identifier(Name(String::from("a")), None),
        Some(Box::new(
          parser.get_expr(Expr::Literal(Literals::Integer(1)), None, None, None)
        )
      )), None, None, None),
      parser.get_expr(Expr::Assignment(
        Some(Type(String::from("const"))),
        Literals::Identifier(Name(String::from("name")), None),
        Some(Box::new(
          parser.get_expr(Expr::Literal(Literals::String(String::from("Jink"))), None, None, None)
        )
      )), None, None, None),
      parser.get_expr(Expr::TypeDef(
        Literals::Identifier(Name(String::from("Number")), None),
        Box::new(Literals::Identifier(Name(String::from("int")), None))
      ), None, None, None),
      parser.get_expr(Expr::Literal(Literals::EOF), None, None, None)
    ]));
  }

  #[test]
  fn test_parse_conditional() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("if (a == 1) {
      return a;
    } else {
      return b;
    }".to_string(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Conditional(
      Type(String::from("if")),
      Some(Box::new(parser.get_expr(Expr::BinaryOperator(
        Operator(String::from("==")),
        Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)),
        Box::new(parser.get_expr(Expr::Literal(Literals::Integer(1)), None, None, None))
      ), None, None, None))),
      Some(Box::new(vec![
        parser.get_expr(Expr::Return(Box::new(
          parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)
        )), None, None, None)
      ])),
      Some(Box::new(vec![
        parser.get_expr(Expr::Conditional(
          Type(String::from("else")),
          None,
          Some(Box::new(vec![
            parser.get_expr(Expr::Return(Box::new(
              parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("b")), None)), None, None, None)
            )), None, None, None)
          ])),
          None
        ), None, None, None)
      ]))
    ), None, None, None)));
  }

  #[test]
  fn test_parse_function_call() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("print(\"Hello, world!\");".to_string(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Call(
      Name(String::from("print")),
      Box::new(vec![parser.get_expr(Expr::Literal(Literals::String(String::from("Hello, world!"))), None, None, None)])
    ), None, None, None)));
  }

  #[test]
  fn test_parse_function_def() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("fun add(let a, let b) {
      return a + b;
    }".to_string(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Function(
      Name(String::from("add")),
      None,
      Some(Box::new(vec![
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("let"))),
          false,
          Literals::Identifier(Name(String::from("a")), None),
          None,
          false
        ), None, None, None),
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("let"))),
          false,
          Literals::Identifier(Name(String::from("b")), None),
          None,
          false
        ), None, None, None)
      ])),
      Some(Box::new(vec![
        parser.get_expr(Expr::Return(Box::new(
          parser.get_expr(Expr::BinaryOperator(
            Operator(String::from("+")),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("b")), None)), None, None, None))
          ), None, None, None)
        )), None, None, None)
      ]))
    ), None, None, None)));
  }

  #[test]
  fn test_parse_function_def_inline() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("fun sub(let a, let b) return a - b;".to_string(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Function(
      Name(String::from("sub")),
      None,
      Some(Box::new(vec![
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("let"))),
          false,
          Literals::Identifier(Name(String::from("a")), None),
          None,
          false
        ), None, None, None),
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("let"))),
          false,
          Literals::Identifier(Name(String::from("b")), None),
          None,
          false
        ), None, None, None)
      ])),
      Some(Box::new(vec![
        parser.get_expr(Expr::Return(Box::new(
          parser.get_expr(Expr::BinaryOperator(
            Operator(String::from("-")),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("b")), None)), None, None, None))
          ), None, None, None)
        )), None, None, None)
      ]))
    ), None, None, None)));
  }

  #[test]
  fn test_parse_function_with_defaults() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("fun pow(let a: 1, let b: 2, let c: 3) {
      return a ^ b ^ c;
    }".to_string(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Function(
      Name(String::from("pow")),
      None,
      Some(Box::new(vec![
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("let"))),
          false,
          Literals::Identifier(Name(String::from("a")), None),
          Some(Box::new(parser.get_expr(Expr::Literal(Literals::Integer(1)), None, None, None))),
          false
        ), None, None, None),
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("let"))),
          false,
          Literals::Identifier(Name(String::from("b")), None),
          Some(Box::new(parser.get_expr(Expr::Literal(Literals::Integer(2)), None, None, None))),
          false
        ), None, None, None),
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("let"))),
          false,
          Literals::Identifier(Name(String::from("c")), None),
          Some(Box::new(parser.get_expr(Expr::Literal(Literals::Integer(3)), None, None, None))),
          false
        ), None, None, None)
      ])),
      Some(Box::new(vec![
        parser.get_expr(Expr::Return(Box::new(
          parser.get_expr(Expr::BinaryOperator(
            Operator(String::from("^")),
            Box::new(parser.get_expr(Expr::BinaryOperator(
              Operator(String::from("^")),
              Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)),
              Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("b")), None)), None, None, None))
            ), None, None, None)),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("c")), None)), None, None, None))
          ), None, None, None)
        )), None, None, None)
      ]))
    ), None, None, None)));
  }

  #[test]
  fn test_parse_function_def_with_return_type() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("fun add(int a, int b) -> int {
      return a + b;
    }".to_string(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Function(
      Name(String::from("add")),
      Some(Literals::Identifier(Name(String::from("int")), None)),
      Some(Box::new(vec![
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("int"))),
          false,
          Literals::Identifier(Name(String::from("a")), None),
          None,
          false
        ), None, None, None),
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("int"))),
          false,
          Literals::Identifier(Name(String::from("b")), None),
          None,
          false
        ), None, None, None)
      ])),
      Some(Box::new(vec![
        parser.get_expr(Expr::Return(Box::new(
          parser.get_expr(Expr::BinaryOperator(
            Operator(String::from("+")),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("b")), None)), None, None, None))
          ), None, None, None)
        )), None, None, None)
      ]))
    ), None, None, None)));
  }

  #[test]
  fn test_parse_function_def_with_inline_conditional() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse("fun are_even(int a, int b) -> int {
      if (a % 2 == 0 && b % 2 == 0) return true
      else return false
    }".to_string(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Function(
      Name(String::from("are_even")),
      Some(Literals::Identifier(Name(String::from("int")), None)),
      Some(Box::new(vec![
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("int"))),
          false,
          Literals::Identifier(Name(String::from("a")), None),
          None,
          false
        ), None, None, None),
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("int"))),
          false,
          Literals::Identifier(Name(String::from("b")), None),
          None,
          false
        ), None, None, None)
      ])),
      Some(Box::new(vec![
        parser.get_expr(Expr::Conditional(
          Type(String::from("if")),
          Some(Box::new(
            parser.get_expr(Expr::BinaryOperator(
              Operator(String::from("&&")),
              Box::new(parser.get_expr(Expr::BinaryOperator(
                Operator(String::from("==")),
                Box::new(parser.get_expr(Expr::BinaryOperator(
                  Operator(String::from("%")),
                  Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)),
                  Box::new(parser.get_expr(Expr::Literal(Literals::Integer(2)), None, None, None))
                ), None, None, None)),
                Box::new(parser.get_expr(Expr::Literal(Literals::Integer(0)), None, None, None))
              ), None, None, None)),
              Box::new(parser.get_expr(Expr::BinaryOperator(
                Operator(String::from("==")),
                Box::new(parser.get_expr(Expr::BinaryOperator(
                  Operator(String::from("%")),
                  Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("b")), None)), None, None, None)),
                  Box::new(parser.get_expr(Expr::Literal(Literals::Integer(2)), None, None, None))
                ), None, None, None)),
                Box::new(parser.get_expr(Expr::Literal(Literals::Integer(0)), None, None, None))
              ), None, None, None))
            ), None, None, None)
          )),
          Some(Box::new(vec![
            parser.get_expr(Expr::Return(Box::new(
              parser.get_expr(Expr::Literal(Literals::Boolean(true)), None, None, None)
            )), None, None, None)
          ])),
          Some(Box::new(vec![
            parser.get_expr(Expr::Conditional(
              Type(String::from("else")),
              None,
              Some(Box::new(vec![
                parser.get_expr(Expr::Return(Box::new(
                  parser.get_expr(Expr::Literal(Literals::Boolean(false)), None, None, None)
                )), None, None, None)
              ])),
              None
            ), None, None, None)
          ]))
        ), None, None, None)
      ]))
    ), None, None, None)));
  }

  #[test]
  fn test_function_with_constants() {
    let mut parser = Parser::new();
    let ast = parser.parse("fun add(const a, const int b) {
      return a + b;
    }".to_string(), false, true).unwrap();
    assert_eq!(ast[0], parser.get_expr(Expr::Function(
      Name(String::from("add")),
      None,
      Some(Box::new(vec![
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("const"))),
          true,
          Literals::Identifier(Name(String::from("a")), None),
          None,
          false
        ), None, None, None),
        parser.get_expr(Expr::FunctionParam(
          Some(Type(String::from("int"))),
          true,
          Literals::Identifier(Name(String::from("b")), None),
          None,
          false
        ), None, None, None)
      ])),
      Some(Box::new(vec![
        parser.get_expr(Expr::Return(Box::new(
          parser.get_expr(Expr::BinaryOperator(
            Operator(String::from("+")),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)), None, None, None)),
            Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("b")), None)), None, None, None))
          ), None, None, None)
        )), None, None, None)
      ])
    )), None, None, None));
  }
}