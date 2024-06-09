use indexmap::IndexMap;

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
use super::module_loader;

#[derive(Debug)]
pub struct Namespace {
  // TODO: Maybe wiser to store array index of expression in AST instead of the expression itself here
  // Would need to make AST part of the parser struct and add a flag not to push to it when parsing modules
  // TODO: Keep track of public names
  pub names: IndexMap<String, Expression>,
  // dependency; dependent
  pub dependencies: IndexMap<String, Vec<String>>,
  // module; (name; alias)
  pub imports: IndexMap<String, Option<Vec<(Option<String>, Option<String>)>>>
}

pub struct Parser {
  pub code: String,
  pub iter: FutureIter,
  pub verbose: bool,
  pub testing: bool,
  pub in_loop: bool,
  pub is_indexing: bool,
  /// Current namespace absolute path
  pub current_namespace: String,
  /// Current named item (imports and function, class and type definitions)
  /// If None, we are in the main scope
  pub current_name: Option<String>,
  /// The current scope's *definitions* that we can ignore when storing dependencies
  /// (they are already within the namespace if locally defined)
  pub current_scope_defs: Vec<String>,
  /// Namespace absolute path; Namespace
  pub namespaces: IndexMap<String, Namespace>,
}

impl Parser {
  pub fn new() -> Self {
    Parser {
      code: String::new(),
      iter: FutureIter::new(vec![]),
      verbose: false,
      testing: false,
      in_loop: false,
      is_indexing: false,
      current_namespace: String::new(),
      current_name: None,
      current_scope_defs: Vec::new(),
      namespaces: IndexMap::new(),
    }
  }

  // Build AST
  pub fn parse(&mut self, code: String, main_file_path: String, verbose: bool, testing: bool) -> Result<Vec<Expression>, Error> {
    let tokens = Lexer::new().lex(code.clone(), false);
    let iterator = FutureIter::new(tokens);
    self.code = code;
    self.iter = iterator;
    self.verbose = verbose;
    self.testing = testing;

    // Add main namespace
    self.current_namespace = main_file_path.clone();
    self.namespaces.insert(main_file_path, Namespace {
      names: IndexMap::new(),
      dependencies: IndexMap::new(),
      imports: IndexMap::new()
    });

    let mut ast: Vec<Expression> = Vec::new();
    while self.iter.current.is_some() {
      self.skip_newlines(None);
      let parsed: Result<Expression, Error> = self.parse_top();

      // Error
      if let Err(err) = parsed { return Err(err); }

      // Validate top level expressions
      if parsed.as_ref().unwrap().expr != Expr::Literal(Literals::EOF) {
        match parsed.as_ref().unwrap().expr {
          Expr::Function(_, _, _, _) => {},
          Expr::Call(_, _) => {},
          Expr::TypeDef(_, _) => {},
          Expr::Class(_, _, _) => {},
          Expr::ModuleParsed(_, _, _, _) => {},
          Expr::Assignment(_, _, _) => {},
          Expr::Conditional(_, _, _, _) => {},
          Expr::UnaryOperator(_, _) => {},
          Expr::BinaryOperator(_, _, _) => {},
          Expr::ForLoop(_, _, _) => {},
          Expr::WhileLoop(_, _) => {},
          Expr::Public(_) => {},
          // Expr::Index(_, _) => {}, // Will only be necessary when indexing at top level - module imports and class methods
          _ => {
            return Err(Error::new(
              Error::UnexpectedExpression,
              Some(self.iter.current.as_ref().unwrap().clone()),
              self.code.lines().nth((parsed.as_ref().unwrap().first_line.unwrap() - 1) as usize).unwrap(),
              parsed.as_ref().unwrap().first_pos,
              parsed.as_ref().unwrap().first_pos,
              "Unexpected top level expression".to_string()
            ));
          }
        }
      }

      // End of file
      if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
        ast.push(parsed.unwrap());

        return Ok(ast);
      }

      // End of statement
      self.consume(&[TokenTypes::Semicolon, TokenTypes::Newline], false)?;
      ast.push(parsed.unwrap());
    }

    return Ok(ast);
  }

  /// Load module via constructed index expression
  /// 
  /// Loads module, attaches AST to expression, builds namespace and returns expression back
  fn process_module(&mut self, expr: Expression) -> Result<Vec<Expression>, Error> {
    if let Expr::Module(path, is_aliased, names) = expr.expr.clone() {
      let code = module_loader::load_module(path.clone(), self.verbose);
      if code.is_none() {
        return Err(Error::new(
          Error::ImportError,
          None,
          &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap(),
          expr.first_pos,
          expr.first_pos,
          format!("Could not resolve module path: '/{}.jk'", path.iter().map(|n| n.0.to_string()).collect::<Vec<String>>().join("/")),
        ));
      }

      // Store current state
      let remaining = self.iter.dump();
      let store_code = self.code.clone();
      let store_namespace = self.current_namespace.clone();

      // Parse module
      let module = path.iter().map(|n| n.0.to_string()).collect::<Vec<String>>().join("/");
      self.current_namespace = module.clone();
      let ast = self.parse(code.unwrap(), self.current_namespace.clone(), self.verbose, self.testing)?;

      // Restore state
      self.iter.load(remaining);
      self.code = store_code;
      self.current_namespace = store_namespace;

      // Add import names to current namespace so dependencies can be resolved
      if names.is_none() {
        // Add every public name to the import dependencies / namespace
        if path.last().unwrap().0 == "*" {
          for expr in ast.clone() {
            if let Expr::Public(exp) = expr.expr {
              if let Expr::TypeDef(Literals::Identifier(Name(name), _), _) = exp.expr.clone() {
                self.add_import_dependency(Some(name), None, module.to_string(), *exp.clone())?;
              } else if let Expr::Function(Name(name), _, _, _) = exp.expr.clone() {
                self.add_import_dependency(Some(name), None, module.to_string(), *exp.clone())?;
              } else if let Expr::Class(Name(name), _, _) = exp.expr.clone() {
                self.add_import_dependency(Some(name), None, module.to_string(), *exp.clone())?;
              } else if let Expr::Assignment(ty, lit, _) = exp.expr.clone() {
                if ty.is_none() { continue; }
                if let Expr::Literal(Literals::Identifier(Name(name), _)) = lit.expr {
                  self.add_import_dependency(Some(name), None, module.to_string(), *exp.clone())?;
                }
              }
            }
          }
        // Add module name to namespace
        } else {
          self.add_import_dependency(None, None, module.to_string(), expr.clone())?;
        }
      } else {
        for (Name(name), alias) in names.as_ref().unwrap() {
          // If alias, add alias to namespace
          if alias.is_some() {

            // The module itself is aliased, no `from` names were specified
            if is_aliased {
              self.add_import_dependency(None, Some(alias.clone().unwrap().0), module.to_string(), expr.clone())?;
            } else {
              self.add_import_dependency(Some(name.to_string()), Some(alias.clone().unwrap().0), module.to_string(), expr.clone())?;
            }

          // If not, add name to namespace
          } else {
            self.add_import_dependency(Some(name.to_string()), None, module.to_string(), expr.clone())?;
          }
        }
      }

      return Ok(ast);
    } else {
      unreachable!();
    }
  }

  /// Enter a definition scope
  /// Assignments, functions, classes and type definitions
  fn enter_scope(&mut self, name: String) {
    // If we are already in a scope, push the current name to the list of scoped definitions
    if self.current_name.is_some() {
      self.current_scope_defs.push(name);

    // Otherwise, set the current name
    } else {
      self.current_name = Some(name);
    }
  }

  /// Exit a definition scope
  fn exit_scope(&mut self) {
    self.current_name = None;
  }

  /// Add various import dependencies to the current namespace
  /// 
  /// No name means the whole module is being imported
  fn add_import_dependency(&mut self, name: Option<String>, alias: Option<String>, from: String, expr: Expression) -> Result<(), Error> {
    let namespace = self.namespaces.get_mut(self.current_namespace.as_str());
    if namespace.is_none() {
      return Err(Error::new(
        Error::CompilerError,
        None,
        "",
        Some(0),
        Some(0),
        format!("Namespace not defined: {}", self.current_namespace)
      ));
    }

    let f = from.split("/").last().unwrap();
    if alias.is_none() {
      if namespace.as_ref().unwrap().names.contains_key(f) {
        return Err(Error::new(
          Error::NameError,
          None,
          "",
          Some(0),
          Some(0),
          format!("Name '{}' from '{}' already defined in file {}", f, from, self.current_namespace)
        ));
      }
    } else {
      if namespace.as_ref().unwrap().names.contains_key(alias.as_ref().unwrap().as_str()) {
        return Err(Error::new(
          Error::NameError,
          None,
          self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap(),
          expr.first_pos,
          expr.first_pos,
          format!("Name '{}' already defined in scope", alias.as_ref().unwrap())
        ));
      }
    }

    for (path, _) in namespace.as_ref().unwrap().imports.iter() {
      let p = path.split("/").last().unwrap();
      if name.is_none() && alias.is_none() && p != "*" && f != "*" && p == f {
        return Err(Error::new(
          Error::ImportError,
          None,
          self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap(),
          expr.first_pos,
          expr.first_pos,
          format!("Name '{}' from '{}' already imported in file {}", f, from, self.current_namespace) 
        ));
      }
    }

    // Import location exists
    if namespace.as_ref().unwrap().imports.contains_key(from.as_str()) {
      // Continue adding import names to existing list
      if let Some(list) = namespace.unwrap().imports.get_mut(from.as_str()).unwrap() {
        list.push((name, alias));

      // List doesn't exist but import does, we already imported the whole module
      } else {
        return Err(Error::new(
          Error::ImportError,
          None,
          self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap(),
          expr.first_pos,
          expr.first_pos,
          format!("Whole module has already been imported: '{}'.", from)
        ));
      }

    // Location isn't set yet
    } else {
      // No name so full module is being imported
      if name.is_none() {
        if alias.is_none() {
          namespace.unwrap().imports.insert(from, None);

        // Aliased import
        } else {
          namespace.unwrap().imports.insert(from, Some(vec![(None, alias)]));
        }

      // from { name as alias }
      } else {
        namespace.unwrap().imports.insert(from, Some(vec![(name, alias)]));
      }
    }

    return Ok(());
  }

  /// Add named items (imports and function, class and type definitions) to the current namespace
  fn add_name(&mut self, name: String, expr: Expression) -> Result<(), Error> {
    let mut namespace = self.namespaces.get_mut(self.current_namespace.as_str());
    if namespace.is_none() {
      self.namespaces.insert(self.current_namespace.clone(), Namespace {
        names: IndexMap::new(),
        dependencies: IndexMap::new(),
        imports: IndexMap::new()
      });
      namespace = self.namespaces.get_mut(self.current_namespace.as_str());
    }

    // If we are not in a scope, add the name to the namespace
    if self.current_name.is_none() {
      if namespace.as_ref().unwrap().names.contains_key(name.as_str()) {
        return Err(Error::new(
          Error::NameError,
          None,
          self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap(),
          expr.first_pos,
          expr.first_pos,
          format!("Name '{}' already defined in scope", name)
        ));
      }
      namespace.unwrap().names.insert(name, expr);

    // If we are in a scope, add the name to the namespace if it is not in the list of scoped definitions
    } else {
      if !self.current_scope_defs.contains(&name) {
        namespace.unwrap().names.insert(name, expr);
      }
    }

    return Ok(());
  }

  /// Add dependencies (names that depend upon others) to the current namespace
  /// 
  /// If there is no current_name, we are in the main scope and we can ignore dependencies
  fn add_dependency(&mut self, dependency: String) -> Result<(), Error> {
    let namespace = self.namespaces.get_mut(self.current_namespace.as_str());
    if namespace.is_none() {
      return Err(Error::new(
        Error::CompilerError,
        None,
        "",
        Some(0),
        Some(0),
        format!("Namespace not defined: {}", self.current_namespace)
      ));
    }

    if self.current_name.is_none() { return Ok(()); }

    // Add dependency to the namespace
    if namespace.as_ref().unwrap().dependencies.contains_key(self.current_name.as_ref().unwrap()) {
      if !namespace.as_ref().unwrap().dependencies.values().any(|deps| deps.contains(&dependency)) {
        namespace.unwrap().dependencies.get_mut(self.current_name.as_ref().unwrap()).unwrap().push(dependency);
      }
    } else {
      namespace.unwrap().dependencies.insert(self.current_name.as_ref().unwrap().to_owned(), vec![dependency]);
    }

    return Ok(());
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
      let identifier = self.consume(&[TokenTypes::Identifier], false)?;

      if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "="
        || [TokenTypes::Newline, TokenTypes::Semicolon].contains(&self.iter.current.as_ref().unwrap().of_type) {
        return self.parse_assignment(Some(&assignment), identifier, None);
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

    } else if init.unwrap().value.as_ref().unwrap() == "pub" {
      return self.parse_public();

    } else if init.unwrap().value.as_ref().unwrap() == "type" {
      self.iter.next();
      return self.parse_type();

    } else if init.unwrap().value.as_ref().unwrap() == "for" {
      return self.parse_for_loop();

    } else if init.unwrap().value.as_ref().unwrap() == "while" {
      return self.parse_while_loop();

    } else if init.unwrap().value.as_ref().unwrap() == "break" {
      return self.parse_break_continue("break");

    } else if init.unwrap().value.as_ref().unwrap() == "continue" {
      return self.parse_break_continue("continue");

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
    let mut left = self.parse_primary()?;

    while self.iter.current.is_some()
      && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator
      && self.get_precedence(self.iter.current.as_ref().unwrap().clone()) >= precedence {

      let operator = self.iter.next().unwrap();
      if ["++", "--"].contains(&&operator.value.as_ref().unwrap().as_str()) {
        return Ok(self.get_expr(Expr::UnaryOperator(
          Operator(String::from(operator.value.as_ref().unwrap().to_owned() + ":post")),
          Box::new(left.clone())
        ), Some(init.line), init.start_pos, Some(operator.line)));
      }

      // Handle assignment for when lhs indexes bring us here
      if operator.value.as_ref().unwrap() == "=" {
        return Ok(self.parse_assignment(None, init, Some(left))?);
      }

      let mut next_precedence = self.get_precedence(operator.clone());
      if self.is_left_associative(operator.clone()) { next_precedence += 1; }

      let right = self.parse_expression(next_precedence)?;
      left = self.get_expr(Expr::BinaryOperator(
        Operator((*operator.to_owned().value.unwrap()).to_string()),
        Box::new(left),
        Box::new(right.clone())
      ), Some(init.line), init.start_pos, right.last_line);
    }

    // if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Semicolon {
    //   self.consume(&[TokenTypes::Semicolon], false);
    // }

    return Ok(left);
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

      // EOF, return ident literal
      if self.iter.current.is_none() || self.iter.current.as_ref().unwrap().of_type == TokenTypes::EOF {
        return Ok(self.get_expr(Expr::Literal(Literals::Identifier(Name(String::from(ident.value.unwrap())), None)),
          Some(ident.line), ident.start_pos, Some(ident.line)
        ));

      // Index
      } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Operator && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "." {
        // Add top-most identifier being indexed as a dependency
        if !self.is_indexing {
          self.add_dependency(ident.value.as_ref().unwrap().to_owned())?;
        }
        return self.parse_index(ident);

      // Array index
      } else if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::LBracket {
        // Add top-most identifier being indexed as a dependency
        self.add_dependency(ident.value.as_ref().unwrap().to_owned())?;
        return self.parse_array_index(ident);

      // Call
      } else if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::LParen {
        return self.parse_call(ident);

      // Assignment
      } else if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "=" {
        return self.parse_assignment(None, ident, None);

      // Typed assignment
      } else if self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type == TokenTypes::Identifier {
        // We have a type, get the identifier 
        let name = self.iter.next().unwrap();
        return self.parse_assignment(Some(&ident), name, None);

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

  fn parse_public(&mut self) -> Result<Expression, Error> {
    let init = self.iter.current.as_ref().unwrap().clone();

    if let Some(token) = &self.iter.current.clone() {
      if token.of_type == TokenTypes::Keyword && token.value.as_ref().unwrap() == "pub" {
        self.iter.next();

        let expression = self.parse_top()?;

        match expression.expr.clone() {
          Expr::Class(Name(name), _, _) | Expr::Function(Name(name), _, _, _) | Expr::TypeDef(Literals::Identifier(Name(name), _), _) => {
            self.add_name(name.to_string(), expression.clone())?;
            return Ok(self.get_expr(Expr::Public(Box::new(expression)),
              Some(token.line), token.start_pos, token.end_pos
            ));
          },
          Expr::Assignment(typ, name_expr, _) => {
            if typ.is_none() {
              return Err(Error::new(
                Error::UnexpectedToken,
                Some(token.clone()),
                self.code.lines().nth((token.line - 1) as usize).unwrap(),
                token.start_pos,
                token.end_pos,
                "Expected named expression after \"pub\"".to_string()
              ));
            }

            if let Expr::Literal(Literals::Identifier(Name(name), _)) = name_expr.expr {
              self.add_name(name.to_string(), expression.clone())?;
              return Ok(self.get_expr(Expr::Public(Box::new(expression)),
                Some(token.line), token.start_pos, token.end_pos
              ));
            }
          },
          _ => {
            return Err(Error::new(
              Error::UnexpectedToken,
              Some(token.clone()),
              self.code.lines().nth((token.line - 1) as usize).unwrap(),
              token.start_pos,
              token.end_pos,
              "Expected named expression after \"pub\"".to_string()
            ));
          },
        }
      }
    }

    return Err(Error::new(
      Error::UnexpectedToken,
      Some(init.clone()),
      self.code.lines().nth((init.line - 1) as usize).unwrap(),
      init.start_pos,
      init.end_pos,
      "Missing expression after \"pub\"".to_string()
    ));
  }

  fn parse_assignment(&mut self, typ: Option<&Token>, identifier: Token, indexed: Option<Expression>) -> Result<Expression, Error> {
    // Get type if it exists
    let mut assignment_type: Option<Type> = None;
    if typ.is_some() {
      assignment_type = Some(Type(typ.unwrap().value.as_ref().unwrap().to_owned()));
    }

    let assignment: Expression;

    // TODO: Handle comma case (multiple assignments at once)

    // If indexed, this is an array assignment
    if indexed.is_some() {
      let index = indexed.unwrap();
      let assign = self.parse_expression(0)?;
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Box::new(index),
        Some(Box::new(assign.clone()))),
        Some(identifier.line), identifier.start_pos, assign.last_line
      );
    } else if [TokenTypes::Newline, TokenTypes::Semicolon].contains(&self.iter.current.as_ref().unwrap().of_type) {
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Box::new(self.get_expr(Expr::Literal(Literals::Identifier(Name(String::from(identifier.value.clone().unwrap())), None)),
          Some(identifier.line), identifier.start_pos, Some(identifier.line)
        )),
        None
      ), Some(identifier.line), identifier.start_pos, Some(identifier.line));
    } else if self.iter.current.as_ref().unwrap().value.as_ref().unwrap() == "=" {
      self.iter.next();
      let assign = self.parse_expression(0)?;
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Box::new(self.get_expr(Expr::Literal(Literals::Identifier(Name(String::from(identifier.value.clone().unwrap())), None)),
          Some(identifier.line), identifier.start_pos, Some(identifier.line)
        )),
        Some(Box::new(assign.clone()))
      ), Some(identifier.line), identifier.start_pos, assign.last_line);
    } else if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RParen {
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Box::new(self.get_expr(Expr::Literal(Literals::Identifier(Name(String::from(identifier.value.clone().unwrap())), None)),
          Some(identifier.line), identifier.start_pos, Some(identifier.line)
        )),
        None
      ), Some(identifier.line), identifier.start_pos, Some(identifier.line));
    } else {
      let assign = self.parse_expression(0)?;
      assignment = self.get_expr(Expr::Assignment(
        assignment_type,
        Box::new(self.get_expr(Expr::Literal(Literals::Identifier(Name(String::from(identifier.value.clone().unwrap())), None)),
          Some(identifier.line), identifier.start_pos, Some(identifier.line)
        )),
        Some(Box::new(assign.clone()))
      ), Some(identifier.line), identifier.start_pos, assign.last_line);
    }

    // If type (let a = 5), definition
    if typ.is_some() {
      self.add_name(identifier.value.as_ref().unwrap().to_owned(), assignment.clone())?;

    // If no type (a = 5), reassigment
    } else {
      self.add_dependency(identifier.value.as_ref().unwrap().to_owned())?;
    }

    return Ok(assignment);
  }

  fn parse_array(&mut self) -> Result<Expression, Error> {
    let mut list = vec![];
    while self.iter.current.is_some() && self.iter.current.as_ref().unwrap().of_type != TokenTypes::RBracket {
      list.push(self.parse_primary()?);
      if self.iter.current.as_ref().unwrap().of_type == TokenTypes::Comma {
        self.iter.next();
        self.skip_newlines(None);
      } else {
        self.skip_newlines(None);
        if self.iter.current.as_ref().unwrap().of_type == TokenTypes::RBracket {
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
      let type_alias = self.get_expr(Expr::TypeDef(
        Literals::Identifier(Name(String::from(ident.value.clone().unwrap())), None),
        Box::new(Literals::Identifier(Name(String::from(t.value.unwrap())), None))
      ), Some(ident.line), ident.start_pos, Some(t.line));
      self.add_name(ident.value.unwrap(), type_alias.clone())?;
      return Ok(type_alias);

    // If typedef / struct
    } else {
      let struct_type = self.parse_object(Some(ident.clone()))?;
      self.add_name(ident.value.unwrap(), struct_type.clone())?;
      return Ok(struct_type);
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
        self.skip_newlines(None);
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
          expr: Expr::Literal(Literals::Identifier(Name(identifier_token.value.unwrap()), None)),
          first_line: Some(identifier_token.line),
          first_pos: identifier_token.start_pos,
          last_line: identifier_token.end_pos,
        })
      } else {
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

    let iterable = self.parse_expression(0)?;

    self.consume(&[TokenTypes::RParen], false)?;

    self.in_loop = true;
    let body = self.parse_loop_block()?;
    self.in_loop = false;

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

  fn parse_while_loop(&mut self) -> Result<Expression, Error> {
    let init = self.iter.next(); // Consume "while"

    self.consume(&[TokenTypes::LParen], false)?;
    let expression = self.parse_expression(0)?;
    self.consume(&[TokenTypes::RParen], false)?;

    self.in_loop = true;
    let body = self.parse_loop_block()?;
    self.in_loop = false;

    return Ok(Expression {
      expr: Expr::WhileLoop(
        Box::new(expression),
        Some(body),
      ),
      first_line: Some(init.as_ref().unwrap().line),
      first_pos: Some(init.as_ref().unwrap().start_pos.unwrap()),
      last_line: Some(init.unwrap().line),
    });
  }

  fn parse_break_continue(&mut self, typ: &str) -> Result<Expression, Error> {
    let token = self.iter.next().unwrap();

    if !self.in_loop {
      return Err(Error::new(
        Error::UnexpectedToken,
        Some(token.clone()),
        self.code.lines().nth((token.line) as usize).unwrap(),
        token.start_pos,
        token.end_pos,
        format!("Unexpected {} outside of loop.", typ),
      ));
    }

    match typ {
      "break" => {
        return Ok(self.get_expr(Expr::BreakLoop,
          Some(token.line),
          token.start_pos,
          Some(token.line)
        ));
      },
      "continue" => {
        return Ok(self.get_expr(Expr::ContinueLoop,
          Some(token.line),
          token.start_pos,
          Some(token.line)
        ));
      },
      _ => unreachable!()
    }
  }

  fn expect_keyword(&mut self, keyword: &str) -> Result<(), Error> {
    if let Some(token) = &self.iter.current {
      if token.of_type == TokenTypes::Keyword && token.value.as_ref().unwrap() == keyword {
        self.iter.next(); // Consume keyword
        Ok(())
      } else {
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

  /// Parse index chain (recursive)
  fn parse_index(&mut self, identifier: Token) -> Result<Expression, Error> {
    self.iter.next(); // Consume `.`
    self.is_indexing = true;
    let index = self.parse_expression(0)?;
    self.is_indexing = false;

    return Ok(self.get_expr(Expr::Index(
      Box::new(self.get_expr(Expr::Literal(
        Literals::Identifier(Name(String::from(identifier.value.unwrap())), None)
      ), Some(identifier.line), identifier.start_pos, Some(identifier.line))),
      Box::new(index.clone())),
      Some(identifier.line), identifier.start_pos, index.last_line
    ));
  }

  fn parse_array_index(&mut self, identifier: Token) -> Result<Expression, Error> {
    self.consume(&[TokenTypes::LBracket], false)?;
    let index = self.parse_expression(0)?;
    self.consume(&[TokenTypes::RBracket], false)?;

    return Ok(self.get_expr(Expr::ArrayIndex(
      Box::new(self.get_expr(Expr::Literal(
        Literals::Identifier(Name(String::from(identifier.value.unwrap())), None)
      ), Some(identifier.line), identifier.start_pos, Some(identifier.line))),
      Box::new(index.clone())),
      Some(identifier.line), identifier.start_pos, index.last_line)
    );
  }

  fn parse_call(&mut self, identifier: Token) -> Result<Expression, Error> {
    // Save name if this is a function call and not an index/method call
    if !self.is_indexing {
      self.add_dependency(identifier.value.as_ref().unwrap().to_owned())?;
    }

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

    self.enter_scope(identifier.value.as_ref().unwrap().to_owned());
    let body = self.parse_func_block()?;
    self.exit_scope();

    if return_type.is_some() {
      let func = self.get_expr(
        Expr::Function(
          Name(identifier.clone().value.unwrap().to_owned()),
          Some(Literals::Identifier(Name(return_type.unwrap().value.unwrap()), None)),
          Some(Box::new(params)),
          Some(Box::new(body))
        ),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(self.iter.current.as_ref().unwrap().line)
      );
      self.add_name(identifier.value.as_ref().unwrap().to_owned(), func.clone())?;
      return Ok(func);
    } else {
      let func = self.get_expr(
        Expr::Function(
          Name(identifier.clone().value.unwrap().to_owned()),
          None,
          Some(Box::new(params)),
          Some(Box::new(body))
        ),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(self.iter.current.as_ref().unwrap().line)
      );
      self.add_name(identifier.value.as_ref().unwrap().to_owned(), func.clone())?;
      return Ok(func);
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

    while let Some(token) = &self.iter.current.clone() {
      if token.of_type == TokenTypes::RBrace {
        self.iter.next();
        break;
      }

      body.push(self.parse_top()?);

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
        "Expected '}' to end loop body, got end of file.".into(),
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
      let cls = self.get_expr(Expr::Class(
        Name(ident.value.clone().unwrap()),
        parents,
        Some(Box::new(body))
      ), Some(init.as_ref().unwrap().line), init.unwrap().start_pos, Some(end.line));
      self.add_name(ident.value.unwrap(), cls.clone())?;
      return Ok(cls);
    } else {
      let cls = self.get_expr(Expr::Class(
        Name(ident.value.clone().unwrap()),
        None,
        Some(Box::new(body))
      ), Some(init.as_ref().unwrap().line), init.unwrap().start_pos, Some(end.line));
      self.add_name(ident.value.unwrap(), cls.clone())?;
      return Ok(cls);
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
          let idx = vec![Name(String::from(index[0].value.as_ref().unwrap()))];
          let expr = self.get_expr(Expr::Module(idx.clone(), false, None),
            Some(init.as_ref().unwrap().line),
            init.unwrap().start_pos,
            Some(index[0].line)
          );
          let mut parsed = expr.clone();
          parsed.expr = Expr::ModuleParsed(
            idx,
            false,
            None,
            self.process_module(expr.clone())?
          );
          return Ok(parsed);
        }

        // Alias
        let idx = vec![Name(String::from(index[0].value.as_ref().unwrap()))];
        let names = Some(vec![(name, alias)]);
        let expr = self.get_expr(Expr::Module(
          idx.clone(),
          true,
          names.clone()
        ), Some(init.as_ref().unwrap().line), init.unwrap().start_pos, Some(index[0].line));
        let mut parsed = expr.clone();
        parsed.expr = Expr::ModuleParsed(
          idx,
          true,
          names.clone(),
          self.process_module(expr.clone())?
        );
        return Ok(parsed);
      }

      // Has index //

      // If last name in index has an alias
      let (name, alias) = self.parse_import_name_or_alias(Some(index[index.iter().len() - 1].clone()))?;

      // No alias
      if alias.is_none() {
        let idx = index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>();
        let expr = self.get_expr(Expr::Module(
          idx.clone(),
          false,
          None),
          Some(init.as_ref().unwrap().line),
          init.unwrap().start_pos,
          Some(index.last().unwrap().line)
        );
        let mut parsed = expr.clone();
        parsed.expr = Expr::ModuleParsed(
          idx,
          false,
          None,
          self.process_module(expr.clone())?
        );
        return Ok(parsed);
      }

      // Alias
      let idx = index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>();
      let names = Some(vec![(name, alias)]);
      let expr = self.get_expr(Expr::Module(
        idx.clone(),
        true,
        names.clone()),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(index.last().unwrap().line)
      );
      let mut parsed = expr.clone();
      parsed.expr = Expr::ModuleParsed(
        idx,
        true,
        names,
        self.process_module(expr.clone())?
      );
      return Ok(parsed);

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

      let idx = index.iter().map(|t| Name(t.value.as_ref().unwrap().to_owned())).collect::<Vec<Name>>();
      let expr = self.get_expr(Expr::Module(
        idx.clone(),
        false,
        Some(names.clone())),
        Some(init.as_ref().unwrap().line),
        init.unwrap().start_pos,
        Some(end.line)
      );
      let mut parsed = expr.clone();
      parsed.expr = Expr::ModuleParsed(
        idx,
        false,
        Some(names),
        self.process_module(expr.clone())?
      );
      return Ok(parsed);

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
    let ast = parser.parse(String::from("let a = 1;
    const name = \"Jink\"
    type Number = int;"), String::new(), false, true)?;
    return Ok(assert_eq!(ast, vec![
      parser.get_expr(Expr::Assignment(
        Some(Type(String::from("let"))),
        Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("a")), None)),
          None, None, None
        )),
        Some(Box::new(
          parser.get_expr(Expr::Literal(Literals::Integer(1)), None, None, None)
        )
      )), None, None, None),
      parser.get_expr(Expr::Assignment(
        Some(Type(String::from("const"))),
        Box::new(parser.get_expr(Expr::Literal(Literals::Identifier(Name(String::from("name")), None)),
          None, None, None
        )),
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
    let ast = parser.parse(String::from("if (a == 1) {
      return a;
    } else {
      return b;
    }"), String::new(), false, true)?;
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
    let ast = parser.parse(String::from("print(\"Hello, world!\");"), String::new(), false, true)?;
    return Ok(assert_eq!(ast[0], parser.get_expr(Expr::Call(
      Name(String::from("print")),
      Box::new(vec![parser.get_expr(Expr::Literal(Literals::String(String::from("Hello, world!"))), None, None, None)])
    ), None, None, None)));
  }

  #[test]
  fn test_parse_function_def() -> Result<(), Error> {
    let mut parser = Parser::new();
    let ast = parser.parse(String::from("fun add(let a, let b) {
      return a + b;
    }"), String::new(), false, true)?;
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
    let ast = parser.parse(String::from("fun sub(let a, let b) return a - b;"), String::new(), false, true)?;
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
    let ast = parser.parse(String::from("fun pow(let a: 1, let b: 2, let c: 3) {
      return a ^ b ^ c;
    }"), String::new(), false, true)?;
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
    let ast = parser.parse(String::from("fun add(int a, int b) -> int {
      return a + b;
    }"), String::new(), false, true)?;
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
    let ast = parser.parse(String::from("fun are_even(int a, int b) -> int {
      if (a % 2 == 0 && b % 2 == 0) return true
      else return false
    }"), String::new(), false, true)?;
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
    let ast = parser.parse(String::from("fun add(const a, const int b) {
      return a + b;
    }"), String::new(), false, true).unwrap();
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