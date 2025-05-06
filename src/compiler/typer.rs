use jink::{Error, ErrorCtx, Expr, Expression, Literals, Name, Operator, Type, JType};
use std::collections::HashMap;

struct Scope {
  variables: HashMap<String, JType>,
}

pub struct TypeChecker {
  source: String,
  scopes: Vec<Scope>,
  // Flag for when we're in a function and expecting a return
  in_function: bool,
  // The expected return type of the current function
  current_return_type: Option<JType>,
  in_loop: bool,
}

impl TypeChecker {
  pub fn new() -> Self {
    TypeChecker {
      scopes: vec![Scope { variables: HashMap::new() }], // Global scope
      in_function: false,
      current_return_type: None,
      in_loop: false,
      source: String::new(),
    }
  }

  pub fn set_source(&mut self, source: String) {
    self.source = source;
  }

  fn enter_scope(&mut self) {
    self.scopes.push(Scope { variables: HashMap::new() });
  }

  fn exit_scope(&mut self) {
    if self.scopes.len() > 1 {
      self.scopes.pop();
    }
  }

  fn enter_function(&mut self, return_type: Option<JType>) {
    self.enter_scope();
    self.in_function = true;
    self.current_return_type = return_type;
  }

  fn exit_function(&mut self) {
    self.exit_scope();
    self.in_function = false;
    self.current_return_type = None;
  }

  fn enter_loop(&mut self) {
    self.enter_scope();
    self.in_loop = true;
  }

  fn exit_loop(&mut self) {
    self.exit_scope();
    self.in_loop = false;
  }

  fn add_variable(&mut self, name: &str, typ: JType) -> Result<(), String> {
    if let Some(scope) = self.scopes.last_mut() {
      // Check for redeclaration in the *current* scope only
      if scope.variables.contains_key(name) {
        return Err(format!("Variable '{}' already declared in this scope.", name));
      }
      scope.variables.insert(name.to_string(), typ);
      Ok(())
    } else {
      Err(format!("Internal Error: No current scope to add variable '{}'", name))
    }
  }

  fn lookup_variable(&self, name: &str) -> Option<JType> {
    for (_, scope) in self.scopes.iter().rev().enumerate() {
      if let Some(typ) = scope.variables.get(name) {
        return Some(typ.clone());
      }
    }
    None
  }

  // TEMP: Check if a function is a built-in function
  fn is_builtin_function(&self, name: &str) -> bool {
    let builtins = [
      "printf", "puts", "scanf", "strlen",
      "malloc", "free",
      "fopen", "fwrite", "fread", "fclose", "fputs"
    ];
    builtins.contains(&name)
  }

  fn string_to_jtype(&self, type_name: &str) -> Result<JType, String> {
    match type_name {
      "int" => Ok(JType::Integer),
      "uint" => Ok(JType::UnsignedInteger),
      "float" => Ok(JType::FloatingPoint),
      "string" => Ok(JType::String),
      "bool" => Ok(JType::Boolean),
      "void" => Ok(JType::Void),
      _ => Ok(JType::TypeName(type_name.to_string())), // Assume custom type name for now
    }
  }

  fn check_type_compatibility(&self, expected: &JType, actual: &JType) -> bool {
    // Allow assigning anything to Unknown for initial inference steps
    if *expected == JType::Unknown || *actual == JType::Unknown {
      return true;
    }

    // TODO: Handle TypeName comparison (lookup definition)
    // TODO: Generics? Not sure how we'll do this in Jink yet
    // TODO: Handle Null compatibility (like assigning null to nullables and objects)
    expected == actual
  }

  // Check an expression and return its type
  fn check_expression(&mut self, expr: &mut Expression) -> Result<JType, String> {
    let typ = match &mut expr.expr {
      Expr::Literal(literal) => match literal {
        Literals::Integer(_) => Ok(JType::Integer),
        Literals::UnsignedInteger(_) => Ok(JType::UnsignedInteger),
        Literals::FloatingPoint(_) => Ok(JType::FloatingPoint),
        Literals::String(_) => Ok(JType::String),
        Literals::Boolean(_) => Ok(JType::Boolean),
        Literals::Null => Ok(JType::Null),
        Literals::Identifier(Name(name)) => {
          self.lookup_variable(name)
            .ok_or_else(|| format!("Variable '{}' not found.", name))
        },
        _ => Ok(JType::Unknown),
      }

      Expr::Array(arr_exprs) => {
        let mut element_types = Vec::new();
        for elem in arr_exprs.iter_mut() {
          let elem_type = self.check_expression(elem)?;
          element_types.push(elem_type);
        }

        // For the moment, assuming all elements are of the same type / homogeneous
        if let Some(first_type) = element_types.first() {
          Ok(JType::Array(Box::new(first_type.clone())))
        } else {
          Ok(JType::Array(Box::new(JType::Unknown))) // Empty array
        }
      }

      Expr::Assignment(assignment_type, ident_or_index, value_expr) => {
        // Determine the type of the right-hand side, if it exists
        let rhs_type = match value_expr {
          Some(expr) => self.check_expression(expr)?,
          None => JType::Unknown, // No initializer, type is unknown unless annotated
        };

        // Determine what kind of lhs assignment target we have
        match &mut ident_or_index.expr {
          // Case: Assignment to identifier (variable)
          Expr::Literal(Literals::Identifier(Name(name))) => {
            let name = name.as_str();
            match assignment_type {
              // Case 1: Typed (`int a = 5;`)
              Some(Type(type_name)) if type_name != "let" && type_name != "const" => {
                let expected_type = self.string_to_jtype(type_name)?;

                // Check compatibility if there's a value assigned
                if value_expr.is_some() && !self.check_type_compatibility(&expected_type, &rhs_type) {
                  return Err(format!(
                    "Type mismatch for variable '{}'. Expected {}, got {}.",
                    name, expected_type, rhs_type
                  ));
                }
                self.add_variable(name, expected_type)?;
                Ok(JType::Null)
              }

              // Case 2: Type inference (`let a = 5;` or `const a = true;`)
              Some(Type(type_name)) if type_name == "let" || type_name == "const" => {
                if rhs_type == JType::Unknown && value_expr.is_none() {
                  // Declaration without initializer (e.g., `let a;`) - type remains Unknown for now
                  self.add_variable(name, JType::Unknown)?;
                } else if rhs_type == JType::Null {
                  return Err(format!("Cannot assign null to variable '{}' without explicit type.", name));
                } else {
                  // Infer type from RHS
                  self.add_variable(name, rhs_type)?;
                }
                Ok(JType::Null)
              }

              // Case 3: Reassignment (`a = 10;`)
              None => {
                match self.lookup_variable(name) {
                  Some(existing_type) => {
                    if value_expr.is_none() {
                      return Err(format!("Cannot reassign variable '{}' without a value.", name));
                    }
                    if !self.check_type_compatibility(&existing_type, &rhs_type) {
                      return Err(format!(
                        "Type mismatch on reassignment of variable '{}'. Expected {}, got {}.",
                        name, existing_type, rhs_type
                      ));
                    }
                    Ok(JType::Null)
                  }
                  None => Err(format!("Cannot assign to undeclared variable '{}'.", name)),
                }
              }
              _ => Err("Internal error: Unexpected assignment type structure.".to_string()),
            }
          }
          // Case: Assignment to an index (array or object property)
          Expr::ArrayIndex(arr_expr, idx_expr) | Expr::Index(arr_expr, idx_expr) => {
            // For now, just check that the array/object exists and the assignment type matches
            let arr_type = self.check_expression(arr_expr)?;
            let _idx_type = self.check_expression(idx_expr)?;
            // Only allow assignment to arrays or objects
            match arr_type {
              JType::Array(boxed_elem_type) => {
                // Check type compatibility for array element
                if !self.check_type_compatibility(&boxed_elem_type, &rhs_type) {
                  return Err("Type mismatch for array element assignment.".to_string());
                }
                Ok(JType::Null)
              }
              JType::Object(_) => {
                // For objects, allow any assignment for now (TODO: check property type)
                Ok(JType::Null)
              }
              _ => Err("Left-hand side of indexed assignment must be array or object.".to_string()),
            }
          }
          // Not supported as assignment target
          _ => Err("Invalid assignment target (must be identifier or index).".to_string()),
        }
      }

      Expr::BinaryOperator(op, left, right) => {
        let left_type = self.check_expression(left)?;
        let right_type = self.check_expression(right)?;

        match op {
          Operator(s) if ["+", "-", "*", "/", "%"].contains(&s.as_str()) => {
            if (left_type == JType::Integer || left_type == JType::FloatingPoint) &&
              (right_type == JType::Integer || right_type == JType::FloatingPoint) {
              // Promote to float if either operand is float
              if left_type == JType::FloatingPoint || right_type == JType::FloatingPoint {
                Ok(JType::FloatingPoint)
              } else {
                Ok(JType::Integer)
              }
            } else {
              Err(format!("Operator '{}' cannot be applied to types {} and {}.", s, left_type, right_type))
            }
          },
          Operator(s) if ["==", "!=", "<", ">", "<=", ">="].contains(&s.as_str()) => {
            let are_both_sides_numeric = (
              left_type == JType::Integer || left_type == JType::FloatingPoint
            ) && (
              right_type == JType::Integer || right_type == JType::FloatingPoint
            );

            // Basic comparison for primitives for now
            if are_both_sides_numeric || (
              self.check_type_compatibility(&left_type, &right_type) &&
              // No placeholder types
              left_type != JType::Object(HashMap::new()) &&
              left_type != JType::Array(Box::new(JType::Unknown)) &&
              left_type != JType::Function(vec![], Box::new(JType::Unknown))
            ) {
              Ok(JType::Boolean)
            } else {
              Err(format!("Operator '{}' cannot compare types {} and {}.", s, left_type, right_type))
            }
          },
          Operator(s) if ["&&", "||"].contains(&s.as_str()) => {
            if left_type == JType::Boolean && right_type == JType::Boolean {
              Ok(JType::Boolean)
            } else {
              Err(format!("Logical operator '{}' requires boolean operands, got {} and {}.", s, left_type, right_type))
            }
          },
          Operator(s) if ["<<", ">>", "&", "|", "^"].contains(&s.as_str()) => {
            if left_type == JType::Integer && right_type == JType::Integer {
              Ok(JType::Integer)
            } else {
              Err(format!("Bitwise operator '{}' requires integer operands, got {} and {}.", s, left_type, right_type))
            }
          },
          _ => Ok(JType::Unknown),
        }
      }

      Expr::UnaryOperator(op, operand) => {
        let operand_type = self.check_expression(operand)?;
        match op {
          // Potential TODO: improve awareness to allow ! to work as a null check on variables (!a)
          // (to avoid also allowing it to be used with literals like !5 here)
          // Node.js allows both, but it may be good to restrict it
          Operator(s) if s == "!" => {
            if operand_type == JType::Boolean {
              Ok(JType::Boolean)
            } else if operand_type == JType::Null {
              Ok(JType::Boolean)
            } else if operand_type == JType::Integer || operand_type == JType::FloatingPoint {
              Ok(JType::Boolean)
            } else if operand_type == JType::String {
              Ok(JType::Boolean)
            } else {
              Err(format!("Operator '!' requires a boolean operand, got {}.", operand_type))
            }
          },
          Operator(s) if ["+", "-", "++", "--"].contains(&s.as_str()) => {
            if operand_type == JType::Integer || operand_type == JType::FloatingPoint {
              Ok(operand_type)
            } else {
              Err(format!("Unary operator '{}' requires a numeric operand, got {}.", s, operand_type))
            }
          },
          _ => Ok(JType::Unknown),
        }
      }

      Expr::Call(Name(func_name), args_expr) => {
        // TODO: Remove builtin definitions from the compiler..
        if self.is_builtin_function(func_name) {
          for arg in args_expr.iter_mut() {
            self.check_expression(arg)?;
          }
          return Ok(JType::Unknown);
        }

        let func_type = self.lookup_variable(func_name)
          .ok_or_else(|| format!("Function '{}' not found.", func_name))?;

        if let JType::Function(param_types, return_type) = func_type {

          // Check for variadic function
          let mut is_variadic = false;
          let mut variadic_type: Option<&Box<JType>> = None;
          let num_expected_params = param_types.len();

          if let Some(JType::VariadicFunParam(typ)) = param_types.last() {
            is_variadic = true;
            variadic_type = typ.as_ref();
          }

          let num_fixed_params = if is_variadic { num_expected_params - 1 } else { num_expected_params};
          let num_provided_args = args_expr.len();

          // TODO: Check for parameter defaults

          // Check numbers of arguments
          if is_variadic {
            if num_provided_args < num_fixed_params {
              return Err(format!(
                "Function '{}' expected at least {} arguments, but got {}.",
                func_name, num_fixed_params, num_provided_args
              ));
            }

          } else {
            if num_provided_args != num_expected_params {
              return Err(format!(
                "Function '{}' expected {} arguments, but got {}.",
                func_name, num_expected_params, num_provided_args
              ));
            }
          }

          // Check argument types against parameter types
          let mut provided_arg_types = Vec::with_capacity(num_provided_args);
          for arg in args_expr.iter_mut() {
            provided_arg_types.push(self.check_expression(arg)?);
          }

          // Check fixed args
          for i in 0..num_fixed_params {
            if !self.check_type_compatibility(&param_types[i], &provided_arg_types[i]) {
              return Err(format!(
                "Type mismatch for argument {} in call to '{}'. Expected {}, got {}.",
                i + 1, func_name, param_types[i], provided_arg_types[i]
              ));
            }
          }

          // Check variadic args if applicable
          if is_variadic {
            // TODO: Determine type of variadic parameter
            if let Some(var_type) = variadic_type {
              for i in num_fixed_params..num_provided_args {
                if !self.check_type_compatibility(&var_type, &provided_arg_types[i]) {
                  return Err(format!(
                    "Type mismatch for variadic argument {} in call to '{}'. Expected {}, got {}.",
                    i + 1, func_name, var_type, provided_arg_types[i]
                  ));
                }
              }
            }
          }

          Ok(*return_type)

        } else {
          Err(format!("'{}' is not a function.", func_name))
        }
      }

      Expr::Function(Name(name), return_type, params, body) => {
        // Determine expected parameter types and return type
        let mut expected_param_types = Vec::new();
        let mut param_names = Vec::new(); // To add to inner scope

        if let Some(param_items) = params {

          for param in param_items.iter_mut() {

            // TODO: Validate const (will definitely require overall refactoring)
            // TODO: Validate spread / variadic parameters
            if let Expr::FunctionParam(Some(Type(ptype)), _is_const, ident_literal, default_value, is_spread) = &mut param.expr {
              if let Literals::Identifier(Name(pname)) = ident_literal {
                let param_type = if ptype == "let" {
                  let typ = JType::Unknown;
                  param.inferred_type = Some(typ.clone());
                  typ
                } else {
                  let typ = self.string_to_jtype(ptype)?;
                  param.inferred_type = Some(typ.clone());
                  typ
                };

                // Check default type against param type if it exists (not variadic)
                if !*is_spread {
                  if let Some(default_value) = default_value {
                    let default_type = self.check_expression(&mut **default_value)?;
                    if !self.check_type_compatibility(&param_type, &default_type) {
                      return Err(format!(
                        "Default value type mismatch for parameter '{}'. Expected {}, got {}.",
                        pname, param_type, default_type
                      ));
                    }
                  }

                  expected_param_types.push(param_type.clone());
                  param_names.push((pname.clone(), param_type));

                // Variadic (parser already confirms this is the last param)
                } else {
                  expected_param_types.push(JType::VariadicFunParam(Some(Box::new(param_type.clone()))));
                  param_names.push((pname.clone(), JType::Array(Box::new(param_type))));
                }
              } else {
                return Err("Invalid function parameter: expected identifier on left-hand side (TODO: Type check function parameters).".to_string());
              }
            } else {
              return Err("Invalid function parameter format.".to_string());
            }
          }
        }

        let expected_return_type = match return_type {
          Some(Literals::Identifier(Name(rt_name))) => self.string_to_jtype(rt_name)?,
          None => JType::Void, // Default to void if no return type specified
          _ => return Err("Invalid return type.".to_string()),
        };

        // Add function signature to scope before checking body scope
        let func_type = JType::Function(expected_param_types.clone(), Box::new(expected_return_type.clone()));
        self.add_variable(name, func_type)?;

        self.enter_function(Some(expected_return_type.clone()));
        // Add parameters to the body scope
        for (pname, ptype) in param_names {
          self.add_variable(&pname, ptype)?;
        }

        if let Some(body) = body {
          // Check the function body
          for stmt in body.iter_mut() {
            self.check_expression(stmt)?;
          }
        }

        self.exit_function();

        Ok(JType::Null) // Function definition itself doesn't have a value type
      }

      Expr::Return(expr_opt) => {
        if !self.in_function {
          return Err("Return statement outside of function.".to_string());
        }

        let return_value_type = match Some(expr_opt) {
          Some(expr) => self.check_expression(expr)?,
          None => JType::Void, // `return;` implies void
        };

        // Check against the current function's expected return type
        if let Some(expected) = &self.current_return_type {
          if !self.check_type_compatibility(expected, &return_value_type) {
            return Err(format!(
              "Return type mismatch. Expected {}, got {}.",
              expected, return_value_type
            ));
          }
        } else {
          return Err("Internal error: Missing expected return type in function scope.".to_string());
        }

        Ok(return_value_type)
      }

      Expr::Conditional(cond_type, condition, body, else_body_opt) => {
        // Check condition - must be boolean
        if let Some(cond_expr) = condition {
          let cond_type = self.check_expression(cond_expr)?;
          if cond_type != JType::Boolean {
            return Err(format!("If condition must be boolean, got {}.", cond_type));
          }
        } else if cond_type.0 != "else" {
          // 'else' doesn't have a condition, 'if'/'elseif' must
          return Err("Conditional statement missing condition.".to_string());
        }

        // Check body
        self.enter_scope();
        for stmt in body.iter_mut() {
          self.check_expression(stmt)?; // Check statements, ignore type for now
        }
        self.exit_scope();

        // Check else/elseif body if it exists
        if let Some(else_body) = else_body_opt {
          for else_stmt in else_body.iter_mut() {
            // Note: Each element in else_body should be another Conditional expr
            self.check_expression(else_stmt)?;
          }
        }

        Ok(JType::Null) // Conditional statements don't produce a value
      }

      Expr::WhileLoop(condition, body) => {
        // TODO: Handle resolving other types to boolean
        let cond_type = self.check_expression(condition)?;
        if cond_type != JType::Boolean {
          return Err(format!("While loop condition must be boolean, got {}.", cond_type));
        }

        self.enter_loop();
        if let Some(body_exprs) = body {
          for stmt in body_exprs.iter_mut() {
            self.check_expression(stmt)?;
          }
        }
        self.exit_loop();

        Ok(JType::Null)
      }

      Expr::ForLoop(loop_var_expr, iterable, body) => {
        let iterable_type = self.check_expression(iterable)?;
        if !matches!(iterable_type, JType::Array(_)) {
          return Err(format!("For loop given non iterable, got {}.", iterable_type));
        }

        // Check and extract the loop variable
        let var_name = match loop_var_expr.expr.clone() {
          // for (let|const|<type> a in ...)
          Expr::Assignment(Some(Type(type_name)), ident_expr, _) => {
            if let Expr::Literal(Literals::Identifier(Name(name))) = &ident_expr.expr {

              // for (<type> a in ...) such as for (int a in ...)
              if !["let", "const"].contains(&type_name.as_str()) {
                let expected_var_type = self.string_to_jtype(&type_name)?;

                // Check if the variable type matches the iterable type
                if expected_var_type != JType::Integer {
                  return Err(format!("Type mismatch in for loop variable. Expected {}, got {}.", type_name, JType::Integer));
                }

                name.clone()

              // for (let|const a in ...)
              } else {
                name.clone()
              }
            } else {
              return Err("Invalid loop variable: expected identifier on left-hand side of assignment.".to_string());
            }
          },
          // TODO when parser is ready:
          // for (a in ...) - must check for explicitly declared variable
          // Expr::Assignment(None, ident_expr, _) => {
          //   // Check if the variable is already declared in the current scope
          // },
          // TEMP: for (let a in ...)
          // The parser only does a literal identifier for now
          Expr::Literal(Literals::Identifier(Name(name))) => {
            name.clone()
          },
          _ => return Err("Invalid loop variable: expected identifier or assignment expression.".to_string()),
        };

        // Check body in a loop scope with the loop variable defined
        self.enter_loop();
        self.add_variable(&var_name, JType::Integer)?; // Add loop variable to scope
        if let Some(body_vec) = body {
          for stmt in body_vec.iter_mut() {
            self.check_expression(stmt)?;
          }
        }
        self.exit_loop();

        Ok(JType::Null)
      }

      Expr::BreakLoop => {
        if !self.in_loop {
          return Err("Break statement outside of loop.".to_string());
        }
        Ok(JType::Null)
      }

      Expr::ContinueLoop => {
        if !self.in_loop {
          return Err("Continue statement outside of loop.".to_string());
        }
        Ok(JType::Null)
      }

      Expr::Public(expr) => {
        // Handle internal expression of the public modifier
        Ok(self.check_expression(expr)?)
      }

      // TODO: Array bounds check in type checker?
      Expr::ArrayIndex(arr, idx) => {
        let arr_type = self.check_expression(arr)?;
        let idx_type = self.check_expression(idx)?;

        // Check array is valid and index is an integer
        if let JType::Array(elem_type) = arr_type {
          if idx_type != JType::Integer {
            return Err(format!("Array index must be an integer, got {}.", idx_type));
          }

          // Return element type of array
          Ok(*elem_type)
        } else {
          Err(format!("Cannot index into non-array type {}.", arr_type))
        }
      }

      // TODO: Module, Delete, Index, Class
      _ => Ok(JType::Unknown),
    }?;

    // Add type
    if expr.inferred_type.is_none() || expr.inferred_type == Some(JType::Unknown) {
      expr.inferred_type = Some(typ.clone());
    }

    Ok(typ)
  }

  pub fn check(&mut self, ast: &mut [Expression]) -> Result<(), Error> {
    for expr in ast {
      match self.check_expression(expr) {
        Ok(_) => {},
        Err(err) => {
          let line = if !self.source.is_empty() && expr.first_line.is_some() {
            let line_num = expr.first_line.unwrap() as usize - 1;
            self.source.lines().nth(line_num).unwrap_or("").to_string()
          } else {
            String::new()
          };

          return Err(Error::CompilerError(ErrorCtx {
            token: None,
            line,
            start_pos: expr.first_pos,
            end_pos: Some(expr.first_pos.unwrap_or(0) + 1),
            message: err,
          }));
        }
      }
    }

    Ok(())
  }
}
