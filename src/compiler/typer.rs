use jink::{Error, ErrorCtx, Expr, Expression, Literals, Name, Operator, Type, JType};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct VariableInfo {
  jtype: JType,
  is_const: bool,
}

struct Scope {
  variables: HashMap<String, VariableInfo>,
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

  fn add_variable(&mut self, name: &str, jtype: JType, is_const: bool) -> Result<(), String> {
    if let Some(scope) = self.scopes.last_mut() {
      // Check for redeclaration in the *current* scope only
      if scope.variables.contains_key(name) {
        return Err(format!("Variable '{}' already declared in this scope.", name));
      }
      scope.variables.insert(name.to_string(), VariableInfo { jtype, is_const });
      Ok(())
    } else {
      Err(format!("Internal Error: No current scope to add variable '{}'", name))
    }
  }

  fn lookup_variable(&self, name: &str) -> Option<VariableInfo> {
    for (_, scope) in self.scopes.iter().rev().enumerate() {
      if let Some(var_info) = scope.variables.get(name) {
        return Some(var_info.clone());
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
      _ => {
        // Check if it's a user-defined type (class, enum, etc.)
        let var = self.lookup_variable(type_name);
        match var {
          Some(VariableInfo { jtype, .. }) => Ok(jtype),
          None => Err(format!("Unknown type '{}'.", type_name)),
        }
      }
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
        Literals::Object(ref mut props) => {
          let obj = self.check_object(props, false)?;
          Ok(JType::Object(obj))
        },
        Literals::Identifier(Name(name)) => {
          Ok(self.lookup_variable(name)
            .ok_or_else(|| format!("Variable '{}' not found.", name))
            .unwrap().jtype)
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
                self.add_variable(name, expected_type, false)?;
                Ok(JType::Null)
              }

              // Case 2: Type inference (`let a = 5;` or `const a = true;`)
              Some(Type(type_name)) if type_name == "let" || type_name == "const" => {
                // Declaration without initializer (e.g., `let a;`) - type remains Unknown for now
                if rhs_type == JType::Unknown && value_expr.is_none() {
                  if type_name == "const" {
                    return Err(format!("Cannot declare constant variable '{}' without an initializer.", name));
                  }
                  self.add_variable(name, JType::Unknown, false)?;
                } else if rhs_type == JType::Null {
                  return Err(format!("Cannot assign null to variable '{}' without explicit type.", name));
                } else {
                  // Infer type from RHS
                  self.add_variable(name, rhs_type, type_name == "const")?;
                }
                Ok(JType::Null)
              }

              // Case 3: Reassignment (`a = 10;`)
              None => {
                match self.lookup_variable(name) {
                  Some(VariableInfo { jtype: existing_type, is_const }) => {
                    if value_expr.is_none() {
                      return Err(format!("Cannot reassign variable '{}' without a value.", name));
                    }

                    // Ensure the existing type is not a constant
                    if is_const {
                      return Err(format!("Cannot reassign constant variable '{}'.", name));
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
          .ok_or_else(|| format!("Function '{}' not found.", func_name))?.jtype;

        if let JType::Function(param_types, return_type) = func_type {

          // Check for variadic function
          let (is_variadic, variadic_type): (bool, Option<&Box<JType>>) = match param_types.last() {
            Some(JType::FunctionParam(typ, _, is_spread)) if *is_spread => {
              (*is_spread, Some(typ))
            },
            _ => (false, None)
          };

          let num_expected_params = param_types.len();
          let num_fixed_params = if is_variadic { num_expected_params - 1 } else { num_expected_params};
          let mut num_fixed_without_defaults = num_fixed_params;
          for i in 0..num_fixed_without_defaults {
            match &param_types[i] {
              JType::FunctionParam(_, Some(_), _) => {
                num_fixed_without_defaults -= 1;
              },
              _ => {},
            }
          }
          let num_provided_args = args_expr.len();

          // Check if parameters without defaults or the variadic exceed the provided arguments
          if num_fixed_without_defaults > num_provided_args {
            return Err(format!(
              "Function '{}' expected at least {} arguments, but got {}.",
              func_name, num_fixed_without_defaults, num_provided_args
            ));
          }

          // Check if arguments exceed the expected number of parameters
          if !is_variadic && num_provided_args > num_expected_params {
            return Err(format!(
              "Function '{}' expected at most {} arguments, but got {}.",
              func_name, num_expected_params, num_provided_args
            ));
          }

          // Check argument types against parameter types
          let mut provided_arg_types = Vec::with_capacity(num_provided_args);
          for arg in args_expr.iter_mut() {
            provided_arg_types.push(self.check_expression(arg)?);
          }

          // Check args against fixed params (up to variadic)
          for i in 0..num_fixed_params {
            match &param_types[i] {
              JType::FunctionParam(param_type, default_type, _) => {

                // If argument doesn't exist and no default was provided
                if provided_arg_types.len() <= i {
                  if default_type.is_none() {
                    return Err(format!(
                      "Missing argument {} in call to '{}'. Expected {}, got nothing.",
                      i + 1, func_name, param_type
                    ));
                  } else {
                    // Use default type for checking
                    provided_arg_types.push(*default_type.as_ref().unwrap().clone());
                    continue;
                  }
                }

                // Check type compatibility of argument and parameter
                if !self.check_type_compatibility(&param_type, &provided_arg_types[i]) {
                  return Err(format!(
                    "Type mismatch for argument {} in call to '{}'. Expected {}, got {}.",
                    i + 1, func_name, param_type, provided_arg_types[i]
                  ));
                }
              },
              _ => {},
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
        // To add to inner scope - name, type, is_const
        let mut param_names: Vec<(String, JType, bool)> = Vec::new();
        let mut is_variadic = false;

        if let Some(param_items) = params {

          for param in param_items.iter_mut() {

            if let Expr::FunctionParam(Some(Type(ptype)), is_const, ident_literal, default_value, is_spread) = &mut param.expr {
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

                let has_default = default_value.is_some();
                let mut default_type: Option<Box<JType>> = None;

                // Push parameter type to the expected list
                if !*is_spread {
                  param_names.push((pname.clone(), param_type.clone(), *is_const));
                // Variadic
                } else {
                  is_variadic = true;
                  param_names.push((pname.clone(), JType::Array(Box::new(param_type.clone())), *is_const));
                }

                // Check default value
                if has_default {
                  default_type = Some(Box::new(self.check_expression(default_value.as_mut().unwrap())?));
                  if !self.check_type_compatibility(&param_names.last().unwrap().1, &default_type.as_ref().unwrap()) {
                    return Err(format!(
                      "Default value type mismatch for parameter '{}'. Expected {}, got {}.",
                      pname, param_names.last().unwrap().1, default_type.unwrap()
                    ));
                  }
                }

                expected_param_types.push(JType::FunctionParam(Box::new(param_type), default_type, *is_spread));
              } else {
                return Err("Invalid function parameter: expected identifier on left-hand side (TODO: Type check function parameters).".to_string());
              }
            } else {
              return Err("Invalid function parameter format.".to_string());
            }
          }
        }

        if is_variadic {
          // Check that the last parameter is the spread parameter
          if let Some(last_param) = expected_param_types.last() {
            if !matches!(last_param, JType::FunctionParam(_, _, true)) {
              return Err("Spread parameter must be the last parameter in the function definition.".to_string());
            }
          }
          // Check for multiple spread parameters
          if expected_param_types.iter().filter(|p| matches!(p, JType::FunctionParam(_, _, true))).count() > 1 {
            return Err("Multiple spread parameters are not allowed.".to_string());
          }
        }

        let expected_return_type = match return_type {
          Some(Literals::Identifier(Name(rt_name))) => self.string_to_jtype(rt_name)?,
          None => JType::Void, // Default to void if no return type specified
          _ => return Err("Invalid return type.".to_string()),
        };

        // Add function signature to scope before checking body scope
        let func_type = JType::Function(expected_param_types.clone(), Box::new(expected_return_type.clone()));
        self.add_variable(name, func_type, false)?;

        self.enter_function(Some(expected_return_type.clone()));
        // Add parameters to the body scope
        for (pname, ptype, is_const) in param_names {
          self.add_variable(&pname, ptype, is_const)?;
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
        self.add_variable(&var_name, JType::Integer, false)?; // Add loop variable to scope
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

      Expr::TypeDef(name, literals) => {
        if self.lookup_variable(&name.0).is_some() {
          return Err(format!("Type '{}' already declared.", &name.0));
        }

        match literals.as_mut() {
          // Case: Alias to custom type
          // type MyType = MyOtherType;
          Literals::Identifier(Name(type_name)) => {
            let typ = self.string_to_jtype(&type_name)?;
            // Store the name pointing to the resolved type
            self.add_variable(&name.0, typ, false)?;
          },
          // Case: Alias to primitive type
          // type MyType = int;
          Literals::Boolean(_) | Literals::Integer(_) | Literals::FloatingPoint(_) | Literals::String(_) | Literals::Null => {
            self.add_variable(&name.0, JType::TypeName(name.0.clone()), false)?;
          },
          // Case: Struct definition
          // type MyType = { a: int, b: string };
          Literals::Object(inner) => {
            let props = self.check_object(inner, true)?;
            self.add_variable(&name.0, JType::Object(props.clone()), false)?;
          },
          _ => {
            return Err(format!("Invalid type definition for '{}'.", name.0));
          }
        }

        Ok(JType::Null)
      }

      Expr::Enum(name, members) => {
        if self.lookup_variable(&name.0).is_some() {
          return Err(format!("Enum '{}' already declared.", &name.0));
        }

        // Check members
        let mut enum_members: Vec<String> = Vec::new();
        for member in members.iter_mut() {
          match member {
            Literals::Identifier(Name(member_name)) => {
              if enum_members.contains(&member_name) {
                return Err(format!("Enum member '{}' already declared.", member_name));
              }
              enum_members.push(member_name.clone());
            }
            _ => return Err(format!("Invalid enum member in '{}'. Members must be identifiers.", name.0)),
          }
        }

        let enum_type = JType::Enum(enum_members);
        self.add_variable(&name.0, enum_type.clone(), false)?;
        Ok(enum_type)
      }

      Expr::Index(initial_lhs_expr, initial_rhs_expr) => {
        let mut cur_lhs_type = self.check_expression(initial_lhs_expr)?;

        // Current link in chain being processed
        // Starts by pointing to initial rhs of the Index
        let mut cur_chain_node: &mut Expression = initial_rhs_expr;

        // Loop to resolve right-associative chain like A.B.C.D
        // Assuming we have, for example, AST: Index(A, Index(B, Index(C, D)))
        // 1: cur_lhs_type = Type(A), cur_chain_node = Index(B, Index(C,D))
        // 2: cur_lhs_type = Type(A.B), cur_chain_node = Index(C,D)
        // 3: cur_lhs_type = Type(A.B.C), cur_chain_node = D
        loop {
          match &mut cur_chain_node.expr {
            Expr::Literal(Literals::Identifier(Name(member_name))) => {
              // Final identifier in the chain (e.g., the 'D' in A.B.C.D)
              // cur_lhs_type is the type of A.B.C
              match cur_lhs_type.clone() {
                JType::Object(fields) => {
                  if let Some(field_type) = fields.get(member_name) {
                    cur_lhs_type = field_type.clone(); // Update type to Type(A.B.C.D)
                    cur_chain_node.inferred_type = Some(cur_lhs_type.clone());
                  } else {
                    return Err(format!("Property '{}' not found on type '{}'.", member_name, cur_lhs_type));
                  }
                },
                JType::Enum(enum_members_list) => {
                  // Accessing MyEnum.MEMBER
                  if enum_members_list.contains(member_name) {
                    // Type of 'MyEnum.MEMBER' is 'MyEnum' itself
                    // cur_lhs_type is already the JType::Enum
                    initial_rhs_expr.inferred_type = Some(cur_lhs_type.clone());
                  } else {
                    return Err(format!("Enum member '{}' not found in enum '{}'.", member_name, cur_lhs_type));
                  }
                },
                _ => return Err(format!("Type '{}' does not support '.' access for member '{}'.", cur_lhs_type, member_name)),
              }

              break;
            }

            // Case: Further chain link rhs (Index(B, Index(C,D)))
            Expr::Index(next_link_ident, next_rhs) => {
              let member_name_to_access = match &next_link_ident.expr {
                Expr::Literal(Literals::Identifier(Name(name_str))) => name_str,
                _ => return Err(format!("Invalid AST structure in Index chain: expected identifier as link, got {:?}.", next_rhs.expr)),
              };

              match cur_lhs_type.clone() {
                JType::Object(fields) => {
                  if let Some(field_type) = fields.get(member_name_to_access) {
                    cur_lhs_type = field_type.clone(); // Update type to Type(A.B)
                    next_rhs.inferred_type = Some(cur_lhs_type.clone());
                    cur_chain_node = next_rhs; // Continue with Index(C,D)
                  } else {
                    return Err(format!("Property '{}' not found on type '{}'.", member_name_to_access, cur_lhs_type));
                  }
                },
                _ => return Err(format!("Type '{}' does not support '.' accessor for member '{}'.", cur_lhs_type, member_name_to_access)),
              }
            },

            // Case: Current part of chain is method call: cur_lhs_type.method_name(...)
            Expr::Call(Name(method_name), args_expr_vec) => {
              let method_type = match cur_lhs_type.clone() {
                JType::Object(object_fields) => object_fields.get(method_name).cloned(),
                _ => None,
              };

              if let Some(JType::Function(_, return_type)) = method_type {
                let mut provided_types = Vec::with_capacity(args_expr_vec.len());
                for arg_node in args_expr_vec.iter_mut() {
                  provided_types.push(self.check_expression(arg_node)?);
                }

                // TODO: DRY call signature checks to separate function when classes are ready for this

                cur_lhs_type = *return_type;
              } else if method_type.is_some() {
                return Err(format!("Member '{}' on type '{}' is not a callable method.", method_name, cur_lhs_type));
              } else {
                return Err(format!("Method '{}' not found on type '{}'.", method_name, cur_lhs_type));
              }

              initial_rhs_expr.inferred_type = Some(cur_lhs_type.clone());
              break;
            },
            _ => return Err("Invalid expression in Index chain. Expected Identifier, further Index, or Call.".to_string()),
          }
        }

        Ok(cur_lhs_type)
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

      Expr::Public(expr) => {
        // Handle internal expression of the public modifier
        Ok(self.check_expression(expr)?)
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

  fn check_object(&mut self, props: &mut Vec<Literals>, is_type_def: bool) -> Result<HashMap<String, JType>, String> {
    let mut obj_properties = HashMap::new();
    for literal in props.iter_mut() {
      match literal.clone() {
        Literals::ObjectProperty(name, mut val) => {
          // Check if property already exists
          if obj_properties.contains_key(&name.0) {
            return Err(format!("Duplicate property '{}' in object.", name.0));
          }

          // If checking for a struct definition handle types literally without checking expressions
          if is_type_def {
            match &mut val.expr {
              Expr::Literal(Literals::Identifier(Name(type_name))) => {
                let typ = self.string_to_jtype(type_name)?;
                obj_properties.insert(name.clone().0, typ.clone());
              },
              _ => {
                return Err(format!("Invalid property type for '{}'. Expected identifier, got {}.", name.0, val.inferred_type.unwrap_or(JType::Unknown)));
              }
            }
          } else {
            obj_properties.insert(name.clone().0, self.check_expression(&mut val)?);
          }
        },
        _ => {
          return Err(format!("Invalid item in object literal or type definition. Expected 'key: Type' or 'key: value'. Got: {:?}", literal));
        }
      }
    }

    Ok(obj_properties.clone())
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
