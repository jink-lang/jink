#![allow(unused)]

use core::{panic, str};
use std::collections::HashSet;
use indexmap::IndexMap;

use inkwell::llvm_sys::orc2::LLVMOrcMaterializationUnitMaterializeFunction;
use jink::{Expression, Name, Type};
use jink::Expr;
use jink::Literals;
use jink::Operator;
use jink::Error;

#[derive(Clone, Debug)]
pub struct Value {
  typ: Type,
  value: TypeKind,
}

#[derive(Clone, Debug)]
enum TypeKind {
  Bool(bool),
  Int32(i32),
  Int64(i64),
  Float(f64),
  String(String),
  Null
}

pub struct Simulator {
  pub code: String,
  verbose: bool,
  symbol_table_stack: Vec<IndexMap<String, (Value, bool)>>
}

impl Simulator {
  pub fn new() -> Self {
    return Simulator {
      code: String::new(),
      verbose: false,
      symbol_table_stack: Vec::new()
    };
  }

  pub fn enter_scope(&mut self) {
    self.symbol_table_stack.push(IndexMap::new());
  }

  pub fn exit_scope(&mut self) {
    self.symbol_table_stack.pop();
  }

  pub fn set_symbol(&mut self, name: String, value: Value, constant: bool) {
    if let Some(scope) = self.symbol_table_stack.last_mut() {
      scope.insert(name, (value, constant));
    } else {
      panic!("Unexpected simulation error, set symbol without a scope");
    }
  }

  pub fn get_symbol(&self, name: &str) -> Option<(Value, bool)> {
    for scope in self.symbol_table_stack.iter().rev() {
      if let Some(value) = scope.get(name) {
        return Some(value.clone());
      }
    }
    return None;
  }

  fn add_builtin_functions(&self) {

  }

  pub fn simulate(&mut self, code: String, ast: Vec<Expression>, verbose: bool) -> Result<(), Error> {
    self.code = code;
    self.verbose = verbose;

    self.enter_scope();
    self.add_builtin_functions();
    self.evaluate(ast)?;

    // Print the symbol table stack
    if self.verbose {
      println!("{:?}", self.symbol_table_stack);
    }

    return Ok(());
  }

  fn evaluate(&mut self, ast: Vec<Expression>) -> Result<(), Error> {
    let mut out = vec![];
    for expr in ast {
      let evaluated = self.evaluate_top(expr)?;
      out.push(evaluated);
    }

    return Ok(());
  }

  fn evaluate_top(&mut self, expr: Expression) -> Result<Value, Error> {
    match expr.clone().expr {
      Expr::Literal(literal) => {
        match literal {
          Literals::Identifier(name, _) => {
            if let Some(value) = self.get_symbol(&name.0) {
              return Ok(value.0);
            } else {
              // println!("Symbol not found: {:?}", name);
              return Ok(Value {
                typ: Type("null".to_string()),
                value: TypeKind::Null
              });
            }
          },
          Literals::Integer(value) => {
            return Ok(Value {
              typ: Type("i64".to_string()),
              value: TypeKind::Int64(value)
            });
          },
          Literals::FloatingPoint(value) => {
            return Ok(Value {
              typ: Type("f64".to_string()),
              value: TypeKind::Float(value)
            });
          },
          Literals::EOF => {
            return Ok(Value {
              typ: Type("null".to_string()),
              value: TypeKind::Null
            });
          },
          _ => {
            println!("Literal: {:?}", literal);
            todo!()
          }
        }
      },
      Expr::Assignment(typ, ident_or_idx, val) => {
        // Get value
        let value: Value;
        if val.is_some() {
          let res = self.evaluate_top(*val.unwrap());
          if let Err(_) = res {
            value = Value { typ: typ.as_ref().unwrap().to_owned(), value: TypeKind::Null };
          } else {
            value = res.unwrap();
          }
        } else {
          value = Value { typ: typ.as_ref().unwrap().to_owned(), value: TypeKind::Null };
        }

        // Get name from identifier
        if let Expr::Literal(Literals::Identifier(name, _)) = ident_or_idx.expr {

          // Type exists, is new assignment
          if typ.is_some() {

            // New constant
            if typ.unwrap().0 == "const" {
              self.set_symbol(name.0, value.clone(), true);

            // Not constant but typed
            } else {

              // If symbol exists already
              let sym = self.get_symbol(&name.0);
              if sym.is_some() {
                panic!("Cannot reassign variable: {:?}", name.0);
              }

              self.set_symbol(name.0, value.clone(), false);
            }

          // Reassignment
          } else {
            // If symbol exists already make sure it's not a constant
            let sym = self.get_symbol(name.0.as_str());
            if sym.is_some() && sym.unwrap().1 {
              panic!("Cannot reassign constant symbol: {:?}", name.0);
            }

            self.set_symbol(name.0, value.clone(), false);
          }

          return Ok(value);
        } else {
          panic!("Unexpected assignment error");
        }
      },
      Expr::BinaryOperator(op, lhs, rhs) => {
        let lhs_value = self.evaluate_top(*lhs)?;
        let rhs_value = self.evaluate_top(*rhs)?;

        match op.0.as_str() {
          "+" => {
            match (lhs_value.value, rhs_value.value) {
              (TypeKind::Int64(lhs), TypeKind::Int64(rhs)) => {
                return Ok(Value {
                  typ: Type("i64".to_string()),
                  value: TypeKind::Int64(lhs + rhs)
                });
              },
              (TypeKind::Float(lhs), TypeKind::Float(rhs)) => {
                return Ok(Value {
                  typ: Type("f64".to_string()),
                  value: TypeKind::Float(lhs + rhs)
                });
              },
              _ => {
                panic!("Invalid value types for binary operator");
              }
            }
          },
          "-" => {
            match (lhs_value.value, rhs_value.value) {
              (TypeKind::Int64(lhs), TypeKind::Int64(rhs)) => {
                return Ok(Value {
                  typ: Type("i64".to_string()),
                  value: TypeKind::Int64(lhs - rhs)
                });
              },
              (TypeKind::Float(lhs), TypeKind::Float(rhs)) => {
                return Ok(Value {
                  typ: Type("f64".to_string()),
                  value: TypeKind::Float(lhs - rhs)
                });
              },
              _ => {
                panic!("Invalid value types for binary operator");
              }
            }
          },
          "*" => {
            match (lhs_value.value, rhs_value.value) {
              (TypeKind::Int64(lhs), TypeKind::Int64(rhs)) => {
                return Ok(Value {
                  typ: Type("i64".to_string()),
                  value: TypeKind::Int64(lhs * rhs)
                });
              },
              (TypeKind::Float(lhs), TypeKind::Float(rhs)) => {
                return Ok(Value {
                  typ: Type("f64".to_string()),
                  value: TypeKind::Float(lhs * rhs)
                });
              },
              _ => {
                panic!("Invalid value types for binary operator");
              }
            }
          },
          "/" => {
            match (lhs_value.value, rhs_value.value) {
              (TypeKind::Int64(lhs), TypeKind::Int64(rhs)) => {
                return Ok(Value {
                  typ: Type("i64".to_string()),
                  value: TypeKind::Int64(lhs / rhs)
                });
              },
              (TypeKind::Float(lhs), TypeKind::Float(rhs)) => {
                return Ok(Value {
                  typ: Type("f64".to_string()),
                  value: TypeKind::Float(lhs / rhs)
                });
              },
              _ => {
                panic!("Invalid value types for binary operator");
              }
            }
          },
          _ => {
            panic!("Unexpected operator for binary operator");
          }
        }
      },
      _ => todo!()
    }

    return Ok(Value { typ: Type("null".to_string()), value: TypeKind::Null });
  }
}