use core::{panic, str};
use indexmap::IndexMap;
use std::collections::HashSet;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::AddressSpace;

use jink::Error;
use jink::Expr;
use jink::Literals;
use jink::Operator;
use jink::{Expression, Name, Type};

pub struct Scope<'ctx> {
  symbols: IndexMap<String, (Option<PointerValue<'ctx>>, BasicValueEnum<'ctx>)>,
  is_function: bool,
}

pub struct CodeGen<'ctx> {
  pub code: String,
  context: &'ctx Context,
  pub module: Module<'ctx>,
  builder: Builder<'ctx>,
  symbol_table_stack: Vec<Scope<'ctx>>,
  struct_table: IndexMap<String, (StructType<'ctx>, Vec<(String, BasicTypeEnum<'ctx>)>)>,
  type_tags: IndexMap<String, u64>,
  execution_engine: ExecutionEngine<'ctx>
}

impl<'ctx> CodeGen<'ctx> {
  pub fn new(context: &'ctx Context) -> Self {
    let module = context.create_module("main");

    let execution_engine_result = module.create_execution_engine();
    let execution_engine = match execution_engine_result {
      Ok(engine) => engine,
      Err(err) => panic!("Failed to create execution engine: {}", err),
    };

    return CodeGen {
      code: String::new(),
      context: &context,
      module,
      builder: context.create_builder(),
      symbol_table_stack: Vec::new(),
      struct_table: IndexMap::new(),
      type_tags: Self::map_types(),
      execution_engine
    };
  }

  // Map tags for builtin types
  fn map_types() -> IndexMap<String, u64> {
    let mut types = IndexMap::new();
    types.insert("null".to_string(), 0b01); // null ptr
    types.insert("int".to_string(), 0b10); // i64
    types.insert("i32".to_string(), 0b11);
    types.insert("float".to_string(), 0b100); // double
    types.insert("bool".to_string(), 0b101);
    types.insert("array".to_string(), 0b110);
    types.insert("object".to_string(), 0b111);
    types.insert("mask".to_string(), 0b1001); // mask for type checking
    return types;
  }

  fn get_tag_by_type(&self, typ: BasicTypeEnum) -> u64 {
    match typ {
      BasicTypeEnum::IntType(_) => *self.type_tags.get("int").unwrap(),
      BasicTypeEnum::FloatType(_) => *self.type_tags.get("float").unwrap(),
      BasicTypeEnum::ArrayType(_) => *self.type_tags.get("array").unwrap(),
      BasicTypeEnum::PointerType(_) => *self.type_tags.get("object").unwrap(),
      _ => panic!("Invalid type for tag"),
    }
  }

  // fn add_type(&mut self, name: String) {
  //   // Pop the mask tag and reuse its bit for the new type
  //   let mask = self.type_tags.pop().unwrap();
  //   self.type_tags.insert(name.clone(), mask.1);
  //   // Push a new mask tag back
  //   self.type_tags.insert(name, mask.1 + 1);
  // }

  pub fn enter_scope(&mut self, is_function: bool) {
    self.symbol_table_stack.push(Scope { symbols: IndexMap::new(), is_function });
  }

  pub fn exit_scope(&mut self) {
    self.symbol_table_stack.pop();
  }

  pub fn set_symbol(&mut self, name: String, inst: Option<PointerValue<'ctx>>, value: BasicValueEnum<'ctx>) {
    if let Some(scope) = self.symbol_table_stack.last_mut() {
      scope.symbols.insert(name, (inst, value));
    } else {
      panic!("Unexpected compilation error, set symbol without a scope");
    }
  }

  /// Get symbol from symbol table stack
  /// 
  /// Returns Result->Option of symbol name, instruction pointer, value and whether we left a function scope to obtain it 
  /// If the instruction pointer is None the symbol is a constant value
  pub fn get_symbol(&self, name: &str) -> Result<Option<(String, Option<PointerValue<'ctx>>, BasicValueEnum<'ctx>, bool)>, Error> {
    let mut exited_function = false;
    for scope in self.symbol_table_stack.iter().rev() {
      if let Some((inst, value)) = scope.symbols.get(name) {
        return Ok(Some((name.to_string(), inst.clone(), value.clone(), exited_function)));
      }

      if scope.is_function {
        exited_function = true;
      }
    }
    return Ok(None);
  }

  fn get_all_cur_symbols(&self) -> IndexMap<String, (Option<PointerValue<'ctx>>, BasicValueEnum<'ctx>, bool)> {
    let mut symbols = IndexMap::new();
    for (name, (inst, value)) in self.symbol_table_stack.last().unwrap().symbols.iter() {
      symbols.insert(name.clone(), (inst.clone(), value.clone(), false));
    }
    return symbols;
  }

  fn create_main_function(&self) -> FunctionValue<'ctx> {
    let void_type = self.context.void_type().fn_type(&[], false);
    let main_fn = self.module.add_function("main", void_type, None);
    let entry_block = self.context.append_basic_block(main_fn, "entry");
    self.builder.position_at_end(entry_block);
    return main_fn;
  }

  fn add_builtin_functions(&self) {
    let ptr_type = self.context.ptr_type(AddressSpace::default());

    let printf_type = self.context.i32_type().fn_type(&[ptr_type.into()], true);
    self.module.add_function("printf", printf_type, Some(Linkage::External));

    let puts_type = self.context.i32_type().fn_type(&[ptr_type.into()], true);
    self.module.add_function("puts", puts_type, Some(Linkage::External));

    let scanf_type = self.context.i32_type().fn_type(&[ptr_type.into(), ptr_type.into()], true);
    self.module.add_function("scanf", scanf_type, Some(Linkage::External));

    let malloc_type = self.context.i8_type().fn_type(&[self.context.i64_type().into()], false);
    self.module.add_function("malloc", malloc_type, Some(Linkage::External));

    let free_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
    self.module.add_function("free", free_type, Some(Linkage::External));

    let strlen_type = self.context.i64_type().fn_type(&[ptr_type.into()], false);
    self.module.add_function("strlen", strlen_type, Some(Linkage::External));

    // Create ptr() function to convert values to pointers
    let ptr_func_type = self.context.ptr_type(AddressSpace::default()).fn_type(&[ptr_type.into()], false);
    self.module.add_function("ptr", ptr_func_type, None);
  }

  pub fn build(&mut self, code: String, ast: Vec<Expression>, verbose: bool, do_execute: bool) -> Result<(), Error> {
    self.code = code;

    // Prepare the program entry point
    let main_fn = self.create_main_function();

    // Enter the main application scope
    self.enter_scope(false);

    self.add_builtin_functions();

    // Process the AST
    let merge_block = self.process_ast(ast, main_fn)?;

    // Position at the end of the last main block
    self.builder.position_at_end(merge_block);

    // Exit the main block
    self.builder.build_return(None).unwrap();

    // Print LLVM IR
    if verbose {
      println!("{}", self.module.print_to_string().to_string());
    }

    // Print the symbol table
    // if verbose {
    //   for scope in self.symbol_table_stack.iter() {
    //     for (key, (inst, value)) in scope.symbols.iter() {
    //       println!("{}: {:?}", key, value.print_to_string());
    //     }
    //   }
    // }

    if let Err(e) = self.module.verify() {
      // println!("Code generation error: {}", e.to_string());
      return Err(Error::new(
        Error::CompilerError,
        None,
        &"",
        Some(0),
        Some(0),
        e.to_string()
      ));
    }

    if do_execute {
      unsafe { self.execution_engine.run_function_as_main(main_fn, &[]); }
    }

    return Ok(());
  }

  fn process_ast(&mut self, ast: Vec<Expression>, main_fn: FunctionValue<'ctx>) -> Result<BasicBlock<'ctx>, Error> {
    // Get entry block
    let mut block = main_fn.get_first_basic_block().unwrap();

    for expr in ast {
      match expr.clone().expr {
        Expr::Literal(_literal) => {
          // println!("Literal: {:?}\n", literal);
        },
        Expr::BinaryOperator(_, _, _) => {
          let res = self.visit(&expr.clone(), block);
          println!("{:?}", res);
        },
        Expr::UnaryOperator(_, _) => {
          self.visit(&expr.clone(), block)?;
        },
        Expr::Assignment(_, _, _) => {
          self.build_assignment(expr, block, false)?;
        },
        Expr::Array(values) => {
          println!("Array: {:?}\n", values);
        },
        Expr::TypeDef(name, value) => {
          self.build_struct(Some(name), value)?;
        },
        Expr::Conditional(typ, expr, body, else_body) => {
          block = self.build_if(typ, expr, body, else_body, main_fn, block)?;
        },
        Expr::ForLoop(value, expr, opt) => {
          println!("ForLoop: {:?} {:?} {:?}\n", value, expr, opt)
        }
        Expr::Call(_, _) => {
          self.visit(&expr, block)?;
        },
        Expr::Function(name, ret_typ, params, body) => {
          self.build_function(name, ret_typ, params, body, block)?;
        },
        Expr::FunctionParam(typ, is_const, ident, default, _) => {
          println!("FunctionParam: {:?} {:?} {:?} {:?}\n", typ, is_const, ident, default);
        },
        Expr::Return(value) => {
          println!("Return: {:?}\n", value);
        },
        Expr::Class(name, parents, body) => {
          println!("Class: {:?} {:?} {:?}\n", name, parents, body);
        },
        Expr::Module(name, body) => {
          println!("Module: {:?} {:?}\n", name, body);
        },
        Expr::ObjectIndex(parent, child) => {
          println!("ObjectIndex: {:?} {:?}\n", parent, child);
        },
      }
    }

    return Ok(block);
  }

  fn build_struct(&mut self, name: Option<Literals>, value: Box<Literals>) -> Result<StructType<'ctx>, Error> {
    // Extract struct name if it's an identifier
    let mut struct_lit_name: Option<String> = None;
    if name.is_some() {
      struct_lit_name = match name.unwrap() {
        Literals::Identifier(name, _) => Some(name.0),
        _ => None,
      };
    }

    // If value is an identifier, handle it as a type alias
    // if let Literals::Identifier(n, _) = value.as_ref().to_owned() {
    //     if self.type_tags.contains_key(&n.0) {
    //         // TODO: Save alias
    //         // self.add_type(name.unwrap());
    //     } else {
    //         let sym = self.get_symbol(&n.0)?;
    //         // TODO: Save alias
    //     }
    //     return Ok(());
    // }

    // Build struct if value is an object
    if let Literals::Object(obj) = value.as_ref().to_owned() {
      let mut fields: Vec<(String, BasicTypeEnum<'ctx>)> = vec![];
      for literal in obj.iter() {
        if let Literals::ObjectProperty(n, val) = literal.to_owned() {

          // Ensure name doesn't already exist in fields
          if fields.iter().any(|(name, _)| name == &n.as_ref().unwrap().0) {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &"",
              Some(0),
              Some(0),
              format!("Field {} already defined", n.unwrap().0)
            ));
          }

          // Build the field
          let field: (String, BasicTypeEnum<'ctx>) = (n.unwrap().0, self.build_struct_field(val.expr)?);
          fields.push(field);

        // Field was not an object property, parsing error
        } else {
          return Err(Error::new(
            Error::CompilerError,
            None,
            "",
            Some(0),
            Some(0),
            "Invalid struct field".to_string()
          ));
        }
      }

      // Create the struct type
      let mut field_types: Vec<BasicTypeEnum> = vec![];
      for (_, t) in fields.clone() {

        // Change child struct types to pointers for mapping
        if t.is_struct_type() {
          field_types.push(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum());
        } else {
          field_types.push(t);
        }
      }

      let struct_type = self.context.struct_type(&field_types, false);

      // Save a reference to the struct
      if let Some(n) = struct_lit_name {
        // Special saving method for structs to keep track of their fields
        if self.struct_table.contains_key(&n) {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &"",
            Some(0),
            Some(0),
            format!("Name {} already defined", n)
          ));
        } else {
          self.struct_table.insert(n, (struct_type, fields));
        }
      }

      return Ok(struct_type);
    } else {
      Err(Error::new(
        Error::CompilerError,
        None,
        &"",
        Some(0),
        Some(0),
        "Invalid struct definition".to_string()
      ))
    }
  }

  fn build_struct_field(&mut self, val: Expr) -> Result<BasicTypeEnum<'ctx>, Error> {
    match val {
      Expr::TypeDef(_, v) => {
        let s = self.build_struct(None, v)?;
        return Ok(s.as_basic_type_enum());
      },
      Expr::Literal(Literals::Identifier(n, _)) => {
        match n.0.as_str() {
          "int" => return Ok(self.context.i64_type().as_basic_type_enum()),
          "float" => return Ok(self.context.f64_type().as_basic_type_enum()),
          "bool" => return Ok(self.context.bool_type().as_basic_type_enum()),
          _ => {
            // Check for existing type alias
            let struct_type = self.struct_table.get(&n.0);
            if struct_type.is_none() {
              return Err(Error::new(
                Error::CompilerError,
                None,
                &"",
                Some(0),
                Some(0),
                format!("Invalid struct field type {}", n.0)
              ));
            } else {
              return Ok(struct_type.unwrap().0.as_basic_type_enum());
            }
          }
        }
      },
      _ => Err(Error::new(
        Error::CompilerError,
        None,
        &"",
        Some(0),
        Some(0),
        "Invalid struct field type".to_string()
      )),
    }
  }

  // Name of struct type; fields
  fn build_initialize_struct(&mut self, struct_name: String, var_name: String, fields: Vec<Literals>, block: BasicBlock<'ctx>) -> Result<PointerValue<'ctx>, Error> {
    let (struct_type, field_types) = self.struct_table.get(&struct_name)
      .ok_or_else(|| Error::new(
        Error::CompilerError,
        None,
        &"",
        Some(0),
        Some(0),
        format!("Unknown struct: {}", struct_name)
      ))?
      .to_owned();

    // Ensure field lengths match
    if fields.len() != field_types.len() {
      return Err(Error::new(
        Error::CompilerError,
        None,
        &"",
        Some(0),
        Some(0),
        format!("Invalid number of fields for struct {}", var_name)
      ));
    }

    // Allocate space for the struct
    let struct_ptr = self.builder.build_alloca(struct_type, &var_name).unwrap();

    for (i, field) in fields.iter().enumerate() {
      if let Literals::ObjectProperty(_n, value) = field {

        // Get field value
        let field_val = self.visit(&value, block)?;
        let mut field_val_type = field_val.get_type();

        // Ensure type consistency
        if field_val_type.is_pointer_type() {
          // Check struct table for ptr name
          let struct_type = self.struct_table.get(&struct_name);
          if struct_type.is_some() {
            field_val_type = struct_type.unwrap().0.as_basic_type_enum();

          // Check symbol table
          } else {
            let symbol = self.get_symbol(field_val.get_name().to_str().unwrap())?;
            if symbol.is_none() {
              return Err(Error::new(
                Error::NameError,
                None,
                &"",
                Some(0),
                Some(0),
                format!("Invalid type for field '{}' on struct '{}'", field_val.get_name().to_str().unwrap(), var_name)
              ));
            }
            let (_, _, val, _) = symbol.unwrap();
            field_val_type = val.get_type();
          }
        }

        if field_val_type.is_struct_type() {
          // We know by this point that this struct type exists
          // TODO: Validate that the struct we expect is the struct we get

        } else {
          if field_val_type.as_basic_type_enum() != field_types[i].1.as_basic_type_enum() {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &"",
              Some(0),
              Some(0),
              format!("Invalid type for field '{}' on struct '{}'", field_types[i].0.clone(), var_name)
            ));
          }
        }

        let field_ptr = self.builder.build_struct_gep(struct_type, struct_ptr, i as u32, &format!("{}-{}", var_name, field_types[i].0)).unwrap();
        self.builder.build_store(field_ptr, field_val).unwrap();
      }
    }

    return Ok(struct_ptr);
  }

  // Top level builder for conditional expressions
  fn build_if(&mut self, _typ: Type, expr: Option<Box<Expression>>, body: Option<Box<Vec<Expression>>>, else_body: Option<Box<Vec<Expression>>>, function: FunctionValue<'ctx>, block: BasicBlock<'ctx>) -> Result<BasicBlock<'ctx>, Error> {
    let mut cond = self.visit(&expr.unwrap(), block)?;

    // If the condition is a number, condition on the basis of it not being 0
    if let BasicValueEnum::IntValue(i) = cond {
      cond = self.builder.build_int_compare(inkwell::IntPredicate::NE, i, i.get_type().const_int(0, false), "ifcond").unwrap().as_basic_value_enum();
    } else if let BasicValueEnum::FloatValue(f) = cond {
      cond = self.builder.build_float_compare(inkwell::FloatPredicate::ONE, f, f.get_type().const_float(0.0), "ifcond").unwrap().as_basic_value_enum();

    // If the condition is a pointer, condition on the basis of it not being null
    } else if let BasicValueEnum::PointerValue(p) = cond {
      if p.is_null() {
        cond = self.context.bool_type().const_int(0, false).as_basic_value_enum();
      } else {
        cond = self.context.bool_type().const_int(1, false).as_basic_value_enum();
      }
    }

    // Set up blocks
    let then_block = self.context.append_basic_block(function, "if_then");
    let else_block = self.context.append_basic_block(function, "else");
    let merge_block = self.context.append_basic_block(function, "merge");

    self.builder.build_conditional_branch(cond.into_int_value(), then_block, else_block).unwrap();

    // Build the conditonal block - if this then do this
    self.builder.position_at_end(then_block);
    // Push a new scope for conditional block
    self.enter_scope(false);
    for exp in body.unwrap().iter() {
      if let Expr::Conditional(typ, expr, body, ebody) = exp.expr.clone() {
        self.build_if(typ, expr, body, ebody, function, then_block)?;

      } else if let Expr::Return(ret) = exp.expr.clone() {
        let val = self.visit(&ret, then_block)?;
        if val != self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum() {
          self.builder.build_return(Some(&val)).unwrap();
        } else {
          self.builder.build_return(None).unwrap();
        }

      } else if let Expr::Assignment(_, _, _) = exp.expr.clone() {
        self.build_assignment(exp.clone(), then_block, false)?;

      } else {
        self.visit(&exp, then_block)?;
      }
    }
    // Save the values of the then block
    let then_vals = self.get_all_cur_symbols();
    // Pop the scope
    self.exit_scope();
    self.builder.build_unconditional_branch(merge_block).unwrap();

    // Build the "else" block
    self.builder.position_at_end(else_block);
    self.enter_scope(false);
    self.build_else_body(else_block, else_body)?;
    let else_vals = self.get_all_cur_symbols();
    self.exit_scope();
    self.builder.build_unconditional_branch(merge_block).unwrap();

    // Position the builder at the merge block
    self.builder.position_at_end(merge_block);

    // Collect all keys from both then_vals and else_vals
    let all_keys: HashSet<&String> = then_vals.keys().chain(else_vals.keys()).collect();

    for name in all_keys {

    // Get values from then_vals and else_vals or use the original value if not present
    let then_val = if let Some(val) = then_vals.get(name) {
      val.clone()
      } else {
        self.get_symbol(name)?
          .map(|(_, pval, val, exited_func)| (pval, val, exited_func))
          .expect("Value should be present in then_vals or symbol table")
      };

      let else_val = if let Some(val) = else_vals.get(name) {
        val.clone()
      } else {
        self.get_symbol(name)?
          .map(|(_, pval, val, exited_func)| (pval, val, exited_func))
          .expect("Value should be present in else_vals or symbol table")
      };

      // Determine the types of the values
      let then_type = then_val.1;
      let else_type = else_val.1;

      // Determine the common type for the phi node
      let phi_type = if then_type.is_int_value() && else_type.is_int_value() {
        self.context.i64_type().as_basic_type_enum()
      } else if then_type.is_float_value() && else_type.is_float_value() {
        self.context.f64_type().as_basic_type_enum()
      } else {
        // Handle cases where only one branch modifies the value or they are of different types
        if then_type.is_int_value() && !else_type.is_int_value() {
          self.context.i64_type().as_basic_type_enum()
        } else if then_type.is_float_value() && !else_type.is_float_value() {
          self.context.f64_type().as_basic_type_enum()
        } else {
          // TODO: Implement logic to handle more complex type compatibility or type promotion
          panic!("Incompatible types for phi node");
        }
      };

      // Create the phi node with the determined type
      let phi = self.builder.build_phi(phi_type, name).unwrap();

      // Add incoming values from both branches
      phi.add_incoming(&[
        (&then_val.1, then_block),
        (&else_val.1, else_block),
      ]);

      // Check if the symbol exists in the symbol table
      if let Some(symbol) = self.get_symbol(name)? {
        // Update the existing variable with the new PHI value
        let (_, inst, _, exited_func) = symbol;
        if exited_func {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &"",
            Some(0),
            Some(0),
            format!("Cannot reinitialize variable {} in function scope", name)
          ));
        }
        self.builder.build_store(inst.unwrap(), phi.as_basic_value()).unwrap();
        self.set_symbol(name.to_string(), inst, phi.as_basic_value());
      } else {
        // Create a new variable and store the PHI value
        let var = self.builder.build_alloca(phi_type, &name).unwrap();
        self.builder.build_store(var, phi.as_basic_value()).unwrap();
        self.set_symbol(name.to_string(), Some(var), phi.as_basic_value());
      }
    }

    // for (name, then_val) in &then_vals {
    //   let else_val = else_vals.get(name).unwrap_or(then_val);
    //   let phi = self.builder.build_phi(self.context.i64_type(), name).unwrap();
    //   phi.add_incoming(&[
    //     (&then_val.1, then_block),
    //     (&else_val.1, else_block),
    //   ]);

    //   // Check if symbol exists
    //   let symbol = self.get_symbol(name);
    //   if symbol.is_none() {
    //     // Create new variable
    //     let var = self.builder.build_alloca(self.context.i64_type(), &name).unwrap();
    //     self.set_symbol(name.to_string(), var, phi.as_basic_value());
    //   } else {
    //     // Update existing variable
    //     let (name, inst, _) = symbol.unwrap();
    //     self.builder.build_store(inst, phi.as_basic_value()).unwrap();
    //     self.set_symbol(name.to_string(), inst, phi.as_basic_value());
    //   }
    // }

    // Return merge block
    return Ok(merge_block);
  }

  // Recursive function to build "else" conditional blocks
  fn build_else_body(&mut self, block: BasicBlock<'ctx>, else_body: Option<Box<Vec<Expression>>>) -> Result<(), Error> {
    if let Some(else_body) = else_body {
      for exp in else_body.iter() {
        if let Expr::Conditional(typ, expr, body, ebody) = exp.expr.clone() {
          if typ.0 == "else" {
            // This is the end, build the body
            self.build_else_body(block, body)?;
          } else {
            // This is a new branch, return to the top
            self.build_if(typ, expr, body, ebody, block.get_parent().unwrap(), block)?;
          }

        } else if let Expr::Assignment(_, _, _) = exp.expr.clone() {
          self.build_assignment(exp.clone(), block, false)?;

        } else {
          self.visit(&exp, block)?;
        }
      }
    }

    return Ok(());
  }

  fn build_assignment(&mut self, expr: Expression, block: BasicBlock<'ctx>, is_new_scope: bool) -> Result<(), Error> {
    // Position the builder at the end of the current block
    self.builder.position_at_end(block);

    // Extract assignment values
    let (ty, ident, value) = match expr.clone().expr {
      Expr::Assignment(ty, ident, value) => (ty, ident, value),
      _ => {
        return Err(Error::new(
          Error::ParserError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          "Invalid assignment".to_string()
        ));
      }
    };
    // Extract identifier name
    let name = match ident {
      Literals::Identifier(name, _) => name,
      _ => {
        return Err(Error::new(
          Error::ParserError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          "Invalid assignment".to_string()
        ));
      }
    }.0;

    let set: BasicValueEnum;
    if value.is_some() {
      if let Expr::Literal(e) = &value.as_ref().unwrap().expr {
        if let Literals::Object(obj) = e {
          let alloca = self.build_initialize_struct(ty.clone().unwrap().0, name.clone(), obj.to_vec(), block)?;
          set = alloca.as_basic_value_enum();
          println!("set: {:?}", set);
        } else {
          set = self.visit(&value.unwrap(), block)?;
        }
      } else {
        set = self.visit(&value.unwrap(), block)?;
      }
    // If no value is provided, set to null
    } else {
      set = self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum();
    }

    // TODO: Check for constants

    // Setting new variable
    if let Some(ty) = ty {

      // Check if variable already exists
      let symbol = self.get_symbol(&name)?;
      if symbol.is_some() && !is_new_scope {
        return Err(Error::new(
          Error::CompilerError,
          None,
          &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          format!("Cannot reinitialize variable {}", name)
        ));
      }

      match ty.0.as_str() {
        "let" => {
          match set {
            BasicValueEnum::IntValue(i) => {
              // Allocate space and store the value
              let ptr = self.builder.build_alloca(self.context.i64_type(), &name).unwrap();
              self.builder.build_store(ptr, i).unwrap();

              // Build tagged pointer
              let tagged = self.tag_ptr(ptr, *self.type_tags.get("int").unwrap());

              // Set the symbol
              self.set_symbol(name, Some(tagged), i.into());
            },
            BasicValueEnum::FloatValue(f) => {
              // Allocate space and store the value
              let ptr = self.builder.build_alloca(self.context.f64_type(), &name).unwrap();
              self.builder.build_store(ptr, f).unwrap();

              // Build tagged pointer
              let tagged = self.tag_ptr(ptr, *self.type_tags.get("float").unwrap());

              // Set the symbol
              self.set_symbol(name, Some(tagged), f.into());
            },
            BasicValueEnum::ArrayValue(a) => {
              let var = self.builder.build_alloca(self.context.i64_type(), &name).unwrap();
              self.builder.build_store(var, a).unwrap();
              self.set_symbol(name, Some(var), a.into());
            },
            BasicValueEnum::PointerValue(a) => {
              let var = self.builder.build_alloca(self.context.ptr_type(AddressSpace::default()), &name).unwrap();
              self.builder.build_store(var, a).unwrap();
              self.set_symbol(name, Some(var), a.into());
            },
            _ => panic!("Invalid type for let"),
          }
        },
        "const" => {
          match set {
            BasicValueEnum::IntValue(i) => {
              let var = self.builder.build_alloca(self.context.i64_type(), &name).unwrap();
              self.builder.build_store(var, i).unwrap();
              self.set_symbol(name, Some(var), var.as_basic_value_enum());
            },
            BasicValueEnum::FloatValue(f) => {
              let var = self.builder.build_alloca(self.context.f64_type(), &name).unwrap();
              self.builder.build_store(var, f).unwrap();
              self.set_symbol(name, Some(var), var.as_basic_value_enum());
            },
            _ => panic!("Invalid type for const"),
          }
        },
        // Custom type (we know exists from builder)
        _ => {
          self.set_symbol(name, None, set);
        },
      }
    } else {
      // Setting existing variable
      let var = self.get_symbol(&name)?;

      if var.is_none() {
        return Err(Error::new(
          Error::NameError,
          None,
          &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
          expr.first_pos,
          Some(name.len() as i32),
          format!("Variable {} not found", name)
        ));
      }
      let named = var.unwrap().1;
      if named.is_none() {
        // An internal error occurred
        panic!("Internal error occurred. Variable {} has no pointer. There may be an issue with a function definition.", name);
      }
      self.builder.build_store(named.unwrap(), set).unwrap();
      self.set_symbol(name, named, set);
    }

    return Ok(());
  }

  // TODO: Clean up & document
  fn tag_ptr(&self, ptr: PointerValue<'ctx>, tag: u64) -> PointerValue<'ctx> {
    let int_ptr = self.builder.build_ptr_to_int(ptr, self.context.i64_type(), "ptr_to_int").unwrap();
    let tag_value = self.context.i64_type().const_int(tag, false);
    let tagged_int = self.builder.build_or(int_ptr, tag_value, "tagged_ptr").unwrap();
    return self.builder.build_int_to_ptr(tagged_int, ptr.get_type(), "tagged_int_ptr").unwrap();
  }

  // TODO: Clean up & document
  fn extract_tag(&self, ptr: PointerValue<'ctx>) -> (PointerValue<'ctx>, IntValue<'ctx>) {
    let int_ptr = self.builder.build_ptr_to_int(ptr, self.context.i64_type(), "ptr_as_int").unwrap();
    let mask = self.type_tags.get("mask").unwrap();
    let tag = self.builder.build_and(int_ptr, self.context.i64_type().const_int(*mask, false), "extract_tag").unwrap();
    let untag_value = self.context.i64_type().const_int(!*mask, false);
    let untagged_int_ptr = self.builder.build_and(int_ptr, untag_value, "untagged_ptr").unwrap();
    let untagged_ptr = self.builder.build_int_to_ptr(untagged_int_ptr, ptr.get_type(), "untagged_ptr_value").unwrap();
    return (untagged_ptr, tag);
  }

  // TODO: Replace with less specific case
  // Probably need to build phi nodes to come out of loading dynamic types
  fn _load_var_to_type(&self, name: String) -> Result<(), Error> {
    let (_, tagged_ptr, _, _exited_func) = self.get_symbol(&name)?.unwrap();
    let (_, tag) = self.extract_tag(tagged_ptr.unwrap());
    // let tag_value = self.builder.build_int_z_extend(tag, self.context.i64_type(), "tag_value").unwrap();

    let a = self.builder.build_int_compare(
      inkwell::IntPredicate::EQ,
      tag,
      self.context.i64_type().const_int(*self.type_tags.get("int").unwrap(), false),
      "is_int"
    ).unwrap();

    println!("{:?}", a);
    return Ok(());
  }

  fn build_function(&mut self, name: Name, ret_typ: Option<Literals>, params: Option<Box<Vec<Expression>>>, body: Option<Box<Vec<Expression>>>, block: BasicBlock<'ctx>) -> Result<(), Error> {
    // Get function parameters
    let mut parameters: Vec<BasicMetadataTypeEnum> = vec![];
    // let mut placeholders: Vec<(usize, String, BasicTypeEnum)> = vec![];
    let mut defaults: Vec<(usize, String, Box<Expression>)> = vec![];
    let mut variadic = false;
    if params.is_some() {
      for (i, param) in params.clone().unwrap().iter().enumerate() {
        let (typ, _is_const, name, val, spread) = match param.clone().expr {
          Expr::FunctionParam(ty, is_const, name, default_val, spread) => {
            (
              ty.unwrap(),
              is_const,
              match name {
                Literals::Identifier(name, _) => name,
                _ => panic!("Invalid function parameter"),
              },
              default_val,
              spread
            )
          },
          _ => panic!("Invalid function parameter"),
        };

        // If spread
        if spread {
          // If not last parameter
          if i < params.clone().unwrap().len() - 1 {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((param.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              param.first_pos,
              param.last_line,
              "Invalid use of spread operator in function parameters, must be last parameter".to_string()
            ));
          }

          // If not the inferred/dynamic type or the array type
          // TODO: Array type
          if !["let", "const"].contains(&typ.0.as_str()) {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((param.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              param.first_pos,
              param.last_line,
              "Invalid use of spread operator in function parameters, must be array type".to_string()
            ));
          }

          // TODO: Spread operator default
          if val.is_some() {
            // defaults.push

          // TODO: Spread operator unpack?
          // We know whether function is variadic so builder might have tool for unpacking variadic args
          // Would need to be pushed into some sort of array/struct construction eventually
          } else {
            // placeholders.push
          }

          variadic = true;

        // Not a spread op parameter
        } else {
          let _t: BasicTypeEnum = match typ.0.as_str() {
            "let" => {
              // let t = self.context.struct_type(&self.get_types_as_list(), false);
              let t = self.context.ptr_type(AddressSpace::default());
              parameters.push(t.into());
              t.into()
            },
            "int" => {
              let t = self.context.i64_type();
              parameters.push(t.into());
              t.into()
            },
            "float" => {
              let t = self.context.f64_type();
              parameters.push(t.into());
              t.into()
            },
            "bool" => {
              let t = self.context.bool_type();
              parameters.push(t.into());
              t.into()
            },
            "string" => {
              let t = self.context.ptr_type(AddressSpace::default());
              parameters.push(t.into());
              t.into()
            },
            _ => {
              // Check for defined type (TODO: add aliases)
              let struct_type = self.struct_table.get(&typ.0);
              if struct_type.is_none() {
                return Err(Error::new(
                  Error::NameError,
                  None,
                  &self.code.lines().nth((param.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                  param.first_pos,
                  Some(typ.0.len() as i32),
                  format!("Type {} not found", typ.0)
                ));
              } else {
                let (struct_type, _) = struct_type.unwrap();
                let t = struct_type.as_basic_type_enum();
                parameters.push(t.into());
                t.into()
              }
            },
          };

          // If parameter has default
          if val.is_some() {
            defaults.push((i, name.0, val.unwrap()));

          // If parameter is a placeholder (no default)
          // TODO: Change this around. I think we can get the parameter pointers instead of re-allocating
          } else {
            // placeholders.push((i, name.0, t));
          }
        }
      }
    }

    // Get return type
    let returns: String = ret_typ.map_or("void".to_string(), |ty| match ty {
      Literals::Identifier(name, _) => name.0,
      _ => panic!("Invalid return type"),
    });

    // Build function
    let func_type = match returns.as_str() {
      "int" => self.context.i64_type().fn_type(&parameters, variadic),
      "float" => self.context.f64_type().fn_type(&parameters, variadic),
      "void" => self.context.void_type().fn_type(&parameters, variadic),
      _ => panic!("Invalid return type"),
    };
    let function = self.module.add_function(&name.0, func_type, None);

    // Create a new block for function body
    let mut func_block = self.context.append_basic_block(function, format!("{}-entry", name.0).as_str());
    self.builder.position_at_end(func_block);

    // Set up function parameters
    for (i, param) in function.get_params().iter().enumerate() {

      // Get name
      let name: String = match params.clone().unwrap()[i].clone().expr {
        Expr::FunctionParam(_, _, name, _, _) => {
          match name {
            Literals::Identifier(name, _) => name.0,
            _ => panic!("Invalid function parameter"),
          }
        },
        _ => panic!("Invalid function parameter"),
      };

      // Set parameter name
      param.set_name(&name);

      // if in defaults
      if let Some((_, _, default)) = defaults.iter().find(|(index, _, _)| *index == i) {
        let val = self.visit(&default, func_block)?;
        self.builder.build_store(param.into_pointer_value(), val).unwrap();
        self.set_symbol(name, Some(param.into_pointer_value()), val);

      // Not in defaults
      } else {
        // param.set_name(&params.clone().unwrap()[i].clone().expr.0);
        // If not typed
        if param.get_type().is_pointer_type() {
          // let var = self.builder.build_alloca(param.get_type(), &param.get_name().to_str().unwrap()).unwrap();
          // self.builder.build_store(var, param.into()).unwrap();
          // self.set_symbol(name, Some(param.into_pointer_value()), *param);
          
          // Load the variable to the correct type
          let (_, tag) = self.extract_tag(param.into_pointer_value());
          let t = self.get_tag_by_type(param.get_type().as_basic_type_enum());
          println!("{:?} {:?}", tag, t);
        } else {
          // let var = self.builder.build_alloca(param.get_type(), &param.get_name().to_str().unwrap()).unwrap();
          // self.builder.build_store(var, param.into()).unwrap();

          // self.builder.build_load(param.into(), param).unwrap();
          // self.builder.build_store(param.into_pointer_value(), param.into()).unwrap();

          self.set_symbol(name, None, *param);
        }
        // self.set_symbol(param.get_name().to_str().unwrap().to_string(), param.into_pointer_value(), *param);
      }
    }

    // Add parameter defaults to the function
    // for (i, n, default) in defaults.iter() {
    //   let val = self.visit(&default, func_block)?;
    //   let arg = function.get_nth_param(*i as u32).unwrap();
    //   arg.set_name(&n);
    //   self.builder.build_store(arg.into_pointer_value(), val).unwrap();
    //   self.set_symbol(n.clone(), arg.into_pointer_value(), val);
    // }

    // Add parameter placeholders to symbol table
    // for (i, n, t) in placeholders.iter() {
    //   let arg = function.get_nth_param(*i as u32).unwrap();
    //   arg.set_name(&n);
    //   let var = self.builder.build_alloca(t.clone(), &n).unwrap();
    //   self.builder.build_store(var, arg).unwrap();
    //   self.set_symbol(n.clone(), var, arg.into());
    // }

    // Enter the function scope
    self.enter_scope(true);

    // Fill the function body
    for ex in body.as_ref().unwrap().iter() {
      if let Expr::Return(ret) = ex.expr.clone() {
        let val = self.visit(&ret, func_block)?;
        if val != self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum() {
          self.builder.build_return(Some(&val)).unwrap();
        } else {
          self.builder.build_return(None).unwrap();
        }

      } else if let Expr::Conditional(typ, expr, body, ebody) = ex.expr.clone() {
        func_block = self.build_if(typ, expr, body, ebody, function, func_block)?;

      } else if let Expr::Assignment(_, _, _) = ex.expr.clone() {
        self.build_assignment(ex.clone(), func_block, false)?;

      } else {
        self.visit(&ex, func_block)?;
      }
    }

    // If last item in void function is not a return, add one
    // TODO: Improve on this logic
    if returns.as_str() == "void" {
      if body.as_ref().unwrap().is_empty() {
        self.builder.build_return(None).unwrap();

      } else {
        if let Expr::Return(_) = body.unwrap().last().unwrap().expr.clone() {
          // Last item is return, ignore
        } else {
          self.builder.build_return(None).unwrap();
        }
      }
    }

    // Exit the function scope
    self.exit_scope();

    // Build the function params
    // let mut items = vec![];
    // for (i, param) in params.unwrap().iter().enumerate() {
    //   let name = match param.clone().expr {
    //     Expr::FunctionParam(_, name, _, _) => {
    //       match name {
    //         Literals::Identifier(name, _) => name,
    //         _ => panic!("Invalid function parameter"),
    //       }
    //     },
    //     _ => panic!("Invalid function parameter"),
    //   };

    //   let arg = function.get_nth_param(i as u32).unwrap();
    //   arg.set_name(&name.0);
    //   items.push(arg);
    // }

    // Ignore if there is already a return
    if function.get_last_basic_block().unwrap().get_last_instruction().is_none() {
      self.builder.build_return(Some(&self.context.f64_type().const_float(0.0).as_basic_value_enum())).unwrap();
    }

    // Terminate block
    self.builder.position_at_end(block);

    return Ok(());
  }

  fn visit(&mut self, expr: &Expression, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {

    // Position the builder at the end of the current block
    self.builder.position_at_end(block);

    match expr.clone().expr {
      Expr::Literal(literal) => self.handle_literal(literal, expr),
      Expr::BinaryOperator(op, lhs, rhs) => self.handle_binop(op, lhs, rhs, block),
      Expr::UnaryOperator(op, value) => self.handle_unop(op, value, block),
      Expr::Array(a) => {
        let mut values = Vec::new();
        for v in a.iter() {
          values.push(self.visit(&v, block)?.into_int_value());
        }

        return Ok(self.context.i64_type().const_array(&values).as_basic_value_enum());
      },
      Expr::TypeDef(_, _) => todo!(),
      Expr::Conditional(_, _, _, _) => panic!("Conditional not allowed here"),
      Expr::Call(name, args) => {
        let function = self.module.get_function(&name.0);
        if function.is_none() {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            expr.first_pos,
            expr.last_line,
            format!("Unknown function: {}", name.0)
          ));
        }

        // If args don't match func params and func is not variadic
        if args.len() != function.unwrap().get_params().len() && !function.unwrap().get_type().is_var_arg() {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            expr.first_pos,
            expr.last_line,
            format!("Function \"{}\" expected {} arguments, got {}", name.0, function.unwrap().get_params().len(), args.len())
          ));
        }

        // Old arg map
        // let arg_values: Vec<BasicMetadataValueEnum> = args.iter().map(|arg| self.visit(&arg, block).unwrap().into()).collect();

        let mut values: Vec<BasicValueEnum> = vec![];
        let mut meta: Vec<BasicMetadataValueEnum> = vec![];

        // Build args
        for (_, arg) in args.iter().enumerate() {
          let val = self.visit(&arg, block)?;
          match val.into() {
            BasicMetadataValueEnum::IntValue(int_value) => {
              let int_type = int_value.get_type();
              if int_type.get_bit_width() == 64 {
                values.push(int_value.into());
                meta.push(BasicMetadataValueEnum::IntValue(int_value));
              } else {
                values.push(val.into());
                meta.push(val.into());
              }
            },
            BasicMetadataValueEnum::PointerValue(ptr_value) => {
              values.push(ptr_value.into());
              meta.push(BasicMetadataValueEnum::PointerValue(ptr_value));
            },
            BasicMetadataValueEnum::ArrayValue(array_value) => {
              let array_type = array_value.get_type();
              let element_type = array_type.get_element_type();
              if let BasicTypeEnum::IntType(int_type) = element_type {
                // If string
                if int_type.get_bit_width() == 8 {
                  let global = self.module.add_global(array_type, None, "global");
                  global.set_initializer(&array_value);
                  let ptr = global.as_pointer_value();
                  values.push(ptr.into());
                  meta.push(BasicMetadataValueEnum::PointerValue(ptr));
                } else {
                  values.push(val.into());
                  meta.push(val.into());
                }
              } else {
                values.push(val.into());
                meta.push(val.into());
              }
            },
            _ => {
              values.push(val.into());
              meta.push(val.into());
            },
          }
        }

        for (i, param) in function.unwrap().get_params().iter().enumerate() {
          let param_type = param.get_type();
          if param_type.is_pointer_type() {
            if !values[i].is_pointer_value() {
              let ptr = self.builder.build_alloca(param_type, "ptr").unwrap();
              self.tag_ptr(ptr, self.get_tag_by_type(values[i].get_type()));
              self.builder.build_store(ptr, values[i]).unwrap();
              meta[i] = ptr.into();
            }
          }
        }

        // Build call and extract value
        let call = self.builder.build_call(function.unwrap(), &meta, &name.0).unwrap();
        match call.try_as_basic_value().left() {
          Some(val) => {
            return Ok(val);
          },
          None => {
            // Return null ptr if no value is returned
            return Ok(self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum());
          },
        }
      },
      Expr::Return(r) => {
        let val = self.visit(&r, block)?;
        if val != self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum() {
          self.builder.build_return(Some(&val)).unwrap();
        } else {
          self.builder.build_return(None).unwrap();
        }

        return Ok(val);
      },
      Expr::Assignment(_, _, _) => panic!("Assignment not allowed here"),
      _ => todo!()
    }
  }

  fn handle_literal(&mut self, literal: Literals, expr: &Expression) -> Result<BasicValueEnum<'ctx>, Error> {
    match literal {
      Literals::Integer(i) => {
        return Ok(self.context.i64_type().const_int(i as u64, false).as_basic_value_enum());
      },
      Literals::UnsignedInteger(_) => todo!(),
      Literals::FloatingPoint(f) => {
        return Ok(self.context.f64_type().const_float(f).as_basic_value_enum());
      },
      Literals::String(s) => {
        return Ok(self.context.const_string(s.as_bytes(), true).as_basic_value_enum());
      },
      Literals::Boolean(b) => {
        return Ok(self.context.bool_type().const_int(b as u64, false).as_basic_value_enum());
      },
      // Literals::Object(obj) => {

      //   println!("{:?}", expr);
      //   panic!("Testing");
      //   let mut values: Vec<BasicValueEnum> = vec![];
      //   for literal in obj.iter() {
      //     let prop = match literal.to_owned() {
      //       Literals::ObjectProperty(name, val) => (name, val),
      //       _ => panic!("Invalid object property"),
      //     };
      //     values.push(self.visit(&prop.1, self.builder.get_insert_block().unwrap()).unwrap());
      //   }

      //   return Ok(self.context.struct_type(&values.iter().map(|v| v.get_type()).collect::<Vec<BasicTypeEnum>>(), false).const_named_struct(&values).as_basic_value_enum());
      // },
      Literals::Object(_) => panic!("Object literal not allowed here"),
      Literals::ObjectProperty(_, _) => todo!(),
      Literals::Null => {
        return Ok(self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum());
      },
      Literals::Identifier(n, _) => {
        if let Some((_, _, value, _exited_func)) = self.get_symbol(&n.0)? {
          return Ok(value);
        } else {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            expr.first_pos,
            expr.last_line,
            format!("Unknown identifier: {}", n.0)
          ));
          // panic!("Unknown identifier: {}", n.0);
        }
      },
      Literals::EOF => todo!(),
    }
  }

  fn handle_unop(&mut self, op: Operator, value: Box<Expression>, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
    let val: BasicValueEnum<'ctx>;
    let mut name: Option<String> = None;
    if let Expr::Literal(Literals::Identifier(n, _)) = value.clone().expr {
      val = self.get_symbol(&n.0)?.unwrap().2;
      name = Some(n.0);
      // val = self.builder.build_load(val.into_pointer_value(), val.into_pointer_value(), &name.0).unwrap().as_basic_value_enum();
    } else {
      val = self.visit(&value, block)?;
    }

    match op {
      Operator(op) => {
        match op.as_str() {
          "-" => {
            if name.is_some() {
              // Reassign the identifier with the new value
              let a = self.get_symbol(&name.unwrap())?.unwrap().1.unwrap();
              if a.is_const() {
                panic!("Cannot assign to a constant");
              } else {
                let val = self.builder.build_int_neg(val.into_int_value(), "negtmp").unwrap();
                self.builder.build_store(a, val).unwrap();
                return Ok(val.as_basic_value_enum());
              }
            } else {
              return Ok(self.builder.build_int_neg(val.into_int_value(), "negtmp").unwrap().as_basic_value_enum());
            }
          },
          "!" => {
            return Ok(self.builder.build_not(val.into_int_value(), "nottmp").unwrap().as_basic_value_enum());
          },
          // Pre-increment (return the new value)
          "++:pre" => {
            if name.is_some() {
              // Reassign the identifier with the new value
              let a = self.get_symbol(&name.as_ref().unwrap())?.unwrap().1.unwrap();
              if a.is_const() {
                panic!("Cannot assign to a constant");
              } else {
                let v = self.builder.build_int_add(val.into_int_value(), self.context.i64_type().const_int(1, false), "addtmp").unwrap();
                self.builder.build_store(a, v).unwrap();
                self.set_symbol(name.unwrap(), Some(a), v.as_basic_value_enum());
                return Ok(v.as_basic_value_enum());
              }
            } else {
              // Return the new value
              return Ok(self.builder.build_int_add(val.into_int_value(), self.context.i64_type().const_int(1, false), "addtmp").unwrap().as_basic_value_enum());
            }
          },
          // Post-increment (return the original value)
          "++:post" => {
            if name.is_some() {
              // Reassign the identifier with the new value
              let a = self.get_symbol(&name.as_ref().unwrap())?.unwrap().1.unwrap();
              if a.is_const() {
                panic!("Cannot assign to a constant");
              } else {
                let v = self.builder.build_int_add(val.into_int_value(), self.context.i64_type().const_int(1, false), "addtmp").unwrap();
                self.builder.build_store(a, v).unwrap();
                self.set_symbol(name.unwrap(), Some(a), v.as_basic_value_enum());
                return Ok(val);
              }
            } else {
              // Return the original value
              return Ok(val);
            }
          },
          // Pre-decrement
          "--:pre" => {
            if name.is_some() {
              // Reassign the identifier with the new value
              let a = self.get_symbol(&name.as_ref().unwrap())?.unwrap().1.unwrap();
              if a.is_const() {
                panic!("Cannot assign to a constant");
              } else {
                let v = self.builder.build_int_sub(val.into_int_value(), self.context.i64_type().const_int(1, false), "subtmp").unwrap();
                self.builder.build_store(a, v).unwrap();
                self.set_symbol(name.unwrap(), Some(a), v.as_basic_value_enum());
                return Ok(v.as_basic_value_enum());
              }
            } else {
              return Ok(self.builder.build_int_sub(val.into_int_value(), self.context.i64_type().const_int(1, false), "subtmp").unwrap().as_basic_value_enum());
            }
          },
          // Post-decrement
          "--:post" => {
            if name.is_some() {
              // Reassign the identifier with the new value
              let a = self.get_symbol(&name.as_ref().unwrap())?.unwrap().1.unwrap();
              if a.is_const() {
                panic!("Cannot assign to a constant");
              } else {
                let v = self.builder.build_int_sub(val.into_int_value(), self.context.i64_type().const_int(1, false), "subtmp").unwrap();
                self.builder.build_store(a, v).unwrap();
                self.set_symbol(name.unwrap(), Some(a), v.as_basic_value_enum());
                return Ok(val);
              }
            } else {
              return Ok(val);
            }
          },
          _ => panic!("Unknown operator: {}", op),
        }
      },
    }
  }

  // Handle binary operators
  // TODO: Improve / rewrite entirely
  // Will either need more comprehensive type inference or an entire type analysis / check step before IR building
  // Consider this entire function a temporary solution or broken
  fn handle_binop(&mut self, op: Operator, lhs: Box<Expression>, rhs: Box<Expression>, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
    let mut left = self.visit(&lhs, block)?;
    let mut right = self.visit(&rhs, block)?;

    // Based on the left and right types, determine the correct operation to perform
    let typ = match (left, right) {
      // One of the values is a float
      (BasicValueEnum::FloatValue(_), _) => "float".to_string(),
      (_, BasicValueEnum::FloatValue(_)) => "float".to_string(),
      // Both values are integers
      (BasicValueEnum::IntValue(_), BasicValueEnum::IntValue(_)) => "int".to_string(),
      (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
        // Get symbols from pointer names
        println!("a: {:?}\nb: {:?}", a, b);
        let (_, a_inst, _a_val, _) = self.get_symbol(&a.get_name().to_string_lossy())?.unwrap();
        let (_, b_inst, _b_val, _) = self.get_symbol(&b.get_name().to_string_lossy())?.unwrap();

        // panic!("Pointer arithmetic not yet implemented");
        let a_value = self.builder.build_load(self.context.i64_type(), a_inst.unwrap(), "lhs_val").unwrap();
        let b_value = self.builder.build_load(self.context.i64_type(), b_inst.unwrap(), "rhs_val").unwrap();

        left = a_value.as_basic_value_enum();
        right = b_value.as_basic_value_enum();

        if a_value.is_int_value() && b_value.is_int_value() {
          "int".to_string()
        } else if a_value.is_float_value() && b_value.is_float_value() {
          "float".to_string()
        } else {
          panic!("Invalid types for operation");
        }
      },
      (BasicValueEnum::PointerValue(a), BasicValueEnum::IntValue(_)) => {
        let (_, a_inst, _a_val, _) = self.get_symbol(&a.get_name().to_string_lossy())?.unwrap();
        let a_value = self.builder.build_load(self.context.i64_type(), a_inst.unwrap(), "lhs_val").unwrap();
        left = a_value.as_basic_value_enum();
        "int".to_string()
      },
      (BasicValueEnum::IntValue(_), BasicValueEnum::PointerValue(b)) => {
        let (_, b_inst, _b_val, _) = self.get_symbol(&b.get_name().to_string_lossy())?.unwrap();
        let b_value = self.builder.build_load(self.context.i64_type(), b_inst.unwrap(), "rhs_val").unwrap();
        left = b_value.as_basic_value_enum();
        "int".to_string()
      },
      (BasicValueEnum::StructValue(a), BasicValueEnum::StructValue(b)) => {
        // Get fields from struct values
        let a_value = self.builder.build_extract_value(a, 0, "lhs_val").unwrap();
        let b_value = self.builder.build_extract_value(b, 0, "rhs_val").unwrap();

        left = a_value.as_basic_value_enum();
        right = b_value.as_basic_value_enum();

        if a_value.is_int_value() && b_value.is_int_value() {
          "int".to_string()
        } else if a_value.is_float_value() && b_value.is_float_value() {
          "float".to_string()
        } else if a_value.is_int_value() && b_value.is_float_value() {
          "float".to_string()
        } else if a_value.is_float_value() && b_value.is_int_value() {
          "float".to_string()
        } else if a_value.is_struct_value() && b_value.is_struct_value() {
          // println!("{:?} {:?}", a_value, b_value);
          todo!()
        } else if a_value.is_pointer_value() && b_value.is_pointer_value() {
          "int".to_string()
        } else {
          // println!("{:?} {:?}", a_value, b_value);
          panic!("Invalid types for operation");
        }
      },
      (BasicValueEnum::StructValue(a), b) => {
        // Get fields from struct values
        let a_value = self.builder.build_extract_value(a, 0, "lhs_val").unwrap();

        left = a_value.as_basic_value_enum();

        if a_value.is_int_value() && b.is_int_value() {
          "int".to_string()
        } else if a_value.is_float_value() && b.is_float_value() {
          "float".to_string()
        } else {
          panic!("Invalid types for operation");
        }
      },
      (a, BasicValueEnum::StructValue(b)) => {
        // Get fields from struct values
        let b_value = self.builder.build_extract_value(b, 0, "rhs_val").unwrap();

        right = b_value.as_basic_value_enum();

        if a.is_int_value() && b_value.is_int_value() {
          "int".to_string()
        } else if a.is_float_value() && b_value.is_float_value() {
          "float".to_string()
        } else {
          panic!("Invalid types for operation");
        }
      },
      // Default to bool
      _ => "bool".to_string(),
    };

    match op {
      Operator(op) => {
        match op.as_str() {
          "+" => {
            match typ.as_str() {
              "float" => {
                if left.is_int_value() {
                  left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
                }
                if right.is_int_value() {
                  right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
                }
                return Ok(self.builder.build_float_add(left.into_float_value(), right.into_float_value(), "addtmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_add(left.into_int_value(), right.into_int_value(), "addtmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for addition: {}", typ), // TODO: pretty up errors
            }
          },
          "-" => {
            match typ.as_str() {
              "float" => {
                if left.is_int_value() {
                  left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
                }
                if right.is_int_value() {
                  right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
                }
                return Ok(self.builder.build_float_sub(left.into_float_value(), right.into_float_value(), "subtmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_sub(left.into_int_value(), right.into_int_value(), "subtmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for subtraction"), // TODO: pretty up errors
            }
          },
          "*" => {
            match typ.as_str() {
              "float" => {
                if left.is_int_value() {
                  left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
                }
                if right.is_int_value() {
                  right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
                }
                return Ok(self.builder.build_float_mul(left.into_float_value(), right.into_float_value(), "multmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_mul(left.into_int_value(), right.into_int_value(), "multmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for multiplication"),
            }
          },
          "/" => {
            match typ.as_str() {
              "float" => {
                if left.is_int_value() {
                  left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
                }
                if right.is_int_value() {
                  right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
                }
                return Ok(self.builder.build_float_div(left.into_float_value(), right.into_float_value(), "divtmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_unsigned_div(left.into_int_value(), right.into_int_value(), "divtmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for division"),
            }
          },
          "//" => {
            todo!();
          },
          "%" => {
            match typ.as_str() {
              "float" => {
                if left.is_int_value() {
                  left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
                }
                if right.is_int_value() {
                  right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
                }
                return Ok(self.builder.build_float_rem(left.into_float_value(), right.into_float_value(), "remtmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_unsigned_rem(left.into_int_value(), right.into_int_value(), "remtmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for modulus"),
            }
          },
          "^" => {
            todo!();
          },
          "&&" => {
            return Ok(self.builder.build_and(left.into_int_value(), right.into_int_value(), "andtmp").unwrap().as_basic_value_enum());
          },
          "||" => {
            return Ok(self.builder.build_or(left.into_int_value(), right.into_int_value(), "ortmp").unwrap().as_basic_value_enum());
          },
          "==" => {
            return Ok(self.builder.build_int_compare(inkwell::IntPredicate::EQ, left.into_int_value(), right.into_int_value(), "eqtmp").unwrap().as_basic_value_enum());
          },
          "!=" => {
            return Ok(self.builder.build_int_compare(inkwell::IntPredicate::NE, left.into_int_value(), right.into_int_value(), "netmp").unwrap().as_basic_value_enum());
          },
          ">" => {
            return Ok(self.builder.build_int_compare(inkwell::IntPredicate::UGT, left.into_int_value(), right.into_int_value(), "gttmp").unwrap().as_basic_value_enum());
          },
          "<" => {
            return Ok(self.builder.build_int_compare(inkwell::IntPredicate::ULT, left.into_int_value(), right.into_int_value(), "lttmp").unwrap().as_basic_value_enum());
          },
          ">=" => {
            return Ok(self.builder.build_int_compare(inkwell::IntPredicate::UGE, left.into_int_value(), right.into_int_value(), "getmp").unwrap().as_basic_value_enum());
          },
          "<=" => {
            return Ok(self.builder.build_int_compare(inkwell::IntPredicate::ULE, left.into_int_value(), right.into_int_value(), "letmp").unwrap().as_basic_value_enum());
          },
          "+=" => {
            match typ.as_str() {
              "float" => {
                if let Expr::Literal(Literals::Identifier(n, _)) = lhs.clone().expr {
                  let a = self.get_symbol(&n.0)?.unwrap().1.unwrap();
                  if a.is_const() {
                    panic!("Cannot assign to a constant");
                  } else {
                    let val = self.builder.build_float_add(left.into_float_value(), right.into_float_value(), "addtmp").unwrap();
                    self.builder.build_store(a, val).unwrap();
                    self.set_symbol(n.0, Some(a), val.as_basic_value_enum());
                    return Ok(val.as_basic_value_enum());
                  }
                } else {
                  return Ok(self.builder.build_float_add(left.into_float_value(), right.into_float_value(), "addtmp").unwrap().as_basic_value_enum());
                }
              },
              "int" => {
                if let Expr::Literal(Literals::Identifier(n, _)) = lhs.clone().expr {
                  let a = self.get_symbol(&n.0)?.unwrap().1.unwrap();
                  if a.is_const() {
                    panic!("Cannot assign to a constant");
                  } else {
                    let val = self.builder.build_int_add(left.into_int_value(), right.into_int_value(), "addtmp").unwrap();
                    self.builder.build_store(a, val).unwrap();
                    self.set_symbol(n.0, Some(a), val.as_basic_value_enum());
                    return Ok(val.as_basic_value_enum());
                  }
                } else {
                  return Ok(self.builder.build_int_add(left.into_int_value(), right.into_int_value(), "addtmp").unwrap().as_basic_value_enum());
                }
              },
              _ => panic!("Invalid types for addition"), // TODO: pretty up errors
            }
          },
          "-=" => todo!(),
          "*=" => todo!(),
          "/=" => todo!(),
          "//=" => todo!(),
          "%=" => todo!(),
          _ => panic!("Unknown operator: {}", op),
        }
      },
    }
  }
}

// Tests
#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_compile_assignment_reassignment() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let a = 1;
    let b = 2;
    a = 3;
    let c = a + b;".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), false, false)?;
    codegen.build(code, ast, false, true)?;
    return Ok(());
  }

  #[test]
  fn test_compile_print_numbers() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let a = 1;
    let b = 2;
    printf(\"a:%d b:%d\", a, b);".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), false, false)?;
    codegen.build(code, ast, false, true)?;
    return Ok(());
  }
}