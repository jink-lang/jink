use core::{panic, str};
use indexmap::IndexMap;
use std::collections::HashSet;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::types::{ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::support::LLVMString;

use jink::{Error, Expr, Expression, Name, Type, Operator, Literals};

use super::parser::Namespace;

pub struct Scope<'ctx> {
  symbols: IndexMap<String, (Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)>,
  is_function: bool,
}

pub struct CodeGen<'ctx> {
  pub code: String,
  pub namespaces: IndexMap<String, Namespace>,
  context: &'ctx Context,
  pub module: Module<'ctx>,
  builder: Builder<'ctx>,
  /// Stack of symbol tables
  symbol_table_stack: Vec<Scope<'ctx>>,
  /// Table of struct types and their fields
  struct_table: IndexMap<String, (StructType<'ctx>, Vec<(String, BasicTypeEnum<'ctx>)>)>,
  /// Stack of struct names and their corresponding type struct names
  struct_stack: IndexMap<String, String>,
  class_table: IndexMap<String, (              // name
    Option<Vec<String>>,                       // parents
    Vec<(String, BasicTypeEnum<'ctx>)>,        // props
    Option<Vec<(String, FunctionValue<'ctx>)>> // methods
  )>,
  _type_tags: IndexMap<String, u64>,
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
      namespaces: IndexMap::new(),
      context: &context,
      module,
      builder: context.create_builder(),
      symbol_table_stack: Vec::new(),
      struct_table: IndexMap::new(),
      struct_stack: IndexMap::new(),
      class_table: IndexMap::new(),
      _type_tags: Self::map_types(),
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
    types.insert("arr".to_string(), 0b110);
    types.insert("obj".to_string(), 0b111);
    types.insert("mask".to_string(), 0b1001); // mask for type checking
    return types;
  }

  fn _get_tag_by_type(&self, typ: BasicTypeEnum) -> u64 {
    match typ {
      BasicTypeEnum::IntType(_) => *self._type_tags.get("int").unwrap(),
      BasicTypeEnum::FloatType(_) => *self._type_tags.get("float").unwrap(),
      BasicTypeEnum::ArrayType(_) => *self._type_tags.get("array").unwrap(),
      BasicTypeEnum::PointerType(_) => *self._type_tags.get("object").unwrap(),
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

  /// name: Name of the symbol
  /// 
  /// inst: Instruction/allocation pointer to value in memory
  /// 
  /// typ: Latest known typ of the symbol
  /// 
  /// value: Latest known value of the symbol
  pub fn set_symbol(&mut self, name: String, inst: Option<PointerValue<'ctx>>, typ: BasicTypeEnum<'ctx>, value: BasicValueEnum<'ctx>) {
    if let Some(scope) = self.symbol_table_stack.last_mut() {
      scope.symbols.insert(name, (inst, typ, value));
    } else {
      panic!("Unexpected compilation error, set symbol without a scope");
    }
  }

  /// Get symbol from symbol table stack
  /// 
  /// Returns Result->Option of symbol name, instruction pointer, value and whether we left a function scope to obtain it 
  /// If the instruction pointer is None the symbol is a constant value
  pub fn get_symbol(&self, name: &str) -> Result<Option<(String, Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>, bool)>, Error> {
    let mut exited_function = false;
    for scope in self.symbol_table_stack.iter().rev() {
      if let Some((inst, typ, value)) = scope.symbols.get(name) {
        return Ok(Some((name.to_string(), inst.clone(), typ.clone(), value.clone(), exited_function)));
      }

      if scope.is_function {
        exited_function = true;
      }
    }
    return Ok(None);
  }

  fn get_all_cur_symbols(&self) -> IndexMap<String, (Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>, bool)> {
    let mut symbols = IndexMap::new();
    for (name, (inst, typ, value)) in self.symbol_table_stack.last().unwrap().symbols.iter() {
      symbols.insert(name.clone(), (inst.clone(), typ.clone(), value.clone(), false));
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

  fn build_external_functions(&self) {
    let ptr_type = self.context.ptr_type(AddressSpace::default());

    let printf_type = self.context.i32_type().fn_type(&[ptr_type.into()], true);
    self.module.add_function("printf", printf_type, Some(Linkage::External));

    let puts_type = self.context.i32_type().fn_type(&[ptr_type.into()], true);
    self.module.add_function("puts", puts_type, Some(Linkage::External));

    let scanf_type = self.context.i32_type().fn_type(&[ptr_type.into(), ptr_type.into()], true);
    self.module.add_function("scanf", scanf_type, Some(Linkage::External));

    let strlen_type = self.context.i64_type().fn_type(&[ptr_type.into()], false);
    self.module.add_function("strlen", strlen_type, Some(Linkage::External));

    let malloc_type = self.context.i8_type().fn_type(&[self.context.i64_type().into()], false);
    self.module.add_function("malloc", malloc_type, Some(Linkage::External));

    let free_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
    self.module.add_function("free", free_type, Some(Linkage::External));

    let fopen_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
    self.module.add_function("fopen", fopen_type, Some(Linkage::External));

    let fwrite_type = self.context.i64_type().fn_type(&[ptr_type.into(), self.context.i64_type().into(), self.context.i64_type().into(), ptr_type.into()], false);
    self.module.add_function("fwrite", fwrite_type, Some(Linkage::External));

    let fread_type = self.context.i64_type().fn_type(&[ptr_type.into(), self.context.i64_type().into(), self.context.i64_type().into(), ptr_type.into()], false);
    self.module.add_function("fread", fread_type, Some(Linkage::External));

    let fclose_type = self.context.i64_type().fn_type(&[ptr_type.into()], false);
    self.module.add_function("fclose", fclose_type, Some(Linkage::External));

    let fputs_type = self.context.i32_type().fn_type(&[ptr_type.into(), ptr_type.into()], true);
    self.module.add_function("fputs", fputs_type, Some(Linkage::External));
  }

  pub fn build(&mut self, code: String, ast: Vec<Expression>, namespaces: IndexMap<String, Namespace>, verbose: bool, do_execute: bool) -> Result<LLVMString, Error> {
    self.code = code;
    self.namespaces = namespaces;

    // Prepare the program entry point
    let main_fn = self.create_main_function();

    // Enter the main application scope
    self.enter_scope(false);

    self.build_external_functions();

    // Process the AST
    let merge_block = self.process_ast(ast, main_fn)?;

    // Position at the end of the last main block
    self.builder.position_at_end(merge_block);

    // Exit the main block
    self.builder.build_return(None).unwrap();

    // Print LLVM IR
    if verbose {
      println!("------------------");
      println!("------- IR -------");
      println!("------------------");
      println!("{}", self.module.print_to_string().to_string());
      println!("------------------");
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

    return Ok(self.module.print_to_string());
  }

  fn process_ast(&mut self, ast: Vec<Expression>, main_fn: FunctionValue<'ctx>) -> Result<BasicBlock<'ctx>, Error> {
    // Get entry block
    let mut block = main_fn.get_first_basic_block().unwrap();

    for expr in ast {
      block = self.process_top(expr, main_fn, block)?;
    }

    return Ok(block);
  }

  fn process_top(&mut self, expr: Expression, main_fn: FunctionValue<'ctx>, block: BasicBlock<'ctx>) -> Result<BasicBlock<'ctx>, Error> {
    let mut block = block;
    match expr.clone().expr {
      // TODO: Validate for allowed top level op expressions
      // (Possibly move to parsing since we already validate for top level expressions there)
      Expr::BinaryOperator(_, _, _) => {
        self.visit(&expr.clone(), block)?;
      },
      Expr::UnaryOperator(_, _) => {
        self.visit(&expr.clone(), block)?;
      },

      Expr::Assignment(_, _, _) => {
        self.build_assignment(expr, block, false)?;
      },
      Expr::TypeDef(name, value) => {
        self.build_struct(Some(name), value)?;
      },
      Expr::Conditional(typ, expr, body, else_body) => {
        let merge_block = self.context.append_basic_block(main_fn, "merge");
        (_, _, block) = self.build_if(true, typ, expr, body, else_body, main_fn, block, merge_block, None, None)?;
      },
      Expr::ForLoop(value, expr, body) => {
        block = self.build_for_loop(value, expr, body, main_fn, block)?;
      },
      Expr::WhileLoop(expr, body) => {
        block = self.build_while_loop(expr, body, main_fn, block)?;
      },
      Expr::Call(_, _) => {
        self.visit(&expr, block)?;
      },
      Expr::Function(name, ret_typ, params, body) => {
        self.build_function(name, ret_typ, params, body, block)?;
      },
      Expr::Class(name, parents, body) => {
        self.build_class(expr, name, parents, body, block)?;
      },
      Expr::ModuleParsed(path, _, opts, ast) => {
        self.build_module(expr, path, opts, ast, main_fn)?;
      },
      Expr::Public(expr) => {
        block = self.process_top(*expr, main_fn, block)?;
      }
      // When we add top level index expressions
      // i.e. hello[0].bye() or hello.bye() or module.property and module.method()
      // Expr::Index(parent, child) => {
      //   self.build_index_extract(parent, child, block)?;
      // },
      // Expr::ArrayIndex(arr, index) => {
      //   println!("ArrayIndex: {:?} {:?}\n", arr, index);
      // },
      _ => {
        if let Expr::Literal(Literals::EOF) = expr.expr {
          return Ok(block);
        } else {
          return Err(Error::new(
            Error::CompilerError,
            None,
            self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap(),
            expr.first_pos,
            expr.first_pos,
            "Invalid top level expression".to_string()
          ));
        }
      }
    }

    return Ok(block);
  }

  /// Build imported modules
  // TODO: Ensure that modules are properly contained
  fn build_module(&mut self, _expr: Expression, path: Vec<Name>, _opts: Option<Vec<(Name, Option<Name>)>>, ast: Vec<Expression>, _main_fn: FunctionValue<'ctx>) -> Result<(), Error> {
    let module_name = path.iter().map(|n| n.0.to_string()).collect::<Vec<String>>().join("/");
    let module = self.context.create_module(module_name.as_str());

    // Enter the module scope
    self.enter_scope(false);

    // Process the AST
    self.process_ast(ast, _main_fn)?;

    if let Err(e) = module.verify() {
      return Err(Error::new(
        Error::CompilerError,
        None,
        &"",
        Some(0),
        Some(0),
        e.to_string()
      ));
    }

    return Ok(());
  }

  fn build_index_extract(&mut self, parent: Box<Expression>, child: Box<Expression>, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
    let parent_val = self.visit(&parent, block)?;

    // If parent is struct
    // Visiting a struct always returns a pointer
    if parent_val.is_pointer_value() {

      // Get struct type from name
      let struct_type = self.struct_stack.get(parent_val.get_name().to_str().unwrap());
      if struct_type.is_none() {
        return Err(Error::new(
          Error::CompilerError,
          None,
          self.code.lines().nth(parent.first_line.unwrap() as usize).unwrap(),
          parent.first_pos,
          parent.first_pos,
          format!("Unknown symbol: {}", parent_val.get_name().to_str().unwrap())
        ));
      }

      // Get type information with name
      let struct_ref = self.struct_table.get(struct_type.unwrap());
      if struct_ref.is_none() {
        return Err(Error::new(
          Error::CompilerError,
          None,
          &"",
          Some(0),
          Some(0),
          format!("Unknown type: {}", struct_type.unwrap())
        ));
      }

      if let Expr::Literal(Literals::Identifier(name)) = child.expr {

        for (i, (field_name, _field_type)) in struct_ref.unwrap().1.iter().enumerate() {
          if field_name == &name.0 {
            let struct_val = self.builder.build_load(struct_ref.unwrap().0, parent_val.into_pointer_value(), "struct_val").unwrap();
            let value = self.builder.build_extract_value(struct_val.into_struct_value(), i as u32, "indexed_value").unwrap();
            return Ok(value);
          }
        }

        // Invalid index at child
        return Err(Error::new(
          Error::CompilerError,
          None,
          self.code.lines().nth((child.first_line.unwrap() - 1) as usize).unwrap(),
          child.first_pos,
          child.first_pos,
          format!("Field {} not found on index {}", name.0, parent_val.get_name().to_str().unwrap())
        ));

      } else {
        // Invalid index at child
        return Err(Error::new(
          Error::CompilerError,
          None,
          self.code.lines().nth(child.first_line.unwrap() as usize).unwrap(),
          child.first_pos,
          child.first_pos,
          "Struct index must be an identifier".to_string()
        ));
      }
    }

    return Err(Error::new(
      Error::CompilerError,
      None,
      self.code.lines().nth(parent.first_line.unwrap() as usize).unwrap(),
      parent.first_pos,
      parent.first_pos,
      "Indexed must be of struct type".to_string()
    ));
  }

  fn build_loop_body(&mut self, typ: &str, body: Option<Vec<Expression>>, var: Option<PointerValue<'ctx>>, current_value: Option<IntValue>, body_block: BasicBlock<'ctx>, cond_block: BasicBlock<'ctx>, end_block: BasicBlock<'ctx>) -> Result<(), Error> {
    self.enter_scope(false);
    let mut current_block = body_block;

    if let Some(body) = body {
      for expr in body {

        if let Expr::Conditional(typ, expr, body, ebody) = expr.expr.clone() {
          let merge_block = self.context.append_basic_block(body_block.get_parent().unwrap(), "merge");
          (_, _, current_block) = self.build_if(true, typ, expr, body, ebody, body_block.get_parent().unwrap(), body_block, merge_block, Some(cond_block), Some(end_block))?;

        } else if let Expr::Return(ret) = expr.expr.clone() {
          let val = self.visit(&ret, current_block)?;
          if val != self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum() {
            self.builder.build_return(Some(&val)).unwrap();
          } else {
            self.builder.build_return(None).unwrap();
          }

        } else if let Expr::Assignment(_, _, _) = expr.expr.clone() {
          self.build_assignment(expr.clone(), current_block, false)?;

        } else if let Expr::ForLoop(value, expr, body) = expr.expr.clone() {
          current_block = self.build_for_loop(value, expr, body, body_block.get_parent().unwrap(), current_block)?;

        } else if let Expr::WhileLoop(expr, body) = expr.expr.clone() {
          current_block = self.build_while_loop(expr, body, body_block.get_parent().unwrap(), current_block)?;

        } else if let Expr::BreakLoop = expr.expr.clone() {
          self.builder.build_unconditional_branch(end_block).unwrap();
          return Ok(());

        } else if let Expr::ContinueLoop = expr.expr.clone() {
          self.builder.build_unconditional_branch(cond_block).unwrap();
          return Ok(());

        } else {
          self.visit(&expr, current_block)?;
        }
      }
    }

    if typ == "for" {
      // If we did not leave the body block, increment the iterator and branch back to the conditional block
      // if self.builder.get_insert_block().unwrap() == body_block || conditional_block.is_some() {
      let next_value = self.builder.build_int_add(current_value.unwrap(), self.context.i64_type().const_int(1, false), "next_value").unwrap();
      self.builder.build_store(var.unwrap(), next_value).unwrap();
      // }
    }

    // Exit the loop body scope
    self.exit_scope();
    return Ok(());
  }

  fn build_for_loop(&mut self, value: Box<Expression>, expr: Box<Expression>, body: Option<Vec<Expression>>, function: FunctionValue<'ctx>, block: BasicBlock<'ctx>) -> Result<BasicBlock<'ctx>, Error> {

    let cond_block = self.context.append_basic_block(function, "loop_cond_block");
    let body_block = self.context.append_basic_block(function, "loop_body");
    let end_block = self.context.append_basic_block(function, "loop_end");

    self.builder.position_at_end(block);

    // Collect the name
    if let Expr::Literal(Literals::Identifier(Name(name))) = value.expr {

      let i64_type = self.context.i64_type();
      let init_value = i64_type.const_int(0, false);

      // Initialize an int to manage the index of the loop
      let var = self.builder.build_alloca(init_value.get_type(), &name).unwrap();
      self.builder.build_store(var, init_value).unwrap();

      // Initialize the conditional block to ascertain whether the loop should continue
      self.builder.build_unconditional_branch(cond_block).unwrap();
      self.builder.position_at_end(cond_block);

      // Load the loop index in the conditional block
      let loop_index = self.builder.build_load(i64_type, var, &name).unwrap().into_int_value();

      // Visit the iterable that we are looping over
      if let Expr::Literal(Literals::Identifier(Name(n))) = expr.expr.clone() {
        let (_, typ, _) = self.var_from_ident(n.clone(), &expr)?;
        if !typ.is_array_type() {
          return Err(Error::new(
            Error::CompilerError,
            None,
            self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap(),
            expr.first_pos,
            Some(expr.first_pos.unwrap() + n.len() as i32),
            "Invalid iterable type".to_string()
          ));
        }

        // Get the length of the array
        let array_len = typ.into_array_type().len();
        let end_condition = i64_type.const_int(array_len as u64, false);

        // Build the conditional branching
        let cond = self.builder.build_int_compare(inkwell::IntPredicate::ULT, loop_index, end_condition, "loop_cond").unwrap();
        self.builder.build_conditional_branch(cond, body_block, end_block).unwrap();

        // Build the loop body
        self.builder.position_at_end(body_block);
        let current_value = self.builder.build_load(i64_type, var, "current_value").unwrap().into_int_value();
        self.set_symbol(name.clone(), Some(var), current_value.get_type().as_basic_type_enum(), current_value.into());
        self.build_loop_body("for", body, Some(var), Some(current_value), body_block, cond_block, end_block)?;

        // If we terminated in a block that is not the end block, branch back to the conditional block to be sure if the loop should continue
        if self.builder.get_insert_block().unwrap() != end_block {
          self.builder.build_unconditional_branch(cond_block).unwrap();
        }

        self.builder.position_at_end(end_block);
        return Ok(end_block);
      }
    }

    panic!("Unreachable");
  }

  // TODO: DRY this up with the repeat pattern in build_for_loop
  fn build_while_loop(&mut self, expr: Box<Expression>, body: Option<Vec<Expression>>, function: FunctionValue<'ctx>, block: BasicBlock<'ctx>) -> Result<BasicBlock<'ctx>, Error> {
    let cond_block = self.context.append_basic_block(function, "loop_cond_block");
    let body_block = self.context.append_basic_block(function, "loop_body");
    let end_block = self.context.append_basic_block(function, "loop_end");

    self.builder.position_at_end(block);

    // Initialize the conditional block to ascertain whether the loop should continue
    self.builder.build_unconditional_branch(cond_block).unwrap();
    self.builder.position_at_end(cond_block);

    // Visit the condition
    let cond = self.visit(&expr, cond_block)?;

    // If the condition is a number, condition on the basis of it not being 0
    if let BasicValueEnum::IntValue(i) = cond {
      let cond = self.builder.build_int_compare(inkwell::IntPredicate::NE, i, i.get_type().const_int(0, false), "loop_cond").unwrap();
      self.builder.build_conditional_branch(cond, body_block, end_block).unwrap();
    } else if let BasicValueEnum::FloatValue(f) = cond {
      let cond = self.builder.build_float_compare(inkwell::FloatPredicate::ONE, f, f.get_type().const_float(0.0), "loop_cond").unwrap();
      self.builder.build_conditional_branch(cond, body_block, end_block).unwrap();
    }

    // Build the loop body
    self.builder.position_at_end(body_block);
    self.build_loop_body("while", body, None, None, body_block, cond_block, end_block)?;

    // If we terminated in a block that is not the end block, branch back to the conditional block to be sure if the loop should continue
    if self.builder.get_insert_block().unwrap() != end_block {
      self.builder.build_unconditional_branch(cond_block).unwrap();
    }

    self.builder.position_at_end(end_block);
    return Ok(end_block);
  }

  fn build_struct(&mut self, name: Option<Literals>, value: Box<Literals>) -> Result<StructType<'ctx>, Error> {
    // Extract struct name if it's an identifier
    let mut struct_lit_name: Option<String> = None;
    if name.is_some() {
      struct_lit_name = match name.unwrap() {
        Literals::Identifier(name) => Some(name.0),
        _ => None,
      };
    }

    // TODO: If value is an identifier, handle it as a type alias

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
      return Err(Error::new(
        Error::CompilerError,
        None,
        &"",
        Some(0),
        Some(0),
        format!("Invalid struct definition at '{}'", struct_lit_name.unwrap_or(String::new()))
      ));
    }
  }

  fn build_struct_field(&mut self, val: Expr) -> Result<BasicTypeEnum<'ctx>, Error> {
    match val {
      Expr::TypeDef(_, v) => {
        let s = self.build_struct(None, v)?;
        return Ok(s.as_basic_type_enum());
      },
      Expr::Literal(Literals::Identifier(n)) => {
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

  // Name of type struct; name of variable; fields
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
            let (_, _, typ, _, _) = symbol.unwrap();
            field_val_type = typ;
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

    self.struct_stack.insert(var_name.clone(), struct_name.clone());
    return Ok(struct_ptr);
  }

  // Builder for conditional expressions
  fn build_if(&mut self, top_level: bool, _typ: Type, expr: Option<Box<Expression>>, body: Box<Vec<Expression>>, else_body: Option<Box<Vec<Expression>>>, function: FunctionValue<'ctx>, block: BasicBlock<'ctx>, top_level_merge_block: BasicBlock<'ctx>, loop_cond_block: Option<BasicBlock<'ctx>>, exit_loop_block: Option<BasicBlock<'ctx>>) -> Result<(BasicBlock<'ctx>, BasicBlock<'ctx>, BasicBlock<'ctx>), Error> {
    let mut cond = self.visit(&expr.unwrap(), block)?;

    // If the condition is a number, condition on the basis of it not being 0
    if let BasicValueEnum::IntValue(i) = cond {
      if i.get_type().get_bit_width() != 1 {
        cond = self.builder.build_int_compare(inkwell::IntPredicate::NE, i, i.get_type().const_int(0, false), "ifcond").unwrap().as_basic_value_enum();
      }
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

    // Build the "then" block and collect the resulting values
    self.enter_scope(false);
    let then_out = self.build_conditional_block(then_block, Some(body.clone()), function, top_level_merge_block, loop_cond_block, exit_loop_block)?;
    let then_vals = self.get_all_cur_symbols();
    self.exit_scope();
    self.builder.build_unconditional_branch(merge_block).unwrap();

    // Build the "else" block and collect the resulting values
    self.enter_scope(false);
    let else_out = self.build_conditional_block(else_block, else_body, function, top_level_merge_block, loop_cond_block, exit_loop_block)?;
    let else_vals = self.get_all_cur_symbols();
    self.exit_scope();
    self.builder.build_unconditional_branch(merge_block).unwrap();

    // Position the builder at the merge block
    self.builder.position_at_end(merge_block);

    // Collect all keys from both then_vals and else_vals
    let all_keys: HashSet<&String> = then_vals.keys().chain(else_vals.keys()).collect();

    for name in all_keys {

      // Get values from then_vals and else_vals or use the value above the loop blocks if it exists
      let then_val: Option<(Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>, bool)> = then_vals.get(name)
        .cloned()
        .or_else(|| {
        self.get_symbol(name).unwrap_or(None).map(|(_, pval, typ, val, exited_func)| {
          (pval, typ, val, exited_func)
        })
      });
      let else_val: Option<(Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>, bool)> = else_vals.get(name)
        .cloned()
        .or_else(|| {
        self.get_symbol(name).unwrap_or(None).map(|(_, pval, typ, val, exited_func)| {
          (pval, typ, val, exited_func)
        })
      });

      // If one branch initializes or uses one of its own new values
      if then_val.is_none() || else_val.is_none() { continue; }

      // Determine the types of the values
      let then_type = then_val.unwrap().1;
      let else_type = else_val.unwrap().1;

      // Determine the common type for the phi node
      let phi_type = if then_type.is_int_type() && else_type.is_int_type() {
        then_type
      } else if then_type.is_float_type() && else_type.is_float_type() {
        then_type
      } else {
        // Handle cases where only one branch modifies the value or they are of different types
        if then_type.is_float_type() && !else_type.is_float_type() {
          then_type
        } else if !then_type.is_float_type() && else_type.is_float_type() {
          else_type
        } else if then_type.is_int_type() && !else_type.is_int_type() {
          then_type
        } else if !then_type.is_int_type() && else_type.is_int_type() {
          else_type
        } else {
          // TODO: Implement logic to handle more complex type compatibility or type promotion
          panic!("Incompatible types for phi node");
        }
      };

      // Create the phi node with the determined type
      let phi = self.builder.build_phi(phi_type, name).unwrap();

      // Add incoming values from both branches
      phi.add_incoming(&[
        (&then_val.unwrap().2, then_out),
        (&else_val.unwrap().2, else_out),
      ]);

      // Check if the symbol exists in the symbol table
      if let Some(symbol) = self.get_symbol(name)? {
        // Update the existing variable with the new PHI value
        let (_, inst, _, _, exited_func) = symbol;
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
        self.set_symbol(name.to_string(), inst, phi_type, phi.as_basic_value());
      } else {
        // Create a new variable and store the PHI value
        let var = self.builder.build_alloca(phi_type, &name).unwrap();
        self.builder.build_store(var, phi.as_basic_value()).unwrap();
        self.set_symbol(name.to_string(), Some(var), phi_type, phi.as_basic_value());
      }
    }

    // Branch to the final merge block
    if top_level {
      self.builder.build_unconditional_branch(top_level_merge_block).unwrap();
    }

    // Return then, else and merge blocks
    if !top_level {
      return Ok((then_block, else_block, merge_block));
    } else {
      return Ok((then_block, else_block, top_level_merge_block));
    }
  }

  fn build_conditional_block(&mut self, block: BasicBlock<'ctx>, body: Option<Box<Vec<Expression>>>, function: FunctionValue<'ctx>, top_level_merge_block: BasicBlock<'ctx>, loop_cond_block: Option<BasicBlock<'ctx>>, exit_loop_block: Option<BasicBlock<'ctx>>) -> Result<BasicBlock<'ctx>, Error> {
    self.builder.position_at_end(block);
    let mut current_block = block;

    if let Some(body) = body {
      for expr in body.iter() {
        match expr.expr.clone() {
          Expr::Conditional(typ, exp, body, ebody) => {
            // This is the end, build the body
            if typ.0 == "else" {
              current_block = self.build_conditional_block(current_block, Some(body), function, top_level_merge_block, loop_cond_block, exit_loop_block)?;
            // This is a new branch, return to the top
            } else {
              (_, _, current_block) = self.build_if(false, typ, exp, body, ebody, function, current_block, top_level_merge_block, loop_cond_block, exit_loop_block)?;
            }
          },
          Expr::Assignment(_, _, _) => {
            self.build_assignment(expr.clone(), block, false)?;
          },
          Expr::Return(ret) => {
            let val = self.visit(&ret, block)?;
            if val != self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum() {
              self.builder.build_return(Some(&val)).unwrap();
            // Null != void
            } else {
              self.builder.build_return(None).unwrap();
            }
          },
          Expr::ForLoop(value, expr, body) => {
            current_block = self.build_for_loop(value, expr, body, function, current_block)?;
          },
          Expr::WhileLoop(expr, body) => {
            current_block = self.build_while_loop(expr, body, function, current_block)?;
          },
          Expr::BreakLoop => {
            if exit_loop_block.is_none() {
              return Err(Error::new(
                Error::CompilerError,
                None,
                &"",
                Some(0),
                Some(0),
                "Cannot break outside of loop".to_string()
              ));
            }

            // TODO: Break out of loop
            // We still need to build else block(s) below so we can't return here

          },
          Expr::ContinueLoop => {
            if exit_loop_block.is_none() {
              return Err(Error::new(
                Error::CompilerError,
                None,
                &"",
                Some(0),
                Some(0),
                "Cannot continue outside of loop".to_string()
              ));
            }

            // TODO: Continue loop (branch back to loop condition block)
            // We still need to build else block(s) below so we can't return here

          }
          _ => {
            self.visit(expr, current_block)?;
          }
        }
      }
    }

    return Ok(current_block);
  }

  fn build_assignment(&mut self, expr: Expression, block: BasicBlock<'ctx>, is_new_scope: bool) -> Result<(), Error> {
    // Position the builder at the end of the current block
    self.builder.position_at_end(block);

    // Extract assignment values
    let (ty, ident_or_idx, value) = match expr.clone().expr {
      Expr::Assignment(ty, ident_or_idx, value) => (ty, ident_or_idx, value),
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

    // Array index isn't typed so we need to extract the type
    if let Expr::ArrayIndex(_, _) = ident_or_idx.expr.clone() {
      let (ptr, val) = self.build_index(&ident_or_idx)?;
      let set = self.visit(&value.clone().unwrap(), block)?;
      if set.get_type() != val.get_type() {
        return Err(Error::new(
          Error::CompilerError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          Some(self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string().len() as i32),
          format!("Invalid assignment type for array index, expected {:?} but got {:?}", val.get_type().print_to_string(), set.get_type().print_to_string())
        ));
      }
      self.builder.build_store(ptr, set).unwrap();
      return Ok(());
    }

    // Extract identifier name
    let name = match ident_or_idx.expr {
      Expr::Literal(Literals::Identifier(Name(name))) => name,
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

    let set: BasicValueEnum;
    if value.is_some() {
      if let Expr::Literal(e) = &value.as_ref().unwrap().expr {
        if let Literals::Object(obj) = e {
          let alloca = self.build_initialize_struct(ty.clone().unwrap().0, name.clone(), obj.to_vec(), block)?;
          set = alloca.as_basic_value_enum();
        } else {
          set = self.visit(&value.unwrap(), block)?;
        }

      // If assignment value is an array, we need to extract the type
      } else if let Expr::Array(_) = value.as_ref().unwrap().expr.clone() {
        let (ptr, arr_type, val) = self.visit_array(*value.unwrap(), block)?;
        self.set_symbol(name, Some(ptr), arr_type.as_basic_type_enum(), val);
        return Ok(());

      } else {
        set = self.visit(&value.unwrap(), block)?;
      }
    // If no value is provided, set to null
    } else {
      set = self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum();
    }

    // TODO: Check for constants

    // Setting new variable
    if let Some(Type(ty)) = ty {

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

      match ty.as_str() {
        "let" => {
          match set {
            BasicValueEnum::IntValue(i) => {
              // Allocate space and store the value
              let ptr = self.builder.build_alloca(self.context.i64_type(), &name).unwrap();
              self.builder.build_store(ptr, i).unwrap();

              // Build tagged pointer
              // let tagged = self.tag_ptr(ptr, *self.type_tags.get("int").unwrap());

              // Set the symbol
              self.set_symbol(name, Some(ptr), i.get_type().as_basic_type_enum(), i.into());
            },
            BasicValueEnum::FloatValue(f) => {
              // Allocate space and store the value
              let ptr = self.builder.build_alloca(self.context.f64_type(), &name).unwrap();
              self.builder.build_store(ptr, f).unwrap();

              // Build tagged pointer
              // let tagged = self.tag_ptr(ptr, *self.type_tags.get("float").unwrap());

              // Set the symbol
              self.set_symbol(name, Some(ptr), f.get_type().as_basic_type_enum(),f.into());
            },
            BasicValueEnum::StructValue(s) => {
              let var = self.builder.build_alloca(s.get_type(), &name).unwrap();
              self.builder.build_store(var, s).unwrap();
              self.set_symbol(name, Some(var), s.get_type().as_basic_type_enum(), s.into());
            },
            // Strings
            BasicValueEnum::ArrayValue(a) => {
              let var = self.builder.build_alloca(a.get_type(), &name).unwrap();
              self.builder.build_store(var, a).unwrap();
              self.set_symbol(name, Some(var), a.get_type().as_basic_type_enum(), a.into());
            },
            BasicValueEnum::PointerValue(a) => {
              let var = self.builder.build_alloca(self.context.ptr_type(AddressSpace::default()), &name).unwrap();
              self.builder.build_store(var, a).unwrap();
              self.set_symbol(name, Some(var), a.get_type().as_basic_type_enum(), a.into());
            },
            _ => panic!("Invalid type for let"),
          }
        },
        "const" => {
          match set {
            BasicValueEnum::IntValue(i) => {
              let var = self.builder.build_alloca(self.context.i64_type(), &name).unwrap();
              self.builder.build_store(var, i).unwrap();
              self.set_symbol(name, Some(var), i.get_type().as_basic_type_enum(), var.as_basic_value_enum());
            },
            BasicValueEnum::FloatValue(f) => {
              let var = self.builder.build_alloca(self.context.f64_type(), &name).unwrap();
              self.builder.build_store(var, f).unwrap();
              self.set_symbol(name, Some(var), f.get_type().as_basic_type_enum(), var.as_basic_value_enum());
            },
            _ => panic!("Invalid type for const"),
          }
        },
        _ => {
          if ["int", "bool", "float", "string"].contains(&ty.as_str()) {
            let var = self.builder.build_alloca(set.get_type(), &name).unwrap();
            self.builder.build_store(var, set).unwrap();
            self.set_symbol(name, Some(var), set.get_type(), set);
          } else {
            // Custom type (we know exists from builder)
            self.set_symbol(name, None, set.get_type(), set);
          }
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
      self.set_symbol(name, named, set.get_type(), set);
    }

    return Ok(());
  }

  // TODO: Fix, clean up & document
  fn _tag_ptr(&self, ptr: PointerValue<'ctx>, tag: u64) -> PointerValue<'ctx> {
    let int_ptr = self.builder.build_ptr_to_int(ptr, self.context.i64_type(), "ptr_to_int").unwrap();
    let tag_value = self.context.i64_type().const_int(tag, false);
    let tagged_int = self.builder.build_or(int_ptr, tag_value, "tagged_ptr").unwrap();
    return self.builder.build_int_to_ptr(tagged_int, ptr.get_type(), "tagged_int_ptr").unwrap();
  }

  // TODO: Fix, clean up & document
  fn _extract_tag(&self, ptr: PointerValue<'ctx>) -> (PointerValue<'ctx>, IntValue<'ctx>) {
    let int_ptr = self.builder.build_ptr_to_int(ptr, self.context.i64_type(), "ptr_as_int").unwrap();
    let mask = self._type_tags.get("mask").unwrap();
    let tag = self.builder.build_and(int_ptr, self.context.i64_type().const_int(*mask, false), "extract_tag").unwrap();
    let untag_value = self.context.i64_type().const_int(!*mask, false);
    let untagged_int_ptr = self.builder.build_and(int_ptr, untag_value, "untagged_ptr").unwrap();
    let untagged_ptr = self.builder.build_int_to_ptr(untagged_int_ptr, ptr.get_type(), "untagged_ptr_value").unwrap();
    return (untagged_ptr, tag);
  }

  // TODO: Replace with less specific case & fix
  // Probably need to build phi nodes to come out of loading dynamic types
  fn _load_var_to_type(&self, name: String) -> Result<(), Error> {
    let (_, tagged_ptr, _, _, _exited_func) = self.get_symbol(&name)?.unwrap();
    let (_, tag) = self._extract_tag(tagged_ptr.unwrap());
    // let tag_value = self.builder.build_int_z_extend(tag, self.context.i64_type(), "tag_value").unwrap();

    let a = self.builder.build_int_compare(
      inkwell::IntPredicate::EQ,
      tag,
      self.context.i64_type().const_int(*self._type_tags.get("int").unwrap(), false),
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
                Literals::Identifier(Name(name)) => name,
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
          // TODO: Unwrap to array type for spread operator
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
            defaults.push((i, name, val.unwrap()));

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
      Literals::Identifier(name) => name.0,
      _ => panic!("Invalid return type"),
    });

    // Build function
    let func_type = match returns.as_str() {
      "int" => self.context.i64_type().fn_type(&parameters, variadic),
      "float" => self.context.f64_type().fn_type(&parameters, variadic),
      "void" => self.context.void_type().fn_type(&parameters, variadic),
      "bool" => self.context.bool_type().fn_type(&parameters, variadic),
      _ => {
        if self.struct_table.contains_key(&returns) {
          let (struct_type, _) = self.struct_table.get(&returns).unwrap();
          struct_type.fn_type(&parameters, variadic)
        } else {
          return Err(Error::new(
            Error::NameError,
            None,
            &"",
            Some(0),
            Some(0),
            format!("Type {} not found", returns)
          ));
        }
      },
    };
    let function = self.module.add_function(&name.0, func_type, None);

    // Create a new block for function body
    let mut func_block = self.context.append_basic_block(function, format!("{}-entry", name.0).as_str());
    self.builder.position_at_end(func_block);

    // Set up function parameters
    for (i, param) in function.get_params().iter().enumerate() {

      // Get name
      let name: String = match params.clone().unwrap()[i].clone().expr {
        Expr::FunctionParam(_, _, Literals::Identifier(Name(name)), _, _) => name,
        _ => panic!("Invalid function parameter"),
      };

      // Set parameter name
      param.set_name(&name);

      // if in defaults
      if let Some((_, _, default)) = defaults.iter().find(|(index, _, _)| *index == i) {
        let val = self.visit(&default, func_block)?;
        self.builder.build_store(param.into_pointer_value(), val).unwrap();
        self.set_symbol(name, Some(param.into_pointer_value()), val.get_type(), val);

      // Not in defaults
      } else {
        // param.set_name(&params.clone().unwrap()[i].clone().expr.0);
        // If not typed
        if param.get_type().is_pointer_type() {
          // let var = self.builder.build_alloca(param.get_type(), &param.get_name().to_str().unwrap()).unwrap();
          // self.builder.build_store(var, param.into()).unwrap();
          // self.set_symbol(name, Some(param.into_pointer_value()), *param);

          // TODO: Load the variable to the correct type
          // let (_, tag) = self.extract_tag(param.into_pointer_value());
          // let t = self.get_tag_by_type(param.get_type().as_basic_type_enum());
          // println!("{:?} {:?}", tag, t);
        } else {
          // let var = self.builder.build_alloca(param.get_type(), &param.get_name().to_str().unwrap()).unwrap();
          // self.builder.build_store(var, param.into()).unwrap();

          // self.builder.build_load(param.into(), param).unwrap();
          // self.builder.build_store(param.into_pointer_value(), param.into()).unwrap();

          self.set_symbol(name, None, param.get_type(), *param);
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
        let merge_block = self.context.append_basic_block(function, "merge");
        (_, _, func_block) = self.build_if(true, typ, expr, body, ebody, function, func_block, merge_block, None, None)?;

      } else if let Expr::ForLoop(val, expr, body) = ex.expr.clone() {
        func_block = self.build_for_loop(val, expr, body, function, func_block)?;

      } else if let Expr::WhileLoop(cond, body) = ex.expr.clone() {
        func_block = self.build_while_loop(cond, body, function, func_block)?;

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

  fn build_class(&mut self, expr: Expression, Name(name): Name, parents: Option<Vec<Name>>, body: Option<Box<Vec<Expression>>>, block: BasicBlock<'ctx>) -> Result<(), Error> {

    // Get parents if any
    let mut parent_classes = vec![];
    if let Some(parents) = parents {
      for Name(parent) in parents {
        let parent_class = self.context.get_struct_type(&parent);
        if parent_class.is_none() {
          return Err(Error::new(
            Error::NameError,
            None,
            &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            expr.first_pos,
            expr.first_pos,
            format!("Parent class '{}' not found", parent)
          ));
        }

        parent_classes.push(parent);
      }
    }

    // Collect method signatures
    let mut methods: IndexMap<String, FunctionValue<'ctx>> = IndexMap::new();
    let mut method_bodies: IndexMap<String, Option<Box<Vec<Expression>>>> = IndexMap::new();
    if let Some(body) = body.as_ref() {
      for expr in body.iter() {
        if let Expr::Function(Name(func_name), ret_typ, params, body) = expr.expr.clone() {
          let signature = self.build_class_method_signature(expr.clone(), name.clone(), func_name.clone(), ret_typ, params, body.clone())?;
          let method_name = if func_name == "new" {
            if body.is_none() {
              return Err(Error::new(
                Error::CompilerError,
                None,
                &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                expr.first_pos,
                expr.last_line,
                format!("'{}' constructor is missing body", name)
              ));
            }

            format!("{}_constructor", name)
          } else {
            format!("{}_{}", name, func_name)
          };
          let value = self.module.add_function(&method_name, signature, None);
          methods.insert(func_name.clone(), value);
          method_bodies.insert(func_name, body);
        }
      }
    }

    // Check for constructor
    if methods.is_empty() || methods.iter().filter(|(name, _)| name == &&"new").count() == 0 {
      return Err(Error::new(
        Error::CompilerError,
        None,
        &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
        expr.first_pos,
        // + 4 for `cls `
        Some(expr.first_pos.unwrap() + 4 + name.len() as i32),
        format!("Class '{}' is missing constructor", name)
      ));
    }

    // Create the class struct
    let class_struct = self.context.opaque_struct_type(&name);
    let mut types = vec![];
    let mut fields: Vec<(String, BasicTypeEnum)> = vec![];

    // Add parent class props and methods to class
    for parent in &parent_classes {
      let (_, parent_fields, parent_methods) = self.class_table.get(parent).unwrap();

      // Add parent fields to the class struct
      for (field_name, field_type) in parent_fields {
        types.push(field_type.clone());
        fields.push((field_name.clone(), field_type.clone()));
      }

      // Add parent methods to the class
      if parent_methods.is_some() {
        for (method_name, method) in parent_methods.as_ref().unwrap().into_iter() {
          if method_name != "new" {
            methods.insert(method_name.clone(), method.clone());
          }
        }
      }
    }

    // Use the constructor body to determine the rest of the struct type

    // Set up the constructor
    let constructor = self.module.get_function(&format!("{}_constructor", name)).unwrap();
    let constructor_entry = self.context.append_basic_block(constructor, "entry");
    self.builder.position_at_end(constructor_entry);

    // Build the constructor body
    let mut constructor_fields: Vec<(String, BasicTypeEnum<'ctx>)> = vec![];
    let mut values: Vec<BasicValueEnum<'ctx>> = vec![];
    // We know constructor exists
    for ex in method_bodies.get("new").unwrap().to_owned().unwrap().into_iter() {
      if let Expr::Assignment(_, lhs, rhs) = ex.expr.clone() {
        if let Expr::Index(parent, child) = lhs.expr.clone() {
          if let Expr::SelfRef = parent.expr {
            let (ptr, val) = self.build_index(&parent)?;
            let set = self.visit(&rhs.unwrap(), constructor_entry)?;
            if set.get_type() != val.get_type() {
              return Err(Error::new(
                Error::CompilerError,
                None,
                &self.code.lines().nth(ex.first_line.unwrap() as usize).unwrap().to_string(),
                ex.first_pos,
                ex.last_line,
                format!("Invalid assignment type for array index, expected {:?} but got {:?}", val.get_type().print_to_string(), set.get_type().print_to_string())
              ));
            }
            values.push(set);
            // self.builder.build_store(ptr, set).unwrap();
          } else {
            self.visit(&ex, constructor_entry)?;
          }
        } else {
          self.visit(&ex, constructor_entry)?;
        }
      } else if let Expr::Function(_, _, _, _) = ex.expr.clone() {
        continue;
      } else {
        self.visit(&ex, constructor_entry)?;
      }
    }

    self.builder.position_at_end(constructor_entry);
    self.builder.build_return(None).unwrap();

    // Set the body of the class struct
    class_struct.set_body(&types, false);

    println!("{:?}", class_struct);

    // Add the class struct to the struct table
    self.struct_table.insert(name.clone(), (class_struct, vec![]));
    self.class_table.insert(name.clone(), (Some(parent_classes), fields, None));

    return Ok(());
  }

  fn build_class_method_signature(&mut self, expr: Expression, class_name: String, method_name: String, ret_typ: Option<Literals>, params: Option<Box<Vec<Expression>>>, body: Option<Box<Vec<Expression>>>) -> Result<FunctionType<'ctx>, Error> {
    // Collect the parameter types
    let mut types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];
    if params.is_some() {
      for param in params.unwrap().into_iter() {
        let Type(typ) = match param.clone().expr {
          Expr::FunctionParam(ty, _, _, _, _) => {
            if ty.is_none() || ty.as_ref().unwrap().0 == "let" {
              return Err(Error::new(
                Error::CompilerError,
                None,
                &self.code.lines().nth((param.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                param.first_pos,
                param.first_pos,
                "Method parameters must be typed".to_string()
              ));
            } else {
              ty.unwrap()
            }
          },
          _ => panic!("Invalid function parameter"),
        };

        let t: BasicTypeEnum = match typ.as_str() {
          "int" => self.context.i64_type().into(),
          "float" => self.context.f64_type().into(),
          "bool" => self.context.bool_type().into(),
          _ => {
            if self.struct_table.contains_key(&typ) {
              let (struct_type, _) = self.struct_table.get(&typ).unwrap();
              struct_type.as_basic_type_enum()
            } else {
              return Err(Error::new(
                Error::NameError,
                None,
                &self.code.lines().nth((param.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                param.first_pos,
                Some(typ.len() as i32),
                format!("Type {} not found", typ)
              ));
            }
          },
        };

        types.push(t.into());
      }
    }

    // Collect the function type
    let function_type = if ret_typ.is_some() {
      match ret_typ.unwrap() {
        Literals::Identifier(Name(name)) => {
          match name.as_str() {
            "int" => self.context.i64_type().fn_type(&types, false),
            "float" => self.context.f64_type().fn_type(&types, false),
            "bool" => self.context.bool_type().fn_type(&types, false),
            "void" => self.context.void_type().fn_type(&types, false),
            _ => {
              if self.struct_table.contains_key(&name) {
                let (struct_type, _) = self.struct_table.get(&name).unwrap();
                struct_type.as_basic_type_enum().fn_type(&types, false)
              } else {
                return Err(Error::new(
                  Error::NameError,
                  None,
                  &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                  expr.first_pos,
                  expr.first_pos,
                  format!("Type '{}' not found", name)
                ));
              }
            },
          }
        },
        _ => panic!("Invalid return type"),
      }
    } else {
      self.context.void_type().fn_type(&types, false)
    };

    return Ok(function_type);
  }

  fn build_index(&mut self, expr: &Expression) -> Result<(PointerValue<'ctx>, BasicValueEnum<'ctx>), Error> {
    let (parent, index) = match expr.clone().expr {
      Expr::Index(parent, index) => (parent, index),
      Expr::ArrayIndex(parent, index) => (parent, index),
      _ => {
        return Err(Error::new(
          Error::ParserError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          "Invalid index".to_string()
        ));
      }
    };

    // TODO: Think about how to handle other types of indexing & recursive indexing
    if let Expr::Index(_, _) = parent.expr {
      return self.build_index(&parent);
    }

    if let Expr::Literal(Literals::Identifier(Name(n))) = parent.expr {
      let symbol = self.get_symbol(&n)?;
      if symbol.is_none() {
        return Err(Error::new(
          Error::NameError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          format!("Variable {} not found", n)
        ));
      }

      let (_, ptr, typ, _val, _) = symbol.unwrap();
      if ptr.is_none() {
        return Err(Error::new(
          Error::CompilerError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          format!("Variable {} is not an array", n)
        ));
      }

      // TODO: Return to this code - will need to extract length when we perform bounds checking
      // Get the array value and length from the array struct type
      // let (_array, _array_len) = if typ.is_array_type() {
      //   let struct_load = self.builder.build_load(
      //     self.context.struct_type(&[self.context.i64_type().into(), self.context.ptr_type(AddressSpace::default()).into()], false),
      //     ptr.unwrap(), "load_array"
      //   ).unwrap();
      //   (
      //     self.builder.build_extract_value(struct_load.into_struct_value(), 1, "arr_ptr").unwrap(),
      //     self.builder.build_extract_value(struct_load.into_struct_value(), 0, "arr_len").unwrap().into_int_value()
      //   )
      // } else {
      //   return Err(Error::new(
      //     Error::CompilerError,
      //     None,
      //     &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
      //     expr.first_pos,
      //     Some(expr.first_pos.unwrap() + 1),
      //     format!("Variable {} is not an array", n)
      //   ));
      // };

      // Get the array's element type
      // TODO: This will have to be modified for nested and dynamic arrays
      let element_type = typ.into_array_type().get_element_type();

      // Load the value to index with
      let idx = self.visit(&index, self.builder.get_insert_block().unwrap())?.into_int_value();

      // TODO: Bounds checking - check index against array_len
      // Will need to return the continue block so building can continue
      // Maybe time for a runtime error / error function?
      // Think about how to handle try/catch / how we'll handle errors in the language

      // let cmp = self.builder.build_int_compare(inkwell::IntPredicate::UGE, idx, array_len, "cmp").unwrap();

      // let error_block = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "error");
      // let continue_block = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "continue");
      // self.builder.build_conditional_branch(cmp, error_block, continue_block).unwrap();

      // // Error block
      // self.builder.position_at_end(error_block);
      // let printf = self.module.get_function("printf").unwrap();
      // let string = self.context.const_string("Array index out of bounds\n".as_bytes(), true);
      // self.builder.build_call(printf, &[string.into()], "printf").unwrap();
      // self.builder.build_return(None).unwrap();

      // // Continue block
      // self.builder.position_at_end(continue_block);

      // Get the pointer to the index and load its value
      let idx_ptr = unsafe { self.builder.build_gep(element_type, ptr.unwrap(), &[idx], "get_index").unwrap() };
      let val = self.builder.build_load(element_type, idx_ptr, "array_value").unwrap();
      return Ok((idx_ptr, val));
    }

    return Err(Error::new(
      Error::ParserError,
      None,
      &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
      expr.first_pos,
      expr.last_line,
      "Invalid index".to_string()
    ));
  }

  fn visit_array(&mut self, expr: Expression, block: BasicBlock<'ctx>) -> Result<(PointerValue<'ctx>, ArrayType<'ctx>, BasicValueEnum<'ctx>), Error> {
    let a = match expr.expr {
      Expr::Array(a) => a,
      _ => {
        return Err(Error::new(
          Error::ParserError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          "Invalid array".to_string()
        ));
      }
    };

    let mut values = Vec::new();
    for v in a.iter() {
      values.push(self.visit(&v, block)?.into_int_value());
    }

    let struct_type = self.context.struct_type(&[
      self.context.i64_type().into(),                        // len
      self.context.ptr_type(AddressSpace::default()).into(), // arr ptr
    ], false);

    let array = self.context.i64_type().const_array(&values);
    let arr_alloca = self.builder.build_alloca(array.get_type(), "arr").unwrap();
    self.builder.build_store(arr_alloca, array).unwrap();

    let array_struct_alloca = self.builder.build_alloca(struct_type, "array_set").unwrap();

    let length = self.context.i64_type().const_int(values.len() as u64, false);
    let length_field_ptr = self.builder.build_struct_gep(struct_type, array_struct_alloca, 0, "arr_len").unwrap();
    self.builder.build_store(length_field_ptr, length).unwrap();

    let array_field_ptr = self.builder.build_struct_gep(struct_type, array_struct_alloca, 1, "arr_ptr").unwrap();
    self.builder.build_store(array_field_ptr, arr_alloca).unwrap();

    return Ok((arr_alloca, array.get_type(), array_struct_alloca.as_basic_value_enum()));
  }

  fn visit(&mut self, expr: &Expression, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {

    // Position the builder at the end of the current block
    self.builder.position_at_end(block);

    match expr.clone().expr {
      Expr::Literal(literal) => self.handle_literal(literal, expr),
      Expr::BinaryOperator(op, lhs, rhs) => self.handle_binop(op, lhs, rhs, block),
      Expr::UnaryOperator(op, value) => self.handle_unop(op, value, block),
      Expr::Array(_) => panic!("Array not allowed in visit(), has its own visitor"),
      Expr::ArrayIndex(_, _) => {
        let (_, val) = self.build_index(expr)?;
        return Ok(val);
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
            Some(expr.first_pos.unwrap() + name.0.len() as i32),
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
        let mut ptrs: IndexMap<usize, Option<PointerValue<'ctx>>> = IndexMap::new();

        // Build args
        for (i, arg) in args.iter().enumerate() {
          let val: BasicValueEnum<'ctx>;

          // If arg is an identifier (variable) get the pointer and value if possible
          if let Expr::Literal(Literals::Identifier(Name(name))) = arg.expr.clone() {
            let (ptr, _, value) = self.var_from_ident(name, arg)?;
            val = value;
            if ptr.is_some() {
              ptrs.insert(i, ptr);
            }

          // Get the value
          } else {
            val = self.visit(&arg, block)?;
          }
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
              // If we can get original pointer of value
              if ptrs.get(&i).is_some() {
                meta[i] = ptrs.get(&i).unwrap().unwrap().into();
              // Build new pointer
              } else {
                let ptr = self.builder.build_alloca(param_type, "ptr").unwrap();
                // self.tag_ptr(ptr, self.get_tag_by_type(values[i].get_type()));
                self.builder.build_store(ptr, values[i]).unwrap();
                meta[i] = ptr.into();
              }
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
      Expr::Index(parent, child) => {
        return self.build_index_extract(parent, child, block);
      }
      Expr::Assignment(_, _, _) => panic!("Assignment not allowed here"),
      _ => panic!("Invalid expression"),
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
      Literals::Identifier(n) => Ok(self.var_from_ident(n.0, expr)?.2),
      Literals::EOF => todo!(),
    }
  }

  fn var_from_ident(&self, name: String, expr: &Expression) -> Result<(Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>), Error> {
    if let Some((_, ptr, typ, val, _exited_func)) = self.get_symbol(&name)? {
      // Constant value
      if ptr.is_none() || typ.is_array_type() {
        return Ok((None, typ, val));
      // If there's a pointer, load the value behind the identifier
      } else {
        let var = self.builder.build_load(typ, ptr.unwrap(), &name).unwrap();
        return Ok((ptr, typ, var));
      }
    } else {
      return Err(Error::new(
        Error::CompilerError,
        None,
        &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
        expr.first_pos,
        Some(expr.first_pos.unwrap() + name.len() as i32),
        format!("Unknown identifier: {}", name)
      ));
    }
  }

  fn handle_unop(&mut self, op: Operator, value: Box<Expression>, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
    let val: BasicValueEnum<'ctx>;
    let mut name: Option<String> = None;
    if let Expr::Literal(Literals::Identifier(n)) = value.clone().expr {
      val = self.get_symbol(&n.0)?.unwrap().3;
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
            if val.is_pointer_value() {
              let null = self.context.ptr_type(AddressSpace::default()).const_null();
              let cmp = self.builder.build_int_compare(inkwell::IntPredicate::EQ, val.into_pointer_value(), null, "notcmp").unwrap();
              return Ok(self.builder.build_int_z_extend(cmp, self.context.i64_type(), "nottmp").unwrap().as_basic_value_enum());
            }
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
                self.set_symbol(name.unwrap(), Some(a), v.get_type().as_basic_type_enum(), v.as_basic_value_enum());
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
                self.set_symbol(name.unwrap(), Some(a), v.get_type().as_basic_type_enum(), v.as_basic_value_enum());
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
                self.set_symbol(name.unwrap(), Some(a), v.get_type().as_basic_type_enum(), v.as_basic_value_enum());
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
                self.set_symbol(name.unwrap(), Some(a), v.get_type().as_basic_type_enum(), v.as_basic_value_enum());
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
      (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => {

        // If only one of the values is a bool, convert it to an int
        if a.get_type().get_bit_width() == 1 && b.get_type().get_bit_width() == 64 {
          left = self.builder.build_int_z_extend(a, self.context.i64_type(), "left_int").unwrap().as_basic_value_enum();
        } else if a.get_type().get_bit_width() == 64 && b.get_type().get_bit_width() == 1 {
          right = self.builder.build_int_z_extend(b, self.context.i64_type(), "right_int").unwrap().as_basic_value_enum();
        }

        "int".to_string()
      },
      (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
        // Get symbols from pointer names
        println!("a: {:?}\nb: {:?}", a, b);
        let (_, a_inst, a_type, _a_val, _) = self.get_symbol(&a.get_name().to_string_lossy())?.unwrap();
        let (_, b_inst, b_type, _b_val, _) = self.get_symbol(&b.get_name().to_string_lossy())?.unwrap();

        // panic!("Pointer arithmetic not yet implemented");
        let a_value = self.builder.build_load(a_type, a_inst.unwrap(), "lhs_val").unwrap();
        let b_value = self.builder.build_load(b_type, b_inst.unwrap(), "rhs_val").unwrap();

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
        } else {
          panic!("Invalid types for operation");
        }
      },
      (BasicValueEnum::PointerValue(a), BasicValueEnum::IntValue(_)) => {
        let (_, a_inst, _, _a_val, _) = self.get_symbol(&a.get_name().to_string_lossy())?.unwrap();
        let a_value = self.builder.build_load(self.context.i64_type(), a_inst.unwrap(), "lhs_val").unwrap();
        left = a_value.as_basic_value_enum();
        "int".to_string()
      },
      (BasicValueEnum::IntValue(_), BasicValueEnum::PointerValue(b)) => {
        let (_, b_inst, _, _b_val, _) = self.get_symbol(&b.get_name().to_string_lossy())?.unwrap();
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
          "<<" => {
            return Ok(self.builder.build_left_shift(left.into_int_value(), right.into_int_value(), "shltmp").unwrap().as_basic_value_enum());
          },
          ">>" => {
            return Ok(self.builder.build_right_shift(left.into_int_value(), right.into_int_value(), false, "shrtmp").unwrap().as_basic_value_enum());
          },
          "&&" => {
            return Ok(self.builder.build_and(left.into_int_value(), right.into_int_value(), "andtmp").unwrap().as_basic_value_enum());
          },
          "||" => {
            return Ok(self.builder.build_or(left.into_int_value(), right.into_int_value(), "ortmp").unwrap().as_basic_value_enum());
          },
          "==" => {
            match typ.as_str() {
              "float" => {
                return Ok(self.builder.build_float_compare(inkwell::FloatPredicate::OEQ, left.into_float_value(), right.into_float_value(), "eqtmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_compare(inkwell::IntPredicate::EQ, left.into_int_value(), right.into_int_value(), "eqtmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for equality"), // TODO: pretty up errors
            }
          },
          "!=" => {
            match typ.as_str() {
              "float" => {
                return Ok(self.builder.build_float_compare(inkwell::FloatPredicate::ONE, left.into_float_value(), right.into_float_value(), "netmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_compare(inkwell::IntPredicate::NE, left.into_int_value(), right.into_int_value(), "netmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for inequality"), // TODO: pretty up errors
            }
          },
          ">" => {
            match typ.as_str() {
              "float" => {
                return Ok(self.builder.build_float_compare(inkwell::FloatPredicate::OGT, left.into_float_value(), right.into_float_value(), "gttmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_compare(inkwell::IntPredicate::SGT, left.into_int_value(), right.into_int_value(), "gttmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for greater than"), // TODO: pretty up errors
            }
          },
          "<" => {
            match typ.as_str() {
              "float" => {
                return Ok(self.builder.build_float_compare(inkwell::FloatPredicate::OLT, left.into_float_value(), right.into_float_value(), "lttmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_compare(inkwell::IntPredicate::SLT, left.into_int_value(), right.into_int_value(), "lttmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for less than"), // TODO: pretty up errors
            }
          },
          ">=" => {
            match typ.as_str() {
              "float" => {
                return Ok(self.builder.build_float_compare(inkwell::FloatPredicate::OGE, left.into_float_value(), right.into_float_value(), "getmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_compare(inkwell::IntPredicate::SGE, left.into_int_value(), right.into_int_value(), "getmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for greater than or equal"), // TODO: pretty up errors
            }
          },
          "<=" => {
            match typ.as_str() {
              "float" => {
                return Ok(self.builder.build_float_compare(inkwell::FloatPredicate::OLE, left.into_float_value(), right.into_float_value(), "letmp").unwrap().as_basic_value_enum());
              },
              "int" => {
                return Ok(self.builder.build_int_compare(inkwell::IntPredicate::SLE, left.into_int_value(), right.into_int_value(), "letmp").unwrap().as_basic_value_enum());
              },
              _ => panic!("Invalid types for less than or equal"), // TODO: pretty up errors
            }
          },
          "+=" => {
            match typ.as_str() {
              "float" => {
                if let Expr::Literal(Literals::Identifier(n)) = lhs.clone().expr {
                  let a = self.get_symbol(&n.0)?.unwrap().1.unwrap();
                  if a.is_const() {
                    panic!("Cannot assign to a constant");
                  } else {
                    let val = self.builder.build_float_add(left.into_float_value(), right.into_float_value(), "addtmp").unwrap();
                    self.builder.build_store(a, val).unwrap();
                    self.set_symbol(n.0, Some(a), val.get_type().as_basic_type_enum(), val.as_basic_value_enum());
                    return Ok(val.as_basic_value_enum());
                  }
                } else {
                  return Ok(self.builder.build_float_add(left.into_float_value(), right.into_float_value(), "addtmp").unwrap().as_basic_value_enum());
                }
              },
              "int" => {
                if let Expr::Literal(Literals::Identifier(n)) = lhs.clone().expr {
                  let a = self.get_symbol(&n.0)?.unwrap().1.unwrap();
                  if a.is_const() {
                    panic!("Cannot assign to a constant");
                  } else {
                    let val = self.builder.build_int_add(left.into_int_value(), right.into_int_value(), "addtmp").unwrap();
                    self.builder.build_store(a, val).unwrap();
                    self.set_symbol(n.0, Some(a), val.get_type().as_basic_type_enum(), val.as_basic_value_enum());
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
  use std::fs;
  use std::fs::File;
  use std::io::Write;
  use std::sync::atomic::{AtomicUsize, Ordering};
  static COUNTER: AtomicUsize = AtomicUsize::new(0);
  use super::*;

  // Compile the IR and assert the output matches
  fn build_and_assert(ir: LLVMString, output: &str) {
    let test_count = COUNTER.fetch_add(1, Ordering::SeqCst);
    let llvm_ir_name = format!("test{}.ll", test_count);
    let executable_name = format!("test{}", test_count);

    // Generate LLVM IR file
    let mut file = File::create(format!("./{}", &llvm_ir_name)).unwrap();
    file.write_all(ir.to_bytes()).unwrap();

    // Compile and execute the binary
    #[cfg(target_os = "windows")]
    std::process::Command::new("clang")
      .arg("-o")
      .arg(format!("{}.exe", &executable_name)) // `test1.exe`
      .arg(&llvm_ir_name)                      // `test1.ll`
      .output()
      .expect("Failed to compile LLVM IR");
    #[cfg(not(target_os = "windows"))]
    std::process::Command::new("clang")
      .arg("-o")
      .arg(&executable_name) // `test1`
      .arg(&llvm_ir_name)   // `test1.ll`
      .output()
      .expect("Failed to compile LLVM IR");
    #[cfg(target_os = "windows")]
    let test = std::process::Command::new(format!("./{}.exe", &executable_name))
      .output()
      .expect("Failed to run executable");
    #[cfg(not(target_os = "windows"))]
    let test = std::process::Command::new(format!("./{}", &executable_name))
      .output()
      .expect("Failed to run executable");

    // Delete the test execution files
    fs::remove_file(llvm_ir_name).unwrap();
    #[cfg(target_os = "windows")]
    fs::remove_file(format!("{}.exe", executable_name)).unwrap();
    #[cfg(not(target_os = "windows"))]
    fs::remove_file(executable_name).unwrap();

    // Assert the output is correct
    if !test.status.success() && String::from_utf8_lossy(&test.stderr) != "" {
      eprintln!("Error: {}", String::from_utf8_lossy(&test.stderr));
      assert!(false);
    } else {
      assert_eq!(String::from_utf8_lossy(&test.stdout), output);
    }
  }

  #[test]
  fn test_compile_assignment_reassignment() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let a = 1;
    let b = 2;
    a = 3;
    let c = a + b;
    printf(\"%d\", c);".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "5");
    return Ok(());
  }

  #[test]
  fn test_compile_multi_binop() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let a = (1 + 2) * 3;
    let b = 3;
    printf(\"%d\", a / b);".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "3");
    return Ok(());
  }

  #[test]
  fn test_assign_array_and_indexing() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let a = [1, 2, 3];
    let b = a[2];
    printf(\"%d\", b);".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "3");
    return Ok(());
  }

  #[test]
  fn test_array_reassign_index() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let a = [1, 2, 3];
    a[0] = 4;
    let b = a[0];
    printf(\"%d\", b);".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "4");
    return Ok(());
  }

  #[test]
  fn test_for_loop() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let array = [1, 2, 3, 4, 5];
    for (let i in array) {
      printf(\"%d\", array[i]);
    }".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "12345");
    return Ok(());
  }

  #[test]
  fn test_while_loop() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let i = 0;
    while (i < 5) {
      printf(\"%d\", i);
      i = i + 1;
    }".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "01234");
    return Ok(());
  }

  #[test]
  fn test_conditional_phi() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "let a = 1;
    if (a == 1) {
      a = 5;
    } else {
      a = 10;
    }
    printf(\"%d\", a);".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "5");
    return Ok(());
  }

  #[test]
  fn test_type_struct_index() -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let code = "type Test = {
      a: int,
      b: int,
    }
    Test test = { a: 1, b: 2 };
    printf(\"%d\", test.b);".to_string();
    let mut parser = crate::Parser::new();
    let ast = parser.parse(code.clone(), String::new(), false, false)?;
    let ir = codegen.build(code, ast, IndexMap::new(), false, true)?;
    build_and_assert(ir, "2");
    return Ok(());
  }
}