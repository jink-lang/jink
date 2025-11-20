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

use jink::{Error, Expr, Expression, JType, Literals, Name, Operator, Type};

use super::parser::Namespace;

struct Scope<'ctx> {
  symbols: IndexMap<String, (Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)>,
  /// Table of struct and enum types
  struct_enum_types: IndexMap<
    String,                // Struct type name
    Vec<(                  // Struct fields
      String,              // Field name
      BasicTypeEnum<'ctx>, // Field type
      Option<String>       // Struct type name if field is a struct
    )>
  >,
  /// Map known struct variables to their corresponding struct type names
  struct_map: IndexMap<String, String>,
  /// Map known enum variables to their enum type names
  enum_map: IndexMap<
    String,                // Enum type name
    String                 // Enum variants
  >,
  is_function: bool,
}

#[derive(Debug, Clone)]
struct FunctionCtx<'ctx> {
  llvm_fun: FunctionValue<'ctx>,
  parameters: Vec<FunctionParamCtx<'ctx>>,
  _ret_type: String,
  variadic: bool,
}

#[derive(Debug, Clone)]
struct FunctionParamCtx<'ctx> {
  name: String,
  jtype: JType,
  _llvm_type: BasicMetadataTypeEnum<'ctx>,
  default_value_expr: Option<Box<Expression>>,
  is_spread: bool
}

pub struct CodeGen<'ctx> {
  pub code: String,
  pub namespaces: IndexMap<String, Namespace>,
  context: &'ctx Context,
  pub module: Module<'ctx>,
  builder: Builder<'ctx>,
  /// Functions defined in the module
  functions: IndexMap<String, FunctionCtx<'ctx>>,
  /// Stack of symbol tables
  symbol_table_stack: Vec<Scope<'ctx>>,
  /// Table of class types, similar to struct table but with references to parents and methods
  class_table: IndexMap<String, (       // name
    Option<Vec<String>>,                // parents
    Vec<(String, BasicTypeEnum<'ctx>)>, // props (field name, field type) - TODO: We don't support struct types for class fields yet
    Vec<(String, FunctionValue<'ctx>)>  // methods
  )>,
  /// Current class being built if any
  current_class: Option<PointerValue<'ctx>>,
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
      functions: IndexMap::new(),
      symbol_table_stack: Vec::new(),
      class_table: IndexMap::new(),
      current_class: None,
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
    self.symbol_table_stack.push(Scope {
      symbols: IndexMap::new(),
      struct_enum_types: IndexMap::new(),
      struct_map: IndexMap::new(),
      enum_map: IndexMap::new(),
      is_function,
    });
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
  /// 
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

  fn get_all_cur_symbols(&self, get_all_scopes: bool) -> IndexMap<String, (Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>, bool)> {
    let mut symbols = IndexMap::new();
    if self.symbol_table_stack.is_empty() { return symbols; }

    let scopes_to_use = if get_all_scopes {
      self.symbol_table_stack.iter().rev()
    } else {
      std::slice::from_ref(self.symbol_table_stack.last().unwrap()).iter().rev()
    };

    for scope in scopes_to_use {
      // Add all symbols to the emerging in-scope symbols
      for (name, (inst, typ, value)) in scope.symbols.iter() {
        symbols.insert(name.clone(), (inst.clone(), typ.clone(), value.clone(), false));
      }

      // Add structs/enums to emerging in-scope symbols
      for (name, fields) in scope.struct_enum_types.iter() {
        let mut field_names = vec![];
        for (field_name, _, _) in fields.iter() {
          field_names.push(field_name.clone());
        }
        symbols.insert(name.clone(), (None, self.context.ptr_type(AddressSpace::default()).as_basic_type_enum(), self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum(), false));
      }
    }

    return symbols;
  }

  fn set_struct(&mut self, name: String, fields: Vec<(String, BasicTypeEnum<'ctx>, Option<String>)>) {
    if let Some(scope) = self.symbol_table_stack.last_mut() {
      scope.struct_enum_types.insert(name.clone(), fields);
    } else {
      panic!("Unexpected compilation error, set struct type without a scope");
    }
  }

  /// Get struct type from symbol table stack
  /// 
  /// Returns Result->Option of struct fields: name, type and other struct type name if any
  fn get_struct(&self, name: &str) -> Option<&Vec<(String, BasicTypeEnum<'ctx>, Option<String>)>> {
    for scope in self.symbol_table_stack.iter().rev() {
      if let Some(fields) = scope.struct_enum_types.get(name) {
        return Some(fields);
      }
    }
    return None;
  }

  fn set_struct_mapping(&mut self, name: String, struct_name: String) {
    if let Some(scope) = self.symbol_table_stack.last_mut() {
      scope.struct_map.insert(name.clone(), struct_name.clone());
    } else {
      panic!("Unexpected compilation error, set struct map without a scope");
    }
  }

  fn get_struct_mapping(&self, name: &str) -> Option<&String> {
    for scope in self.symbol_table_stack.iter().rev() {
      if let Some(struct_name) = scope.struct_map.get(name) {
        return Some(struct_name);
      }
    }
    return None;
  }

  fn set_enum_mapping(&mut self, name: String, enum_name: String) {
    if let Some(scope) = self.symbol_table_stack.last_mut() {
      scope.enum_map.insert(name.clone(), enum_name.clone());
    } else {
      panic!("Unexpected compilation error, set enum map without a scope");
    }
  }

  fn get_enum_mapping(&self, name: &str) -> Option<&String> {
    for scope in self.symbol_table_stack.iter().rev() {
      if let Some(enum_name) = scope.enum_map.get(name) {
        return Some(enum_name);
      }
    }
    return None;
  }

  fn get_required_function_param_count(&self, params: &[FunctionParamCtx]) -> usize {
    params.iter().filter(|p| p.default_value_expr.is_none() && !p.is_spread).count()
  }

  fn create_main_function(&self) -> FunctionValue<'ctx> {
    let void_type = self.context.void_type().fn_type(&[], false);
    let main_fn = self.module.add_function("main", void_type, None);
    let entry_block = self.context.append_basic_block(main_fn, "entry");
    self.builder.position_at_end(entry_block);
    return main_fn;
  }

  // TODO: Narrow this down to intrinsics
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

    // declare void @llvm.va_start(ptr) - va_list i8* / ptr
    let va_start_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
    self.module.add_function("llvm.va_start", va_start_type, None);

    // declare void @llvm.va_end(ptr)
    let va_end_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
    self.module.add_function("llvm.va_end", va_end_type, None);
  }

  pub fn jtype_to_basic_type(&self, jtype: JType) -> Result<BasicTypeEnum<'ctx>, Error> {
    match jtype {
      JType::Integer => Ok(self.context.i64_type().as_basic_type_enum()),
      JType::UnsignedInteger => Ok(self.context.i64_type().as_basic_type_enum()),
      JType::FloatingPoint => Ok(self.context.f64_type().as_basic_type_enum()),
      JType::String => Ok(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()),
      JType::Boolean => Ok(self.context.bool_type().as_basic_type_enum()),
      JType::Object(_) => Ok(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()),
      JType::Array(_) => Ok(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()),
      JType::Null => Ok(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()),
      JType::Unknown => Ok(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum()),
      _ => {
        return Err(Error::new(
          Error::CompilerError,
          None,
          &"",
          Some(0),
          Some(0),
          format!("Invalid type: {:?}", jtype)
        ));
      }
    }
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
      Expr::TypeDef(Name(name), value) => {
        self.build_struct(Some(name), value)?;
      },
      Expr::Enum(Name(name), variants) => {
        self.build_enum(name, variants)?;
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
        self.build_class(expr, name, parents, body)?;
      },
      Expr::ModuleParsed(path, _, opts, ast) => {
        self.build_module(expr, path, opts, ast, main_fn)?;
      },
      Expr::Public(expr) => {
        block = self.process_top(*expr, main_fn, block)?;
      },
      Expr::Delete(expr) => {
        println!("Delete: {:?}", expr);
      },
      // When we add top level index expressions
      // i.e. hello[0].bye() or hello.bye() or module.property and module.method()
      Expr::Index(parent, child) => {
        self.build_index_extract(true, false, None, parent, child, block)?;
      },
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

  fn build_index_extract(&mut self, getting_inner_parent: bool, passing_type_name: bool, parent_struct_ptr_if_recursing: Option<PointerValue<'ctx>>, parent: Box<Expression>, child: Box<Expression>, block: BasicBlock<'ctx>) -> Result<(BasicValueEnum<'ctx>, Option<String>), Error> {
    // Get struct type name from parent
    let (parent_val, parent_name) = if getting_inner_parent && !passing_type_name {
      let parent_val = self.visit(&parent, block)?;
      // Ensure we are visiting a struct
      // TODO: Do further analysis to ensure that we have a struct when it's a pointer?
      if !parent_val.is_pointer_value() && !parent_val.is_struct_value() {
        return Err(Error::new(
          Error::CompilerError,
          None,
          self.code.lines().nth((parent.first_line.unwrap() - 1) as usize).unwrap(),
          parent.first_pos,
          parent.first_pos,
          "Indexed must be of struct type".to_string()
        ));
      }

      // Hack to make sure the parent name is correct
      // LLVM type name can get loaded with a number for multiple enum references
      if let Expr::Literal(Literals::Identifier(Name(ref name))) = parent.expr {
        (Some(parent_val), name.to_owned())
      } else if let Expr::SelfRef = parent.expr {
        (Some(parent_val), "self".to_string())
      } else {
        (Some(parent_val), parent_val.get_name().to_str().unwrap().to_owned())
      }
    } else {
      match parent.expr {
        Expr::Literal(Literals::Identifier(Name(ref name))) => (
          Some(parent_struct_ptr_if_recursing.unwrap().as_basic_value_enum()),
          name.to_owned()
        ),
        _ => unreachable!(),
      }
    };

    // Get struct information by type
    let struct_type_name = if getting_inner_parent && !passing_type_name {
      self.get_struct_mapping(&parent_name)
    } else {
      Some(&parent_name)
    };
    if struct_type_name.is_none() {
      return Err(Error::new(
        Error::CompilerError,
        None,
        self.code.lines().nth((parent.first_line.unwrap() - 1) as usize).unwrap(),
        parent.first_pos,
        parent.first_pos,
        format!("Unknown symbol: {}", &parent_name)
      ));
    }

    // Get value of child
    // Used at final index as well as passing indexes back through recursively
    if let Expr::Literal(Literals::Identifier(Name(name))) = child.expr {
      let struct_ref = self.get_struct(struct_type_name.unwrap());
      if struct_ref.is_none() {
        return Err(Error::new(
          Error::CompilerError,
          None,
          self.code.lines().nth((parent.first_line.unwrap() - 1) as usize).unwrap(),
          parent.first_pos,
          parent.first_pos,
          format!("Unknown type: {}", struct_type_name.unwrap())
        ));
      }
      let struct_type = self.context.get_struct_type(struct_type_name.unwrap()).unwrap();

      // Try to find the index field on the struct
      for (i, (field_name, _, struct_type_if_any)) in struct_ref.unwrap().iter().enumerate() {
        if field_name == &name {
          // If we already have a struct value we can extract the field directly - currently only enums are passed this way
          let struct_val = if parent_val.unwrap().is_struct_value() {
            parent_val.unwrap().into_struct_value()
          } else {
            self.builder.build_load(struct_type, parent_val.unwrap().into_pointer_value(), "struct_val").unwrap().into_struct_value()
          };
          let value = self.builder.build_extract_value(struct_val, i as u32, "indexed_value").unwrap();
          return Ok((value, struct_type_if_any.clone()));
        }
      }

      // Invalid index at child
      return Err(Error::new(
        Error::CompilerError,
        None,
        self.code.lines().nth((child.first_line.unwrap() - 1) as usize).unwrap(),
        child.first_pos,
        child.first_pos,
        format!("Field `{}` not found on index type `{}`", name, struct_type_name.unwrap())
      ));

    // Nested field access
    } else if let Expr::Index(inner_parent, inner_child) = child.expr {
      // If we are already getting an inner parent, we can't pass the type name
      let (parent_value, field_struct_type_name) = if getting_inner_parent {
        self.build_index_extract(true, false, Some(parent_val.unwrap().into_pointer_value()), parent.clone(), inner_parent, block)?
      } else {
        self.build_index_extract(true, true, Some(parent_val.unwrap().into_pointer_value()), parent.clone(), inner_parent, block)?
      };

      // Wrap the extracted field type in an expression to use as the new parent expr
      // Hacky but it works
      let parent_expr = Expression {
        expr: Expr::Literal(Literals::Identifier(Name(field_struct_type_name.unwrap()))),
        first_line: parent.first_line,
        first_pos: parent.first_pos,
        last_line: parent.last_line,
        inferred_type: parent.inferred_type,
      };

      // Recurse to get the inner child
      if parent_value.is_pointer_value() {
        return self.build_index_extract(false, true, Some(parent_value.into_pointer_value()), Box::new(parent_expr), inner_child, block);
      // We are getting the value next
      } else {
        return self.build_index_extract(false, true, None, Box::new(parent_expr), inner_child, block);
      }

    // Method call
    } else if let Expr::Call(Name(method_name), args) = child.expr {
      let struct_name = struct_type_name.unwrap();
      let full_method_name = format!("{}_{}", struct_name, method_name);

      // Find function
      let llvm_function = self.module.get_function(&full_method_name);
      if llvm_function.is_none() {
        return Err(Error::new(
          Error::NameError,
          None,
          &self.code.lines().nth((child.first_line.unwrap() - 1) as usize).unwrap().to_string(),
          child.first_pos,
          Some(child.first_pos.unwrap() + method_name.len() as i32),
          format!("Method '{}' not found on type '{}'", method_name, struct_name)
        ));
      }
      let llvm_fun = llvm_function.unwrap();

      // Prepare args: self + args
      let mut llvm_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len() + 1);

      // Add self
      if parent_val.unwrap().is_pointer_value() {
        llvm_args.push(parent_val.unwrap().into_pointer_value().into());
      } else {
        let alloca = self.builder.build_alloca(parent_val.unwrap().get_type(), "self_tmp").unwrap();
        self.builder.build_store(alloca, parent_val.unwrap()).unwrap();
        llvm_args.push(alloca.into());
      }

      // Add other args
      for arg in args.iter() {
        let val = self.visit(&arg, block)?;
        llvm_args.push(val.into());
      }

      let call = self.builder.build_call(llvm_fun, &llvm_args, &method_name).unwrap();

      // Return result
      let result = match call.try_as_basic_value().basic() {
        Some(val) => val,
        None => self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum(),
      };

      return Ok((result, None));

    } else {
      // Invalid index at child
      return Err(Error::new(
        Error::CompilerError,
        None,
        self.code.lines().nth((child.first_line.unwrap() - 1) as usize).unwrap(),
        child.first_pos,
        child.first_pos,
        "Struct index must be an identifier".to_string()
      ));
    }
  }

  fn build_loop_body(&mut self, body: Option<Vec<Expression>>, body_block: BasicBlock<'ctx>, continue_block_target: BasicBlock<'ctx>, break_block_target: BasicBlock<'ctx>) -> Result<BasicBlock<'ctx>, Error> {
    self.enter_scope(false);
    let mut current_block = body_block;

    if let Some(body) = body {
      for expr in body {

        if let Expr::Conditional(typ, expr, body, ebody) = expr.expr.clone() {
          let merge_block = self.context.append_basic_block(body_block.get_parent().unwrap(), "merge");
          (_, _, current_block) = self.build_if(true, typ, expr, body, ebody, body_block.get_parent().unwrap(), body_block, merge_block, Some(continue_block_target), Some(break_block_target))?;

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
          self.builder.build_unconditional_branch(break_block_target).unwrap();
          return Ok(current_block);

        } else if let Expr::ContinueLoop = expr.expr.clone() {
          self.builder.build_unconditional_branch(continue_block_target).unwrap();
          return Ok(current_block);

        } else {
          self.visit(&expr, current_block)?;
        }
      }
    }

    // Exit the loop body scope
    self.exit_scope();
    return Ok(current_block);
  }

  fn build_for_loop(&mut self, value: Box<Expression>, expr: Box<Expression>, body: Option<Vec<Expression>>, function: FunctionValue<'ctx>, block: BasicBlock<'ctx>) -> Result<BasicBlock<'ctx>, Error> {
    let cond_block = self.context.append_basic_block(function, "loop_cond_block");
    let body_block = self.context.append_basic_block(function, "loop_body");
    let increment_block = self.context.append_basic_block(function, "loop_var_increment");
    let end_block = self.context.append_basic_block(function, "loop_end");

    self.builder.position_at_end(block);

    // Get loop variable name (e.g., 'i')
    let loop_index_name = if let Expr::Literal(Literals::Identifier(Name(name))) = value.expr {
      name
    } else {
      // Handle error: loop variable must be an identifier
      return Err(Error::new(
        Error::CompilerError,
        None,
        "",
        value.first_pos,
        value.last_line,
        "Loop variable must be an identifier".to_string()
      ));
    };

    // Initialize loop index variable to 0
    let i64_type = self.context.i64_type();
    let init_value = i64_type.const_int(0, false);
    let loop_index_ptr = self.builder.build_alloca(init_value.get_type(), &loop_index_name).unwrap();
    self.builder.build_store(loop_index_ptr, init_value).unwrap();

    let end_condition_val: IntValue<'ctx>;

    // Get the name of the iterable expression ('args' etc.)
    let iterable_name = if let Expr::Literal(Literals::Identifier(Name(n))) = expr.expr.clone() {
      n
    } else {
      // TODO: Handle non-identifier iterables if supported (function calls returning arrays, etc.)
      return Err(Error::new(
        Error::CompilerError,
        None,
        "",
        expr.first_pos,
        expr.last_line,
        "Iterable in for loop must be an identifier for now".to_string()
      ));
    };

    // Check if the iterable is the variadic parameter array for the current function
    let current_function_ctx = self.functions.get(function.get_name().to_str().unwrap());
    let is_variadic_iterable = current_function_ctx.map_or(false, |ctx| {
      // Check if the function is variadic AND if the iterable name matches the name of the spread parameter
      ctx.variadic && ctx.parameters.iter().any(|p| p.is_spread && p.name == iterable_name)
    });

    // For the variadic function array we just get and use the hidden count
    if is_variadic_iterable {
      if function.count_params() == 0 {
        return Err(Error::new(Error::CompilerError,
          None, "",
          expr.first_pos,
          expr.last_line,
          "Internal error: Variadic function seems to be missing its hidden count parameter.".to_string()
        ));
      }
      end_condition_val = function.get_nth_param(0).unwrap().into_int_value();
    } else {
      // Handle regular arrays/iterables
      let (_iterable_ptr_opt, iterable_type, _iterable_val) = self.var_from_ident(iterable_name.clone(), &expr)?;

      // Case: Compile-time array
      if iterable_type.is_array_type() {
        let array_len = iterable_type.into_array_type().len();
        end_condition_val = i64_type.const_int(array_len as u64, false);

      // Case: Dynamic array struct { len, ptr }
      } else if iterable_type.is_struct_type() {
        let struct_ptr = _iterable_ptr_opt.ok_or_else(|| Error::new(Error::CompilerError, None, "", expr.first_pos, expr.last_line, "Expected pointer for dynamic array struct".to_string()))?;
        let array_struct_type = self.context.struct_type(&[i64_type.into(), self.context.ptr_type(AddressSpace::default()).into()], false);

        // Ensure the pointer points to the expected struct type
        if struct_ptr.get_type().as_basic_type_enum() != array_struct_type.as_basic_type_enum() {
          return Err(Error::new(
            Error::CompilerError,
            None,
            "",
            expr.first_pos,
            expr.last_line,
            format!("Iterable '{}' is not the expected dynamic array type.", iterable_name).to_string()
          ));
        }

        // Load the length field (index 0)
        let length_ptr = self.builder.build_struct_gep(array_struct_type, struct_ptr, 0, "dyn_arr_len_ptr").unwrap();
        end_condition_val = self.builder.build_load(i64_type, length_ptr, "dyn_arr_len").unwrap().into_int_value();
      } else {
        return Err(Error::new(
          Error::CompilerError,
          None,
          self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap(),
          expr.first_pos,
          expr.first_pos,
          format!("'{}' not recognized as a valid iterable type.", iterable_name).to_string()
        ));
      }
    }

    // Initialize the conditional block to ascertain whether the loop should continue
    self.builder.build_unconditional_branch(cond_block).unwrap();

    // Build the condition block
    self.builder.position_at_end(cond_block);
    let loop_index_val = self.builder.build_load(i64_type, loop_index_ptr, &loop_index_name).unwrap().into_int_value();
    let cond = self.builder.build_int_compare(inkwell::IntPredicate::ULT, loop_index_val, end_condition_val, "loop_cond").unwrap();
    self.builder.build_conditional_branch(cond, body_block, end_block).unwrap();

    // Build the loop body
    self.builder.position_at_end(body_block);
    self.set_symbol(loop_index_name.clone(), Some(loop_index_ptr), i64_type.as_basic_type_enum(), loop_index_val.as_basic_value_enum());
    let last_body_block = self.build_loop_body(body, body_block, increment_block, end_block)?;
    if last_body_block.get_terminator().is_none() {
      self.builder.position_at_end(last_body_block);
      self.builder.build_unconditional_branch(increment_block).unwrap();
    }

    // Build the increment block
    self.builder.position_at_end(increment_block);
    let current_val = self.builder.build_load(self.context.i64_type(), loop_index_ptr, "current_loop_idx").unwrap().into_int_value();
    let next_val = self.builder.build_int_add(current_val, self.context.i64_type().const_int(1, false), "next_loop_idx").unwrap();
    self.builder.build_store(loop_index_ptr, next_val).unwrap();
    // After increment go to the condition block
    self.builder.build_unconditional_branch(cond_block).unwrap(); 

    self.builder.position_at_end(end_block);
    return Ok(end_block);
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
    let last_body_block = self.build_loop_body(body, body_block, cond_block, end_block)?;
    if last_body_block.get_terminator().is_none() {
      self.builder.position_at_end(last_body_block);
      self.builder.build_unconditional_branch(cond_block).unwrap();
    }

    self.builder.position_at_end(end_block);
    return Ok(end_block);
  }

  fn build_enum(&mut self, name: String, variants: Vec<Literals>) -> Result<(), Error> {
    let mut fields: Vec<(String, BasicTypeEnum<'ctx>, Option<String>)> = vec![];
    let mut field_types: Vec<BasicTypeEnum> = vec![];
    let mut field_values: Vec<BasicValueEnum> = vec![];
    for (i, variant) in variants.iter().enumerate() {
      let variant_type = self.context.i64_type();
      // Get the int value of the variant
      let val = variant_type.const_int(i as u64, false);
      field_values.push(val.as_basic_value_enum());
      field_types.push(variant_type.as_basic_type_enum());

      match variant {
        Literals::Identifier(Name(variant_name)) => {
          fields.push((variant_name.clone(), variant_type.as_basic_type_enum(), None));
        },
        _ => unreachable!()
      }
    }

    // Create the enum struct
    let opaque = self.context.opaque_struct_type(&format!("{}_enum", name));
    opaque.set_body(&field_types, false);

    // Allocate space for the enum struct
    let enum_struct_type = self.context.struct_type(&field_types, false);
    let enum_ptr = self.builder.build_alloca(enum_struct_type, &format!("{}_enum", name)).unwrap();

    // Store the enum fields
    for (i, (field_name, _, _)) in fields.iter().enumerate() {
      let field_ptr = self.builder.build_struct_gep(enum_struct_type, enum_ptr, i as u32, &format!("{}_{}", &name, field_name)).unwrap();
      self.builder.build_store(field_ptr, field_values[i]).unwrap();
    }

    self.set_symbol(name.clone(), Some(enum_ptr), enum_struct_type.as_basic_type_enum(), enum_ptr.as_basic_value_enum());
    self.set_struct(format!("{}_enum", name), fields);
    self.set_struct_mapping(name.clone(), format!("{}_enum", name));
    self.set_enum_mapping(name.clone(), format!("{}_enum", name));
    return Ok(());
  }

  fn build_struct(&mut self, struct_lit_name: Option<String>, value: Box<Literals>) -> Result<StructType<'ctx>, Error> {
    // TODO: If value is an identifier, handle it as a type alias

    // Build struct if value is an object
    if let Literals::Object(obj) = value.as_ref().to_owned() {
      let mut fields: Vec<(String, BasicTypeEnum<'ctx>, Option<String>)> = vec![];
      for literal in obj.iter() {
        if let Literals::ObjectProperty(n, val) = literal.to_owned() {

          // Ensure name doesn't already exist in fields
          if fields.iter().any(|(name, _, _)| name == &n.0) {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &"",
              Some(0),
              Some(0),
              format!("Field {} already defined", n.0)
            ));
          }

          // Build the field
          let (typ, struct_name_if_any) = self.build_struct_field(val.expr)?;
          let field: (String, BasicTypeEnum<'ctx>, Option<String>) = (n.0, typ, struct_name_if_any);
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
      for (_, t, _) in fields.clone() {

        // Change child struct types to pointers for mapping
        if t.is_struct_type() {
          field_types.push(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum());
        } else {
          field_types.push(t);
        }
      }

      let struct_type = if struct_lit_name.is_none() {
        self.context.struct_type(&field_types, false)
      } else {
        let struct_type = self.context.opaque_struct_type(&struct_lit_name.clone().unwrap());
        struct_type.set_body(&field_types, false);
        struct_type
      };

      // Save a reference to the struct
      if let Some(n) = struct_lit_name {
        // Special saving method for structs to keep track of their fields
        if self.get_struct(&n).is_some() {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &"",
            Some(0),
            Some(0),
            format!("Name {} already defined", n)
          ));
        } else {
          self.set_struct(n, fields);
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

  fn build_struct_field(&mut self, val: Expr) -> Result<(BasicTypeEnum<'ctx>, Option<String>), Error> {
    match val {
      Expr::TypeDef(_, v) => {
        let s = self.build_struct(None, v)?;
        return Ok((s.as_basic_type_enum(), Some(s.get_name().unwrap().to_str().unwrap().to_string())));
      },
      Expr::Literal(Literals::Identifier(Name(name))) => {
        match name.as_str() {
          "int" => return Ok((self.context.i64_type().as_basic_type_enum(), None)),
          "float" => return Ok((self.context.f64_type().as_basic_type_enum(), None)),
          "bool" => return Ok((self.context.bool_type().as_basic_type_enum(), None)),
          "string" => return Ok((self.context.ptr_type(AddressSpace::default()).as_basic_type_enum(), None)),
          _ => {
            // Check for enum
            if let Some(enum_type) = self.get_enum_mapping(&name) {
              return Ok((self.context.i64_type().as_basic_type_enum(), Some(enum_type.to_string())));

            // Check for type alias
            } else if self.get_struct(&name).is_some() {
              let struct_type = self.context.get_struct_type(&name);
              return Ok((struct_type.unwrap().as_basic_type_enum(), Some(name)));

            } else {
              return Err(Error::new(
                Error::CompilerError,
                None,
                &"",
                Some(0),
                Some(0),
                format!("Invalid struct field type {}", name)
              ));
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
    let struct_type = self.context.get_struct_type(&struct_name).unwrap();
    let field_types = self.get_struct(&struct_name)
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
          // Check structs for ptr name
          let struct_ref = self.get_struct(&struct_name);
          if struct_ref.is_some() {
            field_val_type = self.context.get_struct_type(&struct_name).unwrap().as_basic_type_enum();

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

    self.set_struct_mapping(var_name.clone(), struct_name.clone());
    return Ok(struct_ptr);
  }

  // Builder for conditional expressions
  fn build_if(&mut self, top_level: bool, _typ: Type, expr: Option<Box<Expression>>, body: Box<Vec<Expression>>, else_body: Option<Box<Vec<Expression>>>, function: FunctionValue<'ctx>, block: BasicBlock<'ctx>, top_level_merge_block: BasicBlock<'ctx>, loop_continue_target: Option<BasicBlock<'ctx>>, loop_break_target: Option<BasicBlock<'ctx>>) -> Result<(BasicBlock<'ctx>, BasicBlock<'ctx>, BasicBlock<'ctx>), Error> {
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
      let is_null = self.builder.build_is_null(p, "is_null").unwrap();
      cond = self.builder.build_not(is_null, "ifcond").unwrap().as_basic_value_enum();
    }

    // Set up blocks
    let then_block = self.context.append_basic_block(function, "if_then");
    let else_block = self.context.append_basic_block(function, "else");
    let merge_block = self.context.append_basic_block(function, "merge");

    self.builder.build_conditional_branch(cond.into_int_value(), then_block, else_block).unwrap();

    // Build the "then" block and collect the resulting values
    self.enter_scope(false);
    let then_out = self.build_conditional_block(then_block, Some(body.clone()), function, top_level_merge_block, loop_continue_target, loop_break_target)?;
    let then_vals = self.get_all_cur_symbols(false);
    self.exit_scope();
    // Merge back if the block wasn't already terminated
    let then_branches_to_merge = then_out.get_terminator().is_none();
    if then_branches_to_merge {
      self.builder.build_unconditional_branch(merge_block).unwrap();
    }

    // Build the "else" block and collect the resulting values
    self.enter_scope(false);
    let else_out = self.build_conditional_block(else_block, else_body, function, top_level_merge_block, loop_continue_target, loop_break_target)?;
    let else_vals = self.get_all_cur_symbols(false);
    self.exit_scope();
    // Merge back if the block wasn't already terminated
    let else_branches_to_merge = else_out.get_terminator().is_none();
    if else_branches_to_merge {
      self.builder.build_unconditional_branch(merge_block).unwrap();
    }

    // Collect all keys from both then_vals and else_vals
    let all_keys: HashSet<&String> = then_vals.keys().chain(else_vals.keys()).collect();

    // Pre-compute the values we need for PHI nodes before positioning at merge block
    // This is important because we might need to load values from memory in the source blocks
    let mut phi_nodes_to_create: Vec<(String, BasicTypeEnum<'ctx>, Vec<(BasicValueEnum<'ctx>, BasicBlock<'ctx>)>)> = Vec::new();

    for name in all_keys {

      // Get values from then_vals and else_vals or use the value above the loop blocks if it exists
      let then_val: Option<(Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>, bool)> = then_vals.get(name)
        .cloned()
        .or_else(|| {
        self.get_symbol(name).ok().flatten().map(|(_, pval, typ, val, exited_func)| {
          (pval, typ, val, exited_func)
        })
      });
      let else_val: Option<(Option<PointerValue<'ctx>>, BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>, bool)> = else_vals.get(name)
        .cloned()
        .or_else(|| {
        self.get_symbol(name).ok().flatten().map(|(_, pval, typ, val, exited_func)| {
          (pval, typ, val, exited_func)
        })
      });

      // If one branch initializes or uses one of its own new values
      if then_val.is_none() || else_val.is_none() { continue; }

      // Determine the types of the values
      let then_type = then_val.as_ref().unwrap().1;
      let else_type = else_val.as_ref().unwrap().1;

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

      // Check if the variable was actually modified in each branch
      let then_modified = then_vals.contains_key(name);
      let else_modified = else_vals.contains_key(name);

      // If neither branch modified the variable, skip PHI creation
      if !then_modified && !else_modified {
        continue;
      }

      let mut incoming_for_phi: Vec<(BasicValueEnum<'ctx>, BasicBlock<'ctx>)> = Vec::new();

      if then_branches_to_merge {
        let value_to_use = if then_modified {
          // Branch modified it, use the modified value
          then_val.as_ref().unwrap().2
        } else {
          // Branch didn't modify it, load the current value from memory before the terminator of then_out block
          if let Some((ptr, typ, _, _)) = then_val.as_ref() {
            if let Some(p) = ptr {
              // Insert load BEFORE the terminator (branch instruction)
              let terminator = then_out.get_terminator().unwrap();
              self.builder.position_before(&terminator);
              let loaded = self.builder.build_load(*typ, *p, &format!("{}_loaded_then", name)).unwrap();
              loaded
            } else {
              then_val.as_ref().unwrap().2
            }
          } else {
            then_val.as_ref().unwrap().2
          }
        };
        incoming_for_phi.push((value_to_use, then_out));
      }

      if else_branches_to_merge {
        let value_to_use = if else_modified {
          // Branch modified it, use the modified value
          else_val.as_ref().unwrap().2
        } else {
          // Branch didn't modify it, load the current value from memory before the terminator of else_out block
          if let Some((ptr, typ, _, _)) = else_val.as_ref() {
            if let Some(p) = ptr {
              // Insert load BEFORE the terminator (branch instruction)
              let terminator = else_out.get_terminator().unwrap();
              self.builder.position_before(&terminator);
              let loaded = self.builder.build_load(*typ, *p, &format!("{}_loaded_else", name)).unwrap();
              loaded
            } else {
              else_val.as_ref().unwrap().2
            }
          } else {
            else_val.as_ref().unwrap().2
          }
        };
        incoming_for_phi.push((value_to_use, else_out));
      }

      // Only create PHI node if we have incoming values
      if !incoming_for_phi.is_empty() {
        phi_nodes_to_create.push((name.clone(), phi_type, incoming_for_phi));
      }
    }

    // Now position the builder at the merge block and create all PHI nodes
    self.builder.position_at_end(merge_block);

    for (name, phi_type, incoming_for_phi) in phi_nodes_to_create {
      let phi = self.builder.build_phi(phi_type, &name).unwrap();
      let incoming_refs: Vec<(&dyn BasicValue<'ctx>, BasicBlock<'ctx>)> = incoming_for_phi.iter().map(|(v, b)| (v as &dyn BasicValue<'ctx>, *b)).collect();
      phi.add_incoming(&incoming_refs);

      // Check if the symbol exists in the symbol table
      if let Some(symbol) = self.get_symbol(&name)? {
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
        self.set_symbol(name.to_string(), None, phi_type, phi.as_basic_value());
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

  fn build_conditional_block(&mut self, block: BasicBlock<'ctx>, body: Option<Box<Vec<Expression>>>, function: FunctionValue<'ctx>, top_level_merge_block: BasicBlock<'ctx>, loop_continue_target: Option<BasicBlock<'ctx>>, loop_break_target: Option<BasicBlock<'ctx>>) -> Result<BasicBlock<'ctx>, Error> {
    self.builder.position_at_end(block);
    let mut current_block = block;

    if let Some(body) = body {
      for expr in body.iter() {
        match expr.expr.clone() {
          Expr::Conditional(typ, exp, body, ebody) => {
            // This is the end, build the body
            if typ.0 == "else" {
              current_block = self.build_conditional_block(current_block, Some(body), function, top_level_merge_block, loop_continue_target, loop_break_target)?;
            // This is a new branch, return to the top
            } else {
              (_, _, current_block) = self.build_if(false, typ, exp, body, ebody, function, current_block, top_level_merge_block, loop_continue_target, loop_continue_target)?;
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
            self.builder.build_unconditional_branch(loop_break_target.unwrap()).unwrap();
          },
          Expr::ContinueLoop => {
            self.builder.build_unconditional_branch(loop_continue_target.unwrap()).unwrap();
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
    // (type or let/const; ident or index; value)
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

    // Case: Array/struct index lhs
    if let Expr::ArrayIndex(_, _) | Expr::Index(_, _) = ident_or_idx.expr.clone() {
      let (ptr, _) = self.build_index(&ident_or_idx)?;
      let set = self.visit(&value.clone().unwrap(), block)?;
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

    let set = match &value {
      // Case: No rhs / value, set to null
      None => self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum(),
      Some(v) => match &v.expr {
        // Case: Object rhs
        Expr::Literal(Literals::Object(obj)) => {
          let alloca = self.build_initialize_struct(ty.clone().unwrap().0, name.clone(), obj.to_vec(), block)?;
          alloca.as_basic_value_enum()
        },
        // Case: Array rhs
        // TODO: Handle heterogeneous arrays
        Expr::Array(_) => {
          let (ptr, typ, val) = self.build_array(*value.clone().unwrap(), block)?;
          self.set_symbol(name, Some(ptr), typ.as_basic_type_enum(), val);
          return Ok(());
        },
        _ => self.visit(&value.unwrap(), block)?,
      }
    };

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
        "let" | "const" | "int" | "bool" | "float" | "string" => {
          // Allocate space and store the value
          let ptr = self.builder.build_alloca(set.get_type(), &name).unwrap();
          self.builder.build_store(ptr, set).unwrap();
          // Set the symbol
          self.set_symbol(name, Some(ptr), set.get_type(), set);
        },
        // Custom type (we know exists from builder)
        _ => {
          self.set_symbol(name.clone(), None, set.get_type(), set);
          if self.get_struct(ty.as_str()).is_some() || self.class_table.contains_key(ty.as_str()) {
            self.set_struct_mapping(name, ty);
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
    let mut parameter_stack: Vec<FunctionParamCtx<'ctx>> = vec![];
    let mut variadic = false;
    let mut variadic_param_name: Option<String> = None;
    let mut variadic_param_type: Option<JType> = None;

    // If variadic, prepend function params to accept var count behind the scenes
    if let Some(params_vec) = &params {
      if params_vec.iter().any(|p| matches!(p.expr, Expr::FunctionParam(_, _, _, _, true))) {
        variadic = true;
        parameters.push(self.context.i64_type().into());
      }
    }

    // Enter the function scope
    // This is up here in case we encounter a struct parameter and need to create local mappings
    self.enter_scope(true);

    for (i, param) in params.clone().unwrap().iter().enumerate() {
      let (typ, _, name, default_value_expr, is_spread) = match param.clone().expr {
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
      if is_spread {
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

        variadic_param_name = Some(name.clone());
        variadic_param_type = Some(param.clone().inferred_type.unwrap());

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

            // Check for enum definition
            if self.get_enum_mapping(&typ.0).is_some() {
              let t = self.context.i64_type();
              parameters.push(t.into());
              // Map parameter name to its enum type within the function scope
              self.set_enum_mapping(name.clone(), typ.0.clone());
              t.into()

            // Check for struct definition
            } else if self.get_struct(&typ.0).is_some() {
              let t = self.context.ptr_type(AddressSpace::default());
              parameters.push(t.into());
              // Map parameter name to its struct type within the function scope
              self.set_struct_mapping(name.clone(), typ.0.clone());
              t.into()

            } else {
              return Err(Error::new(
                Error::NameError,
                None,
                &self.code.lines().nth((param.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                param.first_pos,
                Some(param.first_pos.unwrap() + typ.0.len() as i32),
                format!("Type {} not found", typ.0)
              ));
            }
          },
        };
      }

      parameter_stack.push(FunctionParamCtx {
        name: name.clone(),
        jtype: param.clone().inferred_type.unwrap(),
        _llvm_type: if is_spread {
          self.context.ptr_type(AddressSpace::default()).into()
        } else {
          *parameters.last().unwrap()
        },
        default_value_expr, is_spread
      });
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
      "string" => self.context.ptr_type(AddressSpace::default()).fn_type(&parameters, variadic),
      _ => {
        // Check for enum
        if self.get_enum_mapping(&returns).is_some() {
          self.context.i64_type().fn_type(&parameters, variadic)
        // Check for struct
        } else if self.get_struct_mapping(&returns).is_some() {
          self.context.get_struct_type(&returns).unwrap().fn_type(&parameters, variadic)
        } else {
          return Err(Error::new(
            Error::NameError,
            None,
            &"",
            Some(0),
            Some(0),
            format!("Type '{}' not found", returns)
          ));
        }
      },
    };
    let function = self.module.add_function(&name.0, func_type, None);

    // Store function context
    let fun_ctx = FunctionCtx {
      llvm_fun: function,
      parameters: parameter_stack.clone(),
      _ret_type: returns.clone(),
      variadic,
    };
    self.functions.insert(name.0.clone(), fun_ctx);

    // Create a new block for function body
    let mut func_block = self.context.append_basic_block(function, format!("{}-entry", name.0).as_str());
    self.builder.position_at_end(func_block);

    // Variadic call will pass hidden count as first argument for va_list
    let mut hidden_argument_offset = 0;

    // Get variadic count and build array if needed
    let mut var_array_ptr: Option<PointerValue<'ctx>> = None;
    let mut var_array_type: Option<BasicTypeEnum<'ctx>> = None;
    if variadic {
      hidden_argument_offset = 1;

      // Get count from hidden first param
      let var_arg_count = function.get_nth_param(0).unwrap().into_int_value();

      // Get types
      let var_llvm_type = self.jtype_to_basic_type(
        variadic_param_type.expect("Variadic parameter missing type")
      )?;

      // Allocate for array to store variadic arguments
      let array_ptr = self.builder.build_array_alloca(
        var_llvm_type,
        var_arg_count,
        &format!("{}-args", name.0).as_str(),
      ).unwrap();
      var_array_ptr = Some(array_ptr);
      var_array_type = Some(var_llvm_type);

      // Define the target-specific va_list type
      // In the x86-64 System V ABI, va_list should be:
      // [1 x struct {
      //   gp_offset:         i32, -- offset to next general-purpose register
      //   fp_offset:         i32, -- offset to next floating-point register
      //   overflow_arg_area: ptr, -- ptr to C stack area for args passed
      //   reg_save_area:     ptr  -- ptr to area where registers are saved
      // }]
      // In Windows x64, va_list is char* -- pointer to the first argument

      let ptr_type = self.context.ptr_type(AddressSpace::default());

      #[cfg(not(target_os = "windows"))]
      let va_list_type = {
        let i32_type = self.context.i32_type();
        let va_list_struct_fields: [BasicTypeEnum<'ctx>; 4] = [
          i32_type.into(), // gp_offset
          i32_type.into(), // fp_offset
          ptr_type.into(), // overflow_arg_area
          ptr_type.into(), // reg_save_area
        ];
        let va_list_struct_type = self.context.struct_type(&va_list_struct_fields, false);
        // va_list = array of the single struct
        va_list_struct_type.array_type(1)
      };
      #[cfg(target_os = "windows")]
      let va_list_type = ptr_type;

      // Allocate for va_list object itself
      let va_list_ptr = self.builder.build_alloca(va_list_type, "va_list_storage").unwrap();

      // llvm.va_start
      let va_start_fn_name = "llvm.va_start";
      let va_start_fn = self.module.get_function(va_start_fn_name).unwrap_or_else(|| {
        let fn_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
        self.module.add_function(va_start_fn_name, fn_type, None)
      });
      self.builder.build_call(va_start_fn, &[va_list_ptr.into()], "call_va_start").unwrap();

      // Loop and fill array using va_arg
      let loop_block = self.context.append_basic_block(function, "va_arg_loop");
      let after_loop_block = self.context.append_basic_block(function, "after_va_arg_loop");

      let index_ptr = self.builder.build_alloca(self.context.i64_type(), "var_idx").unwrap();
      self.builder.build_store(index_ptr, self.context.i64_type().const_int(0, false)).unwrap();
      self.builder.build_unconditional_branch(loop_block).unwrap();
      self.builder.position_at_end(loop_block);

      let current_index = self.builder.build_load(self.context.i64_type(), index_ptr, "idx").unwrap().into_int_value();
      let loop_cond = self.builder.build_int_compare(inkwell::IntPredicate::ULT, current_index, var_arg_count, "var_loop_cond").unwrap();

      let body_block = self.context.append_basic_block(function, "va_arg_body");
      self.builder.build_conditional_branch(loop_cond, body_block, after_loop_block).unwrap();
      self.builder.position_at_end(body_block);

      {
        // Build va_arg
        // va_list pointer and the type to extract
        let arg_val = self.builder.build_va_arg(va_list_ptr, var_llvm_type, "va_arg_val").unwrap();

        // Get pointer to the array element
        let element_ptr = unsafe {
          self.builder.build_gep(var_llvm_type, array_ptr, &[current_index], "var_elem_ptr").unwrap()
        };
        // Store the retrieved value
        self.builder.build_store(element_ptr, arg_val).unwrap();

        // Increment index
        let next_index = self.builder.build_int_add(current_index, self.context.i64_type().const_int(1, false), "next_idx").unwrap();
        self.builder.build_store(index_ptr, next_index).unwrap();
        self.builder.build_unconditional_branch(loop_block).unwrap(); // Branch back to loop condition
      }

      // Position at the end of the loop
      self.builder.position_at_end(after_loop_block);

      // llvm.va_end
      let va_end_fn_name = "llvm.va_end";
      let va_end_fn = self.module.get_function(va_end_fn_name).unwrap_or_else(|| {
        let fn_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
        self.module.add_function(va_end_fn_name, fn_type, None)
      });
      self.builder.build_call(va_end_fn, &[va_list_ptr.into()], "call_va_end").unwrap();

      func_block = after_loop_block;
    }

    // Set up function parameters
    for (i, llvm_param_val) in function.get_params().iter().enumerate() {
      // Skip hidden arg count
      if variadic && i == 0 { continue; }

      // Get param info
      let param_info = parameter_stack[i - hidden_argument_offset].clone();
      if param_info.is_spread { continue; }

      // Set parameter name
      llvm_param_val.set_name(&param_info.name);

      let jtype = param_info.jtype.clone();
      match jtype {
        // TODO: Handle when type is unknown at compile time
        JType::Unknown => {
          todo!("Handle unknown JType for parameter {} during code generation", param_info.name);
        },
        _ => {}
      }

      let alloca = self.builder.build_alloca(llvm_param_val.get_type(), &param_info.name).unwrap();
      self.builder.build_store(alloca, *llvm_param_val).unwrap();
      self.set_symbol(param_info.name, Some(alloca), llvm_param_val.get_type(), *llvm_param_val);
    }

    // If variadic, set the array pointer in the function context
    if let (Some(name), Some(array_ptr), Some(array_type)) = (variadic_param_name, var_array_ptr, var_array_type) {
      self.set_symbol(name, Some(array_ptr), array_type.array_type(0).as_basic_type_enum(), array_ptr.as_basic_value_enum());
    }

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

    // Ignore if there is already a return
    if function.get_last_basic_block().unwrap().get_last_instruction().is_none() {
      self.builder.build_return(Some(&self.context.f64_type().const_float(0.0).as_basic_value_enum())).unwrap();
    }

    // Terminate block
    self.builder.position_at_end(block);

    return Ok(());
  }

  fn build_class(&mut self, exp: Expression, Name(name): Name, parents: Option<Vec<Name>>, body: Option<Box<Vec<Expression>>>) -> Result<(), Error> {
    // Get parents if any
    let mut parent_classes = vec![];
    if let Some(parents) = parents {
      for Name(parent) in parents {
        let parent_class = self.context.get_struct_type(&parent);
        if parent_class.is_none() {
          return Err(Error::new(
            Error::NameError,
            None,
            &self.code.lines().nth((exp.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            exp.first_pos,
            exp.first_pos,
            format!("Parent class '{}' not found", parent)
          ));
        }

        parent_classes.push(parent);
      }
    }

    // Set up the class struct information
    let mut types: Vec<BasicTypeEnum<'ctx>> = vec![];
    let mut types_as_fields: Vec<(String, BasicTypeEnum<'ctx>)> = vec![];
    // i32 index in types
    let mut defaults: Vec<(String, BasicValueEnum<'ctx>)> = vec![];
    // Set up method signature information
    let mut methods: IndexMap<String, FunctionValue<'ctx>> = IndexMap::new();
    let mut method_params: IndexMap<String, IndexMap<String, BasicTypeEnum>> = IndexMap::new();
    let mut method_bodies: IndexMap<String, Option<Box<Vec<Expression>>>> = IndexMap::new();
    let mut method_param_exprs: IndexMap<String, Option<Box<Vec<Expression>>>> = IndexMap::new();

    // Add parent class props and methods to class
    for parent in &parent_classes {
      let (_, parent_fields, parent_methods) = self.class_table.get(parent).unwrap();

      // Add parent fields to the class struct
      for (field_name, field_type) in parent_fields {
        types.push(field_type.clone());
        types_as_fields.push((field_name.clone(), field_type.clone()));
      }

      // Add parent methods to the class
      for (method_name, method) in parent_methods.into_iter() {
        if method_name != "new" {
          methods.insert(method_name.clone(), method.clone());
        }
      }
    }

    if let Some(body) = body.as_ref() {
      for expr in body.iter() {

        // Check for class methods
        let func_expr = if let Expr::Public(inner) = expr.expr.clone() {
          inner.expr
        } else {
          expr.expr.clone()
        };

        if let Expr::Function(Name(func_name), ret_typ, params, body) = func_expr {
          let (signature, mparams) = self.build_class_method_signature(expr.clone(), func_name.clone(), ret_typ, params.clone())?;
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
          method_params.insert(func_name.clone(), mparams);
          method_bodies.insert(func_name.clone(), body);
          method_param_exprs.insert(func_name, params);
        }
      }
    }

    // Check for constructor
    if methods.is_empty() || methods.iter().filter(|(name, _)| name == &&"new").count() == 0 {
      return Err(Error::new(
        Error::CompilerError,
        None,
        &self.code.lines().nth((exp.first_line.unwrap() - 1) as usize).unwrap().to_string(),
        exp.first_pos,
        // + 4 for `cls `
        Some(exp.first_pos.unwrap() + 4 + name.len() as i32),
        format!("Class '{}' is missing constructor", name)
      ));
    }

    // Add class to the known class information
    self.set_struct(name.clone(), vec![]);
    self.set_struct_mapping(name.clone(), name.clone());
    self.class_table.insert(name.clone(), (Some(parent_classes), types_as_fields.clone(), methods.clone().into_iter().collect()));

    // Set up the constructor
    let constructor = self.module.get_function(&format!("{}_constructor", name)).unwrap();
    let constructor_entry_block = self.context.append_basic_block(constructor, "entry");
    self.builder.position_at_end(constructor_entry_block);
    self.enter_scope(true);

    // Set up the class fields
    let mut struct_fields: Vec<(String, BasicTypeEnum<'ctx>, Option<String>)> = vec![];
    for expr in body.unwrap().into_iter() {
      if let Expr::Assignment(typ, idx_or_ident, opt_var) = expr.expr.clone() {
        if let Expr::Index(parent, child) = expr.expr.clone() {
          if let Expr::Index(_, _) = child.expr {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((exp.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              child.first_pos,
              child.first_pos,
              "Nested indexing is not supported in constructor definition.".to_string()
            ));
          } else if let Expr::SelfRef = parent.expr {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((parent.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              parent.first_pos,
              parent.first_pos,
              "Cannot assign to self in class definition, use a constructor.".to_string()
            ));
          } else {
            todo!()
          }
        }

        if typ.is_none() || typ.as_ref().unwrap().0 == "let" || typ.as_ref().unwrap().0 == "const" {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            expr.first_pos,
            expr.first_pos,
            "Fields must be typed".to_string()
          ));
        }

        let field_type = match typ.as_ref().unwrap().0.as_str() {
          "int" => self.context.i64_type().as_basic_type_enum(),
          "float" => self.context.f64_type().as_basic_type_enum(),
          "bool" => self.context.bool_type().as_basic_type_enum(),
          "string" => self.context.ptr_type(AddressSpace::default()).as_basic_type_enum(),
          _ => {

            // Check for enum definition
            let enum_type = self.get_enum_mapping(&typ.as_ref().unwrap().0);
            if enum_type.is_some() {
              self.context.i64_type().as_basic_type_enum()
            // Check for struct definition
            } else {
              let struct_ref = self.get_struct_mapping(&typ.as_ref().unwrap().0);
              if struct_ref.is_none() {
                return Err(Error::new(
                  Error::NameError,
                  None,
                  &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                  expr.first_pos,
                  Some(expr.first_pos.unwrap() + typ.as_ref().unwrap().0.len() as i32),
                  format!("Type '{}' not found", typ.as_ref().unwrap().0)
                ));
              } else {
                self.context.get_struct_type(&typ.as_ref().unwrap().0).unwrap().as_basic_type_enum()
              }
            }
          },
        };

        // Field with default
        if opt_var.is_some() {
          let set = self.visit(&opt_var.unwrap(), constructor_entry_block)?;

          if set.get_type().as_basic_type_enum() != field_type {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              expr.first_pos,
              Some(self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string().len() as i32),
              format!("Invalid assignment type for field, expected {:?} but got {:?}", field_type.print_to_string(), set.get_type().print_to_string())
            ));
          }

          types.push(set.get_type());
          let (_, field_name) = match idx_or_ident.expr {
            Expr::Literal(Literals::Identifier(Name(name))) => (idx_or_ident.first_pos.unwrap_or(0), name),
            _ => unreachable!(),
          };
          types_as_fields.push((field_name.clone(), field_type));
          defaults.push((field_name.clone(), set));

          let type_name = typ.as_ref().unwrap().0.clone();
          let struct_type_name = if self.get_struct(&type_name).is_some() {
            Some(type_name)
          } else {
            None
          };
          struct_fields.push((field_name.clone(), field_type, struct_type_name));

          // Update the class table with the new fields
          let (_, class_fields, _) = self.class_table.get_mut(&name).unwrap();
          class_fields.push((field_name.clone(), set.get_type()));

        // Field without default
        } else {
          let (_, field_name) = match idx_or_ident.expr {
            Expr::Literal(Literals::Identifier(Name(name))) => (idx_or_ident.first_pos.unwrap_or(0), name),
            _ => unreachable!(),
          };
          types.push(field_type);
          types_as_fields.push((field_name.clone(), field_type));

          let type_name = typ.as_ref().unwrap().0.clone();
          let struct_type_name = if self.get_struct(&type_name).is_some() {
            Some(type_name)
          } else {
            None
          };
          struct_fields.push((field_name.clone(), field_type, struct_type_name));

          // Update the struct and class tables with the new fields
          let (_, class_fields, _) = self.class_table.get_mut(&name).unwrap();
          class_fields.push((field_name.clone(), field_type));
          // TODO: Fix struct_table dead code:
          // self.struct_table.get_mut(&name).unwrap().push((field_name, field_type, None));
        }
      }
    }

    // Create the class struct
    let class_struct = self.context.opaque_struct_type(&name);
    class_struct.set_body(&types, false);

    // Build the class struct pointer to operate on in the constructor
    let class_ptr = constructor.get_nth_param(0).unwrap().into_pointer_value();
    self.current_class = Some(class_ptr);

    let new_params_exprs = method_param_exprs.get("new").unwrap();
    // Build the constructor parameters
    for (i, param) in constructor.get_params().iter().enumerate() {
      if i == 0 { continue; } // Skip self
      let i = i - 1;

      // Get the nth parameter name
      let field_name = method_params.get("new").unwrap().into_iter().nth(i).unwrap().0.clone();
      // Store parameter
      let var = self.builder.build_alloca(param.get_type(), &field_name).unwrap();
      self.builder.build_store(var, *param).unwrap();
      // Ensure the constructor block is aware of the parameter
      self.set_symbol(field_name.clone(), Some(var), param.get_type(), *param);

      // Set struct mapping if it is a struct
      if let Some(params) = new_params_exprs {
        let p = &params[i];
        if let Expr::FunctionParam(Some(Type(type_name)), _, _, _, _) = &p.expr {
          if self.get_struct(type_name).is_some() {
            self.set_struct_mapping(field_name.clone(), type_name.clone());
          }
        }
      }
    }

    // Map self to class name
    self.set_struct_mapping("self".to_string(), name.clone());

    // Build the constructor body
    let mut initialized_values: Vec<(String, BasicValueEnum<'ctx>)> = vec![];
    // We know constructor exists
    for ex in method_bodies.get("new").unwrap().to_owned().unwrap().into_iter() {
      if let Expr::Assignment(_, lhs, val) = ex.expr.clone() {
        if let Expr::Index(parent, child) = lhs.expr.clone() {
          if let Expr::SelfRef = parent.expr {
            if let Expr::Literal(Literals::Identifier(Name(field_name))) = child.expr {
              let set = self.visit(&val.unwrap(), constructor_entry_block)?;

              if set.get_type().as_basic_type_enum() != types_as_fields.iter().filter(|(name, _)| name == &field_name).nth(0).unwrap().1 {
                return Err(Error::new(
                  Error::CompilerError,
                  None,
                  &self.code.lines().nth((ex.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                  ex.first_pos,
                  ex.first_pos,
                  format!(
                    "Invalid assignment type for field '{}', expected {:?} but got {:?}",
                    field_name,
                    types_as_fields.iter().filter(|(name, _)| name == &field_name).nth(0).unwrap().1.print_to_string(),
                    set.get_type().print_to_string()
                  )
                ));
              }

              initialized_values.push((field_name, set));
              continue;
            }
          }
        }
        self.build_assignment(ex, constructor_entry_block, false)?;
      } else if let Expr::Function(_, _, _, _) = ex.expr.clone() {
        continue;
      } else {
        self.visit(&ex, constructor_entry_block)?;
      }
    }

    // Set the initialized values
    for (i, (n, _)) in types_as_fields.iter().enumerate() {
      if initialized_values.iter().filter(|(name, _)| name == n).count() > 0 {
        let field_ptr = self.builder.build_struct_gep(class_struct, class_ptr, i as u32, &format!("{}_field{}", name, i)).unwrap();
        let field_val = initialized_values.iter().filter(|(name, _)| name == n).nth(0).unwrap().1;
        self.builder.build_store(field_ptr, field_val).unwrap();
      } else if defaults.iter().filter(|(name, _)| name == n).count() > 0 {
        let field_ptr = self.builder.build_struct_gep(class_struct, class_ptr, i as u32, &format!("{}_field{}", name, i)).unwrap();
        let field_val = defaults.iter().filter(|(name, _)| name == n).nth(0).unwrap().1;
        self.builder.build_store(field_ptr, field_val).unwrap();
      }
    }

    // Return from the constructor
    self.builder.build_return(Some(&class_ptr)).unwrap();

    self.exit_scope();

    // Update struct table in the parent scope
    self.set_struct(name.clone(), struct_fields);

    // Build the other class methods
    for (method_name, body) in method_bodies.iter() {
      if method_name == "new" {
        continue;
      }

      if let Some(body_exprs) = body {
        let function = self.module.get_function(&format!("{}_{}", name, method_name)).unwrap();
        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);
        self.enter_scope(true);

        // Handle self parameter
        let self_param = function.get_nth_param(0).unwrap();
        // We treat self as a pointer to the struct
        self.current_class = Some(self_param.into_pointer_value());
        self.set_struct_mapping("self".to_string(), name.clone());

        // Handle other parameters
        if let Some(params) = method_param_exprs.get(method_name).unwrap() {
          for (i, param) in params.iter().enumerate() {
            let (param_name, Type(type_name)) = match &param.expr {
              Expr::FunctionParam(ty, _, Literals::Identifier(Name(ident)), _, _) => (ident, ty.as_ref().unwrap()),
              _ => panic!("Invalid function parameter"),
            };

            let val = function.get_nth_param((i + 1) as u32).unwrap();
            let ptr = self.builder.build_alloca(val.get_type(), param_name).unwrap();
            self.builder.build_store(ptr, val).unwrap();

            self.set_symbol(param_name.clone(), Some(ptr), val.get_type(), val);

            if self.get_struct(type_name).is_some() {
              self.set_struct_mapping(param_name.clone(), type_name.clone());
            }
          }
        }

        // Build body
        for expr in body_exprs.iter() {
           self.visit(expr, entry_block)?;
        }

        // Handle void return if needed
        if function.get_type().get_return_type().is_none() {
           if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
              self.builder.build_return(None).unwrap();
           }
        }

        self.exit_scope();
      }
    }

    self.current_class = None;
    return Ok(());
  }

  fn build_class_method_signature(&mut self, expr: Expression, method_name: String, ret_typ: Option<Literals>, params: Option<Box<Vec<Expression>>>) -> Result<(FunctionType<'ctx>, IndexMap<String, BasicTypeEnum<'ctx>>), Error> {
    // Collect the parameter types
    let mut fields: IndexMap<String, BasicTypeEnum<'ctx>> = IndexMap::new();
    let mut types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

    // Add self parameter
    let self_ptr_type = self.context.ptr_type(AddressSpace::default());
    types.push(self_ptr_type.into());

    if params.is_some() {
      for param in params.unwrap().into_iter() {
        let (param_name, Type(typ)) = match param.clone().expr {
          Expr::FunctionParam(ty, _, Literals::Identifier(Name(ident)), _, _) => {
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
              (ident, ty.unwrap())
            }
          },
          _ => panic!("Invalid function parameter"),
        };

        let t: BasicTypeEnum = match typ.as_str() {
          "int" => self.context.i64_type().into(),
          "float" => self.context.f64_type().into(),
          "bool" => self.context.bool_type().into(),
          "string" => self.context.ptr_type(AddressSpace::default()).into(),
          _ => {
            if self.get_struct(&typ).is_some() {
              self.context.ptr_type(AddressSpace::default()).into()
            } else {
              return Err(Error::new(
                Error::NameError,
                None,
                &self.code.lines().nth((param.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                param.first_pos,
                Some(param.first_pos.unwrap() + typ.len() as i32),
                format!("Type '{}' not found", typ)
              ));
            }
          },
        };

        fields.insert(param_name.clone(), t);
        types.push(t.into());
      }
    }

    // Collect the function type
    let function_type = if method_name == "new" {
      // Constructor returns pointer to class struct
      self.context.ptr_type(AddressSpace::default()).fn_type(&types, false)
    } else if ret_typ.is_some() {
      match ret_typ.unwrap() {
        Literals::Identifier(Name(name)) => {
          match name.as_str() {
            "int" => self.context.i64_type().fn_type(&types, false),
            "float" => self.context.f64_type().fn_type(&types, false),
            "bool" => self.context.bool_type().fn_type(&types, false),
            "string" => self.context.ptr_type(AddressSpace::default()).fn_type(&types, false),
            "void" => self.context.void_type().fn_type(&types, false),
            _ => {
              if self.get_struct(&name).is_some() {
                self.context.ptr_type(AddressSpace::default()).fn_type(&types, false)
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

    return Ok((function_type, fields));
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
          Some(expr.first_pos.unwrap() + n.len() as i32),
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

  fn build_array(&mut self, expr: Expression, block: BasicBlock<'ctx>) -> Result<(PointerValue<'ctx>, ArrayType<'ctx>, BasicValueEnum<'ctx>), Error> {
    let arr_exprs = match expr.expr {
      Expr::Array(exprs) => exprs,
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

    // Get array element type
    let elem_jtype = match expr.inferred_type.clone() {
      Some(JType::Array(typ)) => *typ,
      _ => {
        // Fallback to the first element type if available
        if let Some(elem) = arr_exprs.first() {
          if let Some(typ) = elem.inferred_type.clone() {
            typ
          } else {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
              expr.first_pos,
              expr.last_line,
              "Array type missing".to_string()
            ));
          }

        // Array is empty
        } else {
          JType::Unknown
        }
      }
    };

    let llvm_elem_type = self.jtype_to_basic_type(elem_jtype.clone())?;
    let array_len = arr_exprs.len() as u32;
    let final_array_type = llvm_elem_type.array_type(array_len);

    // Allocate memory for array
    let array_alloca = self.builder.build_alloca(final_array_type, "arr_data").unwrap();

    // Populate array memory
    for (i, elem_expr) in arr_exprs.iter().enumerate() {
      let elem_val = self.visit(elem_expr, block)?;
      if elem_val.get_type() != llvm_elem_type {
        return Err(Error::new(
          Error::CompilerError,
          None,
          &self.code.lines().nth(expr.first_line.unwrap() as usize).unwrap().to_string(),
          expr.first_pos,
          expr.last_line,
          format!("Array element type mismatch: expected {:?}, got {:?}", llvm_elem_type, elem_val.get_type())
        ));
      }

      let idx = self.context.i64_type().const_int(i as u64, false);
      let elem_ptr = unsafe { self.builder.build_gep(final_array_type, array_alloca, &[self.context.i64_type().const_zero(), idx], "arr_elem") }.unwrap();
      self.builder.build_store(elem_ptr, elem_val).unwrap();
    }

    // Create the array object ({ len, arr_ptr })
    let array_struct_type = self.context.struct_type(&[
      self.context.i64_type().into(),                        // len
      self.context.ptr_type(AddressSpace::default()).into(), // arr ptr
    ], false);

    let array_struct_alloca = self.builder.build_alloca(array_struct_type, "array_alloc").unwrap();

    // Store pointer to length
    let length = self.context.i64_type().const_int(array_len as u64, false);
    let length_field_ptr = self.builder.build_struct_gep(array_struct_type, array_struct_alloca, 0, "arr_len_ptr").unwrap();
    self.builder.build_store(length_field_ptr, length).unwrap();

    // Store pointer to array
    let array_field_ptr = self.builder.build_struct_gep(array_struct_type, array_struct_alloca, 1, "arr_data_ptr").unwrap();
    self.builder.build_store(array_field_ptr, array_alloca).unwrap();

    return Ok((array_alloca, final_array_type, array_struct_alloca.as_basic_value_enum()));
  }

  fn visit(&mut self, expr: &Expression, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {

    // Position the builder at the end of the current block
    self.builder.position_at_end(block);

    match expr.clone().expr {
      Expr::Literal(literal) => self.handle_literal(literal, expr),
      Expr::BinaryOperator(op, lhs, rhs) => self.handle_binop(op, lhs, rhs, expr.clone().inferred_type.unwrap(), block),
      Expr::UnaryOperator(op, value) => self.handle_unop(op, value, block),
      Expr::Array(_) => panic!("Array not allowed in visit(), has its own visitor"),
      Expr::ArrayIndex(_, _) => {
        let (_, val) = self.build_index(expr)?;
        return Ok(val);
      },
      Expr::TypeDef(_, _) => todo!(),
      Expr::Conditional(_, _, _, _) => panic!("Conditional not allowed here"),
      Expr::Call(name, args) => {
        let func_name = if self.class_table.contains_key(&name.0) {
          format!("{}_constructor", name.0)
        } else {
          name.0.clone()
        };

        // Attempt to find function mapping
        if let Some(fun_ctx) = self.functions.get(&func_name) {

          // Collect function info
          let llvm_fun = fun_ctx.llvm_fun;
          let num_args_provided = args.len();
          let num_args_required = self.get_required_function_param_count(&fun_ctx.parameters);
          let num_params_total = fun_ctx.parameters.len();
          let num_params_fixed = if fun_ctx.variadic { num_params_total - 1 } else { num_params_total };
          let variadic = fun_ctx.variadic;
          let param_names: Vec<String> = fun_ctx.parameters.iter().map(|p| p.name.clone()).collect();
          let param_defaults: Vec<Option<Box<Expression>>> = fun_ctx.parameters.iter().map(|p| p.default_value_expr.clone()).collect();

          // Check args count
          if num_args_provided < num_args_required || (!variadic && num_args_provided > num_params_fixed) {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              expr.first_pos,
              expr.last_line,
              format!("Function \"{}\" expected {} arguments, got {}", name.0, num_args_required, args.len())
            ));
          }

          // Build args list
          let mut llvm_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(num_params_fixed + num_args_provided.saturating_sub(num_params_fixed));

          // Check if this is a constructor call
          if self.class_table.contains_key(&name.0) {
            // Allocate memory for the struct in the caller's stack
            let struct_type = self.context.get_struct_type(&name.0).unwrap();
            let struct_alloca = self.builder.build_alloca(struct_type, &format!("{}_inst", name.0)).unwrap();
            llvm_args.push(struct_alloca.into());
          }

          // Add hidden count for variadic functions
          if variadic {
            let count = num_args_provided as i64 - num_params_fixed as i64;
            let var_arg_count = if count > 0 { count } else { 0 };
            llvm_args.push(self.context.i64_type().const_int(var_arg_count as u64, false).into());
          }

          for i in 0..num_params_fixed {

            // Argument was provided
            let arg_val = if i < num_args_provided {
              self.visit(&args[i], block)?

            // Argument missing, confirm the function has a default to replace it
            } else {
              if param_defaults[i].is_some() {
                // Get default value from function context
                self.visit(param_defaults[i].as_ref().unwrap(), block)?

              } else {
                // Should be caught by earlier check
                return Err(Error::new(
                  Error::CompilerError,
                  None,
                  &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
                  expr.first_pos,
                  expr.last_line,
                  format!("Missing argument for parameter \"{}\"", param_names[i])
                ));
              }
            };

            // TODO: Type checking against the LLVM type
            // Example: if param_info.llvm_type is float and arg_val is int, build conversion
            // let coerced_val = self.coerce_value(arg_val, param_info.llvm_type)?;
            llvm_args.push(arg_val.into()); // Push coerced_val eventually
          }

          if variadic {
            for i in num_params_fixed..num_args_provided {
              let arg_val = self.visit(&args[i], block)?;
              llvm_args.push(arg_val.into());
            }
          }

          let call = self.builder.build_call(llvm_fun, &llvm_args, &func_name).unwrap();
          match call.try_as_basic_value().basic() {
            Some(val) => Ok(val),
            None => Ok(self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum()),
          }

        // Our mapping was not found, check for built-in functions
        } else {

          let llvm_function = self.module.get_function(&func_name);
          if llvm_function.is_none() {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              expr.first_pos,
              expr.last_line,
              format!("Function \"{}\" not found", name.0)
            ));
          }

          // Check args
          let num_expected = llvm_function.unwrap().get_params().len() as usize;
          let num_provided = args.len();
          let is_llvm_variadic = llvm_function.unwrap().get_type().is_var_arg();

          // Adjust for constructor call
          let is_constructor = self.class_table.contains_key(&name.0);
          let num_provided_adjusted = if is_constructor { num_provided + 1 } else { num_provided };

          if !is_llvm_variadic && num_provided_adjusted != num_expected {
            return Err(Error::new(
              Error::CompilerError,
              None,
              &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
              expr.first_pos,
              expr.last_line,
              format!("Function \"{}\" expected {} arguments, got {}", name.0, num_expected, num_provided_adjusted)
            ));
          }

          // Visit provided args
          let mut llvm_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(num_provided_adjusted);

          if is_constructor {
             // Allocate memory for the struct in the caller's stack
             let struct_type = self.context.get_struct_type(&name.0).unwrap();
             let struct_alloca = self.builder.build_alloca(struct_type, &format!("{}_inst", name.0)).unwrap();
             llvm_args.push(struct_alloca.into());
          }

          for arg in args.iter() {
            let val = self.visit(arg, block)?;
            llvm_args.push(val.into());
          }

          let call = self.builder.build_call(llvm_function.unwrap(), &llvm_args, &func_name).unwrap();

          // If constructor, return the struct pointer (first arg)
          if is_constructor {
             return Ok(llvm_args[0].into_pointer_value().as_basic_value_enum());
          }

          return match call.try_as_basic_value().basic() {
            Some(val) => Ok(val),
            None => Ok(self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum()),
          };
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
      Expr::SelfRef => {
        if self.current_class == None {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((expr.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            expr.first_pos,
            expr.last_line,
            "Self reference outside of class".to_string()
          ));
        }

        let class = self.current_class.as_ref().unwrap();
        return Ok(class.as_basic_value_enum());
      },
      Expr::Index(parent, child) => {
        return Ok(self.build_index_extract(true, false, None, parent, child, block)?.0);
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
        let ptr = self.builder.build_global_string_ptr(s.as_str(), "str").unwrap();
        return Ok(ptr.as_basic_value_enum());
      },
      Literals::Boolean(b) => {
        return Ok(self.context.bool_type().const_int(b as u64, false).as_basic_value_enum());
      },
      Literals::Object(obj) => {
        let struct_name = match expr.inferred_type.clone() {
          Some(JType::TypeName(name)) => name,
          _ => panic!("Object literal must be typed (inferred as {:?})", expr.inferred_type),
        };

        // Get struct definition to know field order
        let fields: Vec<(String, BasicTypeEnum)> = if let Some((_, fields_ref, _)) = self.class_table.get(&struct_name) {
          fields_ref.clone()
        } else if let Some(fields_ref) = self.get_struct(&struct_name) {
          fields_ref.iter().map(|(n, t, _)| (n.clone(), *t)).collect()
        } else {
          panic!("Struct definition not found: {}", struct_name);
        };

        let mut values: Vec<BasicValueEnum> = vec![];

        // Iterate over fields in order
        for (field_name, field_type) in fields {
          // Find property in object literal
          let mut found = false;
          for literal in obj.iter() {
            if let Literals::ObjectProperty(Name(prop_name), val) = literal {
              if *prop_name == field_name {
                let val_compiled = self.visit(val, self.builder.get_insert_block().unwrap())?;

                // If field is a struct, we need to load it if we have a pointer
                if field_type.is_struct_type() && val_compiled.is_pointer_value() {
                   let loaded = self.builder.build_load(field_type, val_compiled.into_pointer_value(), "load_struct_field").unwrap();
                   values.push(loaded);
                } else {
                   values.push(val_compiled);
                }
                found = true;
                break;
              }
            }
          }
          if !found {
            panic!("Missing field '{}' in object literal for type '{}'", field_name, struct_name);
          }
        }

        // Allocate memory for the struct
        let struct_type = self.context.get_struct_type(&struct_name).unwrap();
        let alloca = self.builder.build_alloca(struct_type, "anon_struct").unwrap();

        // Store values
        for (i, val) in values.iter().enumerate() {
          let field_ptr = self.builder.build_struct_gep(struct_type, alloca, i as u32, "field_ptr").unwrap();
          self.builder.build_store(field_ptr, *val).unwrap();
        }

        return Ok(alloca.as_basic_value_enum());
      },
      Literals::ObjectProperty(_, _) => todo!(),
      Literals::Null => Ok(self.context.ptr_type(AddressSpace::default()).const_null().as_basic_value_enum()),
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
    let op_type = value.inferred_type.as_ref().ok_or_else(|| Error::new(
      Error::CompilerError,
      None,
      &self.code.lines().nth((value.first_line.unwrap() - 1) as usize).unwrap().to_string(),
      value.first_pos,
      value.last_line,
      "Invalid unary operation".to_string()
    ))?;
    let val = self.visit(&value, block)?;

    match op.0.as_str() {
      "-" => {
        match op_type {
          JType::Integer => Ok(self.builder.build_int_neg(val.into_int_value(), "negtmp").unwrap().as_basic_value_enum()),
          JType::FloatingPoint => Ok(self.builder.build_float_neg(val.into_float_value(), "negtmp").unwrap().as_basic_value_enum()),
          _ => unreachable!()
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
      "++:pre" | "++:post" | "--:pre" | "--:post" => {
        // Get the identifier
        let name = if let Expr::Literal(Literals::Identifier(ref n)) = value.expr {
          n.0.clone()
        } else {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((value.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            value.first_pos,
            value.last_line,
            "Invalid unary operation".to_string()
          ));
        };

        // Get previous value info
        let (ptr, typ, val) = self.var_from_ident(name.clone(), value.as_ref())?;
        if ptr.is_none() {
          return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((value.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            value.first_pos,
            value.last_line,
            "Invalid use of value in unary operation".to_string()
          ));
        }
        let ptr = ptr.unwrap();

        // Build new value
        let new_val = match op_type {
          JType::Integer => {
            let one = self.context.i64_type().const_int(1, false);
            if op.0.starts_with("++") {
              self.builder.build_int_add(val.into_int_value(), one, "inc").unwrap().as_basic_value_enum()
            } else {
              self.builder.build_int_sub(val.into_int_value(), one, "dec").unwrap().as_basic_value_enum()
            }
          },
          JType::FloatingPoint => {
            let one = self.context.f64_type().const_float(1.0);
            if op.0.starts_with("++") {
              self.builder.build_float_add(val.into_float_value(), one, "inc").unwrap().as_basic_value_enum()
            } else {
              self.builder.build_float_sub(val.into_float_value(), one, "dec").unwrap().as_basic_value_enum()
            }
          },
          _ => return Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((value.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            value.first_pos,
            value.last_line,
            "Invalid unary operation".to_string()
          )),
        };

        // Store the new value
        self.builder.build_store(ptr, new_val).unwrap();
        // Update the symbol table
        self.set_symbol(name, Some(ptr), typ, new_val);

        // Return one of the values
        if op.0.ends_with(":pre") {
          return Ok(new_val);
        } else {
          return Ok(val);
        }
      }
      _ => panic!("Unknown operator: {}", op.0.as_str()),
    }
  }

  // Handle binary operators
  fn handle_binop(&mut self, op: Operator, lhs: Box<Expression>, rhs: Box<Expression>, expected_type: JType, block: BasicBlock<'ctx>) -> Result<BasicValueEnum<'ctx>, Error> {
    let mut left = self.visit(&lhs, block)?;
    let mut right = self.visit(&rhs, block)?;
    let lhs_type = lhs.inferred_type.as_ref().expect("Type checker should set this");
    let rhs_type = rhs.inferred_type.as_ref().expect("Type checker should set this");

    match op.0.as_str() {
      "+" => {
        match expected_type {
          JType::FloatingPoint => {
            if left.is_int_value() {
              left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
            }
            if right.is_int_value() {
              right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
            }

            Ok(self.builder.build_float_add(
              left.into_float_value(),
              right.into_float_value(),
              "addtmp"
            ).unwrap().as_basic_value_enum())
          },
          JType::Integer => Ok(self.builder.build_int_add(
            left.into_int_value(),
            right.into_int_value(),
            "addtmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for addition"), // TODO: pretty up errors
        }
      },
      "-" => {
        match expected_type {
          JType::FloatingPoint => {
            if left.is_int_value() {
              left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
            }
            if right.is_int_value() {
              right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
            }

            Ok(self.builder.build_float_sub(
              left.into_float_value(),
              right.into_float_value(),
              "subtmp"
            ).unwrap().as_basic_value_enum())
          },
          JType::Integer => Ok(self.builder.build_int_sub(
            left.into_int_value(),
            right.into_int_value(),
            "subtmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for subtraction"), // TODO: pretty up errors
        }
      },
      "*" => {
        match expected_type {
          JType::FloatingPoint => {
            if left.is_int_value() {
              left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
            }
            if right.is_int_value() {
              right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
            }

            Ok(self.builder.build_float_mul(
              left.into_float_value(),
              right.into_float_value(),
              "multmp"
            ).unwrap().as_basic_value_enum())
          },
          JType::Integer =>  Ok(self.builder.build_int_mul(
            left.into_int_value(),
            right.into_int_value(),
            "multmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for multiplication"),
        }
      },
      "/" => {
        match expected_type {
          JType::FloatingPoint => {
            if left.is_int_value() {
              left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
            }
            if right.is_int_value() {
              right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
            }

            Ok(self.builder.build_float_div(
              left.into_float_value(),
              right.into_float_value(),
              "divtmp"
            ).unwrap().as_basic_value_enum())
          },
          JType::Integer => Ok(self.builder.build_int_unsigned_div(
            left.into_int_value(),
            right.into_int_value(),
            "divtmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for division"),
        }
      },
      "//" => {
        todo!();
      },
      "%" => {
        match expected_type {
          JType::FloatingPoint => {
            if left.is_int_value() {
              left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
            }
            if right.is_int_value() {
              right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
            }

            Ok(self.builder.build_float_rem(
              left.into_float_value(),
              right.into_float_value(),
              "remtmp"
            ).unwrap().as_basic_value_enum())
          },
          JType::Integer => Ok(self.builder.build_int_unsigned_rem(
            left.into_int_value(),
            right.into_int_value(),
            "remtmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for modulus"),
        }
      },
      "^" => {
        todo!();
      },
      "<<" => Ok(self.builder.build_left_shift(
        left.into_int_value(),
        right.into_int_value(),
        "shltmp"
      ).unwrap().as_basic_value_enum()),
      ">>" => Ok(self.builder.build_right_shift(
        left.into_int_value(),
        right.into_int_value(),
        false,
        "shrtmp"
      ).unwrap().as_basic_value_enum()),
      "&&" | "&" => Ok(self.builder.build_and(
        left.into_int_value(),
        right.into_int_value(),
        "andtmp"
      ).unwrap().as_basic_value_enum()),
      "||" | "|" => Ok(self.builder.build_or(
        left.into_int_value(),
        right.into_int_value(),
        "ortmp"
      ).unwrap().as_basic_value_enum()),
      "==" => {
        match (lhs_type, rhs_type) {
          (JType::FloatingPoint, JType::FloatingPoint) => Ok(self.builder.build_float_compare(
            inkwell::FloatPredicate::OEQ,
            left.into_float_value(),
            right.into_float_value(),
            "eqtmp"
          ).unwrap().as_basic_value_enum()),
          (JType::Integer, JType::Integer) => Ok(self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            left.into_int_value(),
            right.into_int_value(),
            "eqtmp"
          ).unwrap().as_basic_value_enum()),
          (JType::FloatingPoint, JType::Integer) | (JType::Integer, JType::FloatingPoint) => {
            if left.is_int_value() {
              left = self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), "left_float").unwrap().as_basic_value_enum();
            }
            if right.is_int_value() {
              right = self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), "right_float").unwrap().as_basic_value_enum();
            }
            Ok(self.builder.build_float_compare(
              inkwell::FloatPredicate::OEQ,
              left.into_float_value(),
              right.into_float_value(),
              "eqtmp"
            ).unwrap().as_basic_value_enum())
          },
          (JType::Enum(a_members), JType::Enum(b_members)) if a_members == b_members => Ok(self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            left.into_int_value(),
            right.into_int_value(),
            "eqtmp"
          ).unwrap().as_basic_value_enum()),
          _ => Err(Error::new(
            Error::CompilerError,
            None,
            &self.code.lines().nth((lhs.first_line.unwrap() - 1) as usize).unwrap().to_string(),
            lhs.first_pos,
            lhs.last_line,
            format!("Invalid types for equality: {} and {}", lhs_type, rhs_type)
          ))
        }
      },
      "!=" => {
        match (lhs_type, rhs_type) {
          (JType::FloatingPoint, JType::FloatingPoint) => Ok(self.builder.build_float_compare(
            inkwell::FloatPredicate::ONE,
            left.into_float_value(),
            right.into_float_value(),
            "netmp"
          ).unwrap().as_basic_value_enum()),
          (JType::Integer, JType::Integer) => Ok(self.builder.build_int_compare(
            inkwell::IntPredicate::NE,
            left.into_int_value(),
            right.into_int_value(),
            "netmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for inequality"), // TODO: pretty up errors
        }
      },
      ">" => {
        match (lhs_type, rhs_type) {
          (JType::FloatingPoint, JType::FloatingPoint) => Ok(self.builder.build_float_compare(
            inkwell::FloatPredicate::OGT,
            left.into_float_value(),
            right.into_float_value(),
            "gttmp"
          ).unwrap().as_basic_value_enum()),
          (JType::Integer, JType::Integer) => Ok(self.builder.build_int_compare(
            inkwell::IntPredicate::SGT,
            left.into_int_value(),
            right.into_int_value(),
            "gttmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for greater than"), // TODO: pretty up errors
        }
      },
      "<" => {
        match (lhs_type, rhs_type) {
          (JType::FloatingPoint, JType::FloatingPoint) => Ok(self.builder.build_float_compare(
            inkwell::FloatPredicate::OLT,
            left.into_float_value(),
            right.into_float_value(),
            "lttmp"
          ).unwrap().as_basic_value_enum()),
          (JType::Integer, JType::Integer) => Ok(self.builder.build_int_compare(
            inkwell::IntPredicate::SLT,
            left.into_int_value(),
            right.into_int_value(),
            "lttmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for less than"), // TODO: pretty up errors
        }
      },
      ">=" => {
        match (lhs_type, rhs_type) {
          (JType::FloatingPoint, JType::FloatingPoint) => Ok(self.builder.build_float_compare(
            inkwell::FloatPredicate::OGE,
            left.into_float_value(),
            right.into_float_value(),
            "getmp"
          ).unwrap().as_basic_value_enum()),
          (JType::Integer, JType::Integer) => Ok(self.builder.build_int_compare(
            inkwell::IntPredicate::SGE,
            left.into_int_value(),
            right.into_int_value(),
            "getmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for greater than or equal"), // TODO: pretty up errors
        }
      },
      "<=" => {
        match (lhs_type, rhs_type) {
          (JType::FloatingPoint, JType::FloatingPoint) => Ok(self.builder.build_float_compare(
            inkwell::FloatPredicate::OLE,
            left.into_float_value(),
            right.into_float_value(),
            "letmp"
          ).unwrap().as_basic_value_enum()),
          (JType::Integer, JType::Integer) => Ok(self.builder.build_int_compare(
            inkwell::IntPredicate::SLE,
            left.into_int_value(),
            right.into_int_value(),
            "letmp"
          ).unwrap().as_basic_value_enum()),
          _ => panic!("Invalid types for less than or equal"), // TODO: pretty up errors
        }
      },
      "+=" => {
        match lhs_type {
          JType::FloatingPoint => {
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
          JType::Integer => {
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
      _ => panic!("Unknown operator: {}", op.0.as_str()),
    }
  }
}

// Tests
#[cfg(test)]
mod tests {
  use core::panic;
  use std::fs;
  use std::fs::File;
  use std::io::Write;
  use std::sync::atomic::{AtomicUsize, Ordering};
  static COUNTER: AtomicUsize = AtomicUsize::new(0);
  use super::*;

  // Compile the IR and assert the output matches
  fn build_and_assert(ir: LLVMString, output: &str) {
    let test_count = COUNTER.fetch_add(1, Ordering::SeqCst);
    let llvm_ir_file = format!("./test{}.ll", test_count);
    let executable_file = if cfg!(target_os = "windows") {
      format!("./test{}.exe", test_count)
    } else {
      format!("./test{}", test_count)
    };

    // Generate LLVM IR file
    let mut file = File::create(&llvm_ir_file).unwrap();
    file.write_all(ir.to_bytes()).unwrap();

    // Compile the executable
    let clang_output = if cfg!(target_os = "windows") {
      std::process::Command::new("clang")
        .arg("-o")
        .arg(&executable_file) // `test1.exe`
        .arg(&llvm_ir_file)    // `test1.ll`
        .output()
        .expect(format!("Failed to execute clang for file {}", llvm_ir_file).as_str())
    } else {
      // If running in GitHub build tests workflow use clang-18
      let clang_cmd = if std::env::var("GH_WORKFLOW").is_ok() {
        std::process::Command::new("clang-18")
        .arg("--version")
        .output()
        .expect("clang-18 is not available in the workflow environment");

        "clang-18"
      } else {
        "clang"
      };

      std::process::Command::new(clang_cmd)
        .arg("-o")
        .arg(&executable_file) // `test1`
        .arg("-no-pie")
        .arg(&llvm_ir_file)    // `test1.ll`
        .output()
        .expect(format!("Failed to execute clang for file {}", llvm_ir_file).as_str())
    };

    // Check for clang errors
    if !clang_output.status.success() {
      panic!(
        "clang compilation failed for '{}' (exit code: {:?}):\nStdout: {}\nStderr: {}",
        llvm_ir_file,
        clang_output.status.code(),
        String::from_utf8_lossy(&clang_output.stdout),
        String::from_utf8_lossy(&clang_output.stderr)
      );
    }

    // Run the executable
    let test = std::process::Command::new(&executable_file)
      .output()
      .expect("Failed to run executable");

    // Delete the test execution files
    fs::remove_file(llvm_ir_file).unwrap();
    fs::remove_file(executable_file).unwrap();

    // Assert the output matches
    if !test.status.success() && String::from_utf8_lossy(&test.stderr) != "" {
      eprintln!("Error: {}", String::from_utf8_lossy(&test.stderr));
      assert!(false);
    } else {
      assert_eq!(String::from_utf8_lossy(&test.stdout), output);
    }
  }

  // Test helper function
  fn run_test(code: &str, expected: &str) -> Result<(), Error> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let mut parser = crate::Parser::new();
    parser.set_source(code.to_string());
    let mut ast = parser.parse(String::new(), false, true).unwrap();
    let mut typer = crate::TypeChecker::new();
    typer.set_source(code.to_string());
    typer.check(&mut ast)?;
    let ir = codegen.build(code.to_string(), ast, IndexMap::new(), false, false).unwrap();
    build_and_assert(ir, expected);
    Ok(())
  }

  #[test]
  fn test_build_assignment_reassignment() -> Result<(), Error> {
    run_test("
      let a = 1;
      let b = 2;
      a = 3;
      let c = a + b;
      printf(\"%d\", c);
    ",
    "5")
  }

  #[test]
  fn test_build_multi_binop() -> Result<(), Error> {
    run_test("
      let a = (1 + 2) * 3;
      let b = 3;
      printf(\"%d\", a / b);
    ",
    "3")
  }

  #[test]
  fn test_build_assign_array_and_indexing() -> Result<(), Error> {
    run_test("
      let a = [1, 2, 3];
      let b = a[2];
      printf(\"%d\", b);
    ",
    "3")
  }

  #[test]
  fn test_build_array_reassign_index() -> Result<(), Error> {
    run_test("
      let a = [1, 2, 3];
      a[0] = 4;
      let b = a[0];
      printf(\"%d\", b);
    ",
    "4")
  }

  #[test]
  fn test_build_for_loop() -> Result<(), Error> {
    run_test("
      let array = [1, 2, 3, 4, 5];
      for (let i in array) {
        printf(\"%d\", array[i]);
      }
    ",
    "12345")
  }

  #[test]
  fn test_build_while_loop() -> Result<(), Error> {
    run_test("
      let i = 0;
      while (i < 5) {
        printf(\"%d\", i);
        i = i + 1;
      }
    ",
    "01234")
  }

  #[test]
  fn test_build_continue() -> Result<(), Error> {
    run_test("
      let i = 0;
      while (i < 5) {
        if (i == 2) {
          i = i + 1;
          continue;
        }
        printf(\"%d\", i);
        i = i + 1;
      }
    ",
    "0134")
  }

  #[test]
  fn test_build_break() -> Result<(), Error> {
    run_test("
      let i = 0;
      while (i < 5) {
        if (i == 3) {
          break;
        }
        printf(\"%d\", i);
        i = i + 1;
      }
    ",
    "012")
  }

  #[test]
  fn test_build_function() -> Result<(), Error> {
    run_test("
      fun add(int a, int b) -> int {
        return a + b;
      }
      let result = add(1, 2);
      printf(\"%d\", result);
    ",
    "3")
  }

  #[test]
  fn test_build_variadic_function() -> Result<(), Error> {
    run_test("
      fun add(int a, int b, int ...nums) -> int {
        let sum = a + b;
        for (let i in nums) {
          sum += nums[i];
        }
        return sum;
      }

      let result = add(1, 2, 3, 4, 5);
      printf(\"%d\", result);
    ",
    "15")
  }

  #[test]
  fn test_build_conditional_phi() -> Result<(), Error> {
    run_test("
      let a = 1;
      if (a == 1) {
        a = 5;
      } else {
        a = 10;
      }
      printf(\"%d\", a);
    ",
    "5")
  }

  #[test]
  fn test_build_type_struct_index() -> Result<(), Error> {
    run_test("
      type Test = {
        a: int,
        b: int,
      }
      Test test = { a: 1, b: 2 };
      printf(\"%d\", test.b);
    ",
    "2")
  }

  #[test]
  fn test_build_anonymous_struct_mapping() -> Result<(), Error> {
    run_test("
      type Person = {
        name: string,
        age: int
      }

      fun print_person(Person p) {
        printf(\"%s is %d years old.\", p.name, p.age);
      }

      print_person({
        name: \"Marceline\",
        age: 1000
      });
    ",
    "Marceline is 1000 years old.")
  }

  #[test]
  fn test_build_deep_type_struct_index() -> Result<(), Error> {
    run_test("
      // types
      type TestType = { a: int, b: int, c: int, d: int, };
      type TestTypeTwo = { test_one: TestType, };
      type TestTypeThree = { test_two: TestTypeTwo, };
      type TestTypeFour = { test_three: TestTypeThree, };
      // vars
      TestType test_var = { a: 1, b: 2, c: 3, d: 4 };
      TestTypeTwo test_var_two = { test_one: test_var };
      TestTypeThree test_var_three = { test_two: test_var_two };
      TestTypeFour test_var_four = { test_three: test_var_three };
      printf(\"%d, %d, %d, %d\",
        test_var_four.test_three.test_two.test_one.a,
        test_var_three.test_two.test_one.b,
        test_var_two.test_one.c,
        test_var.d
      );
    ",
    "1, 2, 3, 4")
  }

  #[test]
  fn test_build_enum_type() -> Result<(), Error> {
    run_test("
      enum TestEnum = {
        TestOne,
        TestTwo,
        TestThree,
      }
      let test_var = TestEnum.TestThree;
      printf(\"%d\", test_var);
    ",
    "2")
  }

  #[test]
  fn test_build_class_self() -> Result<(), Error> {
    run_test("
      cls TestClass = {
        int a;

        fun new(int a) -> self {
          self.a = a;
        }

        fun print_self() -> void {
          printf(\"%d\", self.a);
        }
      }

      TestClass obj = TestClass(42);
      obj.print_self();
    ",
    "42")
  }
}