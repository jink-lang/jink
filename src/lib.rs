use std::fmt::{ Display, Formatter, Result };
use std::collections::HashMap;

#[derive(strum_macros::Display, Debug, Eq, PartialEq, Clone, Default)]
pub enum TokenTypes {
  #[default]
  EOF,
  Newline,
  Keyword,
  Identifier,
  Number,
  String,
  Operator,
  LParen,
  RParen,
  LBracket,
  RBracket,
  LBrace,
  RBrace,
  Semicolon,
  Colon,
  Comma
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Token {
  pub of_type: TokenTypes,
  pub value: Option<String>,
  pub line: i32,
  /// Start position of token on the line
  pub start_pos: Option<i32>,
  /// End position of token on the line
  pub end_pos: Option<i32>
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "Token {{ {}:{:?} }} at line {}", self.of_type, self.value, self.line)
  }
}

pub const OPERATORS: &[&str] = &[
  "+", "-", "*", "/", "//", "%", "^", "=", "->", "...",
  ">", "<", ">=", "<=", "==", "!=", "..",
  "-=", "+=", "*=", "/=", "//=", "%=",
  "!", "&", "?", "|", "::", "~", "#", ".",
  "&&", "||", "++", "--", "<<", ">>"
];

pub const KEYWORDS: &[&str] = &[
  "if", "else", "elseif",
  "while", "for", "in", "break", "continue",
  "return", "del",
  "true", "false", "null",
  "fun", "let", "const", "type", "enum",
  "cls", "self", "pub",
  "import", "from", "as", "extern"
];

#[derive(Debug, PartialEq, Clone)]
// May need specific type information if operators ever have different behaviour in different contexts
// pub struct Operator { of_type: String }
pub struct Operator(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct Name(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct Type(pub String);

#[derive(Debug, PartialEq, Clone)]
pub enum Literals {
  Integer(i64),
  UnsignedInteger(u64),
  FloatingPoint(f64),
  String(String),
  Boolean(bool),
  Object(Box<Vec<Self>>),
  ObjectProperty(Name, Box<Expression>),
  Identifier(Name),
  Null,
  EOF
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
  /// literal expr
  Literal(Literals),

  /// op; left; right
  BinaryOperator(Operator, Box<Expression>, Box<Expression>),

  /// op; value
  UnaryOperator(Operator, Box<Expression>),

  /// type or let/const; ident or index; value
  Assignment(Option<Type>, Box<Expression>, Option<Box<Expression>>),

  Array(Box<Vec<Expression>>),

  /// assignment; iterator; body
  ForLoop(Box<Expression>, Box<Expression>, Option<Vec<Expression>>),

  // condition; body
  WhileLoop(Box<Expression>, Option<Vec<Expression>>),

  ContinueLoop, BreakLoop,

  /// name; literal
  TypeDef(Name, Box<Literals>),

  // name; literals
  Enum(Name, Vec<Literals>),

  /// if/else/elseif; expression; body; else-body
  Conditional(Type, Option<Box<Expression>>, Box<Vec<Expression>>, Option<Box<Vec<Expression>>>),

  /// func name; args
  Call(Name, Box<Vec<Expression>>),

  /// func name; return type; params; body
  Function(Name, Option<Literals>, Option<Box<Vec<Expression>>>, Option<Box<Vec<Expression>>>),

  /// type/let; is constant; ident; default; is spread
  FunctionParam(Option<Type>, bool, Literals, Option<Box<Expression>>, bool),

  /// value
  Return(Box<Expression>),

  /// class name; parent classes; body
  Class(Name, Option<Vec<Name>>, Option<Box<Vec<Expression>>>),
  SelfRef,

  /// abi; func name; return type; params; variadic
  Extern(String, Name, Option<Literals>, Option<Box<Vec<Expression>>>, bool),

  /// expr
  Public(Box<Expression>),

  /// expr
  Delete(Box<Expression>),

  /// parent; child
  /// 
  /// hello.there()
  /// 
  /// hello.hi
  /// 
  /// hello.greetings\[2]
  /// 
  /// hello.bye().seeya
  Index(Box<Expression>, Box<Expression>),

  /// arr; index
  /// 
  /// hello\[0]
  ArrayIndex(Box<Expression>, Box<Expression>),

  /// index; is_aliased; aliases<parent; opt child>[]; ast when parsed
  /// 
  /// index is list of names in import ["std", "io"]
  /// 
  /// aliases is optional list of tuples of names and optional alias
  /// 
  /// for modules with no aliases, it will be none
  /// 
  /// only `from` imports will have multiple aliases, regular imports can only have one
  Module(Vec<Name>, bool, Option<Vec<(Name, Option<Name>)>>),
  ModuleParsed(Vec<Name>, bool, Option<Vec<(Name, Option<Name>)>>, Vec<Expression>)
}

// Type checker types
#[derive(Debug, PartialEq, Clone)]
pub enum JType {
  Integer,
  UnsignedInteger,
  FloatingPoint,
  String,
  Boolean,
  Pointer,
  Object(HashMap<String, JType>),
  Array(Box<JType>),
  /// type; default value; is spread
  FunctionParam(Box<JType>, Option<Box<JType>>, bool),
  /// parameters; return type
  Function(Vec<JType>, Box<JType>),
  /// fields
  StructDef(HashMap<String, JType>),
  /// members
  Enum(Vec<String>),
  /// Absence of a value or type, e.g., for statements
  Null,
  Void,
  /// Type that cannot be determined yet
  Unknown,
  /// Named types such as classes or type aliases
  TypeName(String),
}

impl std::fmt::Display for JType {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      JType::Integer => write!(f, "int"),
      JType::UnsignedInteger => write!(f, "uint"),
      JType::FloatingPoint => write!(f, "float"),
      JType::String => write!(f, "string"),
      JType::Boolean => write!(f, "bool"),
      JType::Pointer => write!(f, "ptr"),
      JType::Object(_) => write!(f, "object"), // TODO: Improve display
      JType::Array(t) => write!(f, "array<{}>", t),
      JType::FunctionParam(t, default, is_spread) => {
        if *is_spread {
          write!(f, "...")?;
        }
        write!(f, "{}", t)?;
        if let Some(default) = default {
          write!(f, " = {}", default)?;
        }
        Ok(())
      },
      JType::Function(params, ret) => {
        write!(f, "fun(")?;
        for (i, param) in params.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", param)?;
        }
        write!(f, ") -> {}", ret)
      },
      JType::StructDef(fields) => {
        write!(f, "struct(")?;
        for (i, (name, field_type)) in fields.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}: {}", name, field_type)?;
        }
        write!(f, ")")
      },
      JType::Enum(members) => {
        write!(f, "enum(")?;
        for (i, member) in members.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", member)?;
        }
        write!(f, ")")
      },
      JType::Null => write!(f, "null"),
      JType::Void => write!(f, "void"),
      JType::Unknown => write!(f, "unknown"),
      JType::TypeName(name) => write!(f, "{}", name),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
  pub expr: Expr,
  pub first_line: Option<i32>,
  pub first_pos: Option<i32>,
  pub last_line: Option<i32>,
  pub inferred_type: Option<JType>,
}

pub struct FutureIter {
  pub input: Vec<Token>,
  iter: std::iter::Peekable<std::vec::IntoIter<Token>>,
  pub current: Option<Token>,
}

impl FutureIter {
  pub fn new(input: Vec<Token>) -> Self {
    let cloned_input = input.clone();
    let mut iter = cloned_input.into_iter().peekable();
    let current = iter.next();

    Self {
      input,
      iter,
      current,
    }
  }

  pub fn dump(&mut self) -> Vec<Token> {
    return self.iter.to_owned().collect::<Vec<Token>>();
  }

  pub fn load(&mut self, tokens: Vec<Token>) {
    self.iter = tokens.into_iter().peekable();
    self.current = self.iter.peek().cloned();
  }

  pub fn next(&mut self) -> Option<Token> {
    let current = self.current.take();
    self.current = self.iter.next();
    return current;
  }

  pub fn peek(&mut self) -> Option<&Token> {
    return self.iter.peek();
  }
}

impl std::fmt::Display for FutureIter {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{:?}", self.input)
  }
}

#[derive(Debug)]
pub struct ErrorCtx {
  pub token: Option<Token>,
  pub line: String,
  pub start_pos: Option<i32>,
  pub end_pos: Option<i32>,
  pub message: String
}

#[derive(Debug)]
pub enum Error {
  UnexpectedToken(ErrorCtx),
  UnexpectedEOF(ErrorCtx),
  EmptyFunctionBody(ErrorCtx),
  UnexpectedExpression(ErrorCtx),
  ParserError(ErrorCtx),
  NameError(ErrorCtx),
  ImportError(ErrorCtx),
  CompilerError(ErrorCtx),
}

impl Error {
  pub fn new(err: fn(ErrorCtx) -> Error, token: Option<Token>, line: &str, start_pos: Option<i32>, end_pos: Option<i32>, message: String) -> Error {
    return err(ErrorCtx {
      token,
      line: line.to_string(),
      start_pos,
      end_pos,
      message: message.to_string()
    });
  }
}

// Original error format here temporarily
// panic!("Unexpected token {:?} at {}:{}\n  {}\n  {}", init.unwrap().of_type,
//   init.unwrap().line, init.unwrap().start_pos.unwrap() + 1,
//   self.code.lines().nth((init.unwrap().line - 1) as usize).unwrap(),
//   " ".repeat((init.unwrap().start_pos.unwrap()) as usize) + "^"
// );

impl std::fmt::Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Error::UnexpectedToken(err) => {
        let spaces;
        if err.token.as_ref().unwrap().start_pos.unwrap() > 0 {
          spaces = " ".repeat(err.token.as_ref().unwrap().start_pos.unwrap() as usize);
        } else {
          spaces = "".to_string();
        }

        let underline;
        if err.token.as_ref().unwrap().end_pos.unwrap() - err.token.as_ref().unwrap().start_pos.unwrap() > 1 {
          underline = "-".repeat((err.token.as_ref().unwrap().end_pos.unwrap() - err.token.as_ref().unwrap().start_pos.unwrap()) as usize);
        } else {
          underline = "-".to_string();
        }

        return write!(f, "{} {:?} at {}:{}\n  {}\n  {}\n  {}", err.message, err.token.as_ref().unwrap().of_type,
          err.token.as_ref().unwrap().line, err.token.as_ref().unwrap().start_pos.unwrap() + 1,
          err.line,
          spaces.to_string() + underline.as_str(),
          spaces + "^",
        );
      },
      Error::UnexpectedExpression(err) => {
        let underline = "-".repeat(err.line.split("\n").collect::<Vec<&str>>()[0].len());
        return write!(f, "{} at {}:{}\n  {}\n  {}\n  {}", err.message, err.token.as_ref().unwrap().line,
          err.start_pos.unwrap() + 1, err.line,
          underline,
          " ".repeat((err.start_pos.unwrap()) as usize) + "^"
        );
      },
      Error::UnexpectedEOF(err) => {
        return write!(f, "Unexpected EOF at {}:{}\n  {}\n  {}", err.line, err.start_pos.unwrap() + 1,
          " ".repeat((err.start_pos.unwrap()) as usize) + "^",
          err.message
        );
      },
      Error::EmptyFunctionBody(err) => {
        let underline = "-".repeat(err.line.split("\n").collect::<Vec<&str>>()[0].len());
        return write!(f, "Empty function body at {}:{}\n  {}\n  {}\n  {}", err.token.as_ref().unwrap().line,
          err.start_pos.unwrap() + 1, err.line,
          underline,
          " ".repeat((err.start_pos.unwrap()) as usize) + "^"
        );
      },
      Error::ParserError(err) => {
        let line = if let Some(token) = &err.token { token.line } else { 0 };
        return write!(f, "Parser error at {}:{}\n  {}\n  {}", line,
          err.start_pos.unwrap_or(0) + 1, err.line, err.message
        );
      },
      Error::NameError(err) => {
        let end = err.end_pos.unwrap_or(err.start_pos.unwrap_or(0) + 1);
        let start = err.start_pos.unwrap_or(0);
        let underline = " ".repeat(start as usize) + &"-".repeat((end - start) as usize);
        return write!(f, "Name error at {}:{}\n  {}\n  {}\n\n{}", end,
          start + 1, err.line, underline, err.message
        );
      },
      Error::ImportError(err) => {
        let end = err.end_pos.unwrap_or(err.start_pos.unwrap_or(0) + 1);
        let start = err.start_pos.unwrap_or(0);
        return write!(f, "Import error at {}:{}\n  {}\n  {}\n\n{}", end,
          start + 1, err.line, " ".repeat(start as usize) + "^", err.message
        );
      },
      Error::CompilerError(err) => {
        let line = err.line.split("\n").next().unwrap_or("");
        let start = err.start_pos.unwrap_or(0);
        let mut dashes = if let Some(end) = err.end_pos {
          end - start
        } else {
          line.trim_start().len() as i32 - start
        };
        if dashes <= 0 { dashes = 1; }
        let underline = " ".repeat(start as usize) + &"-".repeat(dashes as usize);
        let end = err.end_pos.unwrap_or(start + 1);
        return write!(f, "Compilation error at {}:{}\n  {}\n  {}\n\n{}", end,
          start + 1, err.line, underline, err.message
        );
      }
    }
  }
}