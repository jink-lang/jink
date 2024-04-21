use std::fmt::{ Display, Formatter, Result };

#[derive(strum_macros::Display, Debug, Eq, PartialEq, Clone)]
pub enum TokenTypes {
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
  pub of_type: TokenTypes,
  pub value: Option<String>,
  pub line: i32
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "Token {{ {}:{:?} }} at line {}", self.of_type, self.value, self.line)
  }
}

pub const OPERATORS: &[&str] = &[
  "+", "-", "*", "/", "//", "%", "^", "=", "->",
  ">", "<", ">=", "<=", "==", "!=",
  "-=", "+=", "*=", "/=", "//=", "%=",
  "!", "&", "?", "|", "::", "~", "#", ".",
  "&&", "||", "++", "--"
];

pub const KEYWORDS: &[&str] = &[
  "if", "else", "elseif",
  "import", "return", "del",
  "true", "false", "null",
  "fun", "let", "const", "type",
  "cls", "pub"
];

#[derive(Debug, PartialEq)]
// May need specific type information if operators ever have different behaviour in different contexts
// pub struct Operator { of_type: String }
pub struct Operator(pub String);

#[derive(Debug, PartialEq)]
pub struct Name(pub String);

#[derive(Debug, PartialEq)]
pub struct Type(pub String);

#[derive(Debug, PartialEq)]
pub enum Literals {
  Integer(i64),
  UnsignedInteger(u64),
  FloatingPoint(f64),
  String(String),
  Boolean(bool),
  Object(Box<Vec<Literals>>),
  ObjectProperty(Option<Name>, Box<Expression>),
  Identifier(Name, Option<Box<Literals>>),
  Null,
  EOF
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  // literal expr
  Literal(Literals),

  // op; left; right
  BinaryOperator(Operator, Box<Expression>, Box<Expression>),
  // op; value
  UnaryOperator(Operator, Box<Expression>),

  // type or let/const; ident; value
  Assignment(Option<Type>, Box<Literals>, Option<Box<Expression>>),

  Array(Box<Vec<Expression>>),

  // name; literal
  TypeDef(Literals, Box<Literals>),

  // if/else/elseif; expression; body; else-body
  Conditional(Type, Option<Box<Expression>>, Option<Box<Vec<Expression>>>, Option<Box<Vec<Expression>>>),

  // func name; args
  Call(Name, Box<Vec<Expression>>),
  // func name; return type; params; body
  Function(Name, Option<Literals>, Option<Box<Vec<Expression>>>, Option<Box<Vec<Expression>>>),
  // type or let/const; ident; default
  // TODO?: Refactor Type here so that we don't have to rely on distinguishing between ident and let/const
  FunctionParam(Type, Literals, Option<Box<Expression>>),
  // value
  Return(Box<Expression>),

  // class name; parent classes; body
  Class(Name, Option<Vec<Name>>, Option<Box<Vec<Expression>>>)

  // name; index
  // Module(Name, )
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

  pub fn next(&mut self) -> Option<Token> {
    let current = self.current.take();
    self.current = self.iter.next();
    current
  }
}

impl std::fmt::Display for FutureIter {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{:?}", self.input)
  }
}
