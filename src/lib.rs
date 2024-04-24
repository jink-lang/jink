use std::fmt::{ Display, Formatter, Result };

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
  "+", "-", "*", "/", "//", "%", "^", "=", "->",
  ">", "<", ">=", "<=", "==", "!=",
  "-=", "+=", "*=", "/=", "//=", "%=",
  "!", "&", "?", "|", "::", "~", "#", ".",
  "&&", "||", "++", "--"
];

pub const KEYWORDS: &[&str] = &[
  "if", "else", "elseif",
  "return", "del",
  "true", "false", "null",
  "fun", "let", "const", "type",
  "cls", "self", "pub",
  "import", "from", "as"
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
  Object(Box<Vec<Self>>),
  ObjectProperty(Option<Name>, Box<Expression>),
  Identifier(Name, Option<Box<Self>>),
  Null,
  EOF
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  /// literal expr
  Literal(Literals),

  /// op; left; right
  BinaryOperator(Operator, Box<Self>, Box<Self>),
  /// op; value
  UnaryOperator(Operator, Box<Self>),

  /// type or let/const; ident; value
  Assignment(Option<Type>, Box<Literals>, Option<Box<Self>>),

  Array(Box<Vec<Self>>),

  /// name; literal
  TypeDef(Literals, Box<Literals>),

  /// if/else/elseif; expression; body; else-body
  Conditional(Type, Option<Box<Self>>, Option<Box<Vec<Self>>>, Option<Box<Vec<Self>>>),

  /// func name; args
  Call(Name, Box<Vec<Self>>),
  /// func name; return type; params; body
  Function(Name, Option<Literals>, Option<Box<Vec<Self>>>, Option<Box<Vec<Self>>>),
  /// type or let/const; ident; default
  /// 
  /// TODO?: Refactor Type here so that we don't have to rely on distinguishing between ident and let/const
  FunctionParam(Type, Literals, Option<Box<Self>>),
  /// value
  Return(Box<Self>),

  /// class name; parent classes; body
  Class(Name, Option<Vec<Name>>, Option<Box<Vec<Self>>>),

  /// parent; child
  /// 
  /// top level we don't need option but last child will be empty
  ObjectIndex(Box<Self>, Option<Box<Self>>),

  /// index; aliases<parent; opt child>[]
  /// 
  /// index is list of names in import ["std", "io"]
  /// 
  /// aliases is optional list of tuples of names and optional alias
  /// 
  /// for modules with no aliases, it will be none
  /// 
  /// only `from` imports will have multiple aliases, regular imports can only have one
  Module(Vec<Name>, Option<Vec<(Name, Option<Name>)>>),
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
    return current;
  }
}

impl std::fmt::Display for FutureIter {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{:?}", self.input)
  }
}
