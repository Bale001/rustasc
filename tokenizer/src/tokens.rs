use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token<'a> {
    // Arithmetic
    Add,
    Sub,
    Div,
    Decr,
    Incr,
    Mod,
    Mul,

    // Arithmetic compound assignment
    AddAssign,
    SubAssign,
    ModAssign,
    DivAssign,
    MulAssign,

    // Bitwise
    BitAnd,
    BitLShift,
    BitNot,
    BitOr,
    BitRShift,
    BitURShift,
    BitXor,

    // 	Bitwise compound assignment
    BitAndAssign,
    BitLShiftAssign,
    BitOrAssign,
    BitRShiftAssign,
    BitURShiftAssign,
    BitXorAssign,

    // Comparison
    Equality,
    Greater,
    GreaterEqual,
    Inequality,
    Less,
    LessEqual,
    StrictEquality,
    StrictInequality,

    // Logic
    And,
    Not,
    Or,

    // Brackets
    OpenBracket,
    CloseBracket,
    OpenParenthesis,
    CloseParenthesis,
    OpenCurly,
    CloseCurly,

    // Statements
    Break,
    Continue,
    Return,
    Function,
    If,
    Else,
    While,
    For,
    Case,
    Class,
    Default,
    Delete,
    Do,
    Try,
    Catch,
    Finally,
    With,
    Throw,
    Switch,
    Var,
    In,
    Instanceof,
    Typeof,

    // Keywords
    Dynamic,
    Extends,
    Get,
    Set,
    Implements,
    Import,
    Interface,
    Intrinsic,
    Private,
    Public,
    Static,
    New,
    Void,

    // Primitives
    Null,
    Undefined,
    Bool(bool),
    Integer(i64),
    Number(f64),
    String(&'a str),

    // Other
    Assign,
    Dot,
    Comma,
    Semicolon,
    Ternary,
    Colon,
    Newline,
    Unknown,
    Eof,
    Word(&'a str),
}

impl<'a> Token<'a> {
    pub fn unwrap_word(self) -> Option<&'a str> {
        match self {
            Token::Word(s) => Some(s),
            _ => None,
        }
    }

    pub fn is_compound_assign(&self) -> bool {
        matches!(
            self,
            Token::AddAssign
                | Token::SubAssign
                | Token::MulAssign
                | Token::DivAssign
                | Token::ModAssign
                | Token::BitAndAssign
                | Token::BitLShiftAssign
                | Token::BitOrAssign
                | Token::BitRShiftAssign
                | Token::BitURShiftAssign
                | Token::BitXorAssign
        )
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Token::Add => "+",
            Token::Sub => "-",
            Token::Div => "/",
            Token::Mul => "*",
            Token::Decr => "--",
            Token::Incr => "++",
            Token::Mod => "%",
            Token::AddAssign => "+=",
            Token::SubAssign => "-=",
            Token::MulAssign => "*=",
            Token::DivAssign => "/=",
            Token::ModAssign => "%=",
            Token::BitAnd => "&",
            Token::BitOr => "|",
            Token::BitLShift => "<<",
            Token::BitRShift => ">>",
            Token::BitNot => "~",
            Token::BitXor => "^",
            Token::BitURShift => ">>>",
            Token::BitAndAssign => "&=",
            Token::BitOrAssign => "|=",
            Token::BitLShiftAssign => "<<=",
            Token::BitRShiftAssign => ">>=",
            Token::BitXorAssign => "^=",
            Token::BitURShiftAssign => ">>>=",
            Token::Equality => "==",
            Token::Greater => ">",
            Token::GreaterEqual => ">=",
            Token::Less => "<",
            Token::LessEqual => "<=",
            Token::Inequality => "!=",
            Token::StrictEquality => "===",
            Token::StrictInequality => "!==",
            Token::And => "&&",
            Token::Or => "||",
            Token::Not => "!",
            Token::Assign => "=",
            Token::Colon => ":",
            Token::Ternary => "?",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Newline => "\\n",
            Token::Eof => "EOF",
            Token::Unknown => "unknown",
            Token::OpenBracket => "[",
            Token::CloseBracket => "]",
            Token::OpenCurly => "{",
            Token::CloseCurly => "}",
            Token::OpenParenthesis => "(",
            Token::CloseParenthesis => ")",
            Token::Null => "null",
            Token::Undefined => "undefined",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::Return => "return",
            Token::Function => "function",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::Do => "do",
            Token::For => "case",
            Token::Switch => "switch",
            Token::Case => "case",
            Token::Default => "default",
            Token::Class => "class",
            Token::Delete => "delete",
            Token::Try => "try",
            Token::Catch => "catch",
            Token::Finally => "finally",
            Token::With => "with",
            Token::Throw => "throw",
            Token::Var => "var",
            Token::In => "in",
            Token::Instanceof => "instanceof",
            Token::Typeof => "typeof",
            Token::Dynamic => "dynamic",
            Token::Extends => "extends",
            Token::Get => "get",
            Token::Set => "set",
            Token::Implements => "implements",
            Token::Import => "import",
            Token::Interface => "interface",
            Token::Intrinsic => "intrinsic",
            Token::Private => "private",
            Token::Public => "public",
            Token::Static => "static",
            Token::New => "new",
            Token::Void => "void",
            Token::Bool(true) => "true",
            Token::Bool(false) => "false",
            Token::Word(_) => "identifier",
            Token::String(_) => "string",
            Token::Integer(_) => "integer",
            Token::Number(_) => "number",
        };
        formatter.write_str(s)
    }
}
