use crate::source::SourceReader;
use crate::tokens::Token;
use std::convert::TryInto;
use strip_bom::StripBom;

#[derive(Clone)]
pub struct TokenReader<'a> {
    /// The source of this TokenReader
    src: SourceReader<'a>,
}

impl<'a> Iterator for TokenReader<'a> {
    type Item = Token<'a>;

    /// Read a token from the current position in the source
    fn next(&mut self) -> Option<Token<'a>> {
        while let Some(c) = self.src.next() {
            return Some(match c {
                '/' => {
                    let peeked = self.src.peek();
                    match peeked {
                        // Comments
                        Some('/') => {
                            self.src.find(|s| *s == '\n');
                            continue;
                        }
                        Some('*') => {
                            self.src.advance(1);
                            self.src.find(|s| *s == '*');
                            self.src.next_eq('/');
                            continue;
                        }
                        _ => (),
                    };
                    // Division
                    self.src.next_match(&[('=', Token::DivAssign)], Token::Div)
                }
                '"' => Token::String(self.src.read_until(|ch| *ch == '"')),
                '\'' => Token::String(self.src.read_until(|ch| *ch == '\'')),
                '(' => Token::OpenParenthesis,
                ')' => Token::CloseParenthesis,
                '{' => Token::OpenCurly,
                '}' => Token::CloseCurly,
                '[' => Token::OpenBracket,
                ']' => Token::CloseBracket,
                '+' => self
                    .src
                    .next_match(&[('+', Token::Incr), ('=', Token::AddAssign)], Token::Add),
                '-' => self
                    .src
                    .next_match(&[('-', Token::Decr), ('=', Token::SubAssign)], Token::Sub),
                '*' => self.src.next_match(&[('=', Token::MulAssign)], Token::Mul),
                '%' => self.src.next_match(&[('=', Token::ModAssign)], Token::Mod),
                '>' => self.src.next_match_multi(
                    &[
                        (">>=", Token::BitURShiftAssign),
                        (">>", Token::BitURShift),
                        (">=", Token::BitRShiftAssign),
                        ("=", Token::GreaterEqual),
                        (">", Token::BitRShift),
                    ],
                    Token::Greater,
                ),
                '<' => self.src.next_match_multi(
                    &[
                        ("<=", Token::BitLShiftAssign),
                        ("=", Token::LessEqual),
                        ("<", Token::BitLShift),
                    ],
                    Token::Less,
                ),
                '&' => self.src.next_match(
                    &[('=', Token::BitAndAssign), ('&', Token::And)],
                    Token::BitAnd,
                ),
                '|' => self
                    .src
                    .next_match(&[('=', Token::BitOrAssign), ('|', Token::Or)], Token::BitOr),
                '~' => Token::BitNot,
                '^' => self
                    .src
                    .next_match(&[('=', Token::BitXorAssign)], Token::BitXor),
                '!' => self.src.next_match_multi(
                    &[("==", Token::StrictInequality), ("=", Token::Inequality)],
                    Token::Not,
                ),
                '=' => self.src.next_match_multi(
                    &[("==", Token::StrictEquality), ("=", Token::Equality)],
                    Token::Assign,
                ),
                '?' => Token::Ternary,
                ':' => Token::Colon,
                '.' => Token::Dot,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '\n' => Token::Newline,
                c if c.is_whitespace() => continue,
                c if c.is_ascii_digit() => {
                    let number = if self.src.next_eq('x') {
                        let hex = self.src.read_until_peek(|ch| !ch.is_ascii_hexdigit());
                        if let Ok(num) = hex.parse::<i64>() {
                            Token::Integer(num)
                        } else {
                            Token::Unknown
                        }
                    } else {
                        self.src.advance_back(1);
                        let number = self
                            .src
                            .read_until_peek(|ch| !ch.is_ascii_digit() && *ch != '.');
                        if number.contains('.') {
                            if let Ok(num) = number.parse::<f64>() {
                                Token::Number(num)
                            } else {
                                Token::Unknown
                            }
                        } else if let Ok(num) = number.parse::<i64>() {
                            Token::Integer(num)
                        } else {
                            Token::Unknown
                        }
                    };
                    // Custom handling of exponents
                    if self.src.next_eq('e') {
                        self.src.next_eq('+'); // Consume + if there is one.
                        let is_negative = self.src.next_eq('-');
                        let exp_str = self.src.read_until_peek(|ch| !ch.is_ascii_digit());
                        let exp = exp_str.parse::<i64>().unwrap();

                        match number {
                            Token::Integer(i) if !is_negative => {
                                Token::Integer(i * (10i64.saturating_pow(exp.try_into().unwrap())))
                            }
                            Token::Integer(i) => {
                                Token::Number(i as f64 * (10.0f64.powf(exp.wrapping_neg() as f64)))
                            }
                            Token::Number(n) => Token::Number(n * (10.0f64.powf(exp as f64))),
                            Token::Unknown => Token::Unknown,
                            _ => unreachable!(),
                        }
                    } else {
                        number
                    }
                }
                c if is_valid_for_word(c) => {
                    self.src.advance_back(1);
                    match self
                        .src
                        .read_until_peek(|ch| !is_valid_for_word_numeric(*ch))
                    {
                        "break" => Token::Break,
                        "continue" => Token::Continue,
                        "return" => Token::Return,
                        "function" => Token::Function,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "new" => Token::New,
                        "while" => Token::While,
                        "for" => Token::For,
                        "case" => Token::Case,
                        "class" => Token::Class,
                        "set" => Token::Set,
                        "default" => Token::Default,
                        "delete" => Token::Delete,
                        "do" => Token::Do,
                        "try" => Token::Try,
                        "catch" => Token::Catch,
                        "finally" => Token::Finally,
                        "with" => Token::With,
                        "throw" => Token::Throw,
                        "switch" => Token::Switch,
                        "var" => Token::Var,
                        "in" => Token::In,
                        "instanceof" => Token::Instanceof,
                        "typeof" => Token::Typeof,
                        "void" => Token::Void,
                        "dynamic" => Token::Dynamic,
                        "extends" => Token::Extends,
                        "get" => Token::Get,
                        "implements" => Token::Implements,
                        "import" => Token::Import,
                        "interface" => Token::Interface,
                        "intrinsic" => Token::Intrinsic,
                        "private" => Token::Private,
                        "public" => Token::Public,
                        "static" => Token::Static,
                        "null" => Token::Null,
                        "undefined" => Token::Undefined,
                        "true" => Token::Bool(true),
                        "false" => Token::Bool(false),
                        word => Token::Word(word),
                    }
                }
                _ => Token::Unknown,
            });
        }
        None
    }
}

fn is_valid_for_word(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_valid_for_word_numeric(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

impl<'a> TokenReader<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src: SourceReader::new(src.strip_bom()),
        }
    }
}
