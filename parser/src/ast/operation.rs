use tokenizer::tokens::Token;

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    BitAnd,
    BitLShift,
    BitOr,
    BitRShift,
    BitURShift,
    BitXor,
    Equality,
    Greater,
    GreaterEqual,
    Inequality,
    Less,
    LessEqual,
    StrictEquality,
    StrictInequality,
    InstanceOf,
}

impl Operation {
    pub fn from_token(token: Token) -> Option<Operation> {
        Some(match token {
            Token::Add => Operation::Add,
            Token::Sub => Operation::Sub,
            Token::Div => Operation::Div,
            Token::Mod => Operation::Mod,
            Token::Mul => Operation::Mul,
            Token::BitAnd => Operation::BitAnd,
            Token::BitLShift => Operation::BitLShift,
            Token::BitOr => Operation::BitOr,
            Token::BitRShift => Operation::BitRShift,
            Token::BitURShift => Operation::BitURShift,
            Token::BitXor => Operation::BitXor,
            Token::Equality => Operation::Equality,
            Token::Greater => Operation::Greater,
            Token::GreaterEqual => Operation::GreaterEqual,
            Token::Inequality => Operation::Inequality,
            Token::Less => Operation::Less,
            Token::LessEqual => Operation::LessEqual,
            Token::StrictEquality => Operation::StrictEquality,
            Token::StrictInequality => Operation::StrictInequality,
            Token::Instanceof => Operation::InstanceOf,
            _ => return None,
        })
    }
}
