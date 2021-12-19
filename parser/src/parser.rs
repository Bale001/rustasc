use crate::ast::{
    expression::{Compare, Expression, Literal, Side, Symbol},
    identifier::{Identifier, ReturnType, SymbolName},
    instruction::{Block, Instruction},
    operation::Operation,
    package::{MethodKind, Package, PackageBody, Script, Trait},
    statements::{Declare, For, ForIn, If, Switch, Try, While, With},
};
use crate::linker::Linker;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::rc::Rc;
use swf::avm1::types::FunctionFlags;
use tokenizer::lexer::TokenReader;
use tokenizer::tokens::Token;

pub struct Parser<'a> {
    stream: Peekable<TokenReader<'a>>,
    definitions: HashMap<Identifier<'a>, Symbol<'a>>,
    imports: HashMap<Cow<'a, str>, Identifier<'a>>,
    linker: Option<Rc<RefCell<Linker<'a>>>>,
    name: Option<Identifier<'a>>,
    flags: FunctionFlags,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            stream: TokenReader::new(src).peekable(),
            definitions: HashMap::new(),
            imports: HashMap::new(),
            linker: None,
            name: None,
            flags: FunctionFlags::SUPPRESS_SUPER
                | FunctionFlags::SUPPRESS_ARGUMENTS
                | FunctionFlags::SUPPRESS_THIS,
        }
    }

    pub fn new_with_linker(src: &'a str, linker: Rc<RefCell<Linker<'a>>>) -> Self {
        Self {
            stream: TokenReader::new(src).peekable(),
            definitions: HashMap::new(),
            imports: HashMap::new(),
            linker: Some(linker),
            name: None,
            flags: FunctionFlags::SUPPRESS_SUPER
                | FunctionFlags::SUPPRESS_ARGUMENTS
                | FunctionFlags::SUPPRESS_THIS,
        }
    }

    pub fn definitions(&self) -> &HashMap<Identifier<'a>, Symbol<'a>> {
        &self.definitions
    }

    pub fn load_package(
        &mut self,
        id: Identifier<'a>,
        content: &'static str,
    ) -> Result<(), ParseError> {
        // Mark this as resolving to avoid circular imports
        if let Some(linker) = &self.linker {
            linker.borrow_mut().set_resolving(id.clone());
            let mut parser = Parser::new_with_linker(content, linker.clone());
            let package = parser.parse_package(id.clone())?;
            linker.borrow_mut().set_resolved(id.clone(), package);
        }

        Ok(())
    }

    pub fn parse_import(&mut self) -> Result<(), ParseError> {
        let base = self.next_word()?;
        if !self.next_is_lossy(|t| matches!(t, Token::Dot)) {
            return Ok(());
        }
        let mut path = Vec::new();
        while let Some(token) = self.peek_lossy() {
            match token {
                Token::Word(word) => {
                    self.next_lossy();
                    path.push(Cow::from(word));
                    if !self.next_is_lossy(|t| matches!(t, Token::Dot)) {
                        self.imports
                            .insert(Cow::from(word), Identifier::new_with_path(base, path));
                        return Ok(());
                    }
                }
                Token::Mul => {
                    self.next_lossy();
                    if let Some(linker) = &self.linker {
                        for name in linker
                            .borrow()
                            .get_listing(base, &path)
                            .ok_or(ParseError::ImportError)?
                        {
                            let mut path = path.clone();
                            path.push(name.clone());
                            let id = Identifier::new_with_path(base, path);
                            self.imports.insert(name, id);
                        }
                        return Ok(());
                    }
                }
                _ => return Ok(()),
            }
        }
        Ok(())
    }

    pub fn parse_script(&mut self) -> Result<Script<'a>, ParseError> {
        let mut instructions = Vec::new();
        while self.next_is_lossy(|t| matches!(t, Token::Import)) {
            self.parse_import()?;
            self.end_line()?;
        }
        while let Some(instr) = self.parse_instruction(true)? {
            self.end_line()?;
            instructions.push(instr);
        }
        let definitions = std::mem::take(&mut self.definitions);
        let main = Block::new(instructions);
        Ok(Script::new(definitions, main))
    }

    pub fn parse_block(&mut self) -> Result<Block<'a>, ParseError> {
        let mut instructions = Vec::new();
        while let Some(instr) = self.parse_instruction(false)? {
            self.end_line()?;
            instructions.push(instr);
        }
        return Ok(Block::new(instructions));
    }

    pub fn parse_package(
        &mut self,
        package_name: Identifier<'a>,
    ) -> Result<Package<'a>, ParseError> {
        self.name = Some(package_name.clone());
        while self.next_is_lossy(|t| matches!(t, Token::Import)) {
            self.parse_import()?;
            self.end_line()?;
        }
        let mut intrinsic = false;
        let mut dynamic = false;
        loop {
            if !intrinsic && self.next_is_lossy(|t| matches!(t, Token::Intrinsic)) {
                intrinsic = true
            } else if !dynamic && self.next_is_lossy(|t| matches!(t, Token::Dynamic)) {
                dynamic = true;
            } else {
                break;
            }
        }
        if self.next_is_lossy(|t| matches!(t, Token::Class)) {
            let name = self.parse_identifier(None, false)?;
            if name != package_name {
                return Err(ParseError::Message("Class name must match package name"));
            }
            if let Some(linker) = &self.linker {
                if let Some(existing) = linker.borrow().find(&name) {
                    if existing != name {
                        return Err(ParseError::Detailed(format!(
                            "Conflicting class name {} with {}",
                            existing, name
                        )));
                    }
                }
            }
            let extends = self
                .next_is_lossy(|t| matches!(t, Token::Extends))
                .then(|| self.parse_identifier(None, !intrinsic))
                .transpose()?;
            let mut implements = Vec::new();
            if self.next_is_lossy(|t| matches!(t, Token::Implements)) {
                loop {
                    implements.push(self.parse_identifier(None, !intrinsic)?);
                    if !self.next_is_lossy(|t| matches!(t, Token::Comma)) {
                        break;
                    }
                }
            }
            self.expect_next(Token::OpenCurly)?;
            let mut body = self.parse_package_body(false, intrinsic)?;
            body.set_extends(extends);
            Ok(Package::Class {
                dynamic,
                intrinsic,
                body,
                implements,
            })
        } else if self.next_is_lossy(|t| matches!(t, Token::Interface)) {
            if intrinsic || dynamic {
                return Err(ParseError::Message(
                    "These attributes not allowed for interface",
                ));
            }
            let name = self.parse_identifier(None, false)?;
            if name != package_name {
                return Err(ParseError::Message(
                    "Interface name must match package name",
                ));
            }
            if let Some(linker) = &self.linker {
                if let Some(existing) = linker.borrow().find(&name) {
                    if existing != name {
                        return Err(ParseError::Detailed(format!(
                            "Conflicting interface name {} with {}",
                            existing, name
                        )));
                    }
                }
            }
            let extends = self
                .next_is_lossy(|t| matches!(t, Token::Extends))
                .then(|| self.parse_identifier(None, !intrinsic))
                .transpose()?;
            self.expect_next(Token::OpenCurly)?;
            let mut body = self.parse_package_body(true, false)?;
            body.set_extends(extends);
            Ok(Package::Interface(body))
        } else {
            Err(ParseError::UnexpectedToken)
        }
    }

    pub fn parse_package_body(
        &mut self,
        interface: bool,
        intrinsic: bool,
    ) -> Result<PackageBody<'a>, ParseError> {
        let mut result: Vec<Trait<'a>> = Vec::new();
        while self.peek_lossy().ok_or(ParseError::UnexpectedToken)? != Token::CloseCurly {
            let mut public = true;
            let mut class_trait = false;
            if !interface {
                for _ in 0..2 {
                    public &= !(public && self.next_is_lossy(|t| matches!(t, Token::Private)));
                    class_trait |=
                        !class_trait && self.next_is_lossy(|t| matches!(t, Token::Static));
                    self.ignore_if_next(Token::Public);
                }
            } else {
                self.ignore_if_next(Token::Public);
            }
            if !interface && self.next_is_lossy(|t| matches!(t, Token::Var)) {
                let name = self.parse_symbol_name(!intrinsic)?;
                let value = self
                    .next_is_lossy(|t| matches!(t, Token::Assign))
                    .then(|| self.parse_symbol())
                    .transpose()?;
                result.push(Trait::Variable {
                    class: class_trait,
                    public,
                    name,
                    value,
                });
            } else if self.next_is_lossy(|t| matches!(t, Token::Function)) {
                let (method_name, method_kind) = match self.next_lossy().ok_or(ParseError::Eof)? {
                    Token::Word(w) => (w, MethodKind::Regular),
                    Token::Set => (self.next_word()?, MethodKind::Set),
                    Token::Get => (self.next_word()?, MethodKind::Get),
                    _ => return Err(ParseError::UnexpectedToken),
                };
                self.expect_next(Token::OpenParenthesis)?;
                let parameters = self.parse_parameters(!intrinsic)?;
                self.expect_next(Token::CloseParenthesis)?;
                let return_type = self.parse_return_type(!intrinsic)?;
                if !interface && self.next_is_lossy(|t| matches!(t, Token::OpenCurly)) {
                    let code = self.parse_block()?;
                    self.expect_next(Token::CloseCurly)?;
                    result.push(Trait::Method {
                        class: class_trait,
                        public,
                        name: method_name,
                        parameters,
                        return_type,
                        method_kind,
                        code: Some(code),
                    });
                } else if interface || intrinsic {
                    result.push(Trait::Method {
                        class: class_trait,
                        public,
                        name: method_name,
                        parameters,
                        return_type,
                        method_kind,
                        code: None,
                    });
                } else {
                    return Err(ParseError::UnexpectedToken);
                }
            } else if self.next_is_lossy(|t| matches!(t, Token::Semicolon)) {
                continue;
            } else {
                return Err(ParseError::UnexpectedToken);
            }
        }
        Ok(PackageBody::new(result))
    }

    pub fn parse_instruction(
        &mut self,
        toplevel: bool,
    ) -> Result<Option<Instruction<'a>>, ParseError> {
        loop {
            return match self.peek_lossy() {
                Some(Token::Var) => {
                    self.next_lossy();
                    let name = self.parse_symbol_name(true)?;
                    let statement = if self.next_is_lossy(|t| matches!(t, Token::Assign)) {
                        let value = self.parse_expression()?;
                        Declare::new(name, Some(value))
                    } else {
                        Declare::new(name, None)
                    };
                    Ok(Some(Instruction::Declare(statement)))
                }
                Some(Token::If) => {
                    self.next_lossy();
                    // if (condition) {code}
                    let mut result = If::default();
                    loop {
                        self.expect_next(Token::OpenParenthesis)?;
                        let condition = self.parse_expression()?;
                        self.expect_next(Token::CloseParenthesis)?;
                        self.expect_next(Token::OpenCurly)?;
                        let code = self.parse_block()?;
                        self.expect_next(Token::CloseCurly)?;
                        result.add_condition((condition, code));
                        if self.next_is_lossy(|t| matches!(t, Token::Else)) {
                            match self.next_lossy() {
                                // else if (condition) {code}
                                Some(Token::If) => continue,
                                // else {code}
                                Some(Token::OpenCurly) => {
                                    let code = self.parse_block()?;
                                    self.expect_next(Token::CloseCurly)?;
                                    result.set_default(code);
                                    break;
                                }
                                _ => break,
                            }
                        }
                        break;
                    }
                    Ok(Some(Instruction::If(result)))
                }
                Some(Token::Switch) => {
                    self.next_lossy();
                    // switch (condition) {cases...default}
                    self.expect_next(Token::OpenParenthesis)?;
                    let test = self.parse_expression()?;
                    self.expect_next(Token::CloseParenthesis)?;
                    let mut result = Switch::new(test);
                    self.expect_next(Token::OpenCurly)?;
                    while self.next_is_lossy(|t| matches!(t, Token::Case)) {
                        let case = self.parse_expression()?;
                        self.expect_next(Token::Colon)?;
                        let code = self.parse_block()?;
                        result.add_case((case, code));
                    }
                    if self.next_is_lossy(|t| matches!(t, Token::Default)) {
                        self.expect_next(Token::Colon)?;
                        let code = self.parse_block()?;
                        result.set_default(code);
                    }
                    self.expect_next(Token::CloseCurly)?;
                    Ok(Some(Instruction::Switch(result)))
                }
                Some(Token::While) => {
                    self.next_lossy();
                    // while (condition) {code}
                    self.expect_next(Token::OpenParenthesis)?;
                    let condition = self.parse_expression()?;
                    self.expect_next(Token::CloseParenthesis)?;
                    self.expect_next(Token::OpenCurly)?;
                    let code = self.parse_block()?;
                    self.expect_next(Token::CloseCurly)?;
                    let result = While::new(condition, code);
                    Ok(Some(Instruction::While(result)))
                }
                Some(Token::Do) => {
                    self.next_lossy();
                    // do {code} while (condition)
                    self.expect_next(Token::OpenCurly)?;
                    let code = self.parse_block()?;
                    self.expect_next(Token::CloseCurly)?;
                    self.expect_next(Token::While)?;
                    self.expect_next(Token::OpenParenthesis)?;
                    let condition = self.parse_expression()?;
                    self.expect_next(Token::CloseParenthesis)?;
                    let result = While::new_do(code, condition);
                    Ok(Some(Instruction::While(result)))
                }
                Some(Token::For) => {
                    self.next_lossy();
                    self.expect_next(Token::OpenParenthesis)?;
                    // First lets try parsing this for loop as a for..in loop
                    // We save the old stream so we can reuse it later
                    let new_stream = self.stream.clone();
                    let old_stream = std::mem::replace(&mut self.stream, new_stream);
                    if let Some(t @ (Token::Var | Token::Word(_))) = self.peek_lossy() {
                        let declaring = t == Token::Var;
                        if declaring {
                            self.next_lossy();
                        }
                        let iter = (|| {
                            let iterator = self.parse_symbol_name(true).ok()?;
                            self.next_is_lossy(|t| matches!(t, Token::In))
                                .then(|| iterator)
                        })();
                        if let Some(iterator) = iter {
                            let object = self.parse_expression()?;
                            self.expect_next(Token::CloseParenthesis)?;
                            self.expect_next(Token::OpenCurly)?;
                            let code = self.parse_block()?;
                            self.expect_next(Token::CloseCurly)?;
                            let result = ForIn::new(iterator, declaring, object, code);
                            return Ok(Some(Instruction::ForIn(result)));
                        }
                    }
                    // If we are here, it means we failed to parse as for..in, so parse as
                    // regular for loop instead.

                    // Set the stream back to the old stream before parsing.
                    self.stream = old_stream;

                    let initial = if !self.next_is_lossy(|t| matches!(t, Token::Semicolon)) {
                        let i = match self.parse_instruction(false)? {
                            // Only declare & exec instruction allowed in this position
                            Some(i @ (Instruction::Declare(_) | Instruction::Exec(_))) => i,
                            _ => return Err(ParseError::UnexpectedToken),
                        };
                        self.expect_next(Token::Semicolon)?;
                        i
                    } else {
                        Instruction::Nop
                    };
                    let condition = if !self.next_is_lossy(|t| matches!(t, Token::Semicolon)) {
                        let c = self.parse_expression()?;
                        self.expect_next(Token::Semicolon)?;
                        c
                    } else {
                        Expression::default()
                    };
                    let update = if !self.next_is_lossy(|t| matches!(t, Token::CloseParenthesis)) {
                        let u = self.parse_expression()?;
                        self.expect_next(Token::CloseParenthesis)?;
                        u
                    } else {
                        Expression::default()
                    };
                    self.expect_next(Token::OpenCurly)?;
                    let code = self.parse_block()?;
                    self.expect_next(Token::CloseCurly)?;
                    let result = For::new(initial, condition, update, code);
                    Ok(Some(Instruction::For(result)))
                }
                Some(Token::Try) => {
                    self.next_lossy();
                    // try {code}
                    self.expect_next(Token::OpenCurly)?;
                    let code = self.parse_block()?;
                    self.expect_next(Token::CloseCurly)?;
                    let mut result = Try::new(code);
                    // catch (error name) {code}
                    while self.next_is_lossy(|t| matches!(t, Token::Catch)) {
                        self.expect_next(Token::OpenParenthesis)?;
                        let error = self.parse_symbol_name(true)?;
                        self.expect_next(Token::CloseParenthesis)?;
                        self.expect_next(Token::OpenCurly)?;
                        let code = self.parse_block()?;
                        self.expect_next(Token::CloseCurly)?;
                        result.add_catch((error, code));
                    }
                    // finally {code}
                    if self.next_is_lossy(|t| matches!(t, Token::Finally)) {
                        self.expect_next(Token::OpenCurly)?;
                        let code = self.parse_block()?;
                        self.expect_next(Token::CloseCurly)?;
                        result.set_finally(code);
                    }
                    Ok(Some(Instruction::Try(result)))
                }
                Some(Token::With) => {
                    self.next_lossy();
                    // with (expression) {code}
                    self.expect_next(Token::OpenParenthesis)?;
                    let this = self.parse_expression()?;
                    self.expect_next(Token::CloseParenthesis)?;
                    self.expect_next(Token::OpenCurly)?;
                    let code = self.parse_block()?;
                    self.expect_next(Token::CloseCurly)?;
                    let result = With::new(this, code);
                    Ok(Some(Instruction::With(result)))
                }
                Some(Token::Function) => {
                    self.next_lossy();
                    let name = self.next_word()?;
                    self.expect_next(Token::OpenParenthesis)?;
                    let parameters = self.parse_parameters(true)?;
                    self.expect_next(Token::CloseParenthesis)?;
                    let return_type = self.parse_return_type(true)?;
                    self.expect_next(Token::OpenCurly)?;
                    let orig = std::mem::replace(
                        &mut self.flags,
                        FunctionFlags::SUPPRESS_SUPER
                            | FunctionFlags::SUPPRESS_ARGUMENTS
                            | FunctionFlags::SUPPRESS_THIS,
                    );
                    let code = self.parse_block()?;
                    let func_meta = std::mem::replace(&mut self.flags, orig);
                    self.expect_next(Token::CloseCurly)?;
                    let function = Symbol::new_function(parameters, code, return_type, func_meta);
                    if toplevel {
                        self.definitions.insert(Identifier::new(name), function);
                    }
                    continue;
                }
                Some(Token::Semicolon) => {
                    self.next_lossy();
                    continue;
                }
                Some(Token::Break) => {
                    self.next_lossy();
                    Ok(Some(Instruction::Break))
                }
                Some(Token::Continue) => {
                    self.next_lossy();
                    Ok(Some(Instruction::Continue))
                }
                Some(Token::Throw) => {
                    self.next_lossy();
                    let exp = self.parse_expression()?;
                    Ok(Some(Instruction::Throw(exp)))
                }
                Some(Token::Return) => {
                    self.next_lossy();
                    Ok(Some(if !self.is_end_line() {
                        let exp = self.parse_expression()?;
                        Instruction::Return(Some(exp))
                    } else {
                        Instruction::Return(None)
                    }))
                }
                Some(
                    Token::CloseParenthesis
                    | Token::CloseCurly
                    | Token::CloseBracket
                    | Token::Case
                    | Token::Default,
                )
                | None => Ok(None),
                _ => {
                    let exp = self.parse_expression()?;
                    Ok(Some(Instruction::Exec(exp)))
                }
            };
        }
    }

    pub fn skip_newline(&mut self) {
        while let Some(token) = self.stream.peek() {
            match token {
                Token::Newline => (),
                _ => return,
            }
            self.stream.next();
        }
    }

    pub fn is_end_line(&mut self) -> bool {
        match self.stream.peek() {
            Some(
                Token::Newline
                | Token::Semicolon
                | Token::CloseCurly
                | Token::CloseBracket
                | Token::CloseParenthesis,
            )
            | None => true,
            Some(_) => false,
        }
    }

    pub fn end_line(&mut self) -> Result<(), ParseError> {
        if !self.is_end_line() {
            return Err(ParseError::UnexpectedToken);
        }
        Ok(())
    }

    pub fn next_is_lossy(&mut self, func: impl FnOnce(&Token<'a>) -> bool) -> bool {
        if let Some(token) = self.peek_lossy() {
            if func(&token) {
                self.next_lossy();
                return true;
            }
        }
        false
    }

    pub fn parse_arguments(&mut self) -> Result<Vec<Expression<'a>>, ParseError> {
        let mut arguments = Vec::new();
        while !matches!(
            self.peek_lossy(),
            Some(Token::CloseParenthesis | Token::CloseBracket)
        ) {
            arguments.push(self.parse_expression()?);
            if !self.next_is_lossy(|t| matches!(t, Token::Comma)) {
                break;
            }
        }
        Ok(arguments)
    }

    pub fn parse_parameters(&mut self, resolve: bool) -> Result<Vec<SymbolName<'a>>, ParseError> {
        let mut parameters = Vec::new();
        if let Token::Word(_) = self.peek_lossy().ok_or(ParseError::Eof)? {
            parameters.push(self.parse_symbol_name(resolve)?);
            while self.next_is_lossy(|t| matches!(t, Token::Comma)) {
                parameters.push(self.parse_symbol_name(resolve)?);
            }
        }
        Ok(parameters)
    }

    pub fn parse_return_type(&mut self, resolve: bool) -> Result<ReturnType<'a>, ParseError> {
        if self.next_is_lossy(|t| matches!(t, Token::Colon)) {
            match self.next_lossy().ok_or(ParseError::Eof)? {
                Token::Void => return Ok(ReturnType::Void),
                Token::Word(w) => {
                    return Ok(ReturnType::Value(self.parse_identifier(Some(w), resolve)?))
                }
                _ => return Err(ParseError::UnexpectedToken),
            }
        }
        Ok(ReturnType::Any)
    }

    pub fn parse_symbol(&mut self) -> Result<Symbol<'a>, ParseError> {
        let mut value = match self.next_lossy().ok_or(ParseError::Eof)? {
            Token::Undefined => Symbol::new_literal(Literal::Undefined),
            Token::Null => Symbol::new_literal(Literal::Null),
            Token::Integer(i) => Symbol::new_literal(Literal::Integer(i)),
            Token::Number(n) => Symbol::new_literal(Literal::Number(n)),
            Token::Bool(b) => Symbol::new_literal(Literal::Bool(b)),
            Token::String(s) => Symbol::new_literal(Literal::String(s)),
            Token::Add => self.parse_symbol()?,
            Token::Sub => {
                match self.parse_symbol()? {
                    // Fast paths for constant numbers
                    #[rustfmt::skip]
                    Symbol::Literal(Literal::Integer(i)) => Symbol::new_literal(Literal::Integer(-i)),
                    Symbol::Literal(Literal::Number(n)) => Symbol::new_literal(Literal::Number(-n)),
                    symbol => {
                        let mut expression =
                            Expression::new(Some(Symbol::new_literal(Literal::Integer(0))));
                        expression.push(Operation::Sub, symbol);
                        Symbol::new_expression(expression)
                    }
                }
            }
            Token::New => {
                match self.parse_symbol()? {
                    // If this symbol is a call, convert it to a construct symbol
                    Symbol::Call(call, false) => Symbol::Call(call, true),
                    s => Symbol::new_call(s, Vec::new(), true),
                }
            }
            Token::Typeof => Symbol::new_typeof(self.parse_symbol()?),
            Token::Function => {
                self.expect_next(Token::OpenParenthesis)?;
                let parameters = self.parse_parameters(true)?;
                self.expect_next(Token::CloseParenthesis)?;
                let return_type = self.parse_return_type(true)?;
                self.expect_next(Token::OpenCurly)?;
                let orig = std::mem::replace(
                    &mut self.flags,
                    FunctionFlags::SUPPRESS_SUPER
                        | FunctionFlags::SUPPRESS_ARGUMENTS
                        | FunctionFlags::SUPPRESS_THIS,
                );
                let code = self.parse_block()?;
                let func_meta = std::mem::replace(&mut self.flags, orig);
                self.expect_next(Token::CloseCurly)?;
                Symbol::new_function(parameters, code, return_type, func_meta)
            }
            Token::Not => {
                let next = self.parse_symbol()?;
                Symbol::new_not(next)
            }
            Token::BitNot => {
                let next = self.parse_symbol()?;
                Symbol::new_bit_not(next)
            }
            Token::Delete => match self.parse_symbol()? {
                s @ (Symbol::Identifier(_) | Symbol::Index(_)) => Symbol::new_delete(s),
                _ => return Err(ParseError::UnexpectedToken),
            },
            Token::Incr => match self.parse_symbol()? {
                s @ (Symbol::Identifier(_) | Symbol::Index(_)) => Symbol::new_incr(s, Side::Left),
                _ => return Err(ParseError::UnexpectedToken),
            },
            Token::Decr => match self.parse_symbol()? {
                s @ (Symbol::Identifier(_) | Symbol::Index(_)) => Symbol::new_decr(s, Side::Left),
                _ => return Err(ParseError::UnexpectedToken),
            },
            Token::OpenParenthesis => {
                let expression = self.parse_expression()?;
                self.expect_next(Token::CloseParenthesis)?;
                Symbol::new_expression(expression)
            }
            Token::OpenBracket => {
                let array = self.parse_arguments()?;
                self.expect_next(Token::CloseBracket)?;
                Symbol::new_array(array)
            }
            Token::OpenCurly => {
                let mut result = Vec::new();
                while let Token::Word(w) = self.peek_lossy().ok_or(ParseError::UnexpectedToken)? {
                    self.next_lossy();
                    self.expect_next(Token::Colon)?;
                    let exp = self.parse_expression()?;
                    result.push((w, exp));
                    if !self.next_is_lossy(|t| matches!(t, Token::Comma)) {
                        break;
                    }
                }
                self.expect_next(Token::CloseCurly)?;
                Symbol::new_object(result)
            }
            // Set token in this position should be treated like a regular word
            Token::Set => Symbol::new_identifier(self.parse_identifier(Some("set"), true)?),
            Token::Word(w) => Symbol::new_identifier(self.parse_identifier(Some(w), true)?),
            token => return Err(ParseError::Detailed(format!("Unexpected '{}'", token))),
        };
        while let Some(token) = self.peek_lossy() {
            match token {
                Token::OpenParenthesis => {
                    self.next_lossy();
                    // Open parenthesis in this position indicates a call
                    let arguments = self.parse_arguments()?;
                    self.expect_next(Token::CloseParenthesis)?;
                    value = Symbol::new_call(value, arguments, false);
                }
                Token::OpenBracket => {
                    self.next_lossy();
                    // Open bracket in this position indicates an index
                    let index = self.parse_expression()?;
                    self.expect_next(Token::CloseBracket)?;
                    value = Symbol::new_index(value, index);
                }
                Token::Dot => {
                    self.next_lossy();
                    // symbol.example -> symbol["example"]
                    let word = Symbol::new_literal(Literal::String(self.next_word()?));
                    value = Symbol::new_index(value, Expression::new(Some(word)));
                }
                Token::Incr => {
                    self.next_lossy();
                    // right increment (example++)
                    if !value.is_identifier() {
                        return Err(ParseError::UnexpectedToken);
                    }
                    value = Symbol::new_incr(value, Side::Right)
                }
                Token::Decr => {
                    self.next_lossy();
                    // right decrement (example--)
                    if !value.is_identifier() {
                        return Err(ParseError::UnexpectedToken);
                    }
                    value = Symbol::new_decr(value, Side::Right)
                }
                Token::Assign => {
                    self.next_lossy();
                    if !value.is_identifier() {
                        return Err(ParseError::UnexpectedToken);
                    }
                    let expr = self.parse_expression()?;
                    value = Symbol::new_assign(value, expr);
                }
                token if token.is_compound_assign() => {
                    self.next_lossy();
                    // For compound assignments, we want to flatten it to an assign expression.
                    // e.g: a /= 2 + 1 -> a = a / (2 + 1)
                    // We put the 2 + 1 in an expression to ensure it is computed first.
                    if !value.is_identifier() {
                        return Err(ParseError::UnexpectedToken);
                    }
                    let op = match token {
                        Token::AddAssign => Operation::Add,
                        Token::SubAssign => Operation::Sub,
                        Token::MulAssign => Operation::Mul,
                        Token::DivAssign => Operation::Div,
                        Token::ModAssign => Operation::Mod,
                        Token::BitAndAssign => Operation::BitAnd,
                        Token::BitLShiftAssign => Operation::BitLShift,
                        Token::BitOrAssign => Operation::BitOr,
                        Token::BitRShiftAssign => Operation::BitRShift,
                        Token::BitURShiftAssign => Operation::BitURShift,
                        Token::BitXorAssign => Operation::BitXor,
                        _ => unreachable!(),
                    };
                    let expr = self.parse_expression()?;
                    let mut result = Expression::new(Some(value.clone()));
                    result.push(op, Symbol::new_expression(expr));
                    value = Symbol::new_assign(value, result);
                }
                _ => break,
            }
        }
        Ok(value)
    }

    pub fn parse_expression(&mut self) -> Result<Expression<'a>, ParseError> {
        let mut base_expr = self.parse_expression_partial()?;
        while let Some(token) = self.peek_lossy() {
            match token {
                Token::Ternary => {
                    self.next_lossy();
                    let left = self.parse_expression()?;
                    self.expect_next(Token::Colon)?;
                    let right = self.parse_expression()?;
                    base_expr.set_ternary(left, right);
                    return Ok(base_expr);
                }
                Token::And => {
                    self.next_lossy();
                    let right = self.parse_expression_partial()?;
                    base_expr.push_comp((Compare::And, right));
                }
                Token::Or => {
                    self.next_lossy();
                    let right = self.parse_expression_partial()?;
                    base_expr.push_comp((Compare::Or, right));
                }
                _ => break,
            }
        }
        Ok(base_expr)
    }

    pub fn parse_expression_partial(&mut self) -> Result<Expression<'a>, ParseError> {
        let mut expression = Expression::new(Some(self.parse_symbol()?));
        let mut priority: Option<(Operation, Expression<'a>)> = None;
        loop {
            if let Some(token) = self.peek_lossy() {
                if let Some(op) = Operation::from_token(token) {
                    self.next_lossy();
                    let value = self.parse_symbol()?;
                    match op {
                        Operation::Mul | Operation::Div => {
                            if let Some((_, priority)) = &mut priority {
                                priority.push(op, value);
                                continue;
                            } else if let Some((operator, right)) = expression.pop() {
                                if !matches!(operator, Operation::Mul | Operation::Div) {
                                    let mut exp = Expression::new(Some(right));
                                    exp.push(op, value);
                                    priority = Some((operator, exp));
                                    continue;
                                } else {
                                    expression.push(operator, right);
                                }
                            }
                            expression.push(op, value);
                        }
                        _ => {
                            if let Some((op, right)) = priority.take() {
                                expression.push(op, Symbol::new_expression(right));
                            }
                            expression.push(op, value);
                        }
                    }
                    continue;
                }
            }
            if let Some((op, right)) = priority {
                expression.push(op, Symbol::new_expression(right));
            }
            return Ok(expression);
        }
    }

    pub fn peek_lossy(&mut self) -> Option<Token<'a>> {
        let peek_stream = self.stream.clone();
        for token in peek_stream {
            match token {
                Token::Newline => continue,
                _ => return Some(token),
            }
        }
        None
    }

    pub fn next_lossy(&mut self) -> Option<Token<'a>> {
        self.skip_newline();
        self.stream.next()
    }

    pub fn next_word(&mut self) -> Result<&'a str, ParseError> {
        self.stream
            .next()
            .ok_or(ParseError::Eof)?
            .unwrap_word()
            .ok_or(ParseError::UnexpectedToken)
    }

    pub fn expect_next(&mut self, t: Token<'a>) -> Result<(), ParseError> {
        if !self.next_is_lossy(|other| *other == t) {
            Err(ParseError::Detailed(format!(
                "Expected '{}', got '{}' instead",
                t,
                self.stream.next().unwrap_or(Token::Eof)
            )))
        } else {
            Ok(())
        }
    }

    pub fn ignore_if_next(&mut self, t: Token<'a>) {
        self.next_is_lossy(|other| *other == t);
    }

    pub fn parse_symbol_name(&mut self, resolve: bool) -> Result<SymbolName<'a>, ParseError> {
        let name = self.next_word()?;
        let signature = self
            .next_is_lossy(|t| matches!(t, Token::Colon))
            .then(|| self.parse_identifier(None, resolve))
            .transpose()?;
        Ok(SymbolName::new(name, signature))
    }

    fn resolve_identifier(&mut self, id: Identifier<'a>) -> Result<(), ParseError> {
        if let Some(linker) = &self.linker {
            let linker = linker.borrow();
            if let Some((real_id, content)) = linker.resolve(id)? {
                drop(linker);
                return self.load_package(real_id, content);
            }
        }
        Ok(())
    }

    pub fn parse_identifier(
        &mut self,
        base: Option<&'a str>,
        resolve: bool,
    ) -> Result<Identifier<'a>, ParseError> {
        let mut base = base
            .or_else(|| self.next_word().ok())
            .ok_or(ParseError::UnexpectedToken)?;
        let mut path = Vec::new();
        if resolve {
            if let Some(id) = self.imports.get(base) {
                base = id.base();
                path = id.path().map(|id| id.to_vec()).unwrap_or_default();
            }
        }
        match base {
            "_root" => self.flags.insert(FunctionFlags::PRELOAD_ROOT),
            "_parent" => self.flags.insert(FunctionFlags::PRELOAD_PARENT),
            "_global" => self.flags.insert(FunctionFlags::PRELOAD_GLOBAL),
            "this" => {
                self.flags.insert(FunctionFlags::PRELOAD_THIS);
                self.flags.remove(FunctionFlags::SUPPRESS_THIS);
            }
            "super" => {
                self.flags.insert(FunctionFlags::PRELOAD_SUPER);
                self.flags.remove(FunctionFlags::SUPPRESS_SUPER);
            }
            "arguments" => {
                self.flags.insert(FunctionFlags::PRELOAD_ARGUMENTS);
                self.flags.remove(FunctionFlags::SUPPRESS_ARGUMENTS);
            }
            _ => (),
        }
        while self.next_is_lossy(|t| matches!(t, Token::Dot)) {
            path.push(Cow::from(self.next_word()?));
        }
        let id = if path.is_empty() {
            Identifier::new(base)
        } else {
            Identifier::new_with_path(base, path)
        };
        if resolve {
            self.resolve_identifier(id.clone())?;
        }
        Ok(id)
    }
}

#[derive(Debug)]
pub enum ParseError {
    Detailed(String),
    Message(&'static str),
    UnexpectedToken,
    CircularImport,
    ReadFile,
    ImportError,
    Eof,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::ops::Deref;
        let (prefix, message) = match self {
            ParseError::Detailed(s) => ("Syntax Error", s.deref()),
            ParseError::Message(s) => ("Syntax Error", *s),
            ParseError::UnexpectedToken => ("Syntax Error", "Unexpected token"),
            ParseError::CircularImport => ("Import Error", "Circular import"),
            ParseError::ReadFile => ("Import Error", "Failed to read file"),
            ParseError::ImportError => ("Import Error", "Import failed"),
            ParseError::Eof => ("Syntax Error", "Unexpected EOF"),
        };
        write!(f, "{}: {}", prefix, message)
    }
}
