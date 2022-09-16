use crate::sheet::AST;
use santiago::lexer::LexerError;
use santiago::parser::ParseError;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum Error<'a> {
    #[error("{0}")]
    Lexer(String),

    #[error("{0}")]
    Parser(String),

    #[error("Invalid expression: {0}")]
    InvalidExpression(&'a str),

    #[error("Circular dependency detected: {0}")]
    CyclicDependency(&'a str),
}

impl<'a> From<LexerError> for Error<'a> {
    fn from(err: LexerError) -> Self {
        Self::Lexer(err.to_string())
    }
}

impl<'a> From<ParseError<AST>> for Error<'a> {
    fn from(err: ParseError<AST>) -> Self {
        Self::Parser(err.to_string())
    }
}
