#![allow(non_camel_case_types)]

mod lexer;

mod parser;

mod cst;
pub mod syntax_kind;

pub use cst::NodeOrToken;
pub use cst::ParseError;
pub use cst::PostgreSQLSyntax;
pub use cst::ResolvedNode;
pub use cst::ResolvedToken;
pub use cst::SyntaxElement;
pub use cst::SyntaxElementRef;
pub use cst::SyntaxNode;
pub use cst::SyntaxToken;

pub fn parse(input: &str) -> Result<ResolvedNode, ParseError> {
    cst::parse(input)
}

#[cfg(test)]
mod tests {}
