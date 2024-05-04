#![allow(non_camel_case_types)]

mod lexer;

mod parser;

mod lst;
pub mod syntax_kind;

pub use lst::NodeOrToken;
pub use lst::ParseError;
pub use lst::PostgreSQLSyntax;
pub use lst::ResolvedNode;
pub use lst::ResolvedToken;
pub use lst::SyntaxElement;
pub use lst::SyntaxElementRef;
pub use lst::SyntaxNode;
pub use lst::SyntaxToken;

pub fn parse(input: &str) -> Result<ResolvedNode, ParseError> {
    lst::parse(input)
}

#[cfg(test)]
mod tests {}
