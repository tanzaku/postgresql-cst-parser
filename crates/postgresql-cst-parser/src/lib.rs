#![allow(non_camel_case_types)]

mod lexer;

mod parser;

mod cst;
pub mod syntax_kind;
mod transform;

#[cfg(feature = "tree-sitter-like")]
pub mod tree_sitter;

use cst::parse_with_transformer;
pub use cst::NodeOrToken;
pub use cst::ParseError;
pub use cst::PostgreSQLSyntax;
pub use cst::ResolvedNode;
pub use cst::ResolvedToken;
pub use cst::SyntaxElement;
pub use cst::SyntaxElementRef;
pub use cst::SyntaxNode;
pub use cst::SyntaxToken;

use transform::ComplementMissingFromTableTransformer;
use transform::ComplementMissingSampleValueTransformer;
use transform::ParseTransformer;
use transform::SkipExtraComma;
use transform::SkipExtraOperator;
pub use tree_sitter::parse as ts_parse;
pub use tree_sitter::parse_2way as ts_parse_2way;

pub fn parse(input: &str) -> Result<ResolvedNode, ParseError> {
    cst::parse(input)
}

/// Corrects and parses the following syntax errors found in 2Way SQL
/// 1. Missing sample values ​​when specifying a table name in the from clause as a replacement string
/// 2. Missing sample values ​​in expressions found in select clauses, etc.
/// 3. Extra commas in select clauses, from clauses, and order by clauses
/// 4. Extra and/or in the where clause
pub fn parse_2way(input: &str) -> Result<ResolvedNode, ParseError> {
    parse_with_transformer(
        input,
        &[
            &ComplementMissingFromTableTransformer,
            &ComplementMissingSampleValueTransformer,
            &SkipExtraComma,
            &SkipExtraOperator,
        ],
    )
}

pub fn parse_2way_with_transformers(
    input: &str,
    transformers: &[&dyn ParseTransformer],
) -> Result<ResolvedNode, ParseError> {
    parse_with_transformer(input, transformers)
}

#[cfg(test)]
mod tests {
    use crate::parse_2way;

    #[test]
    fn test() {
        let s = r#"select /*param*/ as A
from /*#foo*/
;
"#;
        assert!(parse_2way(s).is_ok());
    }

    #[test]
    fn test_2way_error_recovery_distinct_on() {
        let s = r#"select distinct on (col1) , t.* from tbl t;
;
"#;
        assert!(parse_2way(s).is_ok());
    }

    #[test]
    fn test_2way_error_recovery_distinct() {
        let s = r#"select distinct , t.* from tbl t;
;
"#;
        assert!(parse_2way(s).is_ok());
    }

    #[test]
    fn test_2way_error_recovery_all() {
        let s = r#"select all , t.* from tbl t;
;
"#;
        assert!(parse_2way(s).is_ok());
    }
}
