#![allow(non_camel_case_types)]

mod lexer;

mod parser;

mod cst;
pub mod syntax_kind;

pub use cst::NodeOrToken;
pub use cst::PostgreSQLSyntax;
pub use cst::ResolvedNode;
pub use cst::ResolvedToken;
pub use cst::SyntaxElement;
pub use cst::SyntaxElementRef;
pub use cst::SyntaxNode;
pub use cst::SyntaxToken;
pub use lexer::parser_error::ParserError;
pub use lexer::parser_error::ScanReport;

/// Parse SQL and construct a Complete Syntax Tree (CST).
///
/// # Examples
///
/// ```
/// use postgresql_cst_parser::{parse, syntax_kind::SyntaxKind};
///
///     // Parse SQL query and get the syntax tree
///     let sql = "SELECT tbl.a as a, tbl.b from TBL tbl WHERE tbl.a > 0;";
///     let root = parse(sql).unwrap();
///
///     // Example 1: Extract all column references from the query
///     let column_refs: Vec<String> = root
///         .descendants()
///         .filter(|node| node.kind() == SyntaxKind::columnref)
///         .map(|node| node.text().to_string())
///         .collect();
///
///     println!("Column references: {:?}", column_refs); // ["tbl.a", "tbl.b", "tbl.a"]
///
///     // Example 2: Find the WHERE condition
///     if let Some(where_clause) = root
///         .descendants()
///         .find(|node| node.kind() == SyntaxKind::where_clause)
///     {
///         println!("WHERE condition: {}", where_clause.text());
///     }
///
///     // Example 3: Get the selected table name
///     if let Some(relation_expr) = root
///         .descendants()
///         .find(|node| node.kind() == SyntaxKind::relation_expr)
///     {
///         if let Some(name_node) = relation_expr
///             .descendants()
///             .find(|node| node.kind() == SyntaxKind::ColId)
///         {
///             println!("Table name: {}", name_node.text());
///         }
///     }
///
///     // Example 4: Parse complex SQL and extract specific nodes
///     let complex_sql = "WITH data AS (SELECT id, value FROM source WHERE value > 10)
///                        SELECT d.id, d.value, COUNT(*) OVER (PARTITION BY d.id)
///                        FROM data d JOIN other o ON d.id = o.id
///                        ORDER BY d.value DESC LIMIT 10;";
///
///     let complex_root = parse(complex_sql).unwrap();
///
///     // Extract CTEs (Common Table Expressions)
///     let ctes: Vec<_> = complex_root
///         .descendants()
///         .filter(|node| node.kind() == SyntaxKind::common_table_expr)
///         .collect();
///
///     // Extract window functions
///     let window_funcs: Vec<_> = complex_root
///         .descendants()
///         .filter(|node| node.kind() == SyntaxKind::over_clause)
///         .collect();
///
///     println!("Number of CTEs: {}", ctes.len());
///     println!("Number of window functions: {}", window_funcs.len());
/// ```
pub fn parse(input: &str) -> Result<ResolvedNode, ParserError> {
    cst::parse(input)
}

#[cfg(test)]
mod tests {
    use crate::lexer::parser_error::ScanReport;

    use super::*;

    #[test]
    fn test_unterminated_hexadecimal_string_literal() {
        let input = r#"select x'CC"#;
        let actual = parse(input);

        let expected = Err(ParserError::ScanError {
            message: "unterminated hexadecimal string literal".to_string(),
        });

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_unterminated_unterminated_bit_string_literal() {
        let input = r#"select b'10"#;
        let actual = parse(input);

        let expected = Err(ParserError::ScanError {
            message: "unterminated bit string literal".to_string(),
        });

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_xeunicodefail() {
        let input = r#"select e'\uD80"#;
        let actual = parse(input);

        let expected = Err(ParserError::ScanReport(ScanReport {
            message: "invalid Unicode escape".to_string(),
            detail: "Unicode escapes must be \\uXXXX or \\UXXXXXXXX.".to_string(),
            position_in_bytes: 9,
        }));

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_invalid_unicode_surrogate_pair() {
        let input = r#"select e'\uD800"#;
        let actual = parse(input);

        let expected = Err(ParserError::ScanError {
            message: "invalid Unicode surrogate pair".to_string(),
        });

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_invalid_unicode_surrogate_first() {
        let input = r#"select e'\u0000"#;
        let actual = parse(input);

        let expected = Err(ParserError::ScanError {
            message: "invalid Unicode escape value".to_string(),
        });

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_invalid_unicode_surrogate_second() {
        let input = r#"select e'\uD800\uD000'"#;
        let actual = parse(input);

        let expected = Err(ParserError::ScanError {
            message: "invalid Unicode surrogate pair".to_string(),
        });

        assert_eq!(actual, expected);
    }
}
