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
use lexer::parser_error::ParserError;

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
