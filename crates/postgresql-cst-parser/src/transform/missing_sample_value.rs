use crate::{
    cst::{lookup_parser_action, Extra, ERROR_ACTION_CODE},
    lexer::TokenKind,
    syntax_kind::SyntaxKind,
};

use super::{LRParseState, ParseTransform, ParseTransformer};

/// Complete missing bind variable sample values
pub struct ComplementMissingSampleValueTransformer;

impl ComplementMissingSampleValueTransformer {
    fn is_bind_variable_comment(s: impl AsRef<str>) -> bool {
        let s = s.as_ref();
        s.starts_with("/*")
            && s.ends_with("*/")
            && !s.contains('\n')
            && !matches!(s.chars().nth(2).unwrap(), '$' | '#')
    }

    fn is_missing_bind_variable<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
        let Some(Extra { comment, .. }) = lr_parse_state.previous_extra() else {
            return false;
        };

        if !Self::is_bind_variable_comment(comment) {
            return false;
        }

        let a = lookup_parser_action(lr_parse_state.state, SyntaxKind::SCONST as u32);
        a != ERROR_ACTION_CODE
    }
}

impl ParseTransformer for ComplementMissingSampleValueTransformer {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform> {
        if !lr_parse_state.adjacent_c_comment() {
            return None;
        }

        if !Self::is_missing_bind_variable(&lr_parse_state) {
            return None;
        }

        Some(ParseTransform::InsertToken(TokenKind::SCONST))
    }
}
