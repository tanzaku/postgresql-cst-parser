use crate::{lexer::TokenKind, syntax_kind::SyntaxKind};

use super::{LRParseState, ParseTransform, ParseTransformer};

/// Skip extra AND/OR at the beginning of the WHERE clause
pub struct SkipExtraOperator;

impl SkipExtraOperator {
    fn allow_extra_operator<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
        matches!(lr_parse_state.last_syntax_kind(), Some(SyntaxKind::WHERE))
    }
}

impl ParseTransformer for SkipExtraOperator {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform> {
        if !Self::allow_extra_operator(&lr_parse_state) {
            return None;
        }

        match &lr_parse_state.token.kind {
            TokenKind::KEYWORD(s) if s == "AND" || s == "OR" => Some(ParseTransform::SkipToken),
            _ => None,
        }
    }
}
