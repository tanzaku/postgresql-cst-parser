use cstree::{RawSyntaxKind, Syntax};

use crate::{lexer::TokenKind, syntax_kind::SyntaxKind};

use super::{num_terminal_symbol, LRParseState, ParseTransform, ParseTransformer};

/// Complete missing replacement string sample values ​​(FROM clause only)
pub struct ComplementMissingFromTableTransformer;

fn is_replacement_string_comment(comment: &str) -> bool {
    comment.starts_with("/*#") && comment.ends_with("*/")
        || comment.starts_with("/*$") && comment.ends_with("*/")
}

fn is_missing_replacement_string_comment<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
    let Some(extra) = lr_parse_state.previous_extra() else {
        return false;
    };

    if !is_replacement_string_comment(extra.comment) {
        return false;
    }

    // If there is a space after the comment immediately following the replacement string, the table name that should be there is omitted.
    extra.end_byte_pos != lr_parse_state.token.start_byte_pos
}

fn is_from_table<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
    match SyntaxKind::from_raw(RawSyntaxKind(
        lr_parse_state.stack.last().unwrap().1.component_id,
    )) {
        SyntaxKind::FROM => true,
        SyntaxKind::Comma => {
            let prev2_kind = lr_parse_state
                .stack
                .iter()
                .nth_back(1)
                .map(|(_, node)| SyntaxKind::from_raw(RawSyntaxKind(node.component_id)));

            if prev2_kind == Some(SyntaxKind::from_list) {
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

fn is_missing_from_replacement_value<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
    if is_from_table(lr_parse_state) {
        // Check if IDENT is in SHIFT enabled state
        let action_index =
            (lr_parse_state.state * num_terminal_symbol()) as usize + SyntaxKind::IDENT as usize;

        let a = lr_parse_state.action_table[action_index];
        a != 0x7FFF
    } else {
        false
    }
}

impl ParseTransformer for ComplementMissingFromTableTransformer {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform> {
        if !is_missing_replacement_string_comment(lr_parse_state) {
            return None;
        }

        if !is_missing_from_replacement_value(&lr_parse_state) {
            return None;
        }

        Some(ParseTransform::InsertToken(TokenKind::IDENT))
    }
}
