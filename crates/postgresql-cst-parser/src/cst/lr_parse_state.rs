use crate::{lexer::Token, syntax_kind::SyntaxKind};

use super::{Extra, Node};

#[allow(dead_code)]
pub(crate) struct LRParseState<'a> {
    pub(crate) state: u32,
    pub(crate) stack: &'a [(u32, Node)],
    pub(crate) action_table: &'a [i16],
    pub(crate) goto_table: &'a [i16],
    pub(crate) extras: &'a [Extra<'a>],
    pub(crate) token: &'a Token,
}

impl<'a> LRParseState<'a> {
    // Determine whether the previous C comment and the token to be processed are adjacent
    pub fn adjacent_c_comment(&self) -> bool {
        match self.extras.last() {
            Some(e)
                if e.end_byte_pos != self.token.start_byte_pos
                    && e.kind == SyntaxKind::C_COMMENT =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn previous_extra(&self) -> Option<&Extra> {
        let Some(last_extra) = self.extras.last() else {
            return None;
        };

        let stack_end_byte_pos = self
            .stack
            .last()
            .map(|(_, node)| node.end_byte_pos)
            .unwrap_or(0);

        if last_extra.end_byte_pos > stack_end_byte_pos {
            Some(last_extra)
        } else {
            None
        }
    }

    pub fn last_syntax_kind(&self) -> Option<SyntaxKind> {
        self.stack.last().map(|(_, node)| node.into())
    }
}
