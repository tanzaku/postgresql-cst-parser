pub(crate) mod missing_from_table;
pub(crate) mod missing_sample_value;
pub(crate) mod skip_extra_comma;
pub(crate) mod skip_extra_operator;

pub use missing_from_table::*;
pub use missing_sample_value::*;
pub use skip_extra_comma::*;
pub use skip_extra_operator::*;

use crate::{cst::LRParseState, lexer::TokenKind, parser::num_terminal_symbol};

pub enum ParseTransform {
    InsertToken(TokenKind),
    SkipToken,
}

pub trait ParseTransformer {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform>;
}
