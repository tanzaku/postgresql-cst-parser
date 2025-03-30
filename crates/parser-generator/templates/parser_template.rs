use crate::lexer::TokenKind;

pub(crate) struct Rule {
    pub(crate) name: &'static str,
    pub(crate) len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Action {
    Shift(usize),
    Reduce(usize),
    Accept,
    Error,
}

#[cfg(not(feature = "compress-parser-table"))]
#[rustfmt::skip]
pub(crate) const ACTION_TABLE: &[i16; {action_table_raw_size}] = &[{action_table_raw}];

#[cfg(not(feature = "compress-parser-table"))]
#[rustfmt::skip]
pub(crate) const GOTO_TABLE: &[i16; {goto_table_raw_size}] = &[{goto_table_raw}];

#[cfg(feature = "compress-parser-table")]
#[rustfmt::skip]
pub(crate) const ACTION_TABLE_COMPRESSED: &[u8; {action_table_size}] = &[{action_table}];

#[cfg(feature = "compress-parser-table")]
#[rustfmt::skip]
pub(crate) const GOTO_TABLE_COMPRESSED: &[u8; {goto_table_size}] = &[{goto_table}];

#[rustfmt::skip]
pub(crate) const RULES: &[Rule; {num_parse_rules}] = &[{parse_rules}];

pub(crate) fn num_terminal_symbol() -> u32 {{num_terminal_symbol}}
pub(crate) fn num_non_terminal_symbol() -> u32 {{num_non_terminal_symbol}}

pub(crate) fn end_rule_kind() -> TokenKind {{end_rule_kind}}

pub(crate) fn end_rule_id() -> u32 {{end_rule_id}}

pub(crate) fn token_kind_to_component_id(kind: &TokenKind) -> u32 {
    match kind {{token_to_component_id}
    _ => unreachable!(),
    }
}

pub(crate) fn rule_name_to_component_id(name: &str) -> u32 {
    match name {{rule_name_to_component_id}
    _ => unreachable!(),
    }
}
