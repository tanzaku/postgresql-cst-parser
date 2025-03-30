#![allow(clippy::all)]
#![allow(unreachable_code)]

/// This file contains ported sources from PostgreSQL

use std::collections::HashMap;

// use regex::bytes::Regex;

use super::{
    Lexer, NAMEDATALEN, ParserError, TokenKind, Yylval,
    lexer_ported::{
        get_char_by_byte_pos, is_highbit_set, is_utf16_surrogate_first, is_utf16_surrogate_second,
        surrogate_pair_to_codepoint,
    },
};

macro_rules! ereport {
    ($lexer:expr, ERROR, (errcode($err_code:expr), errmsg($err_msg:expr), errdetail($err_detail:expr), $err_position:expr)) => {
        return Err(ParserError::new_report($err_msg, $err_detail, $err_position));
    };
    ($lexer:expr, ERROR, (errcode($err_code:expr), errmsg($err_msg:expr), errhint($err_detail:expr), $err_position:expr)) => {
        return Err(ParserError::new_report($err_msg, $err_detail, $err_position));
    };
    ($lexer:expr, WARNING, (errcode($err_code:expr), errmsg($err_msg:expr), errdetail($err_detail:expr), $err_position:expr)) => {
        $lexer.add_warning(ScanReport::new($err_msg, $err_detail, $err_position));
    };
}

macro_rules! yyerror {
    ($msg:expr) => {
        return Err(ParserError::new_error($msg))
    };
}

macro_rules! yyterminate {
    () => {
        return Ok(None);
    };
}

#[cfg(not(feature = "regex-match"))]
pub mod dfa {
    use super::State;

    {{dfa_table}}

    pub fn get_dfa_table(state: State) -> (&'static [[u8; 256]], &'static [u8]) {
        let state_id = state as u8;

        match state_id {
            {{state_id_to_dfa_table}}
            // 0 => (TRANSITION_TABLE_0.as_slice(), ACCEPT_TABLE_0.as_slice()),
            // 1 => (TRANSITION_TABLE_1.as_slice(), ACCEPT_TABLE_1.as_slice()),
            // 2 => (TRANSITION_TABLE_2.as_slice(), ACCEPT_TABLE_2.as_slice()),
            // 3 => (TRANSITION_TABLE_3.as_slice(), ACCEPT_TABLE_3.as_slice()),
            // 4 => (TRANSITION_TABLE_4.as_slice(), ACCEPT_TABLE_4.as_slice()),
            // 5 => (TRANSITION_TABLE_5.as_slice(), ACCEPT_TABLE_5.as_slice()),
            // 6 => (TRANSITION_TABLE_6.as_slice(), ACCEPT_TABLE_6.as_slice()),
            // 7 => (TRANSITION_TABLE_7.as_slice(), ACCEPT_TABLE_7.as_slice()),
            // 8 => (TRANSITION_TABLE_8.as_slice(), ACCEPT_TABLE_8.as_slice()),
            // 9 => (TRANSITION_TABLE_9.as_slice(), ACCEPT_TABLE_9.as_slice()),
            // 10 => (TRANSITION_TABLE_10.as_slice(), ACCEPT_TABLE_10.as_slice()),
            // 11 => (TRANSITION_TABLE_11.as_slice(), ACCEPT_TABLE_11.as_slice()),
            _ => unreachable!(),
        }
    }
}

#[cfg(feature = "regex-match")]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RuleKind {{rule_kinds}}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum State {{states}}

const STANDARD_CONFORMING_STRINGS: bool = true;

impl Lexer {
    #[cfg(not(feature = "regex-match"))]
    pub fn parse_token(&mut self) -> Result<Option<TokenKind>, ParserError> {
        loop {
            let (match_len, pattern_index) = self.find_match_len();

            self.yyleng = match_len;
            let yytext = self.yytext();

            match pattern_index {{pattern_actions_by_index}}
            self.advance();
        }
    }

    #[cfg(feature = "regex-match")]
    pub fn parse_token(&mut self) -> Result<Option<TokenKind>, ParserError> {
        loop {
            let (match_len, kind) = self.find_match_len();

            self.yyleng = match_len;
            let yytext = self.yytext();

            match kind {{actions}}
            self.advance();
        }
    }
}

# [cfg(feature="regex-match")]
pub fn get_rules() -> Vec<crate::lexer::Rule> {
    use crate::lexer::Rule;
    use super::generated::RuleKind;

    vec![{rule_defs}]
}

pub fn get_keyword_map() -> HashMap<&'static str, &'static str> {
    let mut map = HashMap::new();
    for (kw, tok) in [{keyword_map}] {
        map.insert(kw, tok);
    }
    map
}
