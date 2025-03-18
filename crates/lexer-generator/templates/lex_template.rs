#![allow(clippy::all)]
#![allow(unreachable_code)]

/// This file contains ported sources from PostgreSQL

use std::collections::HashMap;

use regex::bytes::Regex;

use super::{
    lexer_ported::{
        get_char_by_byte_pos, is_highbit_set, is_utf16_surrogate_first, is_utf16_surrogate_second,
        surrogate_pair_to_codepoint,
    },
    Lexer, ParserError, Rule, TokenKind, Yylval, NAMEDATALEN,
};

#[macro_export]
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

#[macro_export]
macro_rules! yyerror {
    ($msg:expr) => {
        return Err(ParserError::new_error($msg))
    };
}

#[macro_export]
macro_rules! yyterminate {
    () => {
        return Ok(None);
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RuleKind {{rule_kinds}}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum State {{states}}

const STANDARD_CONFORMING_STRINGS: bool = true;

impl Lexer {
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

pub fn get_rules() -> Vec<Rule> {
    vec![{rule_defs}]
}

pub fn get_keyword_map() -> HashMap<&'static str, &'static str> {
    let mut map = HashMap::new();
    for (kw, tok) in [{keyword_map}] {
        map.insert(kw, tok);
    }
    map
}
