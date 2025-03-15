#![allow(clippy::all)]
#![allow(unreachable_code)]

use std::collections::HashMap;

use regex::bytes::Regex;

use super::{
    util::{get_char_by_byte_pos, yyerror},
    {Lexer, Rule, TokenKind, Yylval, NAMEDATALEN},
};

macro_rules! yyterminate {
    () => {
        return None;
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RuleKind {{rule_kinds}}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum State {{states}}

const STANDARD_CONFORMING_STRINGS: bool = true;

impl Lexer {
    pub fn parse_token(&mut self) -> Option<TokenKind> {
        loop {
            let (m, kind) = self.find_match();

            self.yyleng = m.len();
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
