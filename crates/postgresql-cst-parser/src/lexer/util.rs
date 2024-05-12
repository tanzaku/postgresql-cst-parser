#![allow(dead_code)]

use regex::bytes::Match;

use super::{
    generated::{get_keyword_map, RuleKind, State},
    {Lexer, Rule, TokenKind, Yylval},
};

pub fn yyerror(msg: &str) {
    eprintln!("{msg}");
    panic!();
}

pub fn get_char_by_byte_pos(s: &str, byte_pos: usize) -> char {
    s.as_bytes()[byte_pos] as char
}

impl Lexer {
    pub fn new(input: &str, rules: Vec<Rule>) -> Self {
        Self {
            input: input.to_string(),
            index_bytes: 0,
            state: State::INITIAL,
            yyleng: 0,

            xcdepth: 0,
            yyllocend_bytes: 0,
            state_before_str_stop: State::INITIAL,
            yylloc_stack: vec![],
            literal: String::new(),
            dolqstart: "".to_string(),

            yylloc_bytes: 0,
            yylval: Yylval::Uninitialized,

            rules,
            keyword_map: get_keyword_map(),
        }
    }

    pub fn get_keyword(&self, s: &str) -> Option<(String, String)> {
        let kw = s.to_ascii_lowercase();
        self.keyword_map
            .get(kw.as_str())
            .map(|kw_token| (kw, kw_token.to_string()))
    }

    pub fn advance(&mut self) {
        self.index_bytes += self.yyleng;
    }

    pub fn begin(&mut self, state: State) {
        self.state = state
    }

    pub fn yytext(&self) -> String {
        self.input[self.index_bytes..][..self.yyleng].to_string()
    }

    pub fn yyless(&mut self, len: usize) {
        self.yyleng = len;
    }

    pub fn set_yylloc(&mut self) {
        self.yylloc_bytes = self.index_bytes;
    }

    pub fn set_yyllocend(&mut self) {
        self.yyllocend_bytes = self.index_bytes + self.yyleng;
    }

    pub fn addlitchar(&mut self, ychar: char) {
        // /* enlarge buffer if needed */
        // if ((yyextra->literallen + 1) >= yyextra->literalalloc)
        // {
        // 	yyextra->literalalloc *= 2;
        // 	yyextra->literalbuf = (char *) repalloc(yyextra->literalbuf,
        // 											yyextra->literalalloc);
        // }
        // /* append new data */
        // yyextra->literalbuf[yyextra->literallen] = ychar;
        // yyextra->literallen += 1;
        self.literal.push(ychar);
    }

    pub fn addlit(&mut self, yyleng: usize) {
        // /* enlarge buffer if needed */
        // if ((yyextra->literallen + yleng) >= yyextra->literalalloc)
        // {
        //     yyextra->literalalloc = pg_nextpower2_32(yyextra->literallen + yleng + 1);
        //     yyextra->literalbuf = (char *) repalloc(yyextra->literalbuf,
        //                                             yyextra->literalalloc);
        // }
        // /* append new data */
        // memcpy(yyextra->literalbuf + yyextra->literallen, ytext, yleng);
        // yyextra->literallen += yleng;
        self.literal += &self.input[self.index_bytes..][..yyleng];
    }

    pub fn push_yylloc(&mut self) {
        self.yylloc_stack.push(self.yylloc_bytes);
    }

    pub fn pop_yylloc(&mut self) {
        self.yylloc_bytes = self.yylloc_stack.pop().unwrap();
    }

    pub fn process_integer_literal(&mut self, radix: usize) -> Option<TokenKind> {
        let yytext_original = self.yytext();
        let mut yytext = yytext_original.as_str();
        let mut neg = false;

        match yytext.bytes().next() {
            Some(b'-') => {
                neg = true;
                yytext = &yytext[1..];
            }
            Some(b'+') => {
                yytext = &yytext[1..];
            }
            _ => (),
        }

        let res_parse_as_i32 = match radix {
            8 => i32::from_str_radix(&yytext[2..], 8),
            10 => yytext.parse::<i32>(),
            16 => i32::from_str_radix(&yytext[2..], 16),
            _ => unreachable!(),
        };

        let ok = res_parse_as_i32
            .as_ref()
            .map(|res| !neg || *res != i32::MIN)
            .unwrap_or_default();

        if ok {
            let res = res_parse_as_i32.unwrap();
            self.yylval = Yylval::I(if neg { -res } else { res });
            Some(TokenKind::ICONST)
        } else {
            self.yylval = Yylval::Str(yytext_original);
            Some(TokenKind::FCONST)
        }
    }

    pub fn downcase_truncate_identifier(&self, yyleng: usize, _warn: bool) -> String {
        self.yytext()[..yyleng].to_ascii_lowercase()
    }

    pub fn find_match(&self) -> (Match, RuleKind) {
        let rules = self.rules.iter().filter(|rule| rule.state == self.state);

        let s = &self.input[self.index_bytes..];

        let mut longest_match: Option<Match> = None;
        let mut kind = RuleKind::INITIAL1;
        for rule in rules {
            if let Some(m) = rule.pattern.find(s.as_bytes()) {
                if longest_match.map_or(-1, |m| m.len() as i64) < m.len() as i64 {
                    longest_match = Some(m);
                    kind = rule.kind;
                }
            }
        }

        (longest_match.unwrap(), kind)
    }
}
