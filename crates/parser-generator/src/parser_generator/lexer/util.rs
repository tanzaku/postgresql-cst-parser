#![allow(dead_code)]

use super::{
    generated::{get_keyword_map, get_rules, RuleKind, State},
    Lexer, ScanReport, Yylval,
};

impl Lexer {
    pub fn new(input: &str) -> Self {
        let rules = get_rules();

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
            warn_on_first_escape: false,
            saw_non_ascii: false,
            utf16_first_part: 0,

            yylloc_bytes: 0,
            yylval: Yylval::Uninitialized,

            rules,
            keyword_map: get_keyword_map(),
            reports: Vec::new(),
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

    pub fn find_match_len(&self) -> (usize, RuleKind) {
        let rules = self.rules.iter().filter(|rule| rule.state == self.state);

        let s = &self.input[self.index_bytes..];

        let mut longest_match = 0;
        let mut eof = false;
        let mut kind = RuleKind::INITIAL1;
        for rule in rules {
            if let Some(m) = rule.pattern.find(s.as_bytes()) {
                // treat eof as single null character
                let match_len = if rule.eof { 1 } else { m.len() };

                if longest_match < match_len {
                    longest_match = match_len;
                    eof = rule.eof;
                    kind = rule.kind;
                }
            }
        }

        // The match length is treated as 1 in the case of EOF to ensure that the state handling EOF is properly selected.
        // Unlike C, strings are not null-terminated, so it's more convenient for subsequent processing to treat the match length as 0, therefore in the case of EOF, it's treated as 0.
        if eof {
            (0, kind)
        } else {
            (longest_match, kind)
        }
    }

    pub fn add_warning(&mut self, report: ScanReport) {
        self.reports.push(report);
    }

    pub fn lexer_errposition(&self) -> usize {
        self.index_bytes
    }
}
