#![allow(dead_code)]

use super::{
    Lexer, ScanReport, Yylval,
    generated::{State, get_keyword_map},
};

impl Lexer {
    pub fn new(input: &str) -> Self {
        #[cfg(feature = "regex-match")]
        let rules = super::generated::get_rules();

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

            #[cfg(feature = "regex-match")]
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

    #[cfg(not(feature = "regex-match"))]
    pub fn find_match_len(&self) -> (usize, u8) {
        use super::generated::dfa::get_dfa_table;

        let s = &self.input[self.index_bytes..];

        let (transition, accept) = get_dfa_table(self.state);

        let mut dfa_state_index = 0_u8;
        let mut accept_rule = accept[dfa_state_index as usize];
        let mut longest_match = 0;

        for (i, byte) in s.as_bytes().iter().enumerate() {
            let transition_index = *byte as usize;
            dfa_state_index = transition[dfa_state_index as usize][transition_index];

            if dfa_state_index == !0 {
                return (longest_match, accept_rule);
            }

            if accept[dfa_state_index as usize] != !0 {
                accept_rule = accept[dfa_state_index as usize];
                longest_match = i + 1;
            }
        }

        // Check for match against EOF
        if transition[dfa_state_index as usize][0] != !0 {
            // Currently, EOF is represented by byte value 0
            dfa_state_index = transition[dfa_state_index as usize][0];

            if accept[dfa_state_index as usize] != !0 {
                accept_rule = accept[dfa_state_index as usize];
                longest_match = s.len();
            }
        }

        (longest_match, accept_rule)
    }

    #[cfg(feature = "regex-match")]
    pub fn find_match_len(&self) -> (usize, super::generated::RuleKind) {
        let rules = self.rules.iter().filter(|rule| rule.state == self.state);

        let s = &self.input[self.index_bytes..];

        let mut longest_match = 0;
        let mut eof = false;
        let mut kind = super::generated::RuleKind::INITIAL1;
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
