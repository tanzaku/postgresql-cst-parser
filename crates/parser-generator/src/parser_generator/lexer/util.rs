#![allow(dead_code)]

use super::{
    generated::{get_keyword_map, get_rules, RuleKind, State},
    parser_error::ParserError,
    Lexer, ScanReport, TokenKind, Yylval,
};

pub fn is_highbit_set(c: char) -> u8 {
    (c as u8) & 0x80
}

pub fn get_char_by_byte_pos(s: &str, byte_pos: usize) -> char {
    // s.bytes().nth(byte_pos).unwrap() as char
    s.as_bytes()[byte_pos] as char
}

/// from postgresql source
pub fn is_valid_unicode_codepoint(c: char) -> bool {
    let c = c as u32;
    c > 0 && c <= 0x10FFFF
}

pub fn is_utf16_surrogate_first(c: u32) -> bool {
    c >= 0xD800 && c <= 0xDBFF
}

pub fn is_utf16_surrogate_second(c: u32) -> bool {
    c >= 0xDC00 && c <= 0xDFFF
}

pub fn surrogate_pair_to_codepoint(first: u32, second: u32) -> char {
    char::from_u32(((first & 0x3FF) << 10) + 0x10000 + (second & 0x3FF)).unwrap()
}

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

    pub fn addunicode(&mut self, c: char) -> Result<(), ParserError> {
        if !is_valid_unicode_codepoint(c) {
            yyerror!("invalid Unicode escape value");
        }

        self.addlit(c.len_utf8());
        Ok(())
    }

    pub fn unescape_single_char(&mut self, c: char) -> char {
        match c {
            //  'b' => '\b',
            'b' => 0x08 as char,
            //  'f' => '\f',
            'f' => 0x0c as char,
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            //  'v'=>'\v',
            'v' => 0x0b as char,
            _ => {
                /* check for backslash followed by non-7-bit-ASCII */
                if c == '\0' || is_highbit_set(c) != 0 {
                    self.saw_non_ascii = true;
                }

                return c;
            }
        }
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
