/// Ported sources from PostgreSQL
use super::{parser_error::ParserError, Lexer, TokenKind, Yylval};

pub fn is_highbit_set(c: char) -> u8 {
    (c as u8) & 0x80
}

pub fn get_char_by_byte_pos(s: &str, byte_pos: usize) -> char {
    // s.bytes().nth(byte_pos).unwrap() as char
    s.as_bytes()[byte_pos] as char
}

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
}
