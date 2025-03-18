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
        return Err(ParserError::new_report(
            $err_msg,
            $err_detail,
            $err_position,
        ));
    };
    ($lexer:expr, ERROR, (errcode($err_code:expr), errmsg($err_msg:expr), errhint($err_detail:expr), $err_position:expr)) => {
        return Err(ParserError::new_report(
            $err_msg,
            $err_detail,
            $err_position,
        ));
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
pub enum RuleKind {
    INITIAL1,
    INITIAL2,
    INITIAL3,
    xc1,
    xc2,
    xc3,
    xc4,
    xc5,
    xc6,
    INITIAL4,
    xh1,
    xb1,
    xb2,
    INITIAL5,
    xh2,
    INITIAL6,
    INITIAL7,
    INITIAL8,
    INITIAL9,
    xb3,
    xh3,
    xq1,
    xe1,
    xus1,
    xqs1,
    xqs2,
    xqs3,
    xqs4,
    xq2,
    xe2,
    xus2,
    xq3,
    xus3,
    xe3,
    xe4,
    xeu1,
    xeu2,
    xeu3,
    xeu4,
    xe5,
    xeu5,
    xe6,
    xe7,
    xe8,
    xe9,
    xq4,
    xe10,
    xus4,
    INITIAL10,
    INITIAL11,
    xdolq1,
    xdolq2,
    xdolq3,
    xdolq4,
    xdolq5,
    INITIAL12,
    INITIAL13,
    xd1,
    xui1,
    xd2,
    xui2,
    xd3,
    xui3,
    xd4,
    xui4,
    INITIAL14,
    INITIAL15,
    INITIAL16,
    INITIAL17,
    INITIAL18,
    INITIAL19,
    INITIAL20,
    INITIAL21,
    INITIAL22,
    INITIAL23,
    INITIAL24,
    INITIAL25,
    INITIAL26,
    INITIAL27,
    INITIAL28,
    INITIAL29,
    INITIAL30,
    INITIAL31,
    INITIAL32,
    INITIAL33,
    INITIAL34,
    INITIAL35,
    INITIAL36,
    INITIAL37,
    INITIAL38,
    INITIAL39,
    INITIAL40,
    INITIAL41,
    INITIAL42,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum State {
    INITIAL,
    xb,
    xc,
    xd,
    xh,
    xq,
    xqs,
    xe,
    xdolq,
    xui,
    xus,
    xeu,
}

const STANDARD_CONFORMING_STRINGS: bool = true;

impl Lexer {
    pub fn parse_token(&mut self) -> Result<Option<TokenKind>, ParserError> {
        loop {
            let (match_len, kind) = self.find_match_len();

            self.yyleng = match_len;
            let yytext = self.yytext();

            match kind {
                RuleKind::INITIAL1 => {
                    { /* ignore */ }
                }
                RuleKind::INITIAL2 => {
                    {
                        // SET_YYLLOC();
                        // return SQL_COMMENT;

                        self.set_yylloc();
                        return Ok(Some(TokenKind::SQL_COMMENT));
                    }
                }
                RuleKind::INITIAL3 => {
                    {
                        // /* Set location in case of syntax error in comment */
                        // SET_YYLLOC();
                        // yyextra->xcdepth = 0;
                        // BEGIN(xc);
                        // /* Put back any characters past slash-star; see above */
                        // yyless(2);

                        /* Set location in case of syntax error in comment */
                        self.set_yylloc();
                        self.xcdepth = 0;
                        self.begin(State::xc);
                        /* Put back any characters past slash-star; see above */
                        self.yyless(2);
                    }
                }
                RuleKind::xc1 => {
                    {
                        // (yyextra->xcdepth)++;
                        // /* Put back any characters past slash-star; see above */
                        // yyless(2);

                        self.xcdepth += 1;
                        /* Put back any characters past slash-star; see above */
                        self.yyless(2);
                    }
                }
                RuleKind::xc2 => {
                    {
                        // if (yyextra->xcdepth <= 0)
                        // {
                        // 	BEGIN(INITIAL);
                        // 	yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // 	return C_COMMENT;
                        // }
                        // else
                        // 	(yyextra->xcdepth)--;

                        if self.xcdepth <= 0 {
                            self.begin(State::INITIAL);
                            self.set_yyllocend();
                            return Ok(Some(TokenKind::C_COMMENT));
                        } else {
                            self.xcdepth -= 1;
                        }
                    }
                }
                RuleKind::xc3 => {
                    { /* ignore */ }
                }
                RuleKind::xc4 => {
                    { /* ignore */ }
                }
                RuleKind::xc5 => {
                    { /* ignore */ }
                }
                RuleKind::xc6 => {
                    yyerror!("unterminated /* comment");
                }
                RuleKind::INITIAL4 => {
                    {
                        // /* Binary bit type.
                        //  * At some point we should simply pass the string
                        //  * forward to the parser and label it there.
                        //  * In the meantime, place a leading "b" on the string
                        //  * to mark it for the input routine as a binary string.
                        //  */
                        // SET_YYLLOC();
                        // BEGIN(xb);
                        // startlit();
                        // addlitchar('b', yyscanner);

                        /* Binary bit type.
                         * At some point we should simply pass the string
                         * forward to the parser and label it there.
                         * In the meantime, place a leading "b" on the string
                         * to mark it for the input routine as a binary string.
                         */
                        self.set_yylloc();
                        self.begin(State::xb);
                        self.literal.clear();
                        self.addlitchar('b');
                    }
                }
                RuleKind::xh1 | RuleKind::xb1 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xb2 => {
                    yyerror!("unterminated bit string literal");
                }
                RuleKind::INITIAL5 => {
                    {
                        // /* Hexadecimal bit type.
                        //  * At some point we should simply pass the string
                        //  * forward to the parser and label it there.
                        //  * In the meantime, place a leading "x" on the string
                        //  * to mark it for the input routine as a hex string.
                        //  */
                        // SET_YYLLOC();
                        // BEGIN(xh);
                        // startlit();
                        // addlitchar('x', yyscanner);

                        /* Hexadecimal bit type.
                         * At some point we should simply pass the string
                         * forward to the parser and label it there.
                         * In the meantime, place a leading "x" on the string
                         * to mark it for the input routine as a hex string.
                         */
                        self.set_yylloc();
                        self.begin(State::xh);
                        self.literal.clear();
                        self.addlitchar('x');
                    }
                }
                RuleKind::xh2 => {
                    yyerror!("unterminated hexadecimal string literal");
                }
                RuleKind::INITIAL6 => {
                    {
                        // /* National character.
                        //  * We will pass this along as a normal character string,
                        //  * but preceded with an internally-generated "NCHAR".
                        //  */
                        // int		kwnum;
                        //
                        // SET_YYLLOC();
                        // yyless(1);	/* eat only 'n' this time */
                        //
                        // kwnum = ScanKeywordLookup("nchar",
                        // 						  yyextra->keywordlist);
                        // if (kwnum >= 0)
                        // {
                        // 	yylval->keyword = GetScanKeyword(kwnum,
                        // 									 yyextra->keywordlist);
                        // 	return yyextra->keyword_tokens[kwnum];
                        // }
                        // else
                        // {
                        // 	/* If NCHAR isn't a keyword, just return "n" */
                        // 	yylval->str = pstrdup("n");
                        // 	yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // 	return IDENT;
                        // }

                        self.set_yylloc();
                        self.yyless(1); /* eat only 'n' this time */

                        if let Some((kw, kw_token)) = self.get_keyword("nchar") {
                            self.yylval = Yylval::Keyword(kw);
                            return Ok(Some(TokenKind::KEYWORD(kw_token)));
                        } else {
                            /* If NCHAR isn't a keyword, just return "n" */
                            self.yylval = Yylval::Str("n".to_string());
                            self.set_yyllocend();
                            return Ok(Some(TokenKind::IDENT));
                        }
                    }
                }
                RuleKind::INITIAL7 => {
                    {
                        // yyextra->warn_on_first_escape = true;
                        // yyextra->saw_non_ascii = false;
                        // SET_YYLLOC();
                        // if (yyextra->standard_conforming_strings)
                        // 	BEGIN(xq);
                        // else
                        // 	BEGIN(xe);
                        // startlit();

                        self.warn_on_first_escape = true;
                        self.saw_non_ascii = false;
                        self.set_yylloc();
                        if STANDARD_CONFORMING_STRINGS {
                            self.begin(State::xq);
                        } else {
                            self.begin(State::xe);
                        }
                        self.literal.clear();
                    }
                }
                RuleKind::INITIAL8 => {
                    {
                        // yyextra->warn_on_first_escape = false;
                        // yyextra->saw_non_ascii = false;
                        // SET_YYLLOC();
                        // BEGIN(xe);
                        // startlit();

                        self.warn_on_first_escape = false;
                        self.saw_non_ascii = false;
                        self.set_yylloc();
                        self.begin(State::xe);
                        self.literal.clear();
                    }
                }
                RuleKind::INITIAL9 => {
                    {
                        // SET_YYLLOC();
                        // if (!yyextra->standard_conforming_strings)
                        // 	ereport(ERROR,
                        // 			(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                        // 			 errmsg("unsafe use of string constant with Unicode escapes"),
                        // 			 errdetail("String constants with Unicode escapes cannot be used when \"standard_conforming_strings\" is off."),
                        // 			 lexer_errposition()));
                        // BEGIN(xus);
                        // startlit();

                        self.set_yylloc();
                        if !STANDARD_CONFORMING_STRINGS {
                            ereport!(self, ERROR,
								(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
								 errmsg("unsafe use of string constant with Unicode escapes"),
								 errdetail("String constants with Unicode escapes cannot be used when standard_conforming_strings is off."),
								 self.lexer_errposition()));
                        }
                        self.begin(State::xus);
                        self.literal.clear();
                    }
                }
                RuleKind::xb3 => {
                    {
                        // /*
                        //  * When we are scanning a quoted string and see an end
                        //  * quote, we must look ahead for a possible continuation.
                        //  * If we don't see one, we know the end quote was in fact
                        //  * the end of the string.  To reduce the lexer table size,
                        //  * we use a single "xqs" state to do the lookahead for all
                        //  * types of strings.
                        //  */
                        // yyextra->state_before_str_stop = YYSTATE;
                        // BEGIN(xqs);

                        /*
                         * When we are scanning a quoted string and see an end
                         * quote, we must look ahead for a possible continuation.
                         * If we don't see one, we know the end quote was in fact
                         * the end of the string.  To reduce the lexer table size,
                         * we use a single "xqs" state to do the lookahead for all
                         * types of strings.
                         */
                        self.state_before_str_stop = self.state;
                        self.begin(State::xqs);
                    }
                }
                RuleKind::xh3 => {
                    {
                        // /*
                        //  * When we are scanning a quoted string and see an end
                        //  * quote, we must look ahead for a possible continuation.
                        //  * If we don't see one, we know the end quote was in fact
                        //  * the end of the string.  To reduce the lexer table size,
                        //  * we use a single "xqs" state to do the lookahead for all
                        //  * types of strings.
                        //  */
                        // yyextra->state_before_str_stop = YYSTATE;
                        // BEGIN(xqs);

                        /*
                         * When we are scanning a quoted string and see an end
                         * quote, we must look ahead for a possible continuation.
                         * If we don't see one, we know the end quote was in fact
                         * the end of the string.  To reduce the lexer table size,
                         * we use a single "xqs" state to do the lookahead for all
                         * types of strings.
                         */
                        self.state_before_str_stop = self.state;
                        self.begin(State::xqs);
                    }
                }
                RuleKind::xq1 => {
                    {
                        // /*
                        //  * When we are scanning a quoted string and see an end
                        //  * quote, we must look ahead for a possible continuation.
                        //  * If we don't see one, we know the end quote was in fact
                        //  * the end of the string.  To reduce the lexer table size,
                        //  * we use a single "xqs" state to do the lookahead for all
                        //  * types of strings.
                        //  */
                        // yyextra->state_before_str_stop = YYSTATE;
                        // BEGIN(xqs);

                        /*
                         * When we are scanning a quoted string and see an end
                         * quote, we must look ahead for a possible continuation.
                         * If we don't see one, we know the end quote was in fact
                         * the end of the string.  To reduce the lexer table size,
                         * we use a single "xqs" state to do the lookahead for all
                         * types of strings.
                         */
                        self.state_before_str_stop = self.state;
                        self.begin(State::xqs);
                    }
                }
                RuleKind::xe1 => {
                    {
                        // /*
                        //  * When we are scanning a quoted string and see an end
                        //  * quote, we must look ahead for a possible continuation.
                        //  * If we don't see one, we know the end quote was in fact
                        //  * the end of the string.  To reduce the lexer table size,
                        //  * we use a single "xqs" state to do the lookahead for all
                        //  * types of strings.
                        //  */
                        // yyextra->state_before_str_stop = YYSTATE;
                        // BEGIN(xqs);

                        /*
                         * When we are scanning a quoted string and see an end
                         * quote, we must look ahead for a possible continuation.
                         * If we don't see one, we know the end quote was in fact
                         * the end of the string.  To reduce the lexer table size,
                         * we use a single "xqs" state to do the lookahead for all
                         * types of strings.
                         */
                        self.state_before_str_stop = self.state;
                        self.begin(State::xqs);
                    }
                }
                RuleKind::xus1 => {
                    {
                        // /*
                        //  * When we are scanning a quoted string and see an end
                        //  * quote, we must look ahead for a possible continuation.
                        //  * If we don't see one, we know the end quote was in fact
                        //  * the end of the string.  To reduce the lexer table size,
                        //  * we use a single "xqs" state to do the lookahead for all
                        //  * types of strings.
                        //  */
                        // yyextra->state_before_str_stop = YYSTATE;
                        // BEGIN(xqs);

                        /*
                         * When we are scanning a quoted string and see an end
                         * quote, we must look ahead for a possible continuation.
                         * If we don't see one, we know the end quote was in fact
                         * the end of the string.  To reduce the lexer table size,
                         * we use a single "xqs" state to do the lookahead for all
                         * types of strings.
                         */
                        self.state_before_str_stop = self.state;
                        self.begin(State::xqs);
                    }
                }
                RuleKind::xqs1 => {
                    {
                        // /*
                        //  * Found a quote continuation, so return to the in-quote
                        //  * state and continue scanning the literal.  Nothing is
                        //  * added to the literal's contents.
                        //  */
                        // BEGIN(yyextra->state_before_str_stop);

                        /*
                         * Found a quote continuation, so return to the in-quote
                         * state and continue scanning the literal.  Nothing is
                         * added to the literal's contents.
                         */
                        self.begin(self.state_before_str_stop);
                    }
                }
                RuleKind::xqs2 | RuleKind::xqs3 | RuleKind::xqs4 => {
                    {
                        // /*
                        //  * Failed to see a quote continuation.  Throw back
                        //  * everything after the end quote, and handle the string
                        //  * according to the state we were in previously.
                        //  */
                        // yyless(0);
                        // BEGIN(INITIAL);
                        //
                        // switch (yyextra->state_before_str_stop)
                        // {
                        // 	case xb:
                        // 		yylval->str = litbufdup(yyscanner);
                        // 		yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // 		return BCONST;
                        // 	case xh:
                        // 		yylval->str = litbufdup(yyscanner);
                        // 		yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // 		return XCONST;
                        // 	case xq:
                        // 	case xe:
                        // 		/*
                        // 		 * Check that the data remains valid, if it might
                        // 		 * have been made invalid by unescaping any chars.
                        // 		 */
                        // 		if (yyextra->saw_non_ascii)
                        // 			pg_verifymbstr(yyextra->literalbuf,
                        // 						   yyextra->literallen,
                        // 						   false);
                        // 		yylval->str = litbufdup(yyscanner);
                        // 		yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // 		return SCONST;
                        // 	case xus:
                        // 		yylval->str = litbufdup(yyscanner);
                        // 		yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // 		return USCONST;
                        // 	default:
                        // 		yyerror("unhandled previous state in xqs");
                        // }

                        /*
                         * Failed to see a quote continuation.  Throw back
                         * everything after the end quote, and handle the string
                         * according to the state we were in previously.
                         */
                        self.yyless(0);
                        self.begin(State::INITIAL);

                        match self.state_before_str_stop {
                            State::xb => {
                                self.yylval = Yylval::Str(self.literal.clone());
                                self.set_yyllocend();
                                return Ok(Some(TokenKind::BCONST));
                            }
                            State::xh => {
                                self.yylval = Yylval::Str(self.literal.clone());
                                self.set_yyllocend();
                                return Ok(Some(TokenKind::XCONST));
                            }
                            State::xq | State::xe => {
                                /*
                                 * Check that the data remains valid, if it might
                                 * have been made invalid by unescaping any chars.
                                 */
                                // TODO verify
                                // if (yyextra->saw_non_ascii)
                                // 	pg_verifymbstr(yyextra->literalbuf,
                                // 				   yyextra->literallen,
                                // 				   false);
                                self.yylval = Yylval::Str(self.literal.clone());
                                self.set_yyllocend();
                                return Ok(Some(TokenKind::SCONST));
                            }
                            State::xus => {
                                self.yylval = Yylval::Str(self.literal.clone());
                                self.set_yyllocend();
                                return Ok(Some(TokenKind::USCONST));
                            }
                            _ => yyerror!("unhandled previous state in xqs"),
                        }
                    }
                }
                RuleKind::xq2 => {
                    {
                        // self.literal += '\'';

                        self.addlitchar('\'');
                    }
                }
                RuleKind::xe2 => {
                    {
                        // self.literal += '\'';

                        self.addlitchar('\'');
                    }
                }
                RuleKind::xus2 => {
                    {
                        // self.literal += '\'';

                        self.addlitchar('\'');
                    }
                }
                RuleKind::xq3 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xus3 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xe3 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xe4 => {
                    {
                        // pg_wchar	c = strtoul(yytext + 2, NULL, 16);
                        //
                        // /*
                        //  * For consistency with other productions, issue any
                        //  * escape warning with cursor pointing to start of string.
                        //  * We might want to change that, someday.
                        //  */
                        // check_escape_warning(yyscanner);
                        //
                        // /* Remember start of overall string token ... */
                        // PUSH_YYLLOC();
                        // /* ... and set the error cursor to point at this esc seq */
                        // SET_YYLLOC();
                        //
                        // if (is_utf16_surrogate_first(c))
                        // {
                        // 	yyextra->utf16_first_part = c;
                        // 	BEGIN(xeu);
                        // }
                        // else if (is_utf16_surrogate_second(c))
                        // 	yyerror("invalid Unicode surrogate pair");
                        // else
                        // 	addunicode(c, yyscanner);
                        //
                        // /* Restore yylloc to be start of string token */
                        // POP_YYLLOC();

                        let c = u32::from_str_radix(
                            &self.input[self.index_bytes + 2..self.index_bytes + self.yyleng],
                            16,
                        )
                        .unwrap();

                        self.push_yylloc();
                        self.set_yylloc();

                        if is_utf16_surrogate_first(c) {
                            self.utf16_first_part = c;
                            self.begin(State::xeu);
                        } else if is_utf16_surrogate_second(c) {
                            yyerror!("invalid Unicode surrogate pair");
                        } else {
                            self.addunicode(char::from_u32(c as u32).unwrap())?;
                        }

                        self.pop_yylloc();
                    }
                }
                RuleKind::xeu1 => {
                    {
                        // pg_wchar	c = strtoul(yytext + 2, NULL, 16);
                        //
                        // /* Remember start of overall string token ... */
                        // PUSH_YYLLOC();
                        // /* ... and set the error cursor to point at this esc seq */
                        // SET_YYLLOC();
                        //
                        // if (!is_utf16_surrogate_second(c))
                        // 	yyerror("invalid Unicode surrogate pair");
                        //
                        // c = surrogate_pair_to_codepoint(yyextra->utf16_first_part, c);
                        //
                        // addunicode(c, yyscanner);
                        //
                        // /* Restore yylloc to be start of string token */
                        // POP_YYLLOC();
                        //
                        // BEGIN(xe);

                        let c = u32::from_str_radix(
                            &self.input[self.index_bytes + 2..self.index_bytes + self.yyleng],
                            16,
                        )
                        .unwrap();

                        self.push_yylloc();
                        self.set_yylloc();

                        if !is_utf16_surrogate_second(c) {
                            yyerror!("invalid Unicode surrogate pair");
                        }

                        let c = surrogate_pair_to_codepoint(self.utf16_first_part, c);
                        self.addunicode(c)?;

                        self.pop_yylloc();
                        self.begin(State::xe);
                    }
                }
                RuleKind::xeu2 | RuleKind::xeu3 | RuleKind::xeu4 => {
                    {
                        // /* Set the error cursor to point at missing esc seq */
                        // SET_YYLLOC();
                        // yyerror("invalid Unicode surrogate pair");

                        /* Set the error cursor to point at missing esc seq */
                        self.set_yylloc();
                        yyerror!("invalid Unicode surrogate pair");
                    }
                }
                RuleKind::xe5 => {
                    {
                        // /* Set the error cursor to point at malformed esc seq */
                        // SET_YYLLOC();
                        // ereport(ERROR,
                        // 		(errcode(ERRCODE_INVALID_ESCAPE_SEQUENCE),
                        // 		 errmsg("invalid Unicode escape"),
                        // 		 errhint("Unicode escapes must be \\uXXXX or \\UXXXXXXXX."),
                        // 		 lexer_errposition()));

                        self.set_yylloc();
                        ereport!(
                            self,
                            ERROR,
                            (
                                errcode(ERRCODE_INVALID_ESCAPE_SEQUENCE),
                                errmsg("invalid Unicode escape"),
                                errhint("Unicode escapes must be \\uXXXX or \\UXXXXXXXX."),
                                self.lexer_errposition()
                            )
                        );
                    }
                }
                RuleKind::xeu5 => {
                    {
                        // /* Set the error cursor to point at malformed esc seq */
                        // SET_YYLLOC();
                        // ereport(ERROR,
                        // 		(errcode(ERRCODE_INVALID_ESCAPE_SEQUENCE),
                        // 		 errmsg("invalid Unicode escape"),
                        // 		 errhint("Unicode escapes must be \\uXXXX or \\UXXXXXXXX."),
                        // 		 lexer_errposition()));

                        self.set_yylloc();
                        ereport!(
                            self,
                            ERROR,
                            (
                                errcode(ERRCODE_INVALID_ESCAPE_SEQUENCE),
                                errmsg("invalid Unicode escape"),
                                errhint("Unicode escapes must be \\uXXXX or \\UXXXXXXXX."),
                                self.lexer_errposition()
                            )
                        );
                    }
                }
                RuleKind::xe6 => {
                    {
                        // if (yytext[1] == '\'')
                        // {
                        // 	if (yyextra->backslash_quote == BACKSLASH_QUOTE_OFF ||
                        // 		(yyextra->backslash_quote == BACKSLASH_QUOTE_SAFE_ENCODING &&
                        // 		 PG_ENCODING_IS_CLIENT_ONLY(pg_get_client_encoding())))
                        // 		ereport(ERROR,
                        // 				(errcode(ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER),
                        // 				 errmsg("unsafe use of \\' in a string literal"),
                        // 				 errhint("Use '' to write quotes in strings. \\' is insecure in client-only encodings."),
                        // 				 lexer_errposition()));
                        // }
                        // check_string_escape_warning(yytext[1], yyscanner);
                        // addlitchar(unescape_single_char(yytext[1], yyscanner),
                        // 		   yyscanner);

                        let c = self.yytext().chars().nth(1).unwrap();
                        // if c == '\'' {
                        // 	if self.backslash_quote == BACKSLASH_QUOTE_OFF ||
                        // 		(self.backslash_quote == BACKSLASH_QUOTE_SAFE_ENCODING &&
                        // 		 PG_ENCODING_IS_CLIENT_ONLY(pg_get_client_encoding())) {
                        // 		ereport!(self, ERROR,
                        // 				(errcode(ERRCODE_NONSTANDARD_USE_OF_ESCAPE_CHARACTER),
                        // 				 errmsg("unsafe use of \\' in a string literal"),
                        // 				 errhint("Use '' to write quotes in strings. \\' is insecure in client-only encodings."),
                        // 				 self.lexer_errposition()));
                        // 	}
                        // }

                        // self.check_string_escape_warning(c);
                        let c = self.unescape_single_char(c);
                        self.addlitchar(c);
                    }
                }
                RuleKind::xe7 => {
                    {
                        // unsigned char c = strtoul(yytext + 1, NULL, 8);
                        //
                        // check_escape_warning(yyscanner);
                        // addlitchar(c, yyscanner);
                        // if (c == '\0' || IS_HIGHBIT_SET(c))
                        // 	yyextra->saw_non_ascii = true;

                        let c = u32::from_str_radix(
                            &self.input[self.index_bytes + 1..self.index_bytes + self.yyleng],
                            8,
                        )
                        .unwrap();
                        let c = char::from_u32(c).unwrap();

                        // self.check_escape_warning();
                        self.addlitchar(c);
                        if c == '\0' || is_highbit_set(c) != 0 {
                            self.saw_non_ascii = true;
                        }
                    }
                }
                RuleKind::xe8 => {
                    {
                        // unsigned char c = strtoul(yytext + 2, NULL, 16);
                        //
                        // check_escape_warning(yyscanner);
                        // addlitchar(c, yyscanner);
                        // if (c == '\0' || IS_HIGHBIT_SET(c))
                        // 	yyextra->saw_non_ascii = true;

                        let c = u32::from_str_radix(
                            &self.input[self.index_bytes + 1..self.index_bytes + self.yyleng],
                            16,
                        )
                        .unwrap();
                        let c = char::from_u32(c).unwrap();

                        // self.check_escape_warning();
                        self.addlitchar(c);
                        if c == '\0' || is_highbit_set(c) != 0 {
                            self.saw_non_ascii = true;
                        }
                    }
                }
                RuleKind::xe9 => {
                    {
                        // /* This is only needed for \ just before EOF */
                        // addlitchar(yytext[0], yyscanner);

                        /* This is only needed for \ just before EOF */
                        let c = self.input[self.index_bytes..].chars().next().unwrap();
                        self.addlitchar(c);
                    }
                }
                RuleKind::xq4 => {
                    yyerror!("unterminated quoted string");
                }
                RuleKind::xe10 => {
                    yyerror!("unterminated quoted string");
                }
                RuleKind::xus4 => {
                    yyerror!("unterminated quoted string");
                }
                RuleKind::INITIAL10 => {
                    {
                        // SET_YYLLOC();
                        // yyextra->dolqstart = pstrdup(yytext);
                        // BEGIN(xdolq);
                        // startlit();

                        self.set_yylloc();
                        self.dolqstart = self.yytext();
                        self.begin(State::xdolq);
                        self.literal.clear();
                    }
                }
                RuleKind::INITIAL11 => {
                    {
                        // SET_YYLLOC();
                        // /* throw back all but the initial "$" */
                        // yyless(1);
                        // /* and treat it as {other} */
                        // return yytext[0];

                        self.set_yylloc();
                        /* throw back all but the initial "$" */
                        self.yyless(1);
                        /* and treat it as {other} */
                        return Ok(Some(TokenKind::RAW(self.yytext())));
                    }
                }
                RuleKind::xdolq1 => {
                    {
                        // if (strcmp(yytext, yyextra->dolqstart) == 0)
                        // {
                        // 	pfree(yyextra->dolqstart);
                        // 	yyextra->dolqstart = NULL;
                        // 	BEGIN(INITIAL);
                        // 	yylval->str = litbufdup(yyscanner);
                        // 	yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // 	return SCONST;
                        // }
                        // else
                        // {
                        // 	/*
                        // 	 * When we fail to match $...$ to dolqstart, transfer
                        // 	 * the $... part to the output, but put back the final
                        // 	 * $ for rescanning.  Consider $delim$...$junk$delim$
                        // 	 */
                        // 	addlit(yytext, yyleng - 1, yyscanner);
                        // 	yyless(yyleng - 1);
                        // }

                        if self.yytext() == self.dolqstart {
                            self.dolqstart = "".to_string();
                            self.begin(State::INITIAL);
                            self.yylval = Yylval::Str(self.literal.clone());
                            self.set_yyllocend();
                            return Ok(Some(TokenKind::SCONST));
                        } else {
                            /*
                             * When we fail to match $...$ to dolqstart, transfer
                             * the $... part to the output, but put back the final
                             * $ for rescanning.  Consider $delim$...$junk$delim$
                             */
                            self.addlit(self.yyleng - 1);
                            self.yyless(self.yyleng - 1);
                        }
                    }
                }
                RuleKind::xdolq2 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xdolq3 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xdolq4 => {
                    {
                        // /* This is only needed for $ inside the quoted text */
                        // addlitchar(yytext[0], yyscanner);

                        let c = self.yytext().chars().next().unwrap();
                        self.addlitchar(c);
                    }
                }
                RuleKind::xdolq5 => {
                    yyerror!("unterminated dollar-quoted string");
                }
                RuleKind::INITIAL12 => {
                    {
                        // SET_YYLLOC();
                        // BEGIN(xd);
                        // startlit();

                        self.set_yylloc();
                        self.begin(State::xd);
                        self.literal.clear();
                    }
                }
                RuleKind::INITIAL13 => {
                    {
                        // SET_YYLLOC();
                        // BEGIN(xui);
                        // startlit();

                        self.set_yylloc();
                        self.begin(State::xui);
                        self.literal.clear();
                    }
                }
                RuleKind::xd1 => {
                    {
                        // char	   *ident;
                        //
                        // BEGIN(INITIAL);
                        // if (yyextra->literallen == 0)
                        // 	yyerror("zero-length delimited identifier");
                        // ident = litbufdup(yyscanner);
                        // if (yyextra->literallen >= NAMEDATALEN)
                        // 	truncate_identifier(ident, yyextra->literallen, true);
                        // yylval->str = ident;
                        // yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // return IDENT;

                        self.begin(State::INITIAL);
                        if self.literal.len() == 0 {
                            yyerror!("zero-length delimited identifier");
                        }
                        let ident = self.literal.clone();
                        if self.literal.len() >= NAMEDATALEN {
                            // TODO
                            // panic!();
                        }
                        self.yylval = Yylval::Str(ident);
                        self.set_yyllocend();
                        return Ok(Some(TokenKind::IDENT));
                    }
                }
                RuleKind::xui1 => {
                    {
                        // BEGIN(INITIAL);
                        // if (yyextra->literallen == 0)
                        // 	yyerror("zero-length delimited identifier");
                        // /* can't truncate till after we de-escape the ident */
                        // yylval->str = litbufdup(yyscanner);
                        // yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // return UIDENT;

                        self.begin(State::INITIAL);
                        if self.literal.len() == 0 {
                            yyerror!("zero-length delimited identifier");
                        }
                        /* can't truncate till after we de-escape the ident */
                        self.yylval = Yylval::Str(self.yytext());
                        self.set_yyllocend();
                        return Ok(Some(TokenKind::UIDENT));
                    }
                }
                RuleKind::xd2 => {
                    {
                        // addlitchar('"', yyscanner);

                        self.addlitchar('"');
                    }
                }
                RuleKind::xui2 => {
                    {
                        // addlitchar('"', yyscanner);

                        self.addlitchar('"');
                    }
                }
                RuleKind::xd3 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xui3 => {
                    {
                        // addlit(yytext, yyleng, yyscanner);

                        self.addlit(self.yyleng);
                    }
                }
                RuleKind::xd4 => {
                    yyerror!("unterminated quoted identifier");
                }
                RuleKind::xui4 => {
                    yyerror!("unterminated quoted identifier");
                }
                RuleKind::INITIAL14 => {
                    {
                        // char	   *ident;
                        //
                        // SET_YYLLOC();
                        // /* throw back all but the initial u/U */
                        // yyless(1);
                        // /* and treat it as {identifier} */
                        // ident = downcase_truncate_identifier(yytext, yyleng, true);
                        // yylval->str = ident;
                        // yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // return IDENT;

                        self.set_yylloc();
                        self.yyless(1);
                        // FIXME
                        // let ident = downcase_truncate_identifier(yytext, yyleng, true);
                        let ident = self.yytext();
                        self.yylval = Yylval::Str(ident);
                        self.set_yyllocend();
                        return Ok(Some(TokenKind::IDENT));
                    }
                }
                RuleKind::INITIAL15 => {
                    {
                        // SET_YYLLOC();
                        // return TYPECAST;

                        self.set_yylloc();
                        return Ok(Some(TokenKind::TYPECAST));
                    }
                }
                RuleKind::INITIAL16 => {
                    {
                        // SET_YYLLOC();
                        // return DOT_DOT;

                        self.set_yylloc();
                        return Ok(Some(TokenKind::DOT_DOT));
                    }
                }
                RuleKind::INITIAL17 => {
                    {
                        // SET_YYLLOC();
                        // return COLON_EQUALS;

                        self.set_yylloc();
                        return Ok(Some(TokenKind::COLON_EQUALS));
                    }
                }
                RuleKind::INITIAL18 => {
                    {
                        // SET_YYLLOC();
                        // return EQUALS_GREATER;

                        self.set_yylloc();
                        return Ok(Some(TokenKind::EQUALS_GREATER));
                    }
                }
                RuleKind::INITIAL19 => {
                    {
                        // SET_YYLLOC();
                        // return LESS_EQUALS;

                        self.set_yylloc();
                        return Ok(Some(TokenKind::LESS_EQUALS));
                    }
                }
                RuleKind::INITIAL20 => {
                    {
                        // SET_YYLLOC();
                        // return GREATER_EQUALS;

                        self.set_yylloc();
                        return Ok(Some(TokenKind::GREATER_EQUALS));
                    }
                }
                RuleKind::INITIAL21 => {
                    {
                        // /* We accept both "<>" and "!=" as meaning NOT_EQUALS */
                        // SET_YYLLOC();
                        // return NOT_EQUALS;

                        /* We accept both "<>" and "!=" as meaning NOT_EQUALS */
                        self.set_yylloc();
                        return Ok(Some(TokenKind::NOT_EQUALS));
                    }
                }
                RuleKind::INITIAL22 => {
                    {
                        // /* We accept both "<>" and "!=" as meaning NOT_EQUALS */
                        // SET_YYLLOC();
                        // return NOT_EQUALS;

                        /* We accept both "<>" and "!=" as meaning NOT_EQUALS */
                        self.set_yylloc();
                        return Ok(Some(TokenKind::NOT_EQUALS));
                    }
                }
                RuleKind::INITIAL23 => {
                    {
                        // SET_YYLLOC();
                        // return yytext[0];

                        self.set_yylloc();
                        return Ok(Some(TokenKind::RAW(self.yytext()[0..1].to_string())));
                    }
                }
                RuleKind::INITIAL24 => {
                    {
                        // /*
                        //  * Check for embedded slash-star or dash-dash; those
                        //  * are comment starts, so operator must stop there.
                        //  * Note that slash-star or dash-dash at the first
                        //  * character will match a prior rule, not this one.
                        //  */
                        // int			nchars = yyleng;
                        // char	   *slashstar = strstr(yytext, "/*");
                        // char	   *dashdash = strstr(yytext, "--");
                        //
                        // if (slashstar && dashdash)
                        // {
                        // 	/* if both appear, take the first one */
                        // 	if (slashstar > dashdash)
                        // 		slashstar = dashdash;
                        // }
                        // else if (!slashstar)
                        // 	slashstar = dashdash;
                        // if (slashstar)
                        // 	nchars = slashstar - yytext;
                        //
                        // /*
                        //  * For SQL compatibility, '+' and '-' cannot be the
                        //  * last char of a multi-char operator unless the operator
                        //  * contains chars that are not in SQL operators.
                        //  * The idea is to lex '=-' as two operators, but not
                        //  * to forbid operator names like '?-' that could not be
                        //  * sequences of SQL operators.
                        //  */
                        // if (nchars > 1 &&
                        // 	(yytext[nchars - 1] == '+' ||
                        // 	 yytext[nchars - 1] == '-'))
                        // {
                        // 	int			ic;
                        //
                        // 	for (ic = nchars - 2; ic >= 0; ic--)
                        // 	{
                        // 		char c = yytext[ic];
                        // 		if (c == '~' || c == '!' || c == '@' ||
                        // 			c == '#' || c == '^' || c == '&' ||
                        // 			c == '|' || c == '`' || c == '?' ||
                        // 			c == '%')
                        // 			break;
                        // 	}
                        // 	if (ic < 0)
                        // 	{
                        // 		/*
                        // 		 * didn't find a qualifying character, so remove
                        // 		 * all trailing [+-]
                        // 		 */
                        // 		do {
                        // 			nchars--;
                        // 		} while (nchars > 1 &&
                        // 			 (yytext[nchars - 1] == '+' ||
                        // 			  yytext[nchars - 1] == '-'));
                        // 	}
                        // }
                        //
                        // SET_YYLLOC();
                        //
                        // if (nchars < yyleng)
                        // {
                        // 	/* Strip the unwanted chars from the token */
                        // 	yyless(nchars);
                        // 	/*
                        // 	 * If what we have left is only one char, and it's
                        // 	 * one of the characters matching "self", then
                        // 	 * return it as a character token the same way
                        // 	 * that the "self" rule would have.
                        // 	 */
                        // 	if (nchars == 1 &&
                        // 		strchr(",()[].;:+-*/%^<>=", yytext[0]))
                        // 		return yytext[0];
                        // 	/*
                        // 	 * Likewise, if what we have left is two chars, and
                        // 	 * those match the tokens ">=", "<=", "=>", "<>" or
                        // 	 * "!=", then we must return the appropriate token
                        // 	 * rather than the generic Op.
                        // 	 */
                        // 	if (nchars == 2)
                        // 	{
                        // 		if (yytext[0] == '=' && yytext[1] == '>')
                        // 			return EQUALS_GREATER;
                        // 		if (yytext[0] == '>' && yytext[1] == '=')
                        // 			return GREATER_EQUALS;
                        // 		if (yytext[0] == '<' && yytext[1] == '=')
                        // 			return LESS_EQUALS;
                        // 		if (yytext[0] == '<' && yytext[1] == '>')
                        // 			return NOT_EQUALS;
                        // 		if (yytext[0] == '!' && yytext[1] == '=')
                        // 			return NOT_EQUALS;
                        // 	}
                        // }
                        //
                        // /*
                        //  * Complain if operator is too long.  Unlike the case
                        //  * for identifiers, we make this an error not a notice-
                        //  * and-truncate, because the odds are we are looking at
                        //  * a syntactic mistake anyway.
                        //  */
                        // if (nchars >= NAMEDATALEN)
                        // 	yyerror("operator too long");
                        //
                        // yylval->str = pstrdup(yytext);
                        // return Op;

                        /*
                         * Check for embedded slash-star or dash-dash; those
                         * are comment starts, so operator must stop there.
                         * Note that slash-star or dash-dash at the first
                         * character will match a prior rule, not this one.
                         */
                        let mut nchars = self.yyleng;
                        let yytext = self.yytext();
                        let mut slashstar = yytext.find("/*");
                        let dashdash = yytext.find("--");

                        let dashdash_first = match (&slashstar, &dashdash) {
                            (Some(slashstar_index), Some(dashdash_index))
                                if slashstar_index > dashdash_index =>
                            {
                                true
                            }
                            (None, Some(_)) => true,
                            _ => false,
                        };

                        if dashdash_first {
                            slashstar = dashdash;
                        }

                        if let Some(i) = slashstar {
                            nchars = i;
                        }

                        /*
                         * For SQL compatibility, '+' and '-' cannot be the
                         * last char of a multi-char operator unless the operator
                         * contains chars that are not in SQL operators.
                         * The idea is to lex '=-' as two operators, but not
                         * to forbid operator names like '?-' that could not be
                         * sequences of SQL operators.
                         */
                        if nchars > 1
                            && (get_char_by_byte_pos(&yytext, nchars - 1) == '+'
                                || get_char_by_byte_pos(&yytext, nchars - 1) == '-')
                        {
                            let b = (0..nchars - 1).any(|ic| {
                                matches!(
                                    get_char_by_byte_pos(&yytext, ic),
                                    '~' | '!' | '@' | '#' | '^' | '&' | '|' | '`' | '?' | '%'
                                )
                            });
                            if !b {
                                loop {
                                    nchars -= 1;

                                    if !(nchars > 1
                                        && (get_char_by_byte_pos(&yytext, nchars - 1) == '+'
                                            || get_char_by_byte_pos(&yytext, nchars - 1) == '-'))
                                    {
                                        break;
                                    }
                                }
                            }
                        }

                        self.set_yylloc();

                        if nchars < self.yyleng {
                            /* Strip the unwanted chars from the token */
                            self.yyless(nchars);
                            /*
                             * If what we have left is only one char, and it's
                             * one of the characters matching "self", then
                             * return it as a character token the same way
                             * that the "self" rule would have.
                             */
                            if nchars == 1
                                && ",()[].;:+-*/%^<>="
                                    .find(get_char_by_byte_pos(&yytext, 0))
                                    .is_some()
                            {
                                return Ok(Some(TokenKind::RAW(yytext[0..1].to_string())));
                            }
                            /*
                             * Likewise, if what we have left is two chars, and
                             * those match the tokens ">=", "<=", "=>", "<>" or
                             * "!=", then we must return the appropriate token
                             * rather than the generic Op.
                             */
                            if nchars == 2 {
                                if &yytext[0..2] == "=>" {
                                    return Ok(Some(TokenKind::EQUALS_GREATER));
                                }
                                if &yytext[0..2] == ">=" {
                                    return Ok(Some(TokenKind::GREATER_EQUALS));
                                }
                                if &yytext[0..2] == "<=" {
                                    return Ok(Some(TokenKind::LESS_EQUALS));
                                }
                                if &yytext[0..2] == "<>" {
                                    return Ok(Some(TokenKind::NOT_EQUALS));
                                }
                                if &yytext[0..2] == "!=" {
                                    return Ok(Some(TokenKind::NOT_EQUALS));
                                }
                            }
                        }

                        /*
                         * Complain if operator is too long.  Unlike the case
                         * for identifiers, we make this an error not a notice-
                         * and-truncate, because the odds are we are looking at
                         * a syntactic mistake anyway.
                         */
                        if nchars >= NAMEDATALEN {
                            yyerror!("operator too long");
                        }

                        self.yylval = Yylval::Str(yytext);
                        return Ok(Some(TokenKind::Op));
                    }
                }
                RuleKind::INITIAL25 => {
                    {
                        // SET_YYLLOC();
                        // yylval->ival = atol(yytext + 1);
                        // return PARAM;

                        self.set_yylloc();
                        self.yylval =
                            Yylval::I(i32::from_str_radix(&self.yytext()[1..], 10).unwrap());
                        return Ok(Some(TokenKind::PARAM));
                    }
                }
                RuleKind::INITIAL26 => {
                    {
                        // SET_YYLLOC();
                        // return process_integer_literal(yytext, yylval, 10);

                        self.set_yylloc();
                        return Ok(self.process_integer_literal(10));
                    }
                }
                RuleKind::INITIAL27 => {
                    {
                        // SET_YYLLOC();
                        // return process_integer_literal(yytext, yylval, 16);

                        self.set_yylloc();
                        return Ok(self.process_integer_literal(16));
                    }
                }
                RuleKind::INITIAL28 => {
                    {
                        // SET_YYLLOC();
                        // return process_integer_literal(yytext, yylval, 8);

                        self.set_yylloc();
                        return Ok(self.process_integer_literal(8));
                    }
                }
                RuleKind::INITIAL29 => {
                    {
                        // SET_YYLLOC();
                        // return process_integer_literal(yytext, yylval, 2);

                        self.set_yylloc();
                        return Ok(self.process_integer_literal(2));
                    }
                }
                RuleKind::INITIAL30 => {
                    {
                        // SET_YYLLOC();
                        // yyerror("invalid hexadecimal integer");

                        self.set_yylloc();
                        yyerror!("invalid hexadecimal integer");
                    }
                }
                RuleKind::INITIAL31 => {
                    {
                        // SET_YYLLOC();
                        // yyerror("invalid octal integer");

                        self.set_yylloc();
                        yyerror!("invalid octal integer");
                    }
                }
                RuleKind::INITIAL32 => {
                    {
                        // SET_YYLLOC();
                        // yyerror("invalid binary integer");

                        self.set_yylloc();
                        yyerror!("invalid binary integer");
                    }
                }
                RuleKind::INITIAL33 => {
                    {
                        // SET_YYLLOC();
                        // yylval->str = pstrdup(yytext);
                        // return FCONST;

                        self.set_yylloc();
                        self.yylval = Yylval::Str(self.yytext());
                        return Ok(Some(TokenKind::FCONST));
                    }
                }
                RuleKind::INITIAL34 => {
                    {
                        // /* throw back the .., and treat as integer */
                        // yyless(yyleng - 2);
                        // SET_YYLLOC();
                        // return process_integer_literal(yytext, yylval, 10);

                        /* throw back the .., and treat as integer */
                        self.yyless(self.yyleng - 2);
                        self.set_yylloc();
                        return Ok(self.process_integer_literal(10));
                    }
                }
                RuleKind::INITIAL35 => {
                    {
                        // SET_YYLLOC();
                        // yylval->str = pstrdup(yytext);
                        // return FCONST;

                        self.set_yylloc();
                        self.yylval = Yylval::Str(self.yytext());
                        return Ok(Some(TokenKind::FCONST));
                    }
                }
                RuleKind::INITIAL36 => {
                    {
                        // SET_YYLLOC();
                        // yyerror("trailing junk after numeric literal");

                        self.set_yylloc();
                        yyerror!("trailing junk after numeric literal");
                    }
                }
                RuleKind::INITIAL37 => {
                    {
                        // SET_YYLLOC();
                        // yyerror("trailing junk after numeric literal");

                        self.set_yylloc();
                        yyerror!("trailing junk after numeric literal");
                    }
                }
                RuleKind::INITIAL38 => {
                    {
                        // SET_YYLLOC();
                        // yyerror("trailing junk after numeric literal");

                        self.set_yylloc();
                        yyerror!("trailing junk after numeric literal");
                    }
                }
                RuleKind::INITIAL39 => {
                    {
                        // SET_YYLLOC();
                        // yyerror("trailing junk after numeric literal");

                        self.set_yylloc();
                        yyerror!("trailing junk after numeric literal");
                    }
                }
                RuleKind::INITIAL40 => {
                    {
                        // int			kwnum;
                        // char	   *ident;
                        //
                        // SET_YYLLOC();
                        //
                        // /* Is it a keyword? */
                        // kwnum = ScanKeywordLookup(yytext,
                        // 						  yyextra->keywordlist);
                        // if (kwnum >= 0)
                        // {
                        // 	yylval->keyword = GetScanKeyword(kwnum,
                        // 									 yyextra->keywordlist);
                        // 	return yyextra->keyword_tokens[kwnum];
                        // }
                        //
                        // /*
                        //  * No.  Convert the identifier to lower case, and truncate
                        //  * if necessary.
                        //  */
                        // ident = downcase_truncate_identifier(yytext, yyleng, true);
                        // yylval->str = ident;
                        // yyextra->yyllocend = yytext - yyextra->scanbuf + yyleng;
                        // return IDENT;

                        self.set_yylloc();

                        /* Is it a keyword? */
                        let yytext = self.yytext();
                        if let Some((kw, kw_token)) = self.get_keyword(&yytext) {
                            self.yylval = Yylval::Keyword(kw);
                            return Ok(Some(TokenKind::KEYWORD(kw_token)));
                        }

                        /*
                         * No.  Convert the identifier to lower case, and truncate
                         * if necessary.
                         */
                        let ident = self.downcase_truncate_identifier(self.yyleng, true);
                        self.yylval = Yylval::Str(ident);
                        self.set_yyllocend();
                        return Ok(Some(TokenKind::IDENT));
                    }
                }
                RuleKind::INITIAL41 => {
                    {
                        // SET_YYLLOC();
                        // return yytext[0];

                        self.set_yylloc();
                        return Ok(Some(TokenKind::RAW(yytext[0..1].to_string())));
                    }
                }
                RuleKind::INITIAL42 => {
                    {
                        // SET_YYLLOC();
                        // yyterminate();

                        self.set_yylloc();
                        yyterminate!();
                    }
                }
            }
            self.advance();
        }
    }
}

pub fn get_rules() -> Vec<Rule> {
    vec![
                // {whitespace}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((([ \t\n\r\f\v])+)))"#).unwrap(),
                    kind: RuleKind::INITIAL1,
                    eof: false,
                },

                // {comment}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((--([^\n\r])*)))"#).unwrap(),
                    kind: RuleKind::INITIAL2,
                    eof: false,
                },

                // {xcstart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((\/\*([\~\!\@\#\^\&\|\`\?\+\-\*\/\%<>\=])*))"#).unwrap(),
                    kind: RuleKind::INITIAL3,
                    eof: false,
                },

                // {xcstart}
                Rule {
                    state: State::xc,
                    pattern: Regex::new(r#"(?-u)^((\/\*([\~\!\@\#\^\&\|\`\?\+\-\*\/\%<>\=])*))"#).unwrap(),
                    kind: RuleKind::xc1,
                    eof: false,
                },

                // {xcstop}
                Rule {
                    state: State::xc,
                    pattern: Regex::new(r#"(?-u)^((\*+\/))"#).unwrap(),
                    kind: RuleKind::xc2,
                    eof: false,
                },

                // {xcinside}
                Rule {
                    state: State::xc,
                    pattern: Regex::new(r#"(?-u)^(([^*/]+))"#).unwrap(),
                    kind: RuleKind::xc3,
                    eof: false,
                },

                // {op_chars}
                Rule {
                    state: State::xc,
                    pattern: Regex::new(r#"(?-u)^(([\~\!\@\#\^\&\|\`\?\+\-\*\/\%<>\=]))"#).unwrap(),
                    kind: RuleKind::xc4,
                    eof: false,
                },

                // \*+
                Rule {
                    state: State::xc,
                    pattern: Regex::new(r#"(?-u)^(\*+)"#).unwrap(),
                    kind: RuleKind::xc5,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xc,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xc6,
                    eof: true,
                },

                // {xbstart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([bB](')))"#).unwrap(),
                    kind: RuleKind::INITIAL4,
                    eof: false,
                },

                // {xhinside}
                Rule {
                    state: State::xh,
                    pattern: Regex::new(r#"(?-u)^(([^']*))"#).unwrap(),
                    kind: RuleKind::xh1,
                    eof: false,
                },

                // {xbinside}
                Rule {
                    state: State::xb,
                    pattern: Regex::new(r#"(?-u)^(([^']*))"#).unwrap(),
                    kind: RuleKind::xb1,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xb,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xb2,
                    eof: true,
                },

                // {xhstart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([xX](')))"#).unwrap(),
                    kind: RuleKind::INITIAL5,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xh,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xh2,
                    eof: true,
                },

                // {xnstart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([nN](')))"#).unwrap(),
                    kind: RuleKind::INITIAL6,
                    eof: false,
                },

                // {xqstart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((')))"#).unwrap(),
                    kind: RuleKind::INITIAL7,
                    eof: false,
                },

                // {xestart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([eE](')))"#).unwrap(),
                    kind: RuleKind::INITIAL8,
                    eof: false,
                },

                // {xusstart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([uU]&(')))"#).unwrap(),
                    kind: RuleKind::INITIAL9,
                    eof: false,
                },

                // {quote}
                Rule {
                    state: State::xb,
                    pattern: Regex::new(r#"(?-u)^(('))"#).unwrap(),
                    kind: RuleKind::xb3,
                    eof: false,
                },

                // {quote}
                Rule {
                    state: State::xh,
                    pattern: Regex::new(r#"(?-u)^(('))"#).unwrap(),
                    kind: RuleKind::xh3,
                    eof: false,
                },

                // {quote}
                Rule {
                    state: State::xq,
                    pattern: Regex::new(r#"(?-u)^(('))"#).unwrap(),
                    kind: RuleKind::xq1,
                    eof: false,
                },

                // {quote}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(('))"#).unwrap(),
                    kind: RuleKind::xe1,
                    eof: false,
                },

                // {quote}
                Rule {
                    state: State::xus,
                    pattern: Regex::new(r#"(?-u)^(('))"#).unwrap(),
                    kind: RuleKind::xus1,
                    eof: false,
                },

                // {quotecontinue}
                Rule {
                    state: State::xqs,
                    pattern: Regex::new(r#"(?-u)^((((((([ \t\f\v])))*([\n\r])((([ \t\n\r\f\v])+))*))(')))"#).unwrap(),
                    kind: RuleKind::xqs1,
                    eof: false,
                },

                // {quotecontinuefail}
                Rule {
                    state: State::xqs,
                    pattern: Regex::new(r#"(?-u)^((((([ \t\n\r\f\v])+))*-?))"#).unwrap(),
                    kind: RuleKind::xqs2,
                    eof: false,
                },

                // {other}
                Rule {
                    state: State::xqs,
                    pattern: Regex::new(r#"(?-u)^((.))"#).unwrap(),
                    kind: RuleKind::xqs3,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xqs,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xqs4,
                    eof: true,
                },

                // {xqdouble}
                Rule {
                    state: State::xq,
                    pattern: Regex::new(r#"(?-u)^(((')(')))"#).unwrap(),
                    kind: RuleKind::xq2,
                    eof: false,
                },

                // {xqdouble}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(((')(')))"#).unwrap(),
                    kind: RuleKind::xe2,
                    eof: false,
                },

                // {xqdouble}
                Rule {
                    state: State::xus,
                    pattern: Regex::new(r#"(?-u)^(((')(')))"#).unwrap(),
                    kind: RuleKind::xus2,
                    eof: false,
                },

                // {xqinside}
                Rule {
                    state: State::xq,
                    pattern: Regex::new(r#"(?-u)^(([^']+))"#).unwrap(),
                    kind: RuleKind::xq3,
                    eof: false,
                },

                // {xqinside}
                Rule {
                    state: State::xus,
                    pattern: Regex::new(r#"(?-u)^(([^']+))"#).unwrap(),
                    kind: RuleKind::xus3,
                    eof: false,
                },

                // {xeinside}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(([^\\']+))"#).unwrap(),
                    kind: RuleKind::xe3,
                    eof: false,
                },

                // {xeunicode}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(([\\](u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})))"#).unwrap(),
                    kind: RuleKind::xe4,
                    eof: false,
                },

                // {xeunicode}
                Rule {
                    state: State::xeu,
                    pattern: Regex::new(r#"(?-u)^(([\\](u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})))"#).unwrap(),
                    kind: RuleKind::xeu1,
                    eof: false,
                },

                // .
                Rule {
                    state: State::xeu,
                    pattern: Regex::new(r#"(?-u)^(.)"#).unwrap(),
                    kind: RuleKind::xeu2,
                    eof: false,
                },

                // \n
                Rule {
                    state: State::xeu,
                    pattern: Regex::new(r#"(?-u)^(\n)"#).unwrap(),
                    kind: RuleKind::xeu3,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xeu,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xeu4,
                    eof: true,
                },

                // {xeunicodefail}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(([\\](u[0-9A-Fa-f]{0,3}|U[0-9A-Fa-f]{0,7})))"#).unwrap(),
                    kind: RuleKind::xe5,
                    eof: false,
                },

                // {xeunicodefail}
                Rule {
                    state: State::xeu,
                    pattern: Regex::new(r#"(?-u)^(([\\](u[0-9A-Fa-f]{0,3}|U[0-9A-Fa-f]{0,7})))"#).unwrap(),
                    kind: RuleKind::xeu5,
                    eof: false,
                },

                // {xeescape}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(([\\][^0-7]))"#).unwrap(),
                    kind: RuleKind::xe6,
                    eof: false,
                },

                // {xeoctesc}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(([\\][0-7]{1,3}))"#).unwrap(),
                    kind: RuleKind::xe7,
                    eof: false,
                },

                // {xehexesc}
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(([\\]x[0-9A-Fa-f]{1,2}))"#).unwrap(),
                    kind: RuleKind::xe8,
                    eof: false,
                },

                // .
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(.)"#).unwrap(),
                    kind: RuleKind::xe9,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xq,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xq4,
                    eof: true,
                },

                // <<EOF>>
                Rule {
                    state: State::xe,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xe10,
                    eof: true,
                },

                // <<EOF>>
                Rule {
                    state: State::xus,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xus4,
                    eof: true,
                },

                // {dolqdelim}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((\$(([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9])*)?\$))"#).unwrap(),
                    kind: RuleKind::INITIAL10,
                    eof: false,
                },

                // {dolqfailed}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((\$([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9])*))"#).unwrap(),
                    kind: RuleKind::INITIAL11,
                    eof: false,
                },

                // {dolqdelim}
                Rule {
                    state: State::xdolq,
                    pattern: Regex::new(r#"(?-u)^((\$(([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9])*)?\$))"#).unwrap(),
                    kind: RuleKind::xdolq1,
                    eof: false,
                },

                // {dolqinside}
                Rule {
                    state: State::xdolq,
                    pattern: Regex::new(r#"(?-u)^(([^$]+))"#).unwrap(),
                    kind: RuleKind::xdolq2,
                    eof: false,
                },

                // {dolqfailed}
                Rule {
                    state: State::xdolq,
                    pattern: Regex::new(r#"(?-u)^((\$([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9])*))"#).unwrap(),
                    kind: RuleKind::xdolq3,
                    eof: false,
                },

                // .
                Rule {
                    state: State::xdolq,
                    pattern: Regex::new(r#"(?-u)^(.)"#).unwrap(),
                    kind: RuleKind::xdolq4,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xdolq,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xdolq5,
                    eof: true,
                },

                // {xdstart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((\")))"#).unwrap(),
                    kind: RuleKind::INITIAL12,
                    eof: false,
                },

                // {xuistart}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([uU]&(\")))"#).unwrap(),
                    kind: RuleKind::INITIAL13,
                    eof: false,
                },

                // {xdstop}
                Rule {
                    state: State::xd,
                    pattern: Regex::new(r#"(?-u)^(((\")))"#).unwrap(),
                    kind: RuleKind::xd1,
                    eof: false,
                },

                // {dquote}
                Rule {
                    state: State::xui,
                    pattern: Regex::new(r#"(?-u)^((\"))"#).unwrap(),
                    kind: RuleKind::xui1,
                    eof: false,
                },

                // {xddouble}
                Rule {
                    state: State::xd,
                    pattern: Regex::new(r#"(?-u)^(((\")(\")))"#).unwrap(),
                    kind: RuleKind::xd2,
                    eof: false,
                },

                // {xddouble}
                Rule {
                    state: State::xui,
                    pattern: Regex::new(r#"(?-u)^(((\")(\")))"#).unwrap(),
                    kind: RuleKind::xui2,
                    eof: false,
                },

                // {xdinside}
                Rule {
                    state: State::xd,
                    pattern: Regex::new(r#"(?-u)^(([^"]+))"#).unwrap(),
                    kind: RuleKind::xd3,
                    eof: false,
                },

                // {xdinside}
                Rule {
                    state: State::xui,
                    pattern: Regex::new(r#"(?-u)^(([^"]+))"#).unwrap(),
                    kind: RuleKind::xui3,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::xd,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xd4,
                    eof: true,
                },

                // <<EOF>>
                Rule {
                    state: State::xui,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::xui4,
                    eof: true,
                },

                // {xufailed}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([uU]&))"#).unwrap(),
                    kind: RuleKind::INITIAL14,
                    eof: false,
                },

                // {typecast}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((::))"#).unwrap(),
                    kind: RuleKind::INITIAL15,
                    eof: false,
                },

                // {dot_dot}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((\.\.))"#).unwrap(),
                    kind: RuleKind::INITIAL16,
                    eof: false,
                },

                // {colon_equals}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((:=))"#).unwrap(),
                    kind: RuleKind::INITIAL17,
                    eof: false,
                },

                // {equals_greater}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((=>))"#).unwrap(),
                    kind: RuleKind::INITIAL18,
                    eof: false,
                },

                // {less_equals}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((<=))"#).unwrap(),
                    kind: RuleKind::INITIAL19,
                    eof: false,
                },

                // {greater_equals}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((>=))"#).unwrap(),
                    kind: RuleKind::INITIAL20,
                    eof: false,
                },

                // {less_greater}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((<>))"#).unwrap(),
                    kind: RuleKind::INITIAL21,
                    eof: false,
                },

                // {not_equals}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((!=))"#).unwrap(),
                    kind: RuleKind::INITIAL22,
                    eof: false,
                },

                // {self}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(([,()\[\].;\:\+\-\*\/\%\^<>\=]))"#).unwrap(),
                    kind: RuleKind::INITIAL23,
                    eof: false,
                },

                // {operator}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((([\~\!\@\#\^\&\|\`\?\+\-\*\/\%<>\=])+))"#).unwrap(),
                    kind: RuleKind::INITIAL24,
                    eof: false,
                },

                // {param}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((\$([0-9])+))"#).unwrap(),
                    kind: RuleKind::INITIAL25,
                    eof: false,
                },

                // {decinteger}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((([0-9])(_?([0-9]))*))"#).unwrap(),
                    kind: RuleKind::INITIAL26,
                    eof: false,
                },

                // {hexinteger}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((0[xX](_?([0-9A-Fa-f]))+))"#).unwrap(),
                    kind: RuleKind::INITIAL27,
                    eof: false,
                },

                // {octinteger}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((0[oO](_?([0-7]))+))"#).unwrap(),
                    kind: RuleKind::INITIAL28,
                    eof: false,
                },

                // {bininteger}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((0[bB](_?([0-1]))+))"#).unwrap(),
                    kind: RuleKind::INITIAL29,
                    eof: false,
                },

                // {hexfail}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((0[xX]_?))"#).unwrap(),
                    kind: RuleKind::INITIAL30,
                    eof: false,
                },

                // {octfail}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((0[oO]_?))"#).unwrap(),
                    kind: RuleKind::INITIAL31,
                    eof: false,
                },

                // {binfail}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((0[bB]_?))"#).unwrap(),
                    kind: RuleKind::INITIAL32,
                    eof: false,
                },

                // {numeric}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((((([0-9])(_?([0-9]))*)\.(([0-9])(_?([0-9]))*)?)|(\.(([0-9])(_?([0-9]))*)))))"#).unwrap(),
                    kind: RuleKind::INITIAL33,
                    eof: false,
                },

                // {numericfail}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((([0-9])(_?([0-9]))*)\.\.))"#).unwrap(),
                    kind: RuleKind::INITIAL34,
                    eof: false,
                },

                // {real}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((((([0-9])(_?([0-9]))*)|((((([0-9])(_?([0-9]))*)\.(([0-9])(_?([0-9]))*)?)|(\.(([0-9])(_?([0-9]))*)))))[Ee][-+]?(([0-9])(_?([0-9]))*)))"#).unwrap(),
                    kind: RuleKind::INITIAL35,
                    eof: false,
                },

                // {realfail}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((((([0-9])(_?([0-9]))*)|((((([0-9])(_?([0-9]))*)\.(([0-9])(_?([0-9]))*)?)|(\.(([0-9])(_?([0-9]))*)))))[Ee][-+]))"#).unwrap(),
                    kind: RuleKind::INITIAL36,
                    eof: false,
                },

                // {integer_junk}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((([0-9])(_?([0-9]))*)(([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9\$])*)))"#).unwrap(),
                    kind: RuleKind::INITIAL37,
                    eof: false,
                },

                // {numeric_junk}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((((((([0-9])(_?([0-9]))*)\.(([0-9])(_?([0-9]))*)?)|(\.(([0-9])(_?([0-9]))*))))(([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9\$])*)))"#).unwrap(),
                    kind: RuleKind::INITIAL38,
                    eof: false,
                },

                // {real_junk}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(((((([0-9])(_?([0-9]))*)|((((([0-9])(_?([0-9]))*)\.(([0-9])(_?([0-9]))*)?)|(\.(([0-9])(_?([0-9]))*)))))[Ee][-+]?(([0-9])(_?([0-9]))*))(([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9\$])*)))"#).unwrap(),
                    kind: RuleKind::INITIAL39,
                    eof: false,
                },

                // {identifier}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((([A-Za-z\x80-\xFF_])([A-Za-z\x80-\xFF_0-9\$])*))"#).unwrap(),
                    kind: RuleKind::INITIAL40,
                    eof: false,
                },

                // {other}
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^((.))"#).unwrap(),
                    kind: RuleKind::INITIAL41,
                    eof: false,
                },

                // <<EOF>>
                Rule {
                    state: State::INITIAL,
                    pattern: Regex::new(r#"(?-u)^(^$)"#).unwrap(),
                    kind: RuleKind::INITIAL42,
                    eof: true,
                }]
}

pub fn get_keyword_map() -> HashMap<&'static str, &'static str> {
    let mut map = HashMap::new();
    for (kw, tok) in [
        ("abort", "ABORT_P"),
        ("absent", "ABSENT"),
        ("absolute", "ABSOLUTE_P"),
        ("access", "ACCESS"),
        ("action", "ACTION"),
        ("add", "ADD_P"),
        ("admin", "ADMIN"),
        ("after", "AFTER"),
        ("aggregate", "AGGREGATE"),
        ("all", "ALL"),
        ("also", "ALSO"),
        ("alter", "ALTER"),
        ("always", "ALWAYS"),
        ("analyse", "ANALYSE"),
        ("analyze", "ANALYZE"),
        ("and", "AND"),
        ("any", "ANY"),
        ("array", "ARRAY"),
        ("as", "AS"),
        ("asc", "ASC"),
        ("asensitive", "ASENSITIVE"),
        ("assertion", "ASSERTION"),
        ("assignment", "ASSIGNMENT"),
        ("asymmetric", "ASYMMETRIC"),
        ("at", "AT"),
        ("atomic", "ATOMIC"),
        ("attach", "ATTACH"),
        ("attribute", "ATTRIBUTE"),
        ("authorization", "AUTHORIZATION"),
        ("backward", "BACKWARD"),
        ("before", "BEFORE"),
        ("begin", "BEGIN_P"),
        ("between", "BETWEEN"),
        ("bigint", "BIGINT"),
        ("binary", "BINARY"),
        ("bit", "BIT"),
        ("boolean", "BOOLEAN_P"),
        ("both", "BOTH"),
        ("breadth", "BREADTH"),
        ("by", "BY"),
        ("cache", "CACHE"),
        ("call", "CALL"),
        ("called", "CALLED"),
        ("cascade", "CASCADE"),
        ("cascaded", "CASCADED"),
        ("case", "CASE"),
        ("cast", "CAST"),
        ("catalog", "CATALOG_P"),
        ("chain", "CHAIN"),
        ("char", "CHAR_P"),
        ("character", "CHARACTER"),
        ("characteristics", "CHARACTERISTICS"),
        ("check", "CHECK"),
        ("checkpoint", "CHECKPOINT"),
        ("class", "CLASS"),
        ("close", "CLOSE"),
        ("cluster", "CLUSTER"),
        ("coalesce", "COALESCE"),
        ("collate", "COLLATE"),
        ("collation", "COLLATION"),
        ("column", "COLUMN"),
        ("columns", "COLUMNS"),
        ("comment", "COMMENT"),
        ("comments", "COMMENTS"),
        ("commit", "COMMIT"),
        ("committed", "COMMITTED"),
        ("compression", "COMPRESSION"),
        ("concurrently", "CONCURRENTLY"),
        ("conditional", "CONDITIONAL"),
        ("configuration", "CONFIGURATION"),
        ("conflict", "CONFLICT"),
        ("connection", "CONNECTION"),
        ("constraint", "CONSTRAINT"),
        ("constraints", "CONSTRAINTS"),
        ("content", "CONTENT_P"),
        ("continue", "CONTINUE_P"),
        ("conversion", "CONVERSION_P"),
        ("copy", "COPY"),
        ("cost", "COST"),
        ("create", "CREATE"),
        ("cross", "CROSS"),
        ("csv", "CSV"),
        ("cube", "CUBE"),
        ("current", "CURRENT_P"),
        ("current_catalog", "CURRENT_CATALOG"),
        ("current_date", "CURRENT_DATE"),
        ("current_role", "CURRENT_ROLE"),
        ("current_schema", "CURRENT_SCHEMA"),
        ("current_time", "CURRENT_TIME"),
        ("current_timestamp", "CURRENT_TIMESTAMP"),
        ("current_user", "CURRENT_USER"),
        ("cursor", "CURSOR"),
        ("cycle", "CYCLE"),
        ("data", "DATA_P"),
        ("database", "DATABASE"),
        ("day", "DAY_P"),
        ("deallocate", "DEALLOCATE"),
        ("dec", "DEC"),
        ("decimal", "DECIMAL_P"),
        ("declare", "DECLARE"),
        ("default", "DEFAULT"),
        ("defaults", "DEFAULTS"),
        ("deferrable", "DEFERRABLE"),
        ("deferred", "DEFERRED"),
        ("definer", "DEFINER"),
        ("delete", "DELETE_P"),
        ("delimiter", "DELIMITER"),
        ("delimiters", "DELIMITERS"),
        ("depends", "DEPENDS"),
        ("depth", "DEPTH"),
        ("desc", "DESC"),
        ("detach", "DETACH"),
        ("dictionary", "DICTIONARY"),
        ("disable", "DISABLE_P"),
        ("discard", "DISCARD"),
        ("distinct", "DISTINCT"),
        ("do", "DO"),
        ("document", "DOCUMENT_P"),
        ("domain", "DOMAIN_P"),
        ("double", "DOUBLE_P"),
        ("drop", "DROP"),
        ("each", "EACH"),
        ("else", "ELSE"),
        ("empty", "EMPTY_P"),
        ("enable", "ENABLE_P"),
        ("encoding", "ENCODING"),
        ("encrypted", "ENCRYPTED"),
        ("end", "END_P"),
        ("enum", "ENUM_P"),
        ("error", "ERROR_P"),
        ("escape", "ESCAPE"),
        ("event", "EVENT"),
        ("except", "EXCEPT"),
        ("exclude", "EXCLUDE"),
        ("excluding", "EXCLUDING"),
        ("exclusive", "EXCLUSIVE"),
        ("execute", "EXECUTE"),
        ("exists", "EXISTS"),
        ("explain", "EXPLAIN"),
        ("expression", "EXPRESSION"),
        ("extension", "EXTENSION"),
        ("external", "EXTERNAL"),
        ("extract", "EXTRACT"),
        ("false", "FALSE_P"),
        ("family", "FAMILY"),
        ("fetch", "FETCH"),
        ("filter", "FILTER"),
        ("finalize", "FINALIZE"),
        ("first", "FIRST_P"),
        ("float", "FLOAT_P"),
        ("following", "FOLLOWING"),
        ("for", "FOR"),
        ("force", "FORCE"),
        ("foreign", "FOREIGN"),
        ("format", "FORMAT"),
        ("forward", "FORWARD"),
        ("freeze", "FREEZE"),
        ("from", "FROM"),
        ("full", "FULL"),
        ("function", "FUNCTION"),
        ("functions", "FUNCTIONS"),
        ("generated", "GENERATED"),
        ("global", "GLOBAL"),
        ("grant", "GRANT"),
        ("granted", "GRANTED"),
        ("greatest", "GREATEST"),
        ("group", "GROUP_P"),
        ("grouping", "GROUPING"),
        ("groups", "GROUPS"),
        ("handler", "HANDLER"),
        ("having", "HAVING"),
        ("header", "HEADER_P"),
        ("hold", "HOLD"),
        ("hour", "HOUR_P"),
        ("identity", "IDENTITY_P"),
        ("if", "IF_P"),
        ("ilike", "ILIKE"),
        ("immediate", "IMMEDIATE"),
        ("immutable", "IMMUTABLE"),
        ("implicit", "IMPLICIT_P"),
        ("import", "IMPORT_P"),
        ("in", "IN_P"),
        ("include", "INCLUDE"),
        ("including", "INCLUDING"),
        ("increment", "INCREMENT"),
        ("indent", "INDENT"),
        ("index", "INDEX"),
        ("indexes", "INDEXES"),
        ("inherit", "INHERIT"),
        ("inherits", "INHERITS"),
        ("initially", "INITIALLY"),
        ("inline", "INLINE_P"),
        ("inner", "INNER_P"),
        ("inout", "INOUT"),
        ("input", "INPUT_P"),
        ("insensitive", "INSENSITIVE"),
        ("insert", "INSERT"),
        ("instead", "INSTEAD"),
        ("int", "INT_P"),
        ("integer", "INTEGER"),
        ("intersect", "INTERSECT"),
        ("interval", "INTERVAL"),
        ("into", "INTO"),
        ("invoker", "INVOKER"),
        ("is", "IS"),
        ("isnull", "ISNULL"),
        ("isolation", "ISOLATION"),
        ("join", "JOIN"),
        ("json", "JSON"),
        ("json_array", "JSON_ARRAY"),
        ("json_arrayagg", "JSON_ARRAYAGG"),
        ("json_exists", "JSON_EXISTS"),
        ("json_object", "JSON_OBJECT"),
        ("json_objectagg", "JSON_OBJECTAGG"),
        ("json_query", "JSON_QUERY"),
        ("json_scalar", "JSON_SCALAR"),
        ("json_serialize", "JSON_SERIALIZE"),
        ("json_table", "JSON_TABLE"),
        ("json_value", "JSON_VALUE"),
        ("keep", "KEEP"),
        ("key", "KEY"),
        ("keys", "KEYS"),
        ("label", "LABEL"),
        ("language", "LANGUAGE"),
        ("large", "LARGE_P"),
        ("last", "LAST_P"),
        ("lateral", "LATERAL_P"),
        ("leading", "LEADING"),
        ("leakproof", "LEAKPROOF"),
        ("least", "LEAST"),
        ("left", "LEFT"),
        ("level", "LEVEL"),
        ("like", "LIKE"),
        ("limit", "LIMIT"),
        ("listen", "LISTEN"),
        ("load", "LOAD"),
        ("local", "LOCAL"),
        ("localtime", "LOCALTIME"),
        ("localtimestamp", "LOCALTIMESTAMP"),
        ("location", "LOCATION"),
        ("lock", "LOCK_P"),
        ("locked", "LOCKED"),
        ("logged", "LOGGED"),
        ("mapping", "MAPPING"),
        ("match", "MATCH"),
        ("matched", "MATCHED"),
        ("materialized", "MATERIALIZED"),
        ("maxvalue", "MAXVALUE"),
        ("merge", "MERGE"),
        ("merge_action", "MERGE_ACTION"),
        ("method", "METHOD"),
        ("minute", "MINUTE_P"),
        ("minvalue", "MINVALUE"),
        ("mode", "MODE"),
        ("month", "MONTH_P"),
        ("move", "MOVE"),
        ("name", "NAME_P"),
        ("names", "NAMES"),
        ("national", "NATIONAL"),
        ("natural", "NATURAL"),
        ("nchar", "NCHAR"),
        ("nested", "NESTED"),
        ("new", "NEW"),
        ("next", "NEXT"),
        ("nfc", "NFC"),
        ("nfd", "NFD"),
        ("nfkc", "NFKC"),
        ("nfkd", "NFKD"),
        ("no", "NO"),
        ("none", "NONE"),
        ("normalize", "NORMALIZE"),
        ("normalized", "NORMALIZED"),
        ("not", "NOT"),
        ("nothing", "NOTHING"),
        ("notify", "NOTIFY"),
        ("notnull", "NOTNULL"),
        ("nowait", "NOWAIT"),
        ("null", "NULL_P"),
        ("nullif", "NULLIF"),
        ("nulls", "NULLS_P"),
        ("numeric", "NUMERIC"),
        ("object", "OBJECT_P"),
        ("of", "OF"),
        ("off", "OFF"),
        ("offset", "OFFSET"),
        ("oids", "OIDS"),
        ("old", "OLD"),
        ("omit", "OMIT"),
        ("on", "ON"),
        ("only", "ONLY"),
        ("operator", "OPERATOR"),
        ("option", "OPTION"),
        ("options", "OPTIONS"),
        ("or", "OR"),
        ("order", "ORDER"),
        ("ordinality", "ORDINALITY"),
        ("others", "OTHERS"),
        ("out", "OUT_P"),
        ("outer", "OUTER_P"),
        ("over", "OVER"),
        ("overlaps", "OVERLAPS"),
        ("overlay", "OVERLAY"),
        ("overriding", "OVERRIDING"),
        ("owned", "OWNED"),
        ("owner", "OWNER"),
        ("parallel", "PARALLEL"),
        ("parameter", "PARAMETER"),
        ("parser", "PARSER"),
        ("partial", "PARTIAL"),
        ("partition", "PARTITION"),
        ("passing", "PASSING"),
        ("password", "PASSWORD"),
        ("path", "PATH"),
        ("placing", "PLACING"),
        ("plan", "PLAN"),
        ("plans", "PLANS"),
        ("policy", "POLICY"),
        ("position", "POSITION"),
        ("preceding", "PRECEDING"),
        ("precision", "PRECISION"),
        ("prepare", "PREPARE"),
        ("prepared", "PREPARED"),
        ("preserve", "PRESERVE"),
        ("primary", "PRIMARY"),
        ("prior", "PRIOR"),
        ("privileges", "PRIVILEGES"),
        ("procedural", "PROCEDURAL"),
        ("procedure", "PROCEDURE"),
        ("procedures", "PROCEDURES"),
        ("program", "PROGRAM"),
        ("publication", "PUBLICATION"),
        ("quote", "QUOTE"),
        ("quotes", "QUOTES"),
        ("range", "RANGE"),
        ("read", "READ"),
        ("real", "REAL"),
        ("reassign", "REASSIGN"),
        ("recheck", "RECHECK"),
        ("recursive", "RECURSIVE"),
        ("ref", "REF_P"),
        ("references", "REFERENCES"),
        ("referencing", "REFERENCING"),
        ("refresh", "REFRESH"),
        ("reindex", "REINDEX"),
        ("relative", "RELATIVE_P"),
        ("release", "RELEASE"),
        ("rename", "RENAME"),
        ("repeatable", "REPEATABLE"),
        ("replace", "REPLACE"),
        ("replica", "REPLICA"),
        ("reset", "RESET"),
        ("restart", "RESTART"),
        ("restrict", "RESTRICT"),
        ("return", "RETURN"),
        ("returning", "RETURNING"),
        ("returns", "RETURNS"),
        ("revoke", "REVOKE"),
        ("right", "RIGHT"),
        ("role", "ROLE"),
        ("rollback", "ROLLBACK"),
        ("rollup", "ROLLUP"),
        ("routine", "ROUTINE"),
        ("routines", "ROUTINES"),
        ("row", "ROW"),
        ("rows", "ROWS"),
        ("rule", "RULE"),
        ("savepoint", "SAVEPOINT"),
        ("scalar", "SCALAR"),
        ("schema", "SCHEMA"),
        ("schemas", "SCHEMAS"),
        ("scroll", "SCROLL"),
        ("search", "SEARCH"),
        ("second", "SECOND_P"),
        ("security", "SECURITY"),
        ("select", "SELECT"),
        ("sequence", "SEQUENCE"),
        ("sequences", "SEQUENCES"),
        ("serializable", "SERIALIZABLE"),
        ("server", "SERVER"),
        ("session", "SESSION"),
        ("session_user", "SESSION_USER"),
        ("set", "SET"),
        ("setof", "SETOF"),
        ("sets", "SETS"),
        ("share", "SHARE"),
        ("show", "SHOW"),
        ("similar", "SIMILAR"),
        ("simple", "SIMPLE"),
        ("skip", "SKIP"),
        ("smallint", "SMALLINT"),
        ("snapshot", "SNAPSHOT"),
        ("some", "SOME"),
        ("source", "SOURCE"),
        ("sql", "SQL_P"),
        ("stable", "STABLE"),
        ("standalone", "STANDALONE_P"),
        ("start", "START"),
        ("statement", "STATEMENT"),
        ("statistics", "STATISTICS"),
        ("stdin", "STDIN"),
        ("stdout", "STDOUT"),
        ("storage", "STORAGE"),
        ("stored", "STORED"),
        ("strict", "STRICT_P"),
        ("string", "STRING_P"),
        ("strip", "STRIP_P"),
        ("subscription", "SUBSCRIPTION"),
        ("substring", "SUBSTRING"),
        ("support", "SUPPORT"),
        ("symmetric", "SYMMETRIC"),
        ("sysid", "SYSID"),
        ("system", "SYSTEM_P"),
        ("system_user", "SYSTEM_USER"),
        ("table", "TABLE"),
        ("tables", "TABLES"),
        ("tablesample", "TABLESAMPLE"),
        ("tablespace", "TABLESPACE"),
        ("target", "TARGET"),
        ("temp", "TEMP"),
        ("template", "TEMPLATE"),
        ("temporary", "TEMPORARY"),
        ("text", "TEXT_P"),
        ("then", "THEN"),
        ("ties", "TIES"),
        ("time", "TIME"),
        ("timestamp", "TIMESTAMP"),
        ("to", "TO"),
        ("trailing", "TRAILING"),
        ("transaction", "TRANSACTION"),
        ("transform", "TRANSFORM"),
        ("treat", "TREAT"),
        ("trigger", "TRIGGER"),
        ("trim", "TRIM"),
        ("true", "TRUE_P"),
        ("truncate", "TRUNCATE"),
        ("trusted", "TRUSTED"),
        ("type", "TYPE_P"),
        ("types", "TYPES_P"),
        ("uescape", "UESCAPE"),
        ("unbounded", "UNBOUNDED"),
        ("uncommitted", "UNCOMMITTED"),
        ("unconditional", "UNCONDITIONAL"),
        ("unencrypted", "UNENCRYPTED"),
        ("union", "UNION"),
        ("unique", "UNIQUE"),
        ("unknown", "UNKNOWN"),
        ("unlisten", "UNLISTEN"),
        ("unlogged", "UNLOGGED"),
        ("until", "UNTIL"),
        ("update", "UPDATE"),
        ("user", "USER"),
        ("using", "USING"),
        ("vacuum", "VACUUM"),
        ("valid", "VALID"),
        ("validate", "VALIDATE"),
        ("validator", "VALIDATOR"),
        ("value", "VALUE_P"),
        ("values", "VALUES"),
        ("varchar", "VARCHAR"),
        ("variadic", "VARIADIC"),
        ("varying", "VARYING"),
        ("verbose", "VERBOSE"),
        ("version", "VERSION_P"),
        ("view", "VIEW"),
        ("views", "VIEWS"),
        ("volatile", "VOLATILE"),
        ("when", "WHEN"),
        ("where", "WHERE"),
        ("whitespace", "WHITESPACE_P"),
        ("window", "WINDOW"),
        ("with", "WITH"),
        ("within", "WITHIN"),
        ("without", "WITHOUT"),
        ("work", "WORK"),
        ("wrapper", "WRAPPER"),
        ("write", "WRITE"),
        ("xml", "XML_P"),
        ("xmlattributes", "XMLATTRIBUTES"),
        ("xmlconcat", "XMLCONCAT"),
        ("xmlelement", "XMLELEMENT"),
        ("xmlexists", "XMLEXISTS"),
        ("xmlforest", "XMLFOREST"),
        ("xmlnamespaces", "XMLNAMESPACES"),
        ("xmlparse", "XMLPARSE"),
        ("xmlpi", "XMLPI"),
        ("xmlroot", "XMLROOT"),
        ("xmlserialize", "XMLSERIALIZE"),
        ("xmltable", "XMLTABLE"),
        ("year", "YEAR_P"),
        ("yes", "YES_P"),
        ("zone", "ZONE"),
    ] {
        map.insert(kw, tok);
    }
    map
}
