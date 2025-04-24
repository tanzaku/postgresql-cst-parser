mod generated;
mod util;

use std::collections::HashMap;

use regex::bytes::Regex;

use crate::lexer::generated::get_rules;

use self::generated::{RuleKind, State};

pub const NAMEDATALEN: usize = 64;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Yylval {
    Str(String),
    I(i32),
    Keyword(String),
    Uninitialized,
}

pub struct Lexer {
    pub input: String,
    pub index_bytes: usize,
    pub state: State,
    pub yyleng: usize,

    pub literal: String,

    // user state
    pub xcdepth: usize,
    pub yyllocend_bytes: usize,
    pub state_before_str_stop: State,
    pub yylloc_stack: Vec<usize>,
    pub dolqstart: String,

    // states
    pub yylval: Yylval,
    pub yylloc_bytes: usize,

    pub rules: Vec<Rule>,
    pub keyword_map: HashMap<&'static str, &'static str>,
}

pub struct Rule {
    pub state: State,
    pub pattern: Regex,
    pub kind: RuleKind,
}

#[allow(clippy::all)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    RAW(String),
    IDENT,
    KEYWORD(String),
    C_COMMENT,
    SQL_COMMENT,
    EOF,
    BCONST,
    XCONST,
    SCONST,
    USCONST,
    UIDENT,
    TYPECAST,
    DOT_DOT,
    COLON_EQUALS,
    EQUALS_GREATER,
    LESS_EQUALS,
    GREATER_EQUALS,
    NOT_EQUALS,
    Op,
    PARAM,
    FCONST,
    ICONST,
}

impl TokenKind {
    #[allow(dead_code)]
    pub fn to_id(&self) -> String {
        match self {
            TokenKind::RAW(s) => format!("'{}'", s),
            TokenKind::IDENT => "IDENT".to_string(),
            TokenKind::KEYWORD(s) => s.to_string(),
            TokenKind::C_COMMENT => "C_COMMENT".to_string(),
            TokenKind::SQL_COMMENT => "SQL_COMMENT".to_string(),
            TokenKind::EOF => "EOF".to_string(),
            TokenKind::BCONST => "BCONST".to_string(),
            TokenKind::XCONST => "XCONST".to_string(),
            TokenKind::SCONST => "SCONST".to_string(),
            TokenKind::USCONST => "USCONST".to_string(),
            TokenKind::UIDENT => "UIDENT".to_string(),
            TokenKind::TYPECAST => "TYPECAST".to_string(),
            TokenKind::DOT_DOT => "DOT_DOT".to_string(),
            TokenKind::COLON_EQUALS => "COLON_EQUALS".to_string(),
            TokenKind::EQUALS_GREATER => "EQUALS_GREATER".to_string(),
            TokenKind::LESS_EQUALS => "LESS_EQUALS".to_string(),
            TokenKind::GREATER_EQUALS => "GREATER_EQUALS".to_string(),
            TokenKind::NOT_EQUALS => "NOT_EQUALS".to_string(),
            TokenKind::Op => "Op".to_string(),
            TokenKind::PARAM => "PARAM".to_string(),
            TokenKind::FCONST => "FCONST".to_string(),
            TokenKind::ICONST => "ICONST".to_string(),
        }
    }
}

impl<T> From<T> for TokenKind
where
    T: AsRef<str>,
{
    fn from(s: T) -> Self {
        match s.as_ref() {
            "IDENT" => TokenKind::IDENT,
            "C_COMMENT" => TokenKind::C_COMMENT,
            "SQL_COMMENT" => TokenKind::SQL_COMMENT,
            "EOF" => TokenKind::EOF,
            "BCONST" => TokenKind::BCONST,
            "XCONST" => TokenKind::XCONST,
            "SCONST" => TokenKind::SCONST,
            "USCONST" => TokenKind::USCONST,
            "UIDENT" => TokenKind::UIDENT,
            "TYPECAST" => TokenKind::TYPECAST,
            "DOT_DOT" => TokenKind::DOT_DOT,
            "COLON_EQUALS" => TokenKind::COLON_EQUALS,
            "EQUALS_GREATER" => TokenKind::EQUALS_GREATER,
            "LESS_EQUALS" => TokenKind::LESS_EQUALS,
            "GREATER_EQUALS" => TokenKind::GREATER_EQUALS,
            "NOT_EQUALS" => TokenKind::NOT_EQUALS,
            "Op" => TokenKind::Op,
            "PARAM" => TokenKind::PARAM,
            "FCONST" => TokenKind::FCONST,
            "ICONST" => TokenKind::ICONST,
            _ => TokenKind::KEYWORD(s.as_ref().to_string()), // TODO check if keyword
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub start_byte_pos: usize,
    pub end_byte_pos: usize,
    pub kind: TokenKind,
    pub value: String,
}

pub fn lex(input: &str) -> Vec<Token> {
    let rules = get_rules();

    let mut lexer = Lexer::new(input, rules);

    let mut tokens = vec![];
    while let Some(kind) = lexer.parse_token() {
        // dbg!(&kind);
        if kind == TokenKind::EOF {
            break;
        }

        let start_byte_pos = lexer.yylloc_bytes;
        let end_byte_pos = if matches!(
            kind,
            TokenKind::SCONST
                | TokenKind::BCONST
                | TokenKind::XCONST
                | TokenKind::IDENT
                | TokenKind::C_COMMENT
        ) {
            lexer.yyllocend_bytes
        } else {
            lexer.yylloc_bytes + lexer.yyleng
        };

        tokens.push(Token {
            start_byte_pos,
            end_byte_pos,
            kind,
            // value: lexer.yytext(),
            value: input[start_byte_pos..end_byte_pos].to_string(),
        });
        lexer.advance();
    }

    // dbg!(&tokens);
    tokens
}
