use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::Hash,
};

// use serde::{Deserialize, Serialize};

use super::lexer::TokenKind;

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Accept,
    Error,
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssocDirective {
    NonAssoc,
    Left,
    Right,
}

// #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assoc {
    pub name: String,
    pub priority: usize,
    pub directive: AssocDirective,
}

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RawComponent {
    Identifier(String),
    Raw(String),
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ComponentId(pub u16);

/// Component of the syntax rule
// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Component {
    /// Non-terminal symbol
    NonTerminal(String),
    /// Terminal symbol
    Terminal(TokenKind),
}

impl Component {
    pub fn to_rule_string(&self) -> String {
        match self {
            Component::NonTerminal(s) => s.clone(),
            Component::Terminal(s) => s.to_id(),
        }
    }

    pub fn to_rule_string_without_quote(&self) -> String {
        match self {
            Component::NonTerminal(s) => s.clone(),
            Component::Terminal(s) => s.to_id().trim_matches('\'').to_string(),
        }
    }

    pub fn to_rule_identifier(&self) -> String {
        fn replace(mut s: String) -> String {
            let replace_candidates = [
                ('-', "Minus"),
                (',', "Comma"),
                (';', "Semicolon"),
                (':', "Colon"),
                ('!', "Exclamation"),
                ('?', "Question"),
                ('.', "Dot"),
                ('"', "DQuote"),
                ('(', "LParen"),
                (')', "RParen"),
                ('[', "LBracket"),
                (']', "RBracket"),
                ('{', "LBrace"),
                ('}', "RBrace"),
                ('@', "At"),
                ('*', "Star"),
                ('/', "Slash"),
                ('\'', "Quote"),
                ('\\', "Backslash"),
                ('&', "Ampersand"),
                ('#', "Pound"),
                ('%', "Percent"),
                ('`', "Backtick"),
                ('^', "Caret"),
                ('+', "Plus"),
                ('<', "Less"),
                ('=', "Equals"),
                ('>', "Greater"),
                ('|', "Pipe"),
                ('~', "Tilde"),
                ('$', "Dollar"),
            ];

            for (pat, replacement) in replace_candidates {
                s = s.replace(pat, replacement);
            }

            s
        }

        let s = match self {
            Component::NonTerminal(s) => s.clone(),
            Component::Terminal(s) => s.to_id().trim_matches('\'').to_string(),
        };

        replace(s)
    }
}

// #[derive(Debug, Clone, Serialize, Deserialize)]
#[derive(Debug, Clone)]
pub struct Rule {
    pub name: String,
    pub components: Vec<Component>,
    pub prec: Option<String>,
}

// #[derive(Debug, Serialize, Deserialize)]
#[derive(Debug)]
pub struct Bison {
    /// %type
    /// key is non terminal symbol
    pub typ: HashMap<String, String>,
    /// %token
    /// key is terminal symbol
    pub token: HashMap<String, Option<String>>,
    pub assoc: HashMap<String, Assoc>,
    pub rules: Vec<Rule>,

    pub rule_names: Vec<String>,
}

impl Bison {
    pub fn is_terminal(&self, s: &str) -> bool {
        !self.is_non_terminal(s)
    }

    pub fn is_non_terminal(&self, s: &str) -> bool {
        self.rule_names.contains(&s.to_string())
    }
}

fn is_start_whitespace(line: impl AsRef<str>) -> bool {
    line.as_ref()
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_whitespace())
}

fn parse_type(bison: &mut Bison, line: &str, deq: &mut VecDeque<String>) {
    let mut line = line["%type ".len()..].to_string();
    let line_sep = line
        .split_ascii_whitespace()
        .map(str::to_string)
        .collect::<Vec<_>>();

    let p: &[_] = &['<', '>'];
    let typ = line_sep[0].trim_matches(p);

    line = line_sep[1..].join(" ").to_string();

    loop {
        for non_terminal_symbol in line.split_whitespace() {
            bison
                .typ
                .insert(non_terminal_symbol.to_string(), typ.to_string());
        }

        // If it starts with a space, consider it as a continuation line
        if deq.front().is_some_and(is_start_whitespace) {
            line = deq.pop_front().unwrap();
        } else {
            break;
        }
    }
}

fn parse_token(bison: &mut Bison, line: &str, deq: &mut VecDeque<String>) {
    let mut line = line["%token ".len()..].to_string();
    let line_sep = line
        .split_ascii_whitespace()
        .map(str::to_string)
        .collect::<Vec<_>>();

    let typ = if line_sep[0].starts_with('<') {
        let p: &[_] = &['<', '>'];
        line = line_sep[1..].join(" ");
        Some(line_sep[0].trim_matches(p).to_string())
    } else {
        line = line_sep.join(" ");
        None
    };

    loop {
        for terminal_symbol in line.split_whitespace() {
            bison.token.insert(terminal_symbol.to_string(), typ.clone());
        }

        // If it starts with a space, consider it as a continuation line
        if deq
            .front()
            .is_some_and(|line| is_start_whitespace(line) || line.is_empty())
        {
            line = deq.pop_front().unwrap();
        } else {
            break;
        }
    }
}

fn parse_assoc(
    bison: &mut Bison,
    line: &str,
    deq: &mut VecDeque<String>,
    directive: AssocDirective,
) {
    let mut line = line
        .split_ascii_whitespace()
        .skip(1)
        .collect::<Vec<_>>()
        .join(" ");

    let priority = bison.assoc.len();

    loop {
        for name in line.split_whitespace() {
            // If we find the start of a block comment, end
            // This is rough but works fine for parsing postgresql's grammar
            if name == "/*" {
                break;
            }

            let assoc = Assoc {
                name: name.to_string(),
                priority,
                directive,
            };

            bison.assoc.insert(name.to_string(), assoc);
        }

        // If it starts with a space, consider it as a continuation line
        if deq.front().is_some_and(is_start_whitespace) {
            line = deq.pop_front().unwrap();
        } else {
            break;
        }
    }
}

#[derive(Debug, Clone)]
pub struct Range {
    start_byte_pos: usize,
    start_row: usize,
    start_col: usize,
    end_byte_pos: usize,
    end_row: usize,
    end_col: usize,
}

#[derive(Debug, Clone)]
pub enum BisonToken {
    Colon { range: Range },
    SemiColon { range: Range },
    Identifier { range: Range, value: String },
    Literal { range: Range, value: String },
    VerticalBar { range: Range },
    Prec { range: Range, value: String },
}

impl BisonToken {
    pub fn identifier(&self) -> Option<String> {
        match self {
            BisonToken::Identifier { value, .. } => Some(value.clone()),
            _ => None,
        }
    }
}

/// scan bison's grammar file
fn scan(body: String) -> Vec<BisonToken> {
    let mut tokens = Vec::new();

    let mut row = 0;
    let mut col = 0;
    let mut byte_pos = 0;

    fn update_range(
        chars: &[char],
        i: &mut usize,
        row: &mut usize,
        col: &mut usize,
        byte_pos: &mut usize,
    ) {
        let c = chars[*i];

        if c == '\r' {
            *i += 1;
            *byte_pos += c.len_utf8();
            return;
        }

        if c == '\n' {
            *i += 1;
            *byte_pos += c.len_utf8();
            *row += 1;
            *col = 0;
            return;
        }

        *i += 1;
        *byte_pos += c.len_utf8();
        *col += 1;
    }

    fn range(s: &str, row: usize, col: usize, byte_pos: usize) -> Range {
        Range {
            start_byte_pos: byte_pos,
            start_row: row,
            start_col: col,
            end_byte_pos: byte_pos + s.len(),
            end_row: row,
            end_col: col + s.len(),
        }
    }

    let chars = body.chars().collect::<Vec<_>>();

    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];

        if c.is_ascii_whitespace() {
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == ':' {
            tokens.push(BisonToken::Colon {
                range: range(&c.to_string(), row, col, byte_pos),
            });
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == ';' {
            tokens.push(BisonToken::SemiColon {
                range: range(&c.to_string(), row, col, byte_pos),
            });
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == '|' {
            tokens.push(BisonToken::VerticalBar {
                range: range(&c.to_string(), row, col, byte_pos),
            });
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == '{' {
            let mut depth = 0;
            loop {
                match chars[i] {
                    '{' => depth += 1,
                    '}' => depth -= 1,
                    _ => (),
                }

                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                if depth == 0 {
                    break;
                }
            }

            continue;
        }

        if c == '/' {
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);

            let c: char = chars[i];
            if c == '/' {
                while chars[i] != '\n' {
                    update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                }

                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                continue;
            }

            if c == '*' {
                while !chars[i..].starts_with(&['*', '/']) {
                    update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                }

                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                continue;
            }

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
        }

        if c == '\'' {
            let mut value = String::new();

            value.push('\'');

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);

            while chars[i] != '\'' {
                value.push(chars[i]);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            }

            value.push('\'');

            tokens.push(BisonToken::Literal {
                range: Range {
                    start_byte_pos: byte_pos - value.len(),
                    start_row: row,
                    start_col: col - value.chars().count(),
                    end_byte_pos: byte_pos,
                    end_row: row,
                    end_col: col,
                },
                value,
            });

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == '%' {
            let mut value = String::new();

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            loop {
                if !chars[i].is_ascii_alphanumeric() && chars[i] != '_' {
                    break;
                }

                value.push(chars[i]);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            }

            tokens.push(BisonToken::Prec {
                range: Range {
                    start_byte_pos: byte_pos - value.len() - 1,
                    start_row: row,
                    start_col: col - value.chars().count() - 1,
                    end_byte_pos: byte_pos,
                    end_row: row,
                    end_col: col,
                },
                value,
            });

            continue;
        }

        if c.is_ascii_alphabetic() {
            let mut value = String::new();

            loop {
                if !chars[i].is_ascii_alphanumeric() && chars[i] != '_' {
                    break;
                }

                value.push(chars[i]);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            }

            tokens.push(BisonToken::Identifier {
                range: Range {
                    start_byte_pos: byte_pos - value.len(),
                    start_row: row,
                    start_col: col - value.chars().count(),
                    end_byte_pos: byte_pos,
                    end_row: row,
                    end_col: col,
                },
                value,
            });

            continue;
        }

        eprintln!("{}", chars[i..][..100].iter().collect::<String>());

        unreachable!();
    }

    tokens
}

fn parse_grammar_rules(bison: &mut Bison, body: String) {
    let tokens = scan(body);

    let mut raw_rules = Vec::new();

    for rules in tokens.split(|t| matches!(t, BisonToken::SemiColon { .. })) {
        if rules.is_empty() {
            continue;
        }

        if !matches!(rules[1], BisonToken::Colon { .. }) {
            panic!("expected colon");
        }

        let rule_name = rules[0].identifier().unwrap();
        bison.rule_names.push(rule_name.clone());

        for rule in rules[2..].split(|t| matches!(t, BisonToken::VerticalBar { .. })) {
            let mut prec = None;
            let mut rule = rule.to_vec();
            for i in (0..rule.len()).rev() {
                if matches!(rule[i], BisonToken::Prec { .. }) {
                    prec = Some(match &rule[i + 1] {
                        BisonToken::Identifier { value, .. } => value.clone(),
                        _ => unreachable!(),
                    });

                    rule.remove(i + 1);
                    rule.remove(i);
                }
            }
            let comp: Vec<RawComponent> = rule
                .iter()
                .filter_map(|t| match t {
                    BisonToken::Identifier { value, .. } => {
                        Some(RawComponent::Identifier(value.clone()))
                    }
                    BisonToken::Literal { value, .. } => Some(RawComponent::Raw(value.clone())),
                    _ => {
                        dbg!(t);
                        unreachable!()
                    }
                })
                .collect();

            raw_rules.push((rule_name.clone(), comp, prec.clone()));
        }
    }

    for (name, comp, prec) in raw_rules {
        let components: Vec<_> = comp
            .into_iter()
            .map(|c| match c {
                RawComponent::Identifier(s) if bison.is_non_terminal(&s) => {
                    Component::NonTerminal(s)
                }
                RawComponent::Identifier(s) => Component::Terminal(TokenKind::from(s)),
                RawComponent::Raw(s) => Component::Terminal(TokenKind::RAW(s)),
            })
            .collect();

        bison.rules.push(Rule {
            name,
            components,
            prec,
        });
    }
}

pub fn parse_bison(s: impl AsRef<str>) -> Bison {
    let mut deq = s
        .as_ref()
        .lines()
        .map(str::to_owned)
        .collect::<VecDeque<_>>();

    let mut bison = Bison {
        typ: HashMap::new(),
        token: HashMap::new(),
        assoc: HashMap::new(),
        rules: Vec::new(),
        rule_names: Vec::new(),
    };

    while let Some(line) = deq.pop_front() {
        if line.starts_with("%%") {
            break;
        }

        match line.split_whitespace().next() {
            Some("%type") => parse_type(&mut bison, &line, &mut deq),
            Some("%token") => parse_token(&mut bison, &line, &mut deq),
            Some("%nonassoc") => parse_assoc(&mut bison, &line, &mut deq, AssocDirective::NonAssoc),
            Some("%left") => parse_assoc(&mut bison, &line, &mut deq, AssocDirective::Left),
            Some("%right") => parse_assoc(&mut bison, &line, &mut deq, AssocDirective::Right),
            _ => (),
        }
    }

    let mut rule_body: String = String::new();
    while let Some(line) = deq.pop_front() {
        if line.starts_with("%%") {
            break;
        }

        rule_body.push_str(&line);
        rule_body.push('\n');
    }

    parse_grammar_rules(&mut bison, rule_body);

    bison
}

#[cfg(test)]
mod tests {
    use crate::parser_generator::lalr::Lalr;

    use super::parse_bison;

    #[test]
    fn test1() {
        let bison = parse_bison(
            r#"
%%
S: V EQUALS E
 | E
 ;

E: V;
V: X;
V: ASTERISK E;
%%
"#,
        );

        let mut lalr = Lalr::new(&bison);
        lalr.build_lalr1_parse_table();

        assert_eq!(lalr.action_table.len(), 17);
    }

    #[test]
    fn test2() {
        let bison = parse_bison(
            r#"
%%
A: E EQUALS E
 | Id
 ;

E: E '+' T;
E: T;
T: Num;
T: Id;
%%
"#,
        );

        let mut lalr = Lalr::new(&bison);
        lalr.build_lalr1_parse_table();

        assert_eq!(lalr.action_table.len(), 26);
    }

    #[test]
    fn test3() {
        let bison = parse_bison(
            r#"
%%
A: E EQUALS E
 | Id
 ;

E: E '+' E
| E '-' E
| E '*' E;
E: T;
T: Num;
T: Id;
%%
"#,
        );

        let mut lalr = Lalr::new(&bison);
        lalr.build_lalr1_parse_table();

        assert_eq!(lalr.action_table.len(), 54);
    }

    #[test]
    fn test4() {
        let bison = parse_bison(include_str!("../../resources/gram.y"));

        let mut lalr = Lalr::new(&bison);
        lalr.build_lalr1_parse_table();

        assert_eq!(lalr.state_set.states.len(), 6236);
    }
}
