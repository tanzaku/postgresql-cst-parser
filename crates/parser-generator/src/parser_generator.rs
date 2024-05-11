#![allow(non_camel_case_types)]
#![allow(dead_code)]

mod bison;
mod id_mapper;
mod lalr;
mod lexer;

use std::{io::BufReader, process::Command};

use bison::Bison;
use miniz_oxide::deflate::compress_to_vec;

use crate::parser_generator::{bison::Action, lexer::TokenKind};

use self::{
    bison::{parse_bison, Component},
    lalr::Lalr,
};

fn build_action_table(lalr: &Lalr, terminal_symbols: &Vec<Component>) -> Vec<u8> {
    let mut action_table = Vec::new();

    for i in 0..lalr.state_set.states.len() {
        for terminal_symbol in terminal_symbols {
            let cid = lalr.id_mapper.to_component_id(terminal_symbol);
            match lalr.action_table.get(&(i, cid)) {
                Some(Action::Shift(s)) => {
                    action_table.push((*s as i16) + 1);
                }
                Some(Action::Reduce(r)) => {
                    action_table.push(-(*r as i16) - 1);
                }
                Some(Action::Accept) => action_table.push(0),
                Some(Action::Error) => action_table.push(0x7FFF),
                None => action_table.push(0x7FFF),
            }
        }
    }

    compress(action_table)
}

fn build_goto_table(lalr: &Lalr, non_terminal_symbols: &Vec<Component>) -> Vec<u8> {
    let mut goto_table = Vec::new();

    for i in 0..lalr.state_set.states.len() {
        for non_terminal_symbol in non_terminal_symbols {
            let cid = lalr.id_mapper.to_component_id(non_terminal_symbol);
            match lalr.goto_table.get(&(i, cid)) {
                Some(s) => goto_table.push(*s as i16),
                None => goto_table.push(-1),
            }
        }
    }

    compress(goto_table)
}

fn write_parser_file(
    bison: &Bison,
    lalr: &Lalr,
    terminal_symbols: &Vec<Component>,
    non_terminal_symbols: &Vec<Component>,
    comments: &Vec<Component>,
) {
    let action_table = build_action_table(lalr, &terminal_symbols);
    let goto_table = build_goto_table(lalr, &non_terminal_symbols);

    let terminal_symbols_with_comment = terminal_symbols
        .iter()
        .chain(comments)
        .cloned()
        .collect::<Vec<_>>();

    let action_table_str = action_table
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(",");

    let goto_table_str = goto_table
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(",");

    let end_rule_id = terminal_symbols
        .iter()
        .position(|t| &lalr.end_rule_component == t)
        .unwrap()
        .to_string();

    let num_state = lalr.state_set.states.len().to_string();

    let token_to_component_id = terminal_symbols_with_comment
        .iter()
        .enumerate()
        .map(|(i, s)| {
            format!(
                r#"TokenKind::{} => {},"#,
                match s {
                    Component::Terminal(TokenKind::RAW(s)) =>
                        format!(r#"RAW(s) if s == "{}""#, s.trim_matches('\'')),
                    Component::Terminal(TokenKind::KEYWORD(s)) =>
                        format!(r#"KEYWORD(s) if s == "{}""#, s),
                    Component::Terminal(s) => s.to_id(),
                    _ => unreachable!(),
                },
                if matches!(
                    s,
                    Component::Terminal(TokenKind::C_COMMENT)
                        | Component::Terminal(TokenKind::SQL_COMMENT)
                ) {
                    // コメントは構文解析表の先読みトークンには含まれないので、非終端記号の分だけオフセットする
                    i + non_terminal_symbols.len()
                } else {
                    i
                }
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let rule_name_to_component_id = non_terminal_symbols
        .iter()
        .enumerate()
        .map(|(i, s)| format!(r#""{}" => {},"#, s.to_rule_string_without_quote(), i))
        .collect::<Vec<_>>()
        .join("\n");

    let parse_rules = bison
        .rules
        .iter()
        .map(|rule| {
            format!(
                r#"Rule {{name:"{}",len:{},}},"#,
                &rule.name,
                rule.components.len(),
            )
        })
        .collect::<Vec<_>>()
        .join("");

    let num_non_terminal_symbol = non_terminal_symbols.len().to_string();

    let parser_template = include_str!("../resources/parser_template.rs")
        .replace("{num_terminal_symbol}", &terminal_symbols.len().to_string())
        .replace("{num_non_terminal_symbol}", &num_non_terminal_symbol)
        .replace("{num_state}", &num_state)
        .replace("{token_to_component_id}", &token_to_component_id)
        .replace("{rule_name_to_component_id}", &rule_name_to_component_id)
        .replace("{num_parse_rules}", &bison.rules.len().to_string())
        .replace("{parse_rules}", &parse_rules)
        .replace("{action_table}", &action_table_str)
        .replace("{goto_table}", &goto_table_str)
        .replace("{action_table_size}", &action_table.len().to_string())
        .replace("{goto_table_size}", &goto_table.len().to_string())
        .replace("{end_rule_kind}", r#"TokenKind::RAW("$end".to_string())"#)
        .replace("{end_rule_id}", &end_rule_id);

    let paths = ["./crates/postgresql-lst-parser/src/parser.rs"];
    for path in paths {
        std::fs::write(path, &parser_template).unwrap();
        let _ = Command::new("rustfmt").arg(path).output();
    }
}

fn write_syntax_file(
    terminal_symbols: &Vec<Component>,
    non_terminal_symbols: &Vec<Component>,
    comments: &Vec<Component>,
) {
    let mut kinds = Vec::new();
    for c in terminal_symbols
        .iter()
        .chain(non_terminal_symbols)
        .chain(comments)
    {
        kinds.push(format!("{},", c.to_rule_identifier()));
    }

    // kinds.sort();

    let source = format!(
        r#"use cstree::Syntax;
    
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Syntax)]
    #[repr(u32)]
    pub enum SyntaxKind {{
        {}
        Whitespace,
        Root,
    }}"#,
        kinds.join("\n\t")
    );

    let path = "./crates/postgresql-lst-parser/src/syntax_kind.rs";
    std::fs::write(path, source).unwrap();
    let _ = Command::new("rustfmt").arg(path).output();
}

fn write_file(bison: &Bison, lalr: &Lalr) {
    let terminal_symbols: Vec<_> = lalr
        .id_mapper
        .components
        .iter()
        .filter(|c| matches!(c, Component::Terminal(_)))
        .cloned()
        .collect();

    let non_terminal_symbols: Vec<_> = lalr
        .id_mapper
        .components
        .iter()
        .filter(|c| matches!(c, Component::NonTerminal(_)))
        .cloned()
        .collect();

    let comments = vec![
        Component::Terminal(TokenKind::C_COMMENT),
        Component::Terminal(TokenKind::SQL_COMMENT),
    ];

    write_parser_file(
        bison,
        lalr,
        &terminal_symbols,
        &non_terminal_symbols,
        &comments,
    );

    write_syntax_file(&terminal_symbols, &non_terminal_symbols, &comments);
}

fn compress(data: Vec<i16>) -> Vec<u8> {
    let bytes = unsafe { data.align_to::<u8>().1 };
    compress_to_vec(bytes, 10)
}

pub fn generate() {
    let (bison, lalr) = match std::fs::File::open("bison.cache") {
        Ok(f) => bincode::deserialize_from(BufReader::new(f)).unwrap(),
        Err(_) => {
            let bison = parse_bison(include_str!("../resources/gram.y"));
            let mut lalr = Lalr::new(&bison);
            lalr.build_lalr1_parse_table();

            let f = std::fs::File::create("bison.cache").unwrap();
            let w = std::io::BufWriter::new(f);
            bincode::serialize_into(w, &(&bison, &lalr)).unwrap();
            (bison, lalr)
        }
    };

    write_file(&bison, &lalr);
}
