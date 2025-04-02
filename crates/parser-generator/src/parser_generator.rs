#![allow(non_camel_case_types)]
#![allow(dead_code)]

mod bison;
mod id_mapper;
mod lalr;
mod lexer;

use std::{collections::BTreeMap, process::Command};

use bison::Bison;

use crate::parser_generator::{bison::Action, lexer::TokenKind};

use self::{
    bison::{Component, parse_bison},
    lalr::Lalr,
};

const ERROR_ACTION_CODE: i16 = 0x7FFF;
const DEFAULT_ACTION_CODE: i16 = 0x7FFE;
const UNUSED_ENTRY_CODE: i16 = !0;
const INVALID_GOTO_CODE: i16 = -1;

/// Compressed parser table with lookup optimization
#[derive(Debug, Clone)]
struct CompressedParserTable {
    state_to_offset: Vec<u32>,
    compressed_values: Vec<i16>,
    index_check: Vec<i16>,
}

/// 構文解析表を圧縮する
/// 圧縮後の構文解析表は以下の条件を満たす
/// 状態iのエントリーは state_to_offset[i] から始まり、j番目の終端記号の情報は state_to_offset[i+j] に格納される
/// index_check[i+j] == j の場合、そのエントリーには compressed_values[i+j] には有効なアクションの値が格納されている
/// index_check[i+j] != j の場合はエラー状態であることを示す
/// compressed_values[i+j] == ERROR_ACTION_CODE の場合、エラーであることを示す
/// compressed_values[i+j] == DEFAULT_ACTION_CODE の場合、デフォルトアクションの値が格納されていることを示す
/// compressed_values[i+j] にそれ以外の値が含まれている場合、プラスの値であればシフトを、負の値であればリデュースを、0であればアクセプトを示す
fn compress_action_table(
    lalr: &Lalr,
    terminal_symbols: &Vec<Component>,
) -> (CompressedParserTable, Vec<i16>) {
    let mut uncompressed_action_table = Vec::new();

    for i in 0..lalr.state_set.states.len() {
        for terminal_symbol in terminal_symbols {
            let cid = lalr.id_mapper.to_component_id(terminal_symbol);
            match lalr.action_table.get(&(i, cid)) {
                Some(Action::Shift(s)) => {
                    uncompressed_action_table.push((*s as i16) + 1);
                }
                Some(Action::Reduce(r)) => {
                    uncompressed_action_table.push(-(*r as i16) - 1);
                }
                Some(Action::Accept) => uncompressed_action_table.push(0),
                Some(Action::Error) => uncompressed_action_table.push(ERROR_ACTION_CODE),
                None => uncompressed_action_table.push(ERROR_ACTION_CODE),
            }
        }
    }

    let mut state_to_offset = vec![0; lalr.state_set.states.len()];
    let mut compressed_values =
        vec![ERROR_ACTION_CODE; terminal_symbols.len() * uncompressed_action_table.len()];
    let mut index_check =
        vec![UNUSED_ENTRY_CODE; terminal_symbols.len() * uncompressed_action_table.len()];
    let mut offset_used = vec![false; terminal_symbols.len() * uncompressed_action_table.len()];
    let mut state_default_actions = vec![0; lalr.state_set.states.len()];

    fn find_most_common_action(
        uncompressed_action_table: &[i16],
        state_index: usize,
        terminal_symbols: &Vec<Component>,
    ) -> i16 {
        let mut cnt: BTreeMap<i16, u16> = BTreeMap::new();
        for j in 0..terminal_symbols.len() {
            let a = uncompressed_action_table[state_index * terminal_symbols.len() + j];
            if a != ERROR_ACTION_CODE {
                *cnt.entry(a).or_default() += 1;
            }
        }

        cnt.iter().max_by_key(|(_, v)| *v).map(|(k, _)| *k).unwrap()
    }

    for state_index in 0..lalr.state_set.states.len() {
        let state_def_rule =
            find_most_common_action(&uncompressed_action_table, state_index, terminal_symbols);
        let row = {
            let mut row = vec![0; terminal_symbols.len()];

            for j in 0..terminal_symbols.len() {
                let a = uncompressed_action_table[state_index * terminal_symbols.len() + j];
                let a = if a == state_def_rule {
                    DEFAULT_ACTION_CODE
                } else {
                    a
                };

                row[j] = a;
            }

            row
        };

        for i in 0.. {
            let mut ok = true;

            for j in 0..terminal_symbols.len() {
                let a = row[j];

                if a == ERROR_ACTION_CODE {
                    if index_check[i + j] == j as i16 {
                        ok = false;
                        break;
                    }
                } else {
                    if index_check[i + j] == j as i16 {
                        if compressed_values[i + j] != a {
                            ok = false;
                            break;
                        }
                    } else if index_check[i + j] != UNUSED_ENTRY_CODE {
                        ok = false;
                        break;
                    } else if offset_used[i] {
                        ok = false;
                        break;
                    }
                }
            }

            if !ok {
                continue;
            }

            state_to_offset[state_index] = i as u32;
            state_default_actions[state_index] = state_def_rule;
            offset_used[i] = true;

            for j in 0..terminal_symbols.len() {
                let a = row[j];
                if a != ERROR_ACTION_CODE {
                    index_check[i + j] = j as i16;
                    compressed_values[i + j] = a;
                }
            }
            break;
        }
    }

    let max_table_index = *state_to_offset.iter().max().unwrap() as usize + terminal_symbols.len();

    let info = CompressedParserTable {
        state_to_offset: state_to_offset,
        compressed_values: compressed_values[..max_table_index].to_vec(),
        index_check: index_check[..max_table_index].to_vec(),
    };

    (info, state_default_actions)
}

/// goto tableを圧縮する
/// action tableとほぼ同様だが、デフォルトアクションに相当するものはない
fn compress_goto_table(
    lalr: &Lalr,
    non_terminal_symbols: &Vec<Component>,
) -> CompressedParserTable {
    let mut base_goto_table = Vec::new();

    for i in 0..lalr.state_set.states.len() {
        for non_terminal_symbol in non_terminal_symbols {
            let cid = lalr.id_mapper.to_component_id(non_terminal_symbol);
            match lalr.goto_table.get(&(i, cid)) {
                Some(s) => base_goto_table.push(*s as i16),
                None => base_goto_table.push(INVALID_GOTO_CODE),
            }
        }
    }

    let mut state_to_offset = vec![0; lalr.state_set.states.len()];
    let mut compressed_values =
        vec![INVALID_GOTO_CODE; non_terminal_symbols.len() * base_goto_table.len()];
    let mut index_check =
        vec![UNUSED_ENTRY_CODE; non_terminal_symbols.len() * base_goto_table.len()];
    let mut offset_used = vec![false; non_terminal_symbols.len() * base_goto_table.len()];

    for state_index in 0..lalr.state_set.states.len() {
        for i in 0..compressed_values.len() {
            let mut ok = true;

            for j in 0..non_terminal_symbols.len() {
                let a = base_goto_table[state_index * non_terminal_symbols.len() + j];

                if a == INVALID_GOTO_CODE {
                    if index_check[i + j] == j as i16 {
                        ok = false;
                        break;
                    }
                } else {
                    if index_check[i + j] == j as i16 {
                        if compressed_values[i + j] != a {
                            ok = false;
                            break;
                        }
                    } else if index_check[i + j] != UNUSED_ENTRY_CODE {
                        ok = false;
                        break;
                    } else if offset_used[i] {
                        ok = false;
                        break;
                    }
                }
            }

            if !ok {
                continue;
            }

            state_to_offset[state_index] = i as u32;
            offset_used[i] = true;

            for j in 0..non_terminal_symbols.len() {
                let a = base_goto_table[state_index * non_terminal_symbols.len() + j];
                if a != INVALID_GOTO_CODE {
                    index_check[i + j] = j as i16;
                    compressed_values[i + j] = a;
                }
            }
            break;
        }
    }

    let max_table_index =
        *state_to_offset.iter().max().unwrap() as usize + non_terminal_symbols.len();

    let info = CompressedParserTable {
        state_to_offset: state_to_offset,
        compressed_values: compressed_values[..max_table_index].to_vec(),
        index_check: index_check[..max_table_index].to_vec(),
    };

    info
}

fn generate_parser_source_code(
    bison: &Bison,
    lalr: &Lalr,
    terminal_symbols: &Vec<Component>,
    non_terminal_symbols: &Vec<Component>,
    comments: &Vec<Component>,
) {
    fn vec_to_string(vs: &[impl ToString]) -> String {
        vs.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(",")
    }

    let (action_table_info, def_rules) = compress_action_table(lalr, &terminal_symbols);
    let goto_table_info = compress_goto_table(lalr, &non_terminal_symbols);

    let terminal_symbols_with_comment = terminal_symbols
        .iter()
        .chain(comments)
        .cloned()
        .collect::<Vec<_>>();

    let action_check_table_str = vec_to_string(&action_table_info.index_check);
    let action_table_str = vec_to_string(&action_table_info.compressed_values);
    let action_table_index_str = vec_to_string(&action_table_info.state_to_offset);
    let def_rules_str = vec_to_string(&def_rules);

    let action_check_table_size = action_table_info.index_check.len().to_string();
    let action_table_size = action_table_info.compressed_values.len().to_string();
    let action_table_index_size = action_table_info.state_to_offset.len().to_string();
    let def_rules_size = def_rules.len().to_string();

    let goto_check_table_str = vec_to_string(&goto_table_info.index_check);
    let goto_table_str = vec_to_string(&goto_table_info.compressed_values);
    let goto_table_index_str = vec_to_string(&goto_table_info.state_to_offset);

    let goto_check_table_size = goto_table_info.index_check.len().to_string();
    let goto_table_size = goto_table_info.compressed_values.len().to_string();
    let goto_table_index_size = goto_table_info.state_to_offset.len().to_string();

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
                    // Comments are not included in the parser lookahead tokens, so offset them by the number of non-terminal symbols
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

    let parser_template = include_str!("../templates/parser_template.rs")
        .replace("{num_terminal_symbol}", &terminal_symbols.len().to_string())
        .replace("{num_non_terminal_symbol}", &num_non_terminal_symbol)
        .replace("{num_state}", &num_state)
        .replace("{token_to_component_id}", &token_to_component_id)
        .replace("{rule_name_to_component_id}", &rule_name_to_component_id)
        .replace("{num_parse_rules}", &bison.rules.len().to_string())
        .replace("{parse_rules}", &parse_rules)
        .replace("{action_check_table}", &action_check_table_str)
        .replace("{action_check_table_size}", &action_check_table_size)
        .replace("{action_table}", &action_table_str)
        .replace("{action_table_size}", &action_table_size)
        .replace("{action_table_index}", &action_table_index_str)
        .replace("{action_table_index_size}", &action_table_index_size)
        .replace("{def_rules_str}", &def_rules_str)
        .replace("{def_rules_size}", &def_rules_size)
        .replace("{goto_check_table}", &goto_check_table_str)
        .replace("{goto_check_table_size}", &goto_check_table_size)
        .replace("{goto_table}", &goto_table_str)
        .replace("{goto_table_size}", &goto_table_size)
        .replace("{goto_table_index}", &goto_table_index_str)
        .replace("{goto_table_index_size}", &goto_table_index_size)
        .replace("{end_rule_kind}", r#"TokenKind::RAW("$end".to_string())"#)
        .replace("{end_rule_id}", &end_rule_id);

    let paths = ["./crates/postgresql-cst-parser/src/parser.rs"];
    for path in paths {
        std::fs::write(path, &parser_template).unwrap();
        let _ = Command::new("rustfmt").arg(path).output();
    }
}

fn generate_syntax_kinds_source_code(
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

    let path = "./crates/postgresql-cst-parser/src/syntax_kind.rs";
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

    generate_parser_source_code(
        bison,
        lalr,
        &terminal_symbols,
        &non_terminal_symbols,
        &comments,
    );

    generate_syntax_kinds_source_code(&terminal_symbols, &non_terminal_symbols, &comments);

    std::fs::copy(
        "./crates/parser-generator/src/parser_generator/lexer/lexer_ported.rs",
        "./crates/postgresql-cst-parser/src/lexer/lexer_ported.rs",
    )
    .unwrap();

    std::fs::copy(
        "./crates/parser-generator/src/parser_generator/lexer/util.rs",
        "./crates/postgresql-cst-parser/src/lexer/util.rs",
    )
    .unwrap();

    std::fs::copy(
        "./crates/parser-generator/src/parser_generator/lexer/parser_error.rs",
        "./crates/postgresql-cst-parser/src/lexer/parser_error.rs",
    )
    .unwrap();

    std::fs::copy(
        "./crates/parser-generator/src/parser_generator/lexer.rs",
        "./crates/postgresql-cst-parser/src/lexer.rs",
    )
    .unwrap();
}

pub fn generate() {
    // generate cache
    // let (bison, lalr) = match std::fs::File::open("bison.cache") {
    //     Ok(f) => bincode::deserialize_from(BufReader::new(f)).unwrap(),
    //     Err(_) => {
    //         let bison = parse_bison(include_str!("../resources/gram.y"));
    //         let mut lalr = Lalr::new(&bison);
    //         lalr.build_lalr1_parse_table();

    //         let f = std::fs::File::create("bison.cache").unwrap();
    //         let w = std::io::BufWriter::new(f);
    //         bincode::serialize_into(w, &(&bison, &lalr)).unwrap();
    //         (bison, lalr)
    //     }
    // };

    let (bison, lalr) = {
        let bison = parse_bison(include_str!("../resources/gram.y"));
        let mut lalr = Lalr::new(&bison);
        lalr.build_lalr1_parse_table();

        (bison, lalr)
    };

    write_file(&bison, &lalr);
}
