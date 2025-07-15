use std::{collections::HashMap, process::Command};

use automata::{FlexPatternDef, NameDefinition, to_flex_pattern_dfa};
use regex::Regex;

use crate::flex_file::{FlexFile, parse_flex_file};

/// Assign unique names to all rules in each state
fn construct_rule_kinds(flex_file: &FlexFile) -> String {
    let mut res = Vec::new();
    let mut map: HashMap<&String, usize> = HashMap::new();

    for rule in &flex_file.rules {
        for s in &rule.states {
            let e = map.entry(s).or_default();
            *e += 1;
            res.push(format!("{}{}", s, *e));
        }
    }

    res.join(",\n")
}

/// Generate code for the action part
fn construct_actions(flex_file: &FlexFile) -> String {
    let mut res = Vec::new();
    let mut map: HashMap<&String, usize> = HashMap::new();

    for rule in &flex_file.rules {
        for s in &rule.states {
            let e = map.entry(s).or_default();
            *e += 1;

            // If the action is |, the subsequent rule will be executed
            if rule.actions.trim() == "|" {
                res.push(format!(r#"RuleKind::{s}{e}|"#));
            } else {
                res.push(format!(
                    r#"RuleKind::{s}{e} => {{
                    {actions}
                }}"#,
                    actions = rule.actions
                ));
            }
        }
    }

    res.join("\n")
}

fn construct_pattern_actions_by_index(flex_file: &FlexFile) -> String {
    let mut res = Vec::new();

    for (i, rule) in flex_file.rules.iter().enumerate() {
        // If the action is |, the subsequent rule will be executed
        if rule.actions.trim() == "|" {
            res.push(format!(r#"{i}|"#));
        } else {
            res.push(format!(
                r#"{i} => {{
                    {actions}
                }}"#,
                actions = rule.actions
            ));
        }
    }

    // どのルールにもマッチしなかったときの値
    res.push(r#"255 => {{}}"#.to_string());
    res.push(
        r#"_ => {{
            unreachable!()
        }}"#
        .to_string(),
    );

    res.join("\n")
}

/// Convert rule patterns to regular expressions
fn extract_rule_pattern(flex_file: &FlexFile, pattern: &str) -> (String, bool) {
    if pattern == "<<EOF>>" {
        return ("^$".to_string(), true);
    }

    // Regular expression pattern to extract {xxx} patterns
    let p = Regex::new(r#"\{([a-zA-Z0-9_]+)\}"#).unwrap();

    // In flex, double quotes need to be escaped, but not in regular expressions
    // Therefore, remove the escaping of double quotes before converting to regex pattern
    fn remove_unnecessary_quote(s: &str) -> String {
        let chars = s.chars().collect::<Vec<_>>();
        let mut remove = vec![false; chars.len()];
        let mut i = 0;

        let mut in_char_class = false;
        let mut escape = false;
        while i < chars.len() {
            if in_char_class {
                if chars[i] == ']' {
                    in_char_class = false;
                }
            } else {
                if chars[i] == '"' && !escape {
                    remove[i] = true;
                }

                if !escape {
                    match chars[i] {
                        '\\' => escape = true,
                        '[' => in_char_class = true,
                        _ => (),
                    }
                } else {
                    escape = false;
                }
            }
            i += 1;
        }

        (0..chars.len())
            .filter(|&i| !remove[i])
            .map(|i| chars[i])
            .collect()
    }

    // Expand {xxx} to actual regular expression patterns
    let replaced = p
        .replace_all(pattern, |caps: &regex::Captures| {
            let name = caps.get(1).unwrap().as_str();

            // Check if xxx in {xxx} is defined
            if let Some(def) = flex_file.definitions.iter().find(|def| def.name == name) {
                let pattern = remove_unnecessary_quote(&def.def);
                let (rep, _) = extract_rule_pattern(flex_file, &pattern);
                format!("({rep})")
            } else {
                format!("{{{name}}}")
            }
        })
        .to_string();

    (replaced, false)
}

/// Generate Rule structures
fn construct_rule_defs(flex_file: &FlexFile) -> String {
    let mut res = Vec::new();
    let mut map: HashMap<&String, usize> = HashMap::new();

    for rule in &flex_file.rules {
        for s in &rule.states {
            let e = map.entry(s).or_default();
            *e += 1;

            let (pattern, eof) = extract_rule_pattern(flex_file, &rule.pattern);
            res.push(format!(
                r###"
                // {original_pattern}
                Rule {{
                    state: State::{s},
                    pattern: regex::bytes::Regex::new(r#"(?-u)^({pattern})"#).unwrap(),
                    kind: RuleKind::{s}{e},
                    eof: {eof},
                }}"###,
                original_pattern = rule.pattern,
                pattern = pattern,
                eof = eof,
            ));
        }
    }

    res.join(",\n")
}

/// Generate enum representing states
fn construct_states(flex_file: &FlexFile) -> String {
    flex_file.all_states.clone().join(",\n")
}

fn construct_dfa(flex_file: &FlexFile) -> (String, String) {
    let state_map = flex_file
        .all_states
        .iter()
        .enumerate()
        .map(|(i, s)| (s.clone(), i))
        .collect::<HashMap<_, _>>();

    let rules = flex_file
        .rules
        .iter()
        .map(|rule| FlexPatternDef {
            state_set: rule
                .states
                .iter()
                .map(|s| *state_map.get(s).unwrap())
                .collect::<Vec<_>>(),
            pattern: rule.pattern.clone(),
        })
        .collect::<Vec<_>>();

    let name_defs = flex_file
        .definitions
        .iter()
        .map(|def| NameDefinition {
            name: def.name.clone(),
            definition: def.def.clone(),
        })
        .collect::<Vec<_>>();

    let dfa = to_flex_pattern_dfa(&rules, &name_defs);

    let mut table_defs = Vec::new();
    let mut state_id_to_dfa_table_defs = Vec::new();

    for (i, dfa) in dfa.into_iter().enumerate() {
        let mut state_transition = Vec::new();
        let mut state_accept = Vec::new();

        for i in 0..dfa.states.len() {
            let mut row_transition = Vec::new();
            for j in 0..dfa.states[i].transitions.len() {
                let v = dfa.states[i].transitions[j] as u8;
                row_transition.push(v.to_string());
            }

            state_transition.push(format!("[{}]", row_transition.join(",")));
            state_accept.push(format!(
                "{}",
                dfa.states[i]
                    .accept_lexer_rule_id
                    .map(|v| v as u8)
                    .unwrap_or(!0)
            ));
        }

        table_defs.push(format!(
            r#"
            // DFA transition table (state={state_name})
            pub const TRANSITION_TABLE_{i}: [[u8; 256]; {state_len}] = [{transition_table}];
    
            // DFA accept table (state={state_name})
            pub const ACCEPT_TABLE_{i}: [u8; {state_len}] = [{accept_table}];
            "#,
            state_len = dfa.states.len(),
            state_name = flex_file.all_states[i],
            transition_table = state_transition.join(","),
            accept_table = state_accept.join(",")
        ));

        state_id_to_dfa_table_defs.push(format!(
            r#"{i} => (TRANSITION_TABLE_{i}.as_slice(), ACCEPT_TABLE_{i}.as_slice()),"#
        ));
    }

    (table_defs.join("\n"), state_id_to_dfa_table_defs.join("\n"))
}

/// Generate Lexer based on scan.l
pub fn generate() {
    let flex_file = parse_flex_file(include_str!("../resources/scan.l"));

    let template = include_str!("../templates/lex_template.rs");

    let rule_kinds = construct_rule_kinds(&flex_file);
    let actions = construct_actions(&flex_file);
    let pattern_actions_by_index = construct_pattern_actions_by_index(&flex_file);
    let rule_defs = construct_rule_defs(&flex_file);
    let states = construct_states(&flex_file);
    let (dfa_table_def, state_id_to_dfa_table_defs) = construct_dfa(&flex_file);

    // Extract keyword list
    let mut keywords = Vec::new();
    for line in include_str!("../resources/kwlist.h").lines() {
        if line.starts_with("PG_KEYWORD") {
            let mut it = line.split(&['(', ',']).skip(1);
            let name = it.next().unwrap().trim();
            let value = it.next().unwrap().trim();
            keywords.push(format!(r#"({name}, "{value}"),"#));
        }
    }

    let res = template
        .replace("{rule_kinds}", &rule_kinds)
        .replace("{actions}", &actions)
        .replace("{pattern_actions_by_index}", &pattern_actions_by_index)
        .replace("{rule_defs}", &rule_defs)
        .replace("{states}", &states)
        .replace("{keyword_map}", &keywords.join("\n"))
        .replace("{{dfa_table}}", &dfa_table_def)
        .replace("{{state_id_to_dfa_table}}", &state_id_to_dfa_table_defs);

    let paths = [
        "./crates/postgresql-cst-parser/src/lexer/generated.rs",
        "./crates/parser-generator/src/parser_generator/lexer/generated.rs",
    ];

    for path in paths {
        std::fs::write(path, &res).unwrap();
        Command::new("rustfmt").arg(path).output().unwrap();
    }
}
