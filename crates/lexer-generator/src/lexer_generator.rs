use std::{collections::HashMap, process::Command};

use self::flex_file::{parse_flex_file, FlexFile};

mod flex_file;

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
                res.push(format!(
                    r#"RuleKind::{kind}|"#,
                    kind = format!("{}{}", s, e)
                ));
            } else {
                res.push(format!(
                    r#"RuleKind::{kind} => {{
                    {actions}
                }}"#,
                    kind = format!("{}{}", s, e),
                    actions = rule.actions
                ));
            }
        }
    }

    res.join("\n")
}

/// Convert rule patterns to regular expressions
fn extract_rule_pattern(flex_file: &FlexFile, pattern: &str) -> (String, bool) {
    if pattern == "<<EOF>>" {
        return ("^$".to_string(), true);
    }

    // Regular expression pattern to extract {xxx} patterns
    let p = regex::Regex::new(r#"\{([a-zA-Z0-9_]+)\}"#).unwrap();

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
        .replace_all(&pattern, |caps: &regex::Captures| {
            let name = caps.get(1).unwrap().as_str();

            // Check if xxx in {xxx} is defined
            if let Some(def) = flex_file.definitions.iter().find(|def| def.name == name) {
                let pattern = remove_unnecessary_quote(&def.def);
                let (rep, _) = extract_rule_pattern(flex_file, &pattern);
                format!("({})", rep)
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
                    pattern: Regex::new(r#"(?-u)^({pattern})"#).unwrap(),
                    kind: RuleKind::{rule_kind},
                    eof: {eof},
                }}"###,
                original_pattern = rule.pattern,
                pattern = pattern,
                eof = eof,
                rule_kind = format!("{}{}", s, e),
            ));
        }
    }

    res.join(",\n")
}

/// Generate enum representing states
fn construct_states(flex_file: &FlexFile) -> String {
    flex_file.all_states.clone().join(",\n")
}

/// Generate Lexer based on scan.l
pub fn generate() {
    let flex_file = parse_flex_file(include_str!("../resources/scan.l"));
    let template = include_str!("../templates/lex_template.rs");

    let rule_kinds = construct_rule_kinds(&flex_file);
    let actions = construct_actions(&flex_file);
    let rule_defs = construct_rule_defs(&flex_file);
    let states = construct_states(&flex_file);

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
        .replace("{rule_defs}", &rule_defs)
        .replace("{states}", &states)
        .replace("{keyword_map}", &keywords.join("\n"));

    let paths = [
        "./crates/postgresql-cst-parser/src/lexer/generated.rs",
        "./crates/parser-generator/src/parser_generator/lexer/generated.rs",
    ];
    for path in paths {
        std::fs::write(path, &res).unwrap();
        Command::new("rustfmt").arg(path).output().unwrap();
    }
}
