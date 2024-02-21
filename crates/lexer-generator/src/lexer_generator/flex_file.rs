use regex::Regex;

#[derive(Debug)]
pub struct Definition {
    pub name: String,
    pub def: String,
}

#[derive(Debug)]
pub struct Rule {
    pub states: Vec<String>,
    pub pattern: String,
    pub actions: String,
}

#[derive(Debug, Default)]
pub struct FlexFile {
    pub all_states: Vec<String>,
    pub definitions: Vec<Definition>,
    pub rules: Vec<Rule>,
    pub rest_code: String,
}

fn parse_actions(mut line: String, it: &mut impl Iterator<Item = String>) -> String {
    let mut depth = 0;
    let mut res = String::new();

    loop {
        for c in line.chars() {
            if c == '{' {
                depth += 1;
            } else if c == '}' {
                depth -= 1;
            }
        }

        res += &line;
        res += "\n";

        if depth == 0 {
            return res;
        }

        line = it.next().unwrap();
    }
}

fn parse_pattern_actions(
    line: String,
    it: &mut impl Iterator<Item = String>,
) -> Option<(String, String)> {
    if line.trim().is_empty() {
        return None;
    }

    if line.starts_with("<<EOF>>") {
        let rest = line["<<EOF>>".len()..].to_owned();
        let actions = parse_actions(rest, it);
        return Some(("<<EOF>>".to_owned(), actions));
    }

    let j = line.chars().position(|c| c.is_whitespace()).unwrap();

    let pattern = line[..j].to_owned();
    let actions = parse_actions(line[j..].to_owned(), it);

    Some((pattern, actions))
}

fn parse_rules(states: Vec<String>, it: &mut impl Iterator<Item = String>) -> Vec<Rule> {
    let mut rules = Vec::new();

    while let Some(mut line) = it.next() {
        if line.starts_with("%%") || line.starts_with('}') {
            break;
        }

        if line.trim().is_empty() {
            continue;
        }

        let mut states = states.clone();
        if line.starts_with("<") && !line.starts_with("<<") {
            let end = line.find('>').unwrap();
            states = line[1..end].split(',').map(str::to_owned).collect();
            line = line[end + 1..].to_owned();

            if line.starts_with('{') && line.len() == 1 {
                let mut nested_rules = parse_rules(states, it);
                rules.append(&mut nested_rules);
                continue;
            }
        }

        if let Some((pattern, actions)) = parse_pattern_actions(line, it) {
            let mut states = states.clone();
            if states.is_empty() {
                states.push("INITIAL".to_owned());
            }

            rules.push(Rule {
                states,
                pattern,
                actions,
            });
        }
    }

    rules
}

pub fn parse_flex_file(s: impl AsRef<str>) -> FlexFile {
    let s = s.as_ref().lines().map(str::to_owned).collect::<Vec<_>>();

    let pattern_identifier = Regex::new("^([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();
    let mut flex_file = FlexFile::default();

    flex_file.all_states.push("INITIAL".to_owned());
    let mut it = s.iter().cloned();
    while let Some(line) = it.next() {
        if line.starts_with("%%") {
            break;
        }

        if line.starts_with("%x ") {
            flex_file.all_states.push(line[3..].to_owned());
            continue;
        }

        if pattern_identifier.is_match(&line) {
            let (a, b) = line.split_once('\t').unwrap();
            let b = b.trim();

            flex_file.definitions.push(Definition {
                name: a.to_owned(),
                def: b.to_owned(),
            });

            continue;
        }
    }

    flex_file.rules = parse_rules(Vec::new(), &mut it);

    while let Some(line) = it.next() {
        flex_file.rest_code += &line;
        flex_file.rest_code += &"\n";
    }

    flex_file
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_flex_file() {
        let input = r#"
%x STATE1
%x STATE2

%%

<STATE1>pattern1		{ action1 }
<STATE2>pattern2		{ action2 }
        "#;

        let flex_file = parse_flex_file(input);

        assert_eq!(flex_file.all_states, vec!["INITIAL", "STATE1", "STATE2"]);

        assert_eq!(flex_file.definitions.len(), 0);

        assert_eq!(flex_file.rules.len(), 2);

        let rule1 = &flex_file.rules[0];
        assert_eq!(rule1.states, vec!["STATE1"]);
        assert_eq!(rule1.pattern, "pattern1");
        assert_eq!(rule1.actions.trim(), "{ action1 }");

        let rule2 = &flex_file.rules[1];
        assert_eq!(rule2.states, vec!["STATE2"]);
        assert_eq!(rule2.pattern, "pattern2");
        assert_eq!(rule2.actions.trim(), "{ action2 }");

        assert_eq!(flex_file.rest_code, "");
    }
}
