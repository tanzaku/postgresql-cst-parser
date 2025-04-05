use std::collections::{HashMap, HashSet};

use dfa::{DFA, INVALID_STATE};
use nfa::{NFA, NFAState, Transition};
use regexp::{RegexpNode, RegexpParser};

pub mod dfa;
pub mod nfa;
pub mod regexp;

/// A name-to-pattern mapping for use in regex pattern references.
/// Similar to Flex's %{ name definition %} directive.
pub struct NameDefinition {
    pub name: String,
    pub definition: String,
}

/// Definition of a pattern in Flex-compatible lexical analyzer.
/// Specifies which lexer states this pattern applies to.
pub struct FlexPatternDef {
    pub state_set: Vec<usize>,
    pub pattern: String,
}

/// A fragment of NFA with designated start and accept states.
/// Used to build larger NFAs from regex components.
pub struct NFAFragment<'a> {
    start_state: &'a NFAState<'a>,
    accept_state: &'a NFAState<'a>,
}

impl<'a> NFAFragment<'a> {
    /// Creates a new NFA fragment with fresh start and accept states.
    /// This fragment can be used as a building block for larger NFAs.
    pub fn new(nfa: &'a NFA<'a>) -> Self {
        NFAFragment {
            start_state: nfa.new_state(),
            accept_state: nfa.new_state(),
        }
    }
}

/// Builds an NFA structure from a regular expression AST node
pub fn build_nfa_from_regex_node<'a>(
    nfa: &'a NFA<'a>,
    regex_nfa: &NFAFragment<'a>,
    node: &RegexpNode,
    named_pattern_definitions: &HashMap<String, RegexpNode>,
) {
    let &NFAFragment {
        start_state: start,
        accept_state: accept,
    } = regex_nfa;

    match node {
        RegexpNode::Alternative(nodes) => {
            for node in nodes {
                let node_nfa @ NFAFragment {
                    start_state: node_start,
                    accept_state: node_accept,
                } = NFAFragment::new(nfa);

                build_nfa_from_regex_node(nfa, &node_nfa, node, named_pattern_definitions);

                start.add_transition(node_start, Transition::Epsilon);
                node_accept.add_transition(accept, Transition::Epsilon);
            }
        }
        RegexpNode::AnyOf(bytes) => {
            for byte in bytes {
                start.add_transition(accept, Transition::Char(*byte));
            }
        }
        RegexpNode::Char(c) => {
            start.add_transition(accept, Transition::Char(*c));
        }
        RegexpNode::Concatenation(nodes) => {
            let mut state = start;

            for node in nodes {
                let node_nfa @ NFAFragment {
                    start_state: node_start,
                    accept_state: node_accept,
                } = NFAFragment::new(nfa);

                build_nfa_from_regex_node(nfa, &node_nfa, node, named_pattern_definitions);

                state.add_transition(node_start, Transition::Epsilon);
                state = node_accept;
            }

            state.add_transition(accept, Transition::Epsilon);
        }
        RegexpNode::ZeroOrMore(node) => {
            let node_nfa @ NFAFragment {
                start_state: node_start,
                accept_state: node_accept,
            } = NFAFragment::new(nfa);

            build_nfa_from_regex_node(nfa, &node_nfa, node, named_pattern_definitions);

            start.add_transition(node_start, Transition::Epsilon);
            node_accept.add_transition(accept, Transition::Epsilon);
            node_accept.add_transition(node_start, Transition::Epsilon);
            start.add_transition(accept, Transition::Epsilon);
        }
        RegexpNode::OneOrMore(node) => {
            let node_nfa @ NFAFragment {
                start_state: node_start,
                accept_state: node_accept,
            } = NFAFragment::new(nfa);

            build_nfa_from_regex_node(nfa, &node_nfa, node, named_pattern_definitions);

            start.add_transition(node_start, Transition::Epsilon);
            node_accept.add_transition(accept, Transition::Epsilon);
            node_accept.add_transition(node_start, Transition::Epsilon);
        }
        RegexpNode::NoneOf(bytes) => {
            // Exclude NULL character (0x00) as it's reserved for the <<EOF>> marker
            for byte in 0x01..=0xFF {
                if !bytes.contains(&byte) {
                    start.add_transition(accept, Transition::Char(byte));
                }
            }
        }
        RegexpNode::Reference(name) => {
            let regexp_node = named_pattern_definitions
                .get(name)
                .unwrap_or_else(|| panic!("Referenced pattern '{}' not found", name));

            let node_nfa @ NFAFragment {
                start_state: node_start,
                accept_state: node_accept,
            } = NFAFragment::new(nfa);

            build_nfa_from_regex_node(nfa, &node_nfa, regexp_node, named_pattern_definitions);
            start.add_transition(node_start, Transition::Epsilon);
            node_accept.add_transition(accept, Transition::Epsilon);
        }
        RegexpNode::Repeat(node, min, max) => {
            let mut state = start;
            for i in 0..*max {
                if i >= *min {
                    state.add_transition(accept, Transition::Epsilon);
                }

                let node_nfa @ NFAFragment {
                    start_state: node_start,
                    accept_state: node_accept,
                } = NFAFragment::new(nfa);

                build_nfa_from_regex_node(nfa, &node_nfa, node, named_pattern_definitions);

                state.add_transition(node_start, Transition::Epsilon);
                state = node_accept;
            }
            state.add_transition(accept, Transition::Epsilon);
        }
    }
}

/// Compiles a set of Flex-compatible pattern definitions into DFA.
/// Creates one DFA per lexer state.
pub fn to_flex_pattern_dfa(
    pattern_defs: &[FlexPatternDef],
    name_definitions: &[NameDefinition],
) -> Vec<DFA> {
    let nfa = NFA::new();
    let mut regex_parser = RegexpParser::new();
    let mut named_pattern_definitions = HashMap::new();

    for name_definition in name_definitions {
        let regex = regex_parser.parse(&name_definition.definition);
        named_pattern_definitions.insert(name_definition.name.clone(), regex);
    }

    let num_state = pattern_defs
        .iter()
        .flat_map(|p| p.state_set.iter().cloned())
        .collect::<HashSet<_>>()
        .len();

    let state_start_node = (0..num_state).map(|_| nfa.new_state()).collect::<Vec<_>>();

    // パターン
    for (i, pattern_def) in pattern_defs.iter().enumerate() {
        let node = regex_parser.parse(&pattern_def.pattern);
        let regex_nfa = NFAFragment::new(&nfa);

        regex_nfa.accept_state.set_accept(i as u32);
        build_nfa_from_regex_node(&nfa, &regex_nfa, &node, &named_pattern_definitions);

        // そのパターンの状態を持つNFAを作成
        for &state_id in &pattern_def.state_set {
            state_start_node[state_id].add_transition(regex_nfa.start_state, Transition::Epsilon);
        }
    }

    state_start_node.into_iter().map(DFA::from).collect()
}

pub fn match_bytes_in_lexer_state(
    dfa: &DFA,
    bs: &[u8],
    dfa_to_state: &[u32],
    state_id: usize,
) -> Option<u32> {
    let mut state = 0;
    let mut accepted = None;

    let state_id_bit = 1 << state_id;

    for &b in bs {
        let next_state = dfa.states[state].transitions[b as usize];

        if next_state == INVALID_STATE || (dfa_to_state[next_state] & state_id_bit) == 0 {
            break;
        }

        state = next_state;
        if dfa.states[state].accept_lexer_rule_id.is_some() {
            accepted = dfa.states[state].accept_lexer_rule_id;
        }
    }

    accepted
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple1() {
        let pattern_defs = vec![
            FlexPatternDef {
                state_set: vec![0],
                pattern: "ad".to_string(),
            },
            FlexPatternDef {
                state_set: vec![1],
                pattern: "a(b|c)*d".to_string(),
            },
            FlexPatternDef {
                state_set: vec![0, 1],
                pattern: "w{name1}[0-9]z+".to_string(),
            },
            FlexPatternDef {
                state_set: vec![0, 1],
                pattern: "[^0-9A-Za-z]".to_string(),
            },
        ];
        let name_definitions = vec![NameDefinition {
            name: "name1".to_string(),
            definition: "x?y".to_string(),
        }];

        let dfa = to_flex_pattern_dfa(&pattern_defs, &name_definitions);

        assert_eq!(dfa[0].match_string("ad"), Some(0));
        assert_eq!(dfa[1].match_string("ad"), Some(1));
        assert_eq!(dfa[1].match_string("abd"), Some(1));
        assert_eq!(dfa[1].match_string("abbbbcbcbd"), Some(1));
        assert_eq!(dfa[1].match_string("abbbbcbcbd"), Some(1));
        assert_eq!(dfa[0].match_string("wxy0zzz"), Some(2));
        assert_eq!(dfa[1].match_string("wxy1zz"), Some(2));
        assert_eq!(dfa[0].match_string("wy9zz"), Some(2));
        assert_eq!(dfa[1].match_string("wx9zz"), None);
        assert_eq!(dfa[0].match_string("wxyz"), None);
        assert_eq!(dfa[0].match_string("0"), None);
        assert_eq!(dfa[0].match_string("a"), None);
        assert_eq!(dfa[0].match_string("A"), None);
        assert_eq!(dfa[0].match_string("@"), Some(3));
    }

    #[test]
    fn test_simple2() {
        let pattern_defs = vec![FlexPatternDef {
            state_set: vec![0],
            pattern: "[^']*".to_string(),
        }];
        let name_definitions = vec![];

        let dfa = to_flex_pattern_dfa(&pattern_defs, &name_definitions);

        assert_eq!(dfa[0].match_string("x"), Some(0));
        assert_eq!(dfa[0].match_string(""), Some(0));
        assert_eq!(dfa[0].match_string("'"), Some(0));
    }

    #[test]
    fn test_simple3() {
        let pattern_defs = vec![FlexPatternDef {
            state_set: vec![0],
            pattern: r#"[\+\-\*]"#.to_string(),
        }];
        let name_definitions = vec![];

        let dfa = to_flex_pattern_dfa(&pattern_defs, &name_definitions);

        assert_eq!(dfa[0].match_string("*"), Some(0));
    }
}
