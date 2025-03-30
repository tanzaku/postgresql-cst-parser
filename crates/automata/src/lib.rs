use std::collections::{HashMap, HashSet};

use dfa::DFA;
use nfa::{NFA, NFAState, Transition};
use regexp::{RegexpNode, RegexpParser};

pub mod dfa;
pub mod nfa;
pub mod regexp;

pub struct NameDefinition {
    pub name: String,
    pub definition: String,
}

pub struct FlexPatternDef {
    pub state_set: Vec<usize>,
    pub pattern: String,
}

pub struct RegexpNFA<'a> {
    start_state: &'a NFAState<'a>,
    accept_state: &'a NFAState<'a>,
}

impl<'a> RegexpNFA<'a> {
    pub fn new(nfa: &'a NFA<'a>) -> Self {
        RegexpNFA {
            start_state: nfa.new_state(),
            accept_state: nfa.new_state(),
        }
    }
}

pub fn construct_nfa_from_regex<'a>(
    nfa: &'a NFA<'a>,
    regex_nfa: &RegexpNFA<'a>,
    node: &RegexpNode,
    regex_node_map: &HashMap<String, RegexpNode>,
) {
    let &RegexpNFA {
        start_state: start,
        accept_state: accept,
    } = regex_nfa;

    match node {
        RegexpNode::Alternative(nodes) => {
            for node in nodes {
                let node_nfa @ RegexpNFA {
                    start_state: node_start,
                    accept_state: node_accept,
                } = RegexpNFA::new(nfa);

                construct_nfa_from_regex(nfa, &node_nfa, node, regex_node_map);

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
                let node_nfa @ RegexpNFA {
                    start_state: node_start,
                    accept_state: node_accept,
                } = RegexpNFA::new(nfa);

                construct_nfa_from_regex(nfa, &node_nfa, node, regex_node_map);

                state.add_transition(node_start, Transition::Epsilon);
                state = node_accept;
            }

            state.add_transition(accept, Transition::Epsilon);
        }
        RegexpNode::Kleene0(node) => {
            let node_nfa @ RegexpNFA {
                start_state: node_start,
                accept_state: node_accept,
            } = RegexpNFA::new(nfa);

            construct_nfa_from_regex(nfa, &node_nfa, node, regex_node_map);

            start.add_transition(node_start, Transition::Epsilon);
            node_accept.add_transition(accept, Transition::Epsilon);
            node_accept.add_transition(node_start, Transition::Epsilon);
            start.add_transition(accept, Transition::Epsilon);
        }
        RegexpNode::Kleene1(node) => {
            let node_nfa @ RegexpNFA {
                start_state: node_start,
                accept_state: node_accept,
            } = RegexpNFA::new(nfa);

            construct_nfa_from_regex(nfa, &node_nfa, node, regex_node_map);

            start.add_transition(node_start, Transition::Epsilon);
            node_accept.add_transition(accept, Transition::Epsilon);
            node_accept.add_transition(node_start, Transition::Epsilon);
        }
        RegexpNode::NoneOf(bytes) => {
            // NULL文字は<<EOF>>のために取っておきたいので除外しておく
            for byte in 0x01..=0xFF {
                if !bytes.contains(&byte) {
                    start.add_transition(accept, Transition::Char(byte));
                }
            }
        }
        RegexpNode::RefAutomata(name) => {
            let regexp_node = regex_node_map.get(name).unwrap();

            let node_nfa @ RegexpNFA {
                start_state: node_start,
                accept_state: node_accept,
            } = RegexpNFA::new(nfa);

            construct_nfa_from_regex(nfa, &node_nfa, regexp_node, regex_node_map);
            start.add_transition(node_start, Transition::Epsilon);
            node_accept.add_transition(accept, Transition::Epsilon);
        }
        RegexpNode::Repeat(node, min, max) => {
            let mut state = start;
            for i in 0..*max {
                if i >= *min {
                    state.add_transition(accept, Transition::Epsilon);
                }

                let node_nfa @ RegexpNFA {
                    start_state: node_start,
                    accept_state: node_accept,
                } = RegexpNFA::new(nfa);

                construct_nfa_from_regex(nfa, &node_nfa, &*node, regex_node_map);

                state.add_transition(node_start, Transition::Epsilon);
                state = node_accept;
            }
            state.add_transition(accept, Transition::Epsilon);
        }
    }
}

// fn traverse_nfa<'a, F>(nfa_state: &'a NFAState<'a>, mut f: F)
// where
//     F: FnMut(&'a NFAState<'a>),
// {
//     let mut visited = HashSet::new();
//     let mut stack = vec![nfa_state];
//     visited.insert(nfa_state.index);
//     f(nfa_state);

//     while let Some(state) = stack.pop() {
//         for transition in state.transitions.borrow().values() {
//             for &next_state in transition {
//                 if visited.insert(next_state.index) {
//                     f(next_state);
//                     stack.push(next_state);
//                 }
//             }
//         }
//     }
// }

// 全体で一つのDFAを作る
// pub fn to_flex_pattern_dfa(
//     pattern_defs: &[FlexPatternDef],
//     name_definitions: &[NameDefinition],
// ) -> (DFA, Vec<u32>) {
//     let nfa = NFA::new();
//     let mut regex_parser = RegexpParser::new();
//     let mut regex_node_map: HashMap<String, RegexpNode> = HashMap::new();

//     for name_definition in name_definitions {
//         let regex = regex_parser.parse(&name_definition.definition);
//         regex_node_map.insert(name_definition.name.clone(), regex);
//     }

//     let num_state = pattern_defs
//         .iter()
//         .flat_map(|p| p.state_set.iter().cloned())
//         .collect::<HashSet<_>>()
//         .len();

//     let state_root_nfa = (0..num_state).map(|_| nfa.new_state()).collect::<Vec<_>>();

//     // パターン
//     for (i, pattern_def) in pattern_defs.iter().enumerate() {
//         let node = regex_parser.parse(&pattern_def.pattern);
//         let regex_nfa = RegexpNFA::new(&nfa);

//         regex_nfa.accept_state.set_accept(i as u32);
//         construct_nfa_from_regex(&nfa, &regex_nfa, &node, &regex_node_map);

//         // そのパターンの状態を持つNFAを作成
//         for &state_id in &pattern_def.state_set {
//             state_root_nfa[state_id].add_transition(regex_nfa.start_state, Transition::Epsilon);
//         }
//     }

//     let mut nfa_to_state_map = HashMap::new();
//     for state_id in 0..num_state {
//         traverse_nfa(state_root_nfa[state_id], |state| {
//             nfa_to_state_map.insert(state.index, state_id);
//         });
//     }

//     let global_start_state = nfa.new_state();
//     for i in 0..num_state {
//         global_start_state.add_transition(state_root_nfa[i], Transition::Epsilon);
//     }

//     // dbg!(nfa.accept(global_start_state, "abd"));
//     // dbg!(nfa.accept(state_root_nfa[0], "abd"));
//     // dbg!(nfa.accept(state_root_nfa[1], "abd"));

//     let mut dfa_to_nfa = Vec::new();
//     let dfa = dfa_from_nfa_with_nfa_id(global_start_state, Some(&mut dfa_to_nfa));

//     // dbg!(dfa.accept("abd"));

//     let mut dfa_to_state = vec![0_u32; dfa.states.len()];
//     for i in 0..dfa.states.len() {
//         dfa_to_state[i] = dfa_to_nfa[i]
//             .iter()
//             .map(|s| nfa_to_state_map.get(s).map(|&i| 1 << i).unwrap_or(0))
//             .fold(0, |a, b| a | b);
//     }

//     (dfa, dfa_to_state)
// }

pub fn to_flex_pattern_dfa(
    pattern_defs: &[FlexPatternDef],
    name_definitions: &[NameDefinition],
) -> Vec<DFA> {
    let nfa = NFA::new();
    let mut regex_parser = RegexpParser::new();
    let mut regex_node_map: HashMap<String, RegexpNode> = HashMap::new();

    for name_definition in name_definitions {
        let regex = regex_parser.parse(&name_definition.definition);
        regex_node_map.insert(name_definition.name.clone(), regex);
    }

    let num_state = pattern_defs
        .iter()
        .flat_map(|p| p.state_set.iter().cloned())
        .collect::<HashSet<_>>()
        .len();

    let state_root_nfa = (0..num_state).map(|_| nfa.new_state()).collect::<Vec<_>>();

    // パターン
    for (i, pattern_def) in pattern_defs.iter().enumerate() {
        let node = regex_parser.parse(&pattern_def.pattern);
        let regex_nfa = RegexpNFA::new(&nfa);

        regex_nfa.accept_state.set_accept(i as u32);
        construct_nfa_from_regex(&nfa, &regex_nfa, &node, &regex_node_map);
        // dbg!(&node);

        // そのパターンの状態を持つNFAを作成
        for &state_id in &pattern_def.state_set {
            state_root_nfa[state_id].add_transition(regex_nfa.start_state, Transition::Epsilon);
        }
    }

    // let mut nfa_to_state_map = HashMap::new();
    // for state_id in 0..num_state {
    //     traverse_nfa(state_root_nfa[state_id], |state| {
    //         nfa_to_state_map.insert(state.index, state_id);
    //     });
    // }

    // let global_start_state = nfa.new_state();
    // for i in 0..num_state {
    //     global_start_state.add_transition(state_root_nfa[i], Transition::Epsilon);
    // }

    // dbg!(nfa.accept(global_start_state, "abd"));
    // dbg!(nfa.accept(global_start_state, "abd"));
    // dbg!(nfa.accept(state_root_nfa[0], ""));
    // dbg!(nfa.accept(state_root_nfa[0], "*"));
    // dbg!(nfa.accept(state_root_nfa[1], "abd"));

    // let mut dfa_to_nfa = Vec::new();
    // let dfa = dfa_from_nfa_with_nfa_id(global_start_state, Some(&mut dfa_to_nfa));

    // // dbg!(dfa.accept("abd"));

    // let mut dfa_to_state = vec![0_u32; dfa.states.len()];
    // for i in 0..dfa.states.len() {
    //     dfa_to_state[i] = dfa_to_nfa[i]
    //         .iter()
    //         .map(|s| nfa_to_state_map.get(s).map(|&i| 1 << i).unwrap_or(0))
    //         .fold(0, |a, b| a | b);
    // }

    // (dfa, dfa_to_state)

    state_root_nfa.into_iter().map(DFA::from).collect()
}

pub fn accept_bytes_with_state(
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
        // dbg!(state, next_state);

        if next_state == !0 || (dfa_to_state[next_state] & state_id_bit) == 0 {
            break;
        }

        // dbg!(&dfa.states[state].accept);
        // dbg!(&dfa.states[next_state].accept);

        state = next_state;
        if dfa.states[state].accept.is_some() {
            accepted = dfa.states[state].accept;
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

        assert_eq!(dfa[0].accept("ad"), Some(0));
        assert_eq!(dfa[1].accept("ad"), Some(1));
        assert_eq!(dfa[1].accept("abd"), Some(1));
        assert_eq!(dfa[1].accept("abbbbcbcbd"), Some(1));
        assert_eq!(dfa[1].accept("abbbbcbcbd"), Some(1));
        assert_eq!(dfa[0].accept("wxy0zzz"), Some(2));
        assert_eq!(dfa[1].accept("wxy1zz"), Some(2));
        assert_eq!(dfa[0].accept("wy9zz"), Some(2));
        assert_eq!(dfa[1].accept("wx9zz"), None);
        assert_eq!(dfa[0].accept("wxyz"), None);
        assert_eq!(dfa[0].accept("0"), None);
        assert_eq!(dfa[0].accept("a"), None);
        assert_eq!(dfa[0].accept("A"), None);
        assert_eq!(dfa[0].accept("@"), Some(3));
    }

    #[test]
    fn test_simple2() {
        let pattern_defs = vec![FlexPatternDef {
            state_set: vec![0],
            pattern: "[^']*".to_string(),
        }];
        let name_definitions = vec![];

        let dfa = to_flex_pattern_dfa(&pattern_defs, &name_definitions);

        assert_eq!(dfa[0].accept("x"), Some(0));
        assert_eq!(dfa[0].accept(""), Some(0));
        assert_eq!(dfa[0].accept("'"), Some(0));
    }

    #[test]
    fn test_simple3() {
        let pattern_defs = vec![FlexPatternDef {
            state_set: vec![0],
            pattern: r#"[\+\-\*]"#.to_string(),
            // pattern: r#"[\*]"#.to_string(),
        }];
        let name_definitions = vec![];

        let dfa = to_flex_pattern_dfa(&pattern_defs, &name_definitions);

        assert_eq!(dfa[0].accept("*"), Some(0));
    }
}
