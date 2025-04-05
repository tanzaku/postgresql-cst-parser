#![allow(clippy::mutable_key_type)]
use std::collections::{BTreeMap, BTreeSet};

use crate::nfa::{NFAState, Transition, collect_epsilon_closure};

pub const INVALID_STATE: usize = !0;

/// Represents a state in a Deterministic Finite Automaton specialized for lexical analysis.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DFAState {
    /// Transition table mapping byte values (0-255) to destination state indices.
    /// Invalid transitions are marked as !0 (usize::MAX).
    pub transitions: [usize; 256],

    /// ID of the lexical rule that accepts this state.
    /// Lower values indicate higher priority rules (those appearing earlier in the flex definition).
    /// When multiple rules can match the same input, the rule with the lowest ID is selected.
    pub accept_lexer_rule_id: Option<u32>,
}

pub struct DFA {
    pub states: Vec<DFAState>,
}

impl DFA {
    pub fn match_bytes(&self, bs: &[u8]) -> Option<u32> {
        let mut state = 0;

        // The initial state may be an accepting state, as with patterns like '.*'
        let mut accepted = self.states[state].accept_lexer_rule_id;

        for &b in bs {
            let next_state = self.states[state].transitions[b as usize];
            if next_state == INVALID_STATE {
                break;
            }

            if self.states[next_state].accept_lexer_rule_id.is_some() {
                accepted = self.states[next_state].accept_lexer_rule_id;
            }
            state = next_state;
        }

        // After processing all input, check for an EOF transition
        // EOF is represented as 0 (byte value) for simplicity, similar to null-terminated strings
        let next_state = self.states[state].transitions[0];
        if next_state != INVALID_STATE && self.states[next_state].accept_lexer_rule_id.is_some() {
            accepted = self.states[next_state].accept_lexer_rule_id;
        }

        accepted
    }

    pub fn match_string(&self, s: &str) -> Option<u32> {
        self.match_bytes(s.as_bytes())
    }
}

impl<'a> From<&'a NFAState<'a>> for DFA {
    /// Subset construction algorithm
    fn from(start_state: &'a NFAState<'a>) -> Self {
        construct_dfa_with_state_mapping(start_state, None)
    }
}

/// Selects the highest priority lexical rule (lowest ID) from all accepting states.
/// This implements Flex-style rule selection semantics, where earlier rules in the
/// definition file have higher priority when multiple rules match the same input.
fn select_highest_priority_rule(set: &BTreeSet<&NFAState>) -> Option<u32> {
    set.iter()
        .fold(None, |acc, s| match *s.accept_lexer_rule_id.borrow() {
            Some(v) if v < acc.unwrap_or(u32::MAX) => Some(v),
            _ => acc,
        })
}

/// Subset construction algorithm
pub fn construct_dfa_with_state_mapping<'a>(
    start_state: &'a NFAState<'a>,
    mut dfa_to_nfa: Option<&mut Vec<Vec<usize>>>,
) -> DFA {
    let mut first_set = BTreeSet::new();
    collect_epsilon_closure(&mut first_set, start_state);

    if let Some(dfa_to_nfa) = dfa_to_nfa.as_mut() {
        dfa_to_nfa.push(first_set.iter().map(|s| s.state_id).collect());
    }

    let mut nfa_map = BTreeMap::new();
    nfa_map.insert(first_set.clone(), 0);

    let mut dfa_states = Vec::new();
    dfa_states.push(DFAState {
        transitions: [INVALID_STATE; 256],
        accept_lexer_rule_id: select_highest_priority_rule(&first_set),
    });

    let mut nfa_states_vec = vec![first_set];

    while let Some(nfa_states) = nfa_states_vec.pop() {
        let src_state_index = *nfa_map.get(&nfa_states).unwrap();

        for b in 0..=255 {
            let mut next_set = BTreeSet::new();

            let t = Transition::Char(b);

            for nfa_state in &nfa_states {
                if let Some(new_states) = nfa_state.transitions.borrow().get(&t) {
                    for &new_state in new_states {
                        collect_epsilon_closure(&mut next_set, new_state);
                    }
                }
            }

            if next_set.is_empty() {
                continue;
            }

            let dst_state_index = if let Some(index) = nfa_map.get(&next_set) {
                *index
            } else {
                let index = nfa_map.len();

                let accept_lexer_rule_id = select_highest_priority_rule(&next_set);

                if let Some(dfa_to_nfa) = dfa_to_nfa.as_mut() {
                    dfa_to_nfa.push(next_set.iter().map(|s| s.state_id).collect());
                }

                nfa_map.insert(next_set.clone(), index);
                nfa_states_vec.push(next_set);

                dfa_states.push(DFAState {
                    transitions: [!0; 256],
                    accept_lexer_rule_id,
                });

                index
            };

            dfa_states[src_state_index].transitions[b as usize] = dst_state_index;
        }
    }

    DFA { states: dfa_states }
}

#[cfg(test)]
mod tests {
    use crate::nfa::NFA;

    use super::*;

    #[test]
    fn test_simple1() {
        let nfa = NFA::new();

        // a(b|c)*d
        let start = nfa.new_state();
        let s_a1 = nfa.new_state();
        let s_a2 = nfa.new_state();
        let s_bc1 = nfa.new_state();
        let s_bc2 = nfa.new_state();
        let s_b1 = nfa.new_state();
        let s_b2 = nfa.new_state();
        let s_c1 = nfa.new_state();
        let s_c2 = nfa.new_state();
        let s_kleene1 = nfa.new_state();
        let s_kleene2 = nfa.new_state();
        let s_d1 = nfa.new_state();
        let s_d2 = nfa.new_state();

        // Concatenation
        s_a1.add_transition(s_a2, Transition::Char('a' as u8));

        // Alternation
        s_bc1.add_transition(s_b1, Transition::Epsilon);
        s_bc1.add_transition(s_c1, Transition::Epsilon);
        s_b1.add_transition(s_b2, Transition::Char('b' as u8));
        s_c1.add_transition(s_c2, Transition::Char('c' as u8));
        s_b2.add_transition(s_bc2, Transition::Epsilon);
        s_c2.add_transition(s_bc2, Transition::Epsilon);

        // Kleene closure (zero or more repetitions)
        s_kleene1.add_transition(s_kleene2, Transition::Epsilon);
        s_kleene1.add_transition(s_bc1, Transition::Epsilon);
        s_bc2.add_transition(s_kleene2, Transition::Epsilon);
        s_bc2.add_transition(s_bc1, Transition::Epsilon);

        // Concatenation
        s_d1.add_transition(s_d2, Transition::Char('d' as u8));

        // Connecting the components
        start.add_transition(s_a1, Transition::Epsilon);
        s_a2.add_transition(s_bc1, Transition::Epsilon);
        s_bc2.add_transition(s_d1, Transition::Epsilon);

        s_d2.set_accept(0);

        assert!(nfa.matches_string(start, "abd"));
        assert!(nfa.matches_string(start, "acd"));
        assert!(nfa.matches_string(start, "abccbd"));
        assert!(!nfa.matches_string(start, "ad"));

        let dfa = DFA::from(start);
        assert_eq!(dfa.match_string("abd"), Some(0));
        assert_eq!(dfa.match_string("acd"), Some(0));
        assert_eq!(dfa.match_string("abccbd"), Some(0));
        assert_eq!(dfa.match_string("ad"), None);
    }
}
