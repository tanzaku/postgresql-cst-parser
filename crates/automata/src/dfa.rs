use std::collections::{BTreeMap, BTreeSet};

use crate::nfa::{NFAState, Transition, epsilon_transition};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DFAState {
    pub transitions: [usize; 256],
    pub accept: Option<u32>,
}

pub struct DFA {
    pub states: Vec<DFAState>,
}

impl<'a> DFA {
    pub fn accept_bytes(&self, bs: &[u8]) -> Option<u32> {
        let mut state = 0;

        // '.*' のように初期状態で受理することがある
        let mut accepted = self.states[state].accept;

        for &b in bs {
            let next_state = self.states[state].transitions[b as usize];
            if next_state == !0 {
                break;
            }

            if self.states[next_state].accept.is_some() {
                accepted = self.states[next_state].accept;
            }
            state = next_state;
        }

        // 最後まで見たらEOFで遷移しないか確認
        // EOFはnull terminatedな文字列のイメージで簡易的に0にしている
        let next_state = self.states[state].transitions[0];
        if next_state != !0 && self.states[next_state].accept.is_some() {
            accepted = self.states[next_state].accept;
        }

        accepted
    }

    pub fn accept(&self, s: &str) -> Option<u32> {
        self.accept_bytes(s.as_bytes())
    }
}

impl<'a> From<&'a NFAState<'a>> for DFA {
    /// 部分集合構成法
    fn from(start_state: &'a NFAState<'a>) -> Self {
        dfa_from_nfa_with_nfa_id(start_state, None)
    }
}

fn accept(set: &BTreeSet<&NFAState>) -> Option<u32> {
    set.iter().fold(None, |acc, s| match *s.accept.borrow() {
        Some(v) if v < acc.unwrap_or(!0) => Some(v),
        _ => acc,
    })
}

/// 部分集合構成法
pub fn dfa_from_nfa_with_nfa_id<'a>(
    start_state: &'a NFAState<'a>,
    mut dfa_to_nfa: Option<&mut Vec<Vec<usize>>>,
) -> DFA {
    let mut first_set = BTreeSet::new();
    epsilon_transition(&mut first_set, start_state);

    if let Some(dfa_to_nfa) = dfa_to_nfa.as_mut() {
        dfa_to_nfa.push(first_set.iter().map(|s| s.index).collect());
    }

    let mut nfa_map = BTreeMap::new();
    nfa_map.insert(first_set.clone(), 0);

    let mut dfa_states = Vec::new();
    dfa_states.push(DFAState {
        transitions: [!0; 256],
        accept: accept(&first_set),
    });

    let mut nfa_states_vec = vec![first_set];

    while let Some(nfa_states) = nfa_states_vec.pop() {
        let src_index = *nfa_map.get(&nfa_states).unwrap();

        for b in 0..=255 {
            let mut next_set = BTreeSet::new();

            let t = Transition::Char(b);

            for nfa_state in &nfa_states {
                if let Some(new_states) = nfa_state.transitions.borrow().get(&t) {
                    for &new_state in new_states {
                        epsilon_transition(&mut next_set, new_state);
                    }
                }
            }

            if next_set.is_empty() {
                continue;
            }

            let dst_index = if let Some(index) = nfa_map.get(&next_set) {
                *index
            } else {
                let index = nfa_map.len();

                let accept = accept(&next_set);

                if let Some(dfa_to_nfa) = dfa_to_nfa.as_mut() {
                    dfa_to_nfa.push(next_set.iter().map(|s| s.index).collect());
                }

                nfa_map.insert(next_set.clone(), index);
                nfa_states_vec.push(next_set);

                dfa_states.push(DFAState {
                    transitions: [!0; 256],
                    accept,
                });

                index
            };

            dfa_states[src_index].transitions[b as usize] = dst_index;
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

        // 連接
        s_a1.add_transition(s_a2, Transition::Char('a' as u8));

        // 選択
        s_bc1.add_transition(s_b1, Transition::Epsilon);
        s_bc1.add_transition(s_c1, Transition::Epsilon);
        s_b1.add_transition(s_b2, Transition::Char('b' as u8));
        s_c1.add_transition(s_c2, Transition::Char('c' as u8));
        s_b2.add_transition(s_bc2, Transition::Epsilon);
        s_c2.add_transition(s_bc2, Transition::Epsilon);

        // Kleene閉包
        s_kleene1.add_transition(s_kleene2, Transition::Epsilon);
        s_kleene1.add_transition(s_bc1, Transition::Epsilon);
        s_bc2.add_transition(s_kleene2, Transition::Epsilon);
        s_bc2.add_transition(s_bc1, Transition::Epsilon);

        // 連接
        s_d1.add_transition(s_d2, Transition::Char('d' as u8));

        // パーツの結合
        start.add_transition(s_a1, Transition::Epsilon);
        s_a2.add_transition(s_bc1, Transition::Epsilon);
        s_bc2.add_transition(s_d1, Transition::Epsilon);

        s_d2.set_accept(0);

        assert!(nfa.accept(start, "abd"));
        assert!(nfa.accept(start, "acd"));
        assert!(nfa.accept(start, "abccbd"));
        assert!(!nfa.accept(start, "ad"));

        let dfa = DFA::from(start);
        assert_eq!(dfa.accept("abd"), Some(0));
        assert_eq!(dfa.accept("acd"), Some(0));
        assert_eq!(dfa.accept("abccbd"), Some(0));
        assert_eq!(dfa.accept("ad"), None);
    }
}
