use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet},
};

use typed_arena::Arena;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Transition {
    Epsilon,
    Char(u8),
}

#[derive(Debug, Clone, Eq)]
pub struct NFAState<'a> {
    pub index: usize,
    pub transitions: RefCell<BTreeMap<Transition, Vec<&'a NFAState<'a>>>>,
    pub accept: RefCell<Option<u32>>,
}

impl PartialEq for NFAState<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl PartialOrd for NFAState<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl Ord for NFAState<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<'a> NFAState<'a> {
    pub fn add_transition(&'a self, state: &'a NFAState<'a>, transition: Transition) {
        self.transitions
            .borrow_mut()
            .entry(transition)
            .or_default()
            .push(state);
    }

    pub fn set_accept(&self, accept: u32) {
        *self.accept.borrow_mut() = Some(accept);
    }
}

pub struct NFA<'a> {
    index: Cell<usize>,
    arena: Arena<NFAState<'a>>,
}

pub fn epsilon_transition<'a>(
    set: &mut BTreeSet<&'a NFAState<'a>>,
    s: &'a NFAState<'a>,
) -> Option<u32> {
    if !set.insert(s) {
        return None;
    }

    let mut candidate = vec![s];
    let mut accepted = None;

    {
        let new_accept = s.accept.borrow().clone();
        if accepted.unwrap_or(!0) > new_accept.unwrap_or(!0) {
            accepted = new_accept;
        }
    }

    while let Some(s) = candidate.pop() {
        if let Some(new_states) = s.transitions.borrow().get(&Transition::Epsilon) {
            for &new_state in new_states {
                if set.insert(new_state) {
                    candidate.push(new_state);

                    let new_accept = new_state.accept.borrow().clone();
                    if accepted.unwrap_or(!0) > new_accept.unwrap_or(!0) {
                        accepted = new_accept;
                    }
                }
            }
        }
    }

    accepted
}

impl<'a> NFA<'a> {
    pub fn new() -> NFA<'a> {
        let arena: Arena<NFAState<'a>> = Arena::new();

        NFA {
            index: Cell::new(0),
            arena,
        }
    }

    pub fn new_state(&'a self) -> &'a NFAState<'a> {
        let index = self.index.get();
        self.index.set(index + 1);

        self.arena.alloc(NFAState {
            index,
            transitions: RefCell::new(BTreeMap::new()),
            accept: RefCell::new(None),
        })
    }

    pub fn accept_bytes(&self, start: &'a NFAState<'a>, bs: &[u8]) -> bool {
        let mut set: BTreeSet<BTreeSet<&'a NFAState<'a>>> = BTreeSet::new();

        let mut first_set = BTreeSet::new();

        if epsilon_transition(&mut first_set, start).is_some() {
            return true;
        }

        set.insert(first_set);

        for &b in bs {
            let mut next_set = BTreeSet::new();

            for state_set in &set {
                let mut next_state_set = BTreeSet::new();

                // 前のステップでイプシロン遷移で到達可能な点にはすべて到達している
                for state in state_set {
                    // if let Some(_) = *state.accept.borrow() {
                    //     return true;
                    // }

                    if let Some(new_states) = state.transitions.borrow().get(&Transition::Char(b)) {
                        for &new_state in new_states {
                            if epsilon_transition(&mut next_state_set, new_state).is_some() {
                                return true;
                            }
                        }
                    }
                }

                if next_state_set.is_empty() {
                    continue;
                }

                // dbg!(b, next_state_set.len());
                next_set.insert(next_state_set);
            }

            set = next_set;
        }

        false
    }

    pub fn accept(&self, start: &'a NFAState<'a>, s: &str) -> bool {
        self.accept_bytes(start, s.as_bytes())
    }
}

#[cfg(test)]
mod tests {
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
    }
}
