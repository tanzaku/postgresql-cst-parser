#![allow(clippy::mutable_key_type)]
use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet},
};

use typed_arena::Arena;

type StateSet<'a> = BTreeSet<&'a NFAState<'a>>;
type ActiveStates<'a> = BTreeSet<StateSet<'a>>;

/// Represents a transition between NFA states.
/// Can be either an epsilon (empty) transition or a character transition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Transition {
    Epsilon,
    Char(u8),
}

/// Represents a state in a Non-deterministic Finite Automaton.
/// States are connected by transitions and can accept input.
#[allow(clippy::mutable_key_type)]
#[derive(Debug, Clone, Eq)]
pub struct NFAState<'a> {
    pub state_id: usize,
    pub transitions: RefCell<BTreeMap<Transition, Vec<&'a NFAState<'a>>>>,

    /// ID of the lexical rule that accepts this state.
    /// Lower values indicate higher priority rules (those appearing earlier in the flex definition).
    /// When multiple rules can match the same input, the rule with the lowest ID is selected.
    pub accept_lexer_rule_id: RefCell<Option<u32>>,
}

impl PartialEq for NFAState<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.state_id == other.state_id
    }
}

impl PartialOrd for NFAState<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NFAState<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.state_id.cmp(&other.state_id)
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

    pub fn set_accept(&self, matches_string: u32) {
        *self.accept_lexer_rule_id.borrow_mut() = Some(matches_string);
    }
}

/// Non-deterministic Finite Automaton implementation.
/// Used for efficient pattern matching with regular expressions.
pub struct NFA<'a> {
    next_state_id: Cell<usize>,
    arena: Arena<NFAState<'a>>,
}

/// Computes the epsilon closure of a state.
/// Returns the acceptance token of the highest-priority accepting state
/// in the closure, if any.
pub fn collect_epsilon_closure<'a>(
    epsilon_closure: &mut StateSet<'a>,
    s: &'a NFAState<'a>,
) -> Option<u32> {
    if !epsilon_closure.insert(s) {
        return None;
    }

    let mut candidate = vec![s];
    let mut accepted_lexer_rule_id = None;

    {
        let id = *s.accept_lexer_rule_id.borrow();
        if accepted_lexer_rule_id.unwrap_or(!0) > id.unwrap_or(!0) {
            accepted_lexer_rule_id = id;
        }
    }

    while let Some(s) = candidate.pop() {
        if let Some(new_states) = s.transitions.borrow().get(&Transition::Epsilon) {
            for &new_state in new_states {
                if epsilon_closure.insert(new_state) {
                    candidate.push(new_state);

                    let id = *new_state.accept_lexer_rule_id.borrow();
                    if accepted_lexer_rule_id.unwrap_or(!0) > id.unwrap_or(!0) {
                        accepted_lexer_rule_id = id;
                    }
                }
            }
        }
    }

    accepted_lexer_rule_id
}

impl<'a> NFA<'a> {
    pub fn new() -> NFA<'a> {
        let arena: Arena<NFAState<'a>> = Arena::new();

        NFA {
            next_state_id: Cell::new(0),
            arena,
        }
    }

    pub fn new_state(&'a self) -> &'a NFAState<'a> {
        let state_id = self.next_state_id.get();
        self.next_state_id.set(state_id + 1);

        self.arena.alloc(NFAState {
            state_id,
            transitions: RefCell::new(BTreeMap::new()),
            accept_lexer_rule_id: RefCell::new(None),
        })
    }

    /// Determines whether the NFA accepts the given byte sequence.
    pub fn matches_bytes(&self, start: &'a NFAState<'a>, bs: &[u8]) -> bool {
        let mut active_states = ActiveStates::new();

        let mut initial_states = StateSet::new();

        if collect_epsilon_closure(&mut initial_states, start).is_some() {
            return true;
        }

        active_states.insert(initial_states);

        for &b in bs {
            let mut next_active_states = ActiveStates::new();

            for state_set in &active_states {
                let mut next_state_set = StateSet::new();

                // All states reachable by epsilon transitions from the previous step have already been included
                for state in state_set {
                    if let Some(new_states) = state.transitions.borrow().get(&Transition::Char(b)) {
                        for &new_state in new_states {
                            if collect_epsilon_closure(&mut next_state_set, new_state).is_some() {
                                return true;
                            }
                        }
                    }
                }

                if next_state_set.is_empty() {
                    continue;
                }

                next_active_states.insert(next_state_set);
            }

            active_states = next_active_states;
        }

        false
    }

    pub fn matches_string(&self, start: &'a NFAState<'a>, s: &str) -> bool {
        self.matches_bytes(start, s.as_bytes())
    }
}

impl Default for NFA<'_> {
    fn default() -> Self {
        NFA::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_regexp_nfa_matching() {
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

        // Kleene closure
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
    }
}
