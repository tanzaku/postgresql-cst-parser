use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque, hash_map::Entry};

// use serde::{Deserialize, Serialize};

use crate::parser_generator::{
    bison::{Action, AssocDirective, Component},
    lexer::TokenKind,
};

use super::{
    bison::{Assoc, Bison, ComponentId},
    id_mapper::IdMapper,
};

// #[derive(Debug, Serialize, Deserialize)]
#[derive(Debug)]
pub struct LalrRule {
    pub name_id: ComponentId,
    pub components: Vec<ComponentId>,
    pub reduce_priority: Option<Assoc>,
}

// #[derive(Debug, Serialize, Deserialize)]
#[derive(Debug)]
pub struct Lalr {
    pub id_mapper: IdMapper,

    pub assoc: Vec<Option<Assoc>>,
    pub rules: Vec<LalrRule>,
    pub rule_indices_by_name_id: Vec<Vec<usize>>,

    pub accept_rule_component_id: ComponentId,
    pub accept_rule_component: Component,
    pub end_rule_component_id: ComponentId,
    pub end_rule_component: Component,

    pub first_set: HashMap<ComponentId, Vec<ComponentId>>,
    pub first_set_after: Vec<Vec<BTreeSet<ComponentId>>>,
    pub nullable: Vec<Vec<bool>>,
    pub state_set: StateSet,
    pub action_table: HashMap<(usize, ComponentId), Action>,
    pub goto_table: HashMap<(usize, ComponentId), usize>,

    pub all_terminal: Vec<ComponentId>,
}

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Item {
    pub rule_index: usize,
    pub dot_pos: usize,
    pub lookahead: BTreeSet<ComponentId>,
}

impl Item {
    fn insert_lookaheads(&mut self, lookaheads: &BTreeSet<ComponentId>) -> bool {
        let prev_len = self.lookahead.len();
        self.lookahead.extend(lookaheads);
        self.lookahead.len() != prev_len
    }

    fn display(&self, lalr: &Lalr) -> String {
        let mut res = String::new();

        let rule = &lalr.rules[self.rule_index];
        res += &format!(
            "{} ->",
            lalr.id_mapper.components[rule.name_id.0 as usize].to_rule_string()
        );
        for j in 0..self.dot_pos {
            res += &format!(
                " {}",
                lalr.id_mapper.components[rule.components[j].0 as usize].to_rule_string()
            );
        }
        res += " .";
        for j in self.dot_pos..rule.components.len() {
            res += &format!(
                " {}",
                lalr.id_mapper.components[rule.components[j].0 as usize].to_rule_string()
            );
        }
        res
    }
}

// #[derive(Debug, Clone, Serialize, Deserialize)]
#[derive(Debug, Clone)]
pub struct State {
    pub items: Vec<Item>,
    pub edge: BTreeSet<(ComponentId, usize)>,
    pub item_indices: HashMap<usize, usize>,
}

impl State {
    // Difference for LALR: ignore lookahead symbols
    fn equals_without_lookahead(&self, other: &State) -> bool {
        if self.items.len() != other.items.len() {
            return false;
        }

        self.items
            .iter()
            .zip(&other.items)
            .all(|(l, r)| l.rule_index == r.rule_index && l.dot_pos == r.dot_pos)
    }

    // Difference for LR: Distinguish lookahead symbols
    fn equals(&self, other: &State) -> bool {
        if self.items.len() != other.items.len() {
            return false;
        }

        self.items.iter().zip(&other.items).all(|(l, r)| {
            l.rule_index == r.rule_index && l.dot_pos == r.dot_pos && l.lookahead == r.lookahead
        })
    }

    fn push_item(&mut self, item: Item) {
        if item.dot_pos == 0 {
            self.item_indices.insert(item.rule_index, self.items.len());
        }
        self.items.push(item);
    }
}

// #[derive(Debug, Serialize, Deserialize)]
#[derive(Debug)]
pub struct StateSet {
    pub states: Vec<State>,
    pub need_update: HashSet<usize>,
}

impl StateSet {
    fn add_state_lalr(
        &mut self,
        que: &mut VecDeque<usize>,
        from_index: usize,
        comp: ComponentId,
        state: State,
    ) {
        if let Some(i) = self
            .states
            .iter()
            .position(|s| state.equals_without_lookahead(s))
        {
            self.states[from_index].edge.insert((comp, i));

            // Skip if it's identical including lookahead symbols
            if state.equals(&self.states[i]) {
                return;
            }

            let mut updated = false;

            self.states[i]
                .items
                .iter_mut()
                .zip(state.items)
                .for_each(|(l, r)| {
                    let len = l.lookahead.len();

                    l.lookahead.extend(r.lookahead);

                    updated |= l.lookahead.len() != len;
                });

            if updated {
                self.need_update.insert(i);
                que.push_back(i);
            }
        } else {
            let i = self.states.len();
            self.states[from_index].edge.insert((comp, i));
            self.states.push(state);
            que.push_back(i);
            self.need_update.insert(i);
        }
    }
}

impl Lalr {
    pub fn new(bison: &Bison) -> Self {
        let mut id_mapper = IdMapper::new();

        let mut terminal_components = Vec::new();
        let mut non_terminal_components = Vec::new();

        for rule in &bison.rules {
            let name_comp = Component::NonTerminal(rule.name.clone());
            non_terminal_components.push(name_comp);
            rule.components
                .iter()
                .filter(|c| matches!(c, Component::Terminal(_)))
                .for_each(|c| terminal_components.push(c.clone()));
        }

        for name in bison.assoc.keys() {
            let kind = TokenKind::from(name);
            terminal_components.push(Component::Terminal(kind));
        }

        let accept_rule_component = Component::NonTerminal("$accept".to_string());
        let end_rule_component = Component::Terminal(TokenKind::RAW("$end".to_string()));

        non_terminal_components.push(accept_rule_component.clone());
        terminal_components.push(end_rule_component.clone());
        terminal_components.sort_by_key(|c| c.to_rule_identifier());
        terminal_components.dedup();

        terminal_components
            .iter()
            .for_each(|c| id_mapper.insert(c.clone()));
        non_terminal_components
            .iter()
            .for_each(|c| id_mapper.insert(c.clone()));

        let all_terminal = terminal_components
            .iter()
            .map(|c| id_mapper.to_component_id(c))
            .collect();

        let accept_rule_component_id = id_mapper.to_component_id(&accept_rule_component);
        let end_rule_component_id = id_mapper.to_component_id(&end_rule_component);

        let mut assoc = vec![None; id_mapper.len()];
        for (name, a) in &bison.assoc {
            let kind = TokenKind::from(name);
            let id = id_mapper.to_component_id(&Component::Terminal(kind));
            assoc[id.0 as usize] = Some(a.clone());
        }

        let mut rule_indices_by_name_id = vec![Vec::new(); id_mapper.len()];
        let mut lalr_rule: Vec<LalrRule> = Vec::new();
        for rule in &bison.rules {
            let name_comp = Component::NonTerminal(rule.name.clone());

            let name_id = id_mapper.to_component_id(&name_comp);

            let components = rule
                .components
                .iter()
                .map(|c| id_mapper.to_component_id(c))
                .collect();

            let reduce_priority: Option<Assoc> = rule
                .prec
                .as_ref()
                .map(|prec| Component::Terminal(TokenKind::from(prec)))
                .map(|c| id_mapper.to_component_id(&c))
                .or_else(|| {
                    rule.components
                        .iter().rfind(|c| matches!(c, Component::Terminal(_)))
                        .map(|c| id_mapper.to_component_id(c))
                })
                .and_then(|component_id| assoc[component_id.0 as usize].clone());

            let rule = LalrRule {
                name_id,
                components,
                reduce_priority,
            };

            rule_indices_by_name_id[name_id.0 as usize].push(lalr_rule.len());
            lalr_rule.push(rule);
        }

        Self {
            id_mapper,
            rules: lalr_rule,
            assoc,
            rule_indices_by_name_id,

            accept_rule_component,
            accept_rule_component_id,
            end_rule_component,
            end_rule_component_id,

            first_set: HashMap::new(),
            first_set_after: Vec::new(),
            nullable: Vec::new(),
            state_set: StateSet {
                states: Vec::new(),
                need_update: HashSet::new(),
            },
            action_table: HashMap::new(),
            goto_table: HashMap::new(),

            all_terminal,
        }
    }

    fn build_first_set(&mut self) {
        let mut nullable: HashMap<ComponentId, bool> = HashMap::new();
        let mut first_set: HashMap<ComponentId, HashSet<ComponentId>> = HashMap::new();

        first_set.insert(
            self.end_rule_component_id,
            HashSet::from([self.end_rule_component_id]),
        );
        nullable.insert(self.end_rule_component_id, false);

        for i in 0..self.rules.len() {
            let rule_id = self.rules[i].name_id;
            nullable.insert(rule_id, false);
            first_set.insert(rule_id, HashSet::new());

            for &c in &self.rules[i].components {
                let comp = &self.id_mapper.components[c.0 as usize];
                if let Component::Terminal(_) = comp {
                    nullable.insert(c, false);
                    first_set.insert(c, HashSet::from([c]));
                }
            }
        }

        loop {
            let mut updated = false;

            for ri in 0..self.rules.len() {
                let rule_id = self.rules[ri].name_id;
                let prev_len = first_set[&rule_id].len();
                let mut i = 0;
                while i < self.rules[ri].components.len() {
                    let set = first_set[&self.rules[ri].components[i]].clone();

                    first_set.get_mut(&rule_id).unwrap().extend(set);

                    if !nullable[&self.rules[ri].components[i]] {
                        break;
                    }

                    i += 1;
                }

                if i == self.rules[ri].components.len() && !nullable[&rule_id] {
                    *nullable.get_mut(&rule_id).unwrap() = true;
                    updated |= true;
                }
                updated |= prev_len != first_set[&rule_id].len();
            }

            if !updated {
                break;
            }
        }

        self.first_set_after = self
            .rules
            .iter()
            .map(|r| vec![Default::default(); r.components.len() + 1])
            .collect();

        self.nullable = self
            .rules
            .iter()
            .map(|r| vec![true; r.components.len() + 1])
            .collect();

        for i in 0..self.first_set_after.len() {
            for j in (0..self.rules[i].components.len()).rev() {
                let c = self.rules[i].components[j];

                if nullable[&c] {
                    self.first_set_after[i][j] = self.first_set_after[i][j + 1].clone();
                    self.nullable[i][j] &= self.nullable[i][j + 1];
                } else {
                    self.nullable[i][j] = false;
                }

                self.first_set_after[i][j].extend(first_set[&c].clone());
            }
        }

        self.first_set = first_set
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect();
    }

    // TODO: Test the closure function
    fn closure(&mut self, state: &mut State) {
        let mut in_deq = vec![false; state.items.len()];

        let prev_item_len = state.items.len();

        // Repeat until there are no more changes in the LR(1) item set for a single state
        let mut deq = VecDeque::from_iter(0..state.items.len());
        while let Some(j) = deq.pop_front() {
            in_deq[j] = false;

            let Item {
                rule_index,
                dot_pos,
                ..
            } = state.items[j];

            if dot_pos >= self.rules[rule_index].components.len() {
                continue;
            }

            // If the element after the dot is a non-terminal symbol, add an item with the dot at the beginning of all rules that have that non-terminal symbol on the left-hand side.
            let component_id = self.rules[rule_index].components[dot_pos];
            if let Component::Terminal(_) = &self.id_mapper.components[component_id.0 as usize] {
                continue;
            }

            let mut lookaheads = self.first_set_after[rule_index][dot_pos + 1].clone();
            let nullable = self.nullable[rule_index][dot_pos + 1];

            // The lookahead symbols are determined by first_set(continuation of non-terminal + lookahead)
            if nullable {
                state.items[j]
                    .lookahead
                    .iter()
                    .for_each(|c| lookaheads.extend(self.first_set[c].clone()));
            }

            self.rule_indices_by_name_id[component_id.0 as usize]
                .iter()
                .for_each(|&new_item_index| {
                    // Check if the item to be added already exists
                    let j: Option<&usize> = state.item_indices.get(&new_item_index);

                    if let Some(&j) = j {
                        // If it exists, only add lookahead symbols
                        if state.items[j].insert_lookaheads(&lookaheads) && !in_deq[j] {
                            deq.push_back(j);
                            in_deq[j] = true;
                        }
                    } else {
                        // If it doesn't exist, add it
                        let new_item = Item {
                            rule_index: new_item_index,
                            dot_pos: 0,
                            lookahead: lookaheads.clone(),
                        };

                        in_deq.push(true);
                        deq.push_back(state.items.len());
                        state.push_item(new_item);
                    }
                });
        }

        if prev_item_len != state.items.len() {
            state.items.sort_by_key(|it| (it.rule_index, it.dot_pos));
        }
    }

    /// Create a syntax analysis table
    /// 1. Create LR(1) item sets
    pub fn build_lalr1_parse_table(&mut self) {
        // In bison, if not explicitly specified, the first rule becomes the starting rule
        // In the case of PostgreSQL, it's not explicitly specified, so we use the first rule as the starting point
        let start_rule_index = self.rules.len();
        let start_component_id = self.rules[0].name_id;

        self.rules.push(LalrRule {
            name_id: self.accept_rule_component_id,
            components: vec![start_component_id],
            reduce_priority: None,
        });

        self.build_first_set();

        let mut state_set = StateSet {
            states: Vec::new(),
            need_update: HashSet::new(),
        };

        state_set.states.push({
            let initial_item = Item {
                rule_index: start_rule_index,
                dot_pos: 0,
                lookahead: BTreeSet::from([self.end_rule_component_id]),
            };

            State {
                items: vec![initial_item],
                edge: BTreeSet::new(),
                item_indices: HashMap::new(),
            }
        });

        self.closure(&mut state_set.states[0]);

        let mut que = VecDeque::new();
        que.push_back(0);
        state_set.need_update.insert(0);

        while let Some(i) = que.pop_front() {
            if !state_set.need_update.contains(&i) {
                continue;
            }
            state_set.need_update.remove(&i);

            // dbg!(i, state_set.states.len());

            // Create states by advancing the dot
            // Group the states where the dot is advanced by the next symbol
            let mut next_states: BTreeMap<ComponentId, Vec<_>> = BTreeMap::new();
            for j in 0..state_set.states[i].items.len() {
                let dot_pos = state_set.states[i].items[j].dot_pos;
                let ri = state_set.states[i].items[j].rule_index;
                if dot_pos >= self.rules[ri].components.len() {
                    continue;
                }

                let comp = self.rules[ri].components[dot_pos];

                let item = &state_set.states[i].items[j];
                next_states.entry(comp).or_default().push(Item {
                    rule_index: item.rule_index,
                    dot_pos: item.dot_pos + 1,
                    lookahead: item.lookahead.clone(),
                });
            }

            for (comp, items) in next_states {
                let mut state = State {
                    items,
                    edge: BTreeSet::new(),
                    item_indices: HashMap::new(),
                };

                self.closure(&mut state);
                state_set.add_state_lalr(&mut que, i, comp, state);
            }
        }

        dbg!(state_set.states.len());

        // Build the syntax analysis table
        let mut action_table: HashMap<(usize, ComponentId), Action> = HashMap::new();
        let mut goto_table: HashMap<(usize, ComponentId), usize> = HashMap::new();

        for (i, s) in state_set.states.iter().enumerate() {
            let reduce_rules: Vec<_> = s
                .items
                .iter()
                .filter(|item| self.rules[item.rule_index].components.len() == item.dot_pos)
                .collect();

            let get_conflicted_reduce_rule = |shift_comp: &ComponentId| -> Option<&&Item> {
                reduce_rules
                    .iter()
                    .find(|item| item.lookahead.contains(shift_comp))
            };

            for e in &s.edge {
                let key = (i, e.0);

                if let Component::NonTerminal(_) = &self.id_mapper.components[e.0.0 as usize] {
                    goto_table.insert(key, e.1);
                } else {
                    action_table.insert(key, Action::Shift(e.1));
                }
            }

            for terminal in &self.all_terminal {
                if let Some(item) = get_conflicted_reduce_rule(terminal) {
                    let reduce_action = if item.rule_index == start_rule_index {
                        Action::Accept
                    } else {
                        Action::Reduce(item.rule_index)
                    };

                    let key = (i, *terminal);

                    // not conflict
                    if let Entry::Vacant(e) = action_table.entry(key) {
                        e.insert(reduce_action);
                        continue;
                    }

                    // shift-reduce conflict
                    let shift = self.assoc[terminal.0 as usize].as_ref();
                    let reduce = self.rules[item.rule_index].reduce_priority.as_ref();

                    match (reduce, shift) {
                        (Some(reduce), Some(shift)) if reduce.priority < shift.priority => {
                            // adopt shift
                        }
                        (Some(reduce), Some(shift)) if reduce.priority > shift.priority => {
                            // adopt reduce
                            action_table.insert(key, reduce_action);
                        }
                        (Some(_), Some(shift)) => {
                            match shift.directive {
                                AssocDirective::NonAssoc => {
                                    // This case is a parse error
                                    action_table.insert(key, Action::Error);
                                }
                                AssocDirective::Left => {
                                    // adopt reduce
                                    action_table.insert(key, reduce_action);
                                }
                                AssocDirective::Right => {
                                    // adopt shift
                                }
                            }
                        }
                        _ => {
                            // If either one doesn't have a priority, shift seems to be preferred
                        }
                    }
                }
            }
        }

        self.state_set = state_set;
        self.action_table = action_table;
        self.goto_table = goto_table;
    }
}
