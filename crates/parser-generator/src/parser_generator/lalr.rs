use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};

use serde::{Deserialize, Serialize};

use crate::parser_generator::{
    bison::{Action, AssocDirective, Component},
    lexer::TokenKind,
};

use super::{
    bison::{Assoc, Bison, ComponentId},
    id_mapper::IdMapper,
};

#[derive(Debug, Serialize, Deserialize)]
pub struct LalrRule {
    pub name_id: ComponentId,
    pub components: Vec<ComponentId>,
    pub reduce_priority: Option<Assoc>,
}

#[derive(Debug, Serialize, Deserialize)]
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
    pub lookaheads: Vec<Vec<BTreeSet<ComponentId>>>,
    pub nullable: Vec<Vec<bool>>,
    pub state_set: StateSet,
    pub action_table: HashMap<(usize, ComponentId), Action>,
    pub goto_table: HashMap<(usize, ComponentId), usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
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

    // fn add_lookahead(&mut self, lookahead: ComponentId) -> bool {
    //     self.lookahead.insert(lookahead)
    // }

    // fn display(&self, lalr: &Lalr, bison: &Bison) -> String {
    //     let mut res = String::new();

    //     let rule = &bison.rules[self.rule_index];
    //     res += &format!("{} ->", rule.name);
    //     for j in 0..self.dot_pos {
    //         res += &format!(
    //             " {}",
    //             lalr.components[rule.components[j].0 as usize].to_rule_string()
    //         );
    //     }
    //     res += " .";
    //     for j in self.dot_pos..rule.components.len() {
    //         res += &format!(
    //             " {}",
    //             lalr.components[rule.components[j].0 as usize].to_rule_string()
    //         );
    //     }
    //     res
    // }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct State {
    pub items: Vec<Item>,
    pub edge: Vec<(ComponentId, usize)>,
    pub item_indices: HashMap<usize, usize>,
}

impl State {
    // LALR用の差分。先読み記号を無視する
    fn equals_without_lookahead(&self, other: &State) -> bool {
        if self.items.len() != other.items.len() {
            return false;
        }

        self.items
            .iter()
            .zip(&other.items)
            .all(|(l, r)| l.rule_index == r.rule_index && l.dot_pos == r.dot_pos)
    }

    // LALR用の差分。先読み記号を無視する
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

#[derive(Debug, Serialize, Deserialize)]
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
            self.states[from_index].edge.push((comp, i));

            // 先読み記号まで含めて同一ならスキップ
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
            self.states[from_index].edge.push((comp, i));
            self.states.push(state);
            que.push_back(i);
            self.need_update.insert(i);
        }
    }
}

impl Lalr {
    pub fn new(bison: &Bison) -> Self {
        let mut id_mapper = IdMapper::new();

        let mut all_components = Vec::new();

        for rule in &bison.rules {
            let name_comp = Component::NonTerminal(rule.name.clone());
            all_components.push(name_comp);
            rule.components
                .iter()
                .for_each(|c| all_components.push(c.clone()));
        }

        for (name, _) in &bison.assoc {
            let kind = TokenKind::from(name);
            all_components.push(Component::Terminal(kind));
        }

        let accept_rule_component = Component::NonTerminal("$accept".to_string());
        let end_rule_component = Component::Terminal(TokenKind::RAW("$end".to_string()));

        all_components.push(accept_rule_component.clone());
        all_components.push(end_rule_component.clone());
        all_components.sort_by_key(|c| {
            let typ = match c {
                Component::Terminal(_) => 0,
                _ => 1,
            };

            (typ, c.to_rule_identifier())
        });

        all_components.into_iter().for_each(|c| id_mapper.insert(c));

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
                        .iter()
                        .filter(|c| matches!(c, Component::Terminal(_)))
                        .last()
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
            lookaheads: Vec::new(),
            nullable: Vec::new(),
            state_set: StateSet {
                states: Vec::new(),
                need_update: HashSet::new(),
            },
            action_table: HashMap::new(),
            goto_table: HashMap::new(),
        }
    }

    pub fn get_priority_by_terminal_symbol(&self, component_id: &ComponentId) -> Option<Assoc> {
        // let c = &self.components[component_id.0 as usize];
        self.assoc[component_id.0 as usize].clone()
    }

    pub fn get_shift_priority(&self, component_id: &ComponentId) -> Option<Assoc> {
        self.get_priority_by_terminal_symbol(component_id)
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

            for c in &self.rules[i].components {
                let comp = &self.id_mapper.components[c.0 as usize];
                if let Component::Terminal(_) = comp {
                    nullable.insert(c.clone(), false);
                    first_set.insert(c.clone(), HashSet::from([c.clone()]));
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

        self.lookaheads = self
            .rules
            .iter()
            .map(|r| vec![Default::default(); r.components.len() + 1])
            .collect();

        self.nullable = self
            .rules
            .iter()
            .map(|r| vec![true; r.components.len() + 1])
            .collect();

        for i in 0..self.lookaheads.len() {
            for j in (0..self.rules[i].components.len()).rev() {
                let c = self.rules[i].components[j];

                if nullable[&c] {
                    self.lookaheads[i][j] = self.lookaheads[i][j + 1].clone();
                    self.nullable[i][j] &= self.nullable[i][j + 1];
                } else {
                    self.nullable[i][j] = false;
                }

                self.lookaheads[i][j].extend(first_set[&c].clone());
            }
        }

        self.first_set = first_set
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect();
    }

    // TODO closureをテストする
    fn closure(&mut self, state: &mut State) {
        let mut in_deq = vec![false; state.items.len()];

        let prev_item_len = state.items.len();

        // LR(1)アイテム集合の単一状態の変化がなくなるまで繰り返す
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

            // ドットの次の要素が非終端記号の場合には、その非終端記号を左辺に持つ全ての規則について、非終端記号の先頭にドットおるアイテムを追加する。
            let component_id = self.rules[rule_index].components[dot_pos];
            if let Component::Terminal(_) = &self.id_mapper.components[component_id.0 as usize] {
                continue;
            }

            let mut lookaheads = self.lookaheads[rule_index][dot_pos + 1].clone();
            let nullable = self.nullable[rule_index][dot_pos + 1];

            // その際の先読み記号は、first_set(非終端記号の続き + lookahead)で求まる
            if nullable {
                state.items[j]
                    .lookahead
                    .iter()
                    .for_each(|c| lookaheads.extend(self.first_set[c].clone()));
            }

            self.rule_indices_by_name_id[component_id.0 as usize]
                .iter()
                .for_each(|&new_item_index| {
                    // 追加予定のアイテムが既に存在するかチェックする
                    let j: Option<&usize> = state.item_indices.get(&new_item_index);

                    if let Some(&j) = j {
                        // あれば先読み記号のみ追加
                        if state.items[j].insert_lookaheads(&lookaheads) && !in_deq[j] {
                            deq.push_back(j);
                            in_deq[j] = true;
                        }
                    } else {
                        // なければ追加
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

    /// 構文解析表を作成する
    /// 1. LR(1)項集合の作成
    pub fn build_lalr1_parse_table(&mut self) {
        // for (i, rule) in self.rules.iter().enumerate() {
        //     self.name_to_rules
        //         .entry(rule.name.clone())
        //         .or_default()
        //         .push(i);
        // }

        // bisonでは明示的に指定しない場合、最初のルールが起点のルールになる
        // PostgreSQLの場合、明示的に指定していないため、最初のルールを起点とする
        // let start_rule = self.rule_names[0].clone();
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
                edge: Vec::new(),
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

            // ドットを進めた状態を作る
            // ドットを進める状態を、次の記号でグループ化
            let mut next_states: BTreeMap<ComponentId, Vec<_>> = BTreeMap::new();
            for j in 0..state_set.states[i].items.len() {
                let dot_pos = state_set.states[i].items[j].dot_pos;
                let ri = state_set.states[i].items[j].rule_index;
                if dot_pos >= self.rules[ri].components.len() {
                    continue;
                }

                let comp = self.rules[ri].components[dot_pos].clone();

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
                    edge: Vec::new(),
                    item_indices: HashMap::new(),
                };

                self.closure(&mut state);
                state_set.add_state_lalr(&mut que, i, comp, state);
            }
        }

        dbg!(state_set.states.len());

        // 構文解析表を構築
        let mut action_table: HashMap<(usize, ComponentId), Action> = HashMap::new();
        let mut goto_table: HashMap<(usize, ComponentId), usize> = HashMap::new();

        for (i, s) in state_set.states.iter().enumerate() {
            for e in &s.edge {
                match &self.id_mapper.components[e.0 .0 as usize] {
                    Component::NonTerminal(_) => {
                        goto_table.insert((i, e.0.clone()), e.1);
                    }
                    Component::Terminal(_) => {
                        action_table.insert((i, e.0.clone()), Action::Shift(e.1));
                    }
                }
            }

            for item in &s.items {
                let rule = &self.rules[item.rule_index];
                if item.dot_pos >= rule.components.len() {
                    // ドットが最後まで進んでいる場合
                    // そのアイテムの先読み記号に対応する構文解析表のエントリに、
                    // そのアイテムの規則のインデックスを追加する
                    for lookahead in &item.lookahead {
                        let action = if item.rule_index == start_rule_index {
                            Action::Accept
                        } else {
                            Action::Reduce(item.rule_index)
                        };

                        let key = (i, lookahead.clone());

                        if action_table.contains_key(&key) {
                            match action_table.get(&key) {
                                Some(Action::Shift(_shift_state)) => {
                                    // shift-reduce conflict
                                    // https://guppy.eng.kagawa-u.ac.jp/2019/Compiler/bison-1.2.8/bison-ja_8.html#:~:text=%E8%A1%9D%E7%AA%81%E3%81%AE%E8%A7%A3%E6%B1%BA%E3%81%AF%E3%80%81%E5%95%8F%E9%A1%8C%E3%81%AB%E3%81%AA%E3%81%A3%E3%81%A6%E3%81%84%E3%82%8B%E8%A6%8F%E5%89%87%E3%81%AE%E5%84%AA%E5%85%88%E9%A0%86%E4%BD%8D%E3%81%A8%E3%80%81%20%E5%85%88%E8%AA%AD%E3%81%BF%E3%83%88%E3%83%BC%E3%82%AF%E3%83%B3%E3%81%AE%E5%84%AA%E5%85%88%E9%A0%86%E4%BD%8D%E3%81%AE%E6%AF%94%E8%BC%83%E3%81%AB%E3%82%88%E3%81%A3%E3%81%A6%E8%A1%8C%E3%82%8F%E3%82%8C%E3%81%BE%E3%81%99
                                    // 衝突の解決は、問題になっている規則の優先順位と、 先読みトークンの優先順位の比較によって行われます

                                    let shift_priority = self.get_shift_priority(lookahead);
                                    let reduce_priority =
                                        &self.rules[item.rule_index].reduce_priority;

                                    match (&shift_priority, &reduce_priority) {
                                        (Some(shift_priority), Some(reduce_priority)) => {
                                            match shift_priority
                                                .priority
                                                .cmp(&reduce_priority.priority)
                                            {
                                                std::cmp::Ordering::Equal => {
                                                    match shift_priority.directive {
                                                        AssocDirective::NonAssoc => {
                                                            // このケースはparse errorのため、action tableから削除してみる
                                                            action_table.remove(&key);
                                                        }
                                                        AssocDirective::Left => {
                                                            // reduceを採用するのでinsert
                                                            action_table.insert(key, action);
                                                        }
                                                        AssocDirective::Right => {
                                                            // shiftを採用するので何もしない
                                                        }
                                                    }
                                                }
                                                std::cmp::Ordering::Greater => {
                                                    // shiftを採用するので何もしない
                                                }
                                                std::cmp::Ordering::Less => {
                                                    // reduceを採用するのでinsert
                                                    action_table.insert(key, action);
                                                }
                                            }
                                        }
                                        (Some(_shift_priority), None) => {
                                            // bisonに合わせて？reduceを採用するのでinsert
                                            action_table.insert(key, action);
                                        }
                                        (None, Some(_reduce_priority)) => {
                                            // bisonの挙動に合わせてshiftを採用するので何もしない
                                        }
                                        (None, None) => {
                                            // Bisonは、演算子優先規則宣言で特に指定されていないかぎり、 シフトを選ぶことで衝突を解決するように設計されています。
                                            // shiftを採用するので何もしない
                                        }
                                    }
                                }
                                Some(Action::Reduce(reduce_rule_index)) => {
                                    // 同一の入力列に対して2個以上の規則が適用可能であると、 還元/還元衝突が起きます。
                                    dbg!(reduce_rule_index);
                                    panic!();
                                }
                                _ => panic!(),
                            };
                        } else {
                            action_table.insert(key, action);
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
