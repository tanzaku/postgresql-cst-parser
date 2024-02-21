use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use serde::{Deserialize, Serialize};

use super::lexer::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Accept,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssocDirective {
    NonAssoc,
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Assoc {
    name: String,
    priority: usize,
    directive: AssocDirective,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum RawComponent {
    Identifier(String),
    Raw(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ComponentId(pub u16);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Component {
    NonTerminal(String),
    Terminal(TokenKind),
}

impl Component {
    pub fn to_rule_string(&self) -> String {
        match self {
            Component::NonTerminal(s) => s.clone(),
            Component::Terminal(s) => s.to_id(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rule {
    pub name: String,
    pub components: Vec<ComponentId>,
    pub prec: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Bison {
    /// %type
    /// key is non terminal symbol
    pub typ: HashMap<String, String>,
    /// %token
    /// key is terminal symbol
    pub token: HashMap<String, Option<String>>,
    pub assoc: HashMap<String, Assoc>,
    pub rules: Vec<Rule>,

    pub rule_names: Vec<String>,

    pub comments: Vec<Token>,

    pub components: Vec<Component>,
    pub component_map: HashMap<Component, ComponentId>,

    pub name_to_rules: HashMap<String, Vec<usize>>,

    pub first_set: HashMap<ComponentId, HashSet<ComponentId>>,
    pub nullable: HashMap<ComponentId, bool>,

    pub state_set: StateSet,
    pub action_table: HashMap<(usize, ComponentId), Action>,
    pub goto_table: HashMap<(usize, ComponentId), usize>,
    pub accept_rule_component_id: ComponentId,
    pub accept_rule_component: Component,
    pub end_rule_component_id: ComponentId,
    pub end_rule_component: Component,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Item {
    pub rule_index: usize,
    pub dot_pos: usize,
    pub lookahead: Vec<ComponentId>,
    pub diff_lookaheads: Vec<ComponentId>,
}

impl Item {
    fn add_lookahead(&mut self, lookahead: ComponentId) -> bool {
        let mut insert_index = self.lookahead.len();

        for i in 0..self.lookahead.len() {
            match self.lookahead[i].cmp(&lookahead) {
                std::cmp::Ordering::Equal => {
                    return false;
                }
                std::cmp::Ordering::Greater => {
                    insert_index = i;
                    break;
                }
                _ => (),
            }
        }

        self.lookahead.insert(insert_index, lookahead.clone());
        self.diff_lookaheads.push(lookahead);
        true
    }

    fn display(&self, bison: &Bison) -> String {
        let mut res = String::new();

        let rule = &bison.rules[self.rule_index];
        res += &format!("{} ->", rule.name);
        for j in 0..self.dot_pos {
            res += &format!(
                " {}",
                bison.components[rule.components[j].0 as usize].to_rule_string()
            );
        }
        res += " .";
        for j in self.dot_pos..rule.components.len() {
            res += &format!(
                " {}",
                bison.components[rule.components[j].0 as usize].to_rule_string()
            );
        }
        res
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct State {
    pub items: Vec<Item>,
    pub edge: Vec<(ComponentId, usize)>,
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
    fn equals_without_diff_lookahead(&self, other: &State) -> bool {
        if self.items.len() != other.items.len() {
            return false;
        }

        self.items.iter().zip(&other.items).all(|(l, r)| {
            l.rule_index == r.rule_index && l.dot_pos == r.dot_pos && l.lookahead == r.lookahead
        })
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
        // let _fg = ::flame::start_guard("add_state_lalr");
        if let Some(i) = self
            .states
            .iter()
            .position(|s| state.equals_without_lookahead(s))
        {
            self.states[from_index].edge.push((comp, i));

            // 先読み記号まで含めて同一ならスキップ
            if state.equals_without_diff_lookahead(&self.states[i]) {
                return;
            }

            let mut updated = false;

            // LALR用の差分。先読み記号をマージする
            let it = self.states[i].items.iter_mut().zip(state.items);
            for (l, r) in it {
                let mut res = Vec::with_capacity(l.lookahead.len() + r.lookahead.len());

                // merge sorted
                let mut i = 0;
                let mut j = 0;
                while i + j < l.lookahead.len() + r.lookahead.len() {
                    if j == r.lookahead.len() {
                        res.push(l.lookahead[i].clone());
                        i += 1;
                        continue;
                    }

                    if i == l.lookahead.len() || l.lookahead[i] > r.lookahead[j] {
                        res.push(r.lookahead[j].clone());
                        l.diff_lookaheads.push(r.lookahead[j].clone());
                        updated = true;
                        j += 1;
                    } else if l.lookahead[i] < r.lookahead[j] {
                        res.push(l.lookahead[i].clone());
                        i += 1;
                    } else {
                        res.push(l.lookahead[i].clone());
                        i += 1;
                        j += 1;
                    }
                }

                l.lookahead = res;
            }

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

impl Bison {
    pub fn is_terminal(&self, s: &str) -> bool {
        !self.is_non_terminal(s)
    }

    pub fn is_non_terminal(&self, s: &str) -> bool {
        self.rule_names.contains(&s.to_string())
    }

    pub fn to_component_id(&mut self, c: Component) -> ComponentId {
        if let Some(&id) = self.component_map.get(&c) {
            return id;
        }

        let id = ComponentId(self.components.len() as u16);
        self.components.push(c.clone());
        self.component_map.insert(c, id);
        id
    }

    pub fn get_priority_by_terminal_symbol(&self, component_id: &ComponentId) -> Option<Assoc> {
        let c = &self.components[component_id.0 as usize];
        self.assoc.get(&c.to_rule_string()).cloned()
    }

    pub fn get_shift_priority(&self, component_id: &ComponentId) -> Option<Assoc> {
        self.get_priority_by_terminal_symbol(component_id)
    }

    pub fn get_reduce_priority(&self, rule_index: usize) -> Option<Assoc> {
        if let Some(prec) = &self.rules[rule_index].prec {
            self.assoc.get(prec).cloned()
        } else {
            self.rules[rule_index]
                .components
                .iter()
                .filter(|c| matches!(self.components[c.0 as usize], Component::Terminal(_)))
                .last()
                .and_then(|c| self.get_priority_by_terminal_symbol(c))
        }
    }

    /// first set of comps
    pub fn first_set(&self, comps: Vec<&ComponentId>) -> Vec<ComponentId> {
        let mut set = HashSet::new();
        for c in comps {
            set.extend(self.first_set[&c].clone());
            if !self.nullable[&c] {
                break;
            }
        }
        set.into_iter().collect()
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
            let rule_id = self.to_component_id(Component::NonTerminal(self.rules[i].name.clone()));
            nullable.insert(rule_id, false);
            first_set.insert(rule_id, HashSet::new());

            for c in &self.rules[i].components {
                let comp = &self.components[c.0 as usize];
                if let Component::Terminal(_) = comp {
                    nullable.insert(c.clone(), false);
                    first_set.insert(c.clone(), HashSet::from([c.clone()]));
                }
            }
        }

        loop {
            let mut updated = false;

            for ri in 0..self.rules.len() {
                let rule_id =
                    self.to_component_id(Component::NonTerminal(self.rules[ri].name.clone()));
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

        self.first_set = first_set;
        self.nullable = nullable;
    }

    /// 構文解析表を作成する
    /// 1. LR(1)項集合の作成
    pub fn build_lr1_parse_table(&mut self) {
        self.accept_rule_component = Component::NonTerminal("$accept".to_string());
        self.accept_rule_component_id = self.to_component_id(self.accept_rule_component.clone());
        self.end_rule_component = Component::Terminal(TokenKind::RAW("$end".to_string()));
        self.end_rule_component_id = self.to_component_id(self.end_rule_component.clone());

        self.build_first_set();

        for (i, rule) in self.rules.iter().enumerate() {
            self.name_to_rules
                .entry(rule.name.clone())
                .or_default()
                .push(i);
        }

        let start_rule = self.rule_names[0].clone();
        let start_rule_index = self.rules.len();
        let start_component_id = self.to_component_id(Component::NonTerminal(start_rule.clone()));
        self.rules.push(Rule {
            name: self.accept_rule_component.to_rule_string(),
            components: vec![start_component_id],
            prec: None,
        });

        self.rule_names
            .push(self.rules.last().unwrap().name.clone());

        let mut state_set = StateSet {
            states: Vec::new(),
            need_update: HashSet::new(),
        };

        state_set.states.push({
            let initial_item = Item {
                rule_index: start_rule_index,
                dot_pos: 0,
                lookahead: vec![self.end_rule_component_id],
                diff_lookaheads: vec![self.end_rule_component_id],
            };

            State {
                items: vec![initial_item],
                edge: Vec::new(),
            }
        });

        let mut map: HashMap<usize, Option<usize>> = HashMap::new();
        let mut done: HashSet<(usize, ComponentId)> = HashSet::new();

        closure(self, &mut state_set.states[0], &mut map, &mut done);

        fn closure(
            bison: &Bison,
            state: &mut State,
            map: &mut HashMap<usize, Option<usize>>,
            done: &mut HashSet<(usize, ComponentId)>,
        ) {
            map.clear();
            done.clear();

            let prev_item_len = state.items.len();

            // LR(1)アイテム集合の単一状態の変化がなくなるまで繰り返す
            let mut deq = VecDeque::from_iter(0..state.items.len());
            while let Some(j) = deq.pop_front() {
                let Item {
                    rule_index,
                    dot_pos,
                    ..
                } = state.items[j];

                if dot_pos >= bison.rules[rule_index].components.len()
                    || state.items[j].diff_lookaheads.is_empty()
                {
                    continue;
                }

                // ドットの次の要素が非終端記号の場合には、その非終端記号を左辺に持つ全ての規則について、非終端記号の先頭にドットおるアイテムを追加する。
                if let Component::NonTerminal(comp) =
                    &bison.components[bison.rules[rule_index].components[dot_pos].0 as usize]
                {
                    // その際の先読み記号は、first_set(非終端記号の続き + lookahead)で求まる
                    let lookaheads: Vec<ComponentId> = state.items[j]
                        .diff_lookaheads
                        .iter()
                        .filter(|&l| done.insert((j, l.clone())))
                        .flat_map(|a| {
                            let after_comp = bison.rules[rule_index].components[dot_pos + 1..]
                                .into_iter()
                                .chain([a])
                                .collect::<Vec<_>>();

                            bison.first_set(after_comp)
                        })
                        .collect();

                    bison.name_to_rules[comp]
                        .iter()
                        .for_each(|&new_item_index| {
                            // 追加予定のアイテムが既に存在するかチェックする
                            let j: Option<usize> =
                                *map.entry(new_item_index).or_insert_with(|| {
                                    state.items.iter().position(|it| {
                                        it.rule_index == new_item_index && it.dot_pos == 0
                                    })
                                });

                            // なければ追加
                            if j.is_none() {
                                let new_item = Item {
                                    rule_index: new_item_index,
                                    dot_pos: 0,
                                    lookahead: lookaheads.clone(),
                                    diff_lookaheads: lookaheads.clone(),
                                };

                                deq.push_back(state.items.len());
                                map.insert(new_item_index, Some(state.items.len()));
                                state.items.push(new_item);
                                return;
                            }

                            // あれば先読み記号のみ追加
                            let j = j.unwrap();
                            for lookahead in &lookaheads {
                                if state.items[j].add_lookahead(lookahead.clone()) {
                                    deq.push_back(j);
                                }
                            }
                        })
                }
            }

            for i in prev_item_len..state.items.len() {
                state.items[i].lookahead.sort();
                state.items[i].lookahead.dedup();
            }

            if prev_item_len != state.items.len() {
                state.items.sort();
            }
        }

        let mut que = VecDeque::new();
        que.push_back(0);
        state_set.need_update.insert(0);

        while let Some(i) = que.pop_front() {
            if !state_set.need_update.contains(&i) {
                continue;
            }
            state_set.need_update.remove(&i);

            dbg!(i, state_set.states.len());

            // ドットを進めた状態を作る
            // ドットを進める状態を、次の記号でグループ化
            let mut next_states: HashMap<ComponentId, Vec<Item>> = HashMap::new();
            for j in 0..state_set.states[i].items.len() {
                let dot_pos = state_set.states[i].items[j].dot_pos;
                let ri = state_set.states[i].items[j].rule_index;
                if dot_pos >= self.rules[ri].components.len() {
                    continue;
                }

                let comp = self.rules[ri].components[dot_pos].clone();

                next_states
                    .entry(comp)
                    .or_default()
                    .push(state_set.states[i].items[j].clone());
            }

            // pos を進める
            next_states
                .iter_mut()
                .for_each(|(_, v)| v.iter_mut().for_each(|it| it.dot_pos += 1));

            for (kind, items) in next_states {
                let mut state = State {
                    items,
                    edge: Vec::new(),
                };

                closure(self, &mut state, &mut map, &mut done);
                state_set.add_state_lalr(&mut que, i, kind, state);
            }
        }

        dbg!(state_set.states.len());

        // 構文解析表を構築
        let mut action_table: HashMap<(usize, ComponentId), Action> = HashMap::new();
        let mut goto_table: HashMap<(usize, ComponentId), usize> = HashMap::new();

        for (i, s) in state_set.states.iter().enumerate() {
            for e in &s.edge {
                match &self.components[e.0 .0 as usize] {
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
                                    let reduce_priority = self.get_reduce_priority(item.rule_index);

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

fn is_start_whitespace(line: impl AsRef<str>) -> bool {
    line.as_ref()
        .chars()
        .next()
        .map_or(false, |c| c.is_ascii_whitespace())
}

fn parse_type(bison: &mut Bison, line: &str, deq: &mut VecDeque<String>) {
    let mut line = line["%type ".len()..].to_string();
    let line_sep = line
        .split_ascii_whitespace()
        .map(str::to_string)
        .collect::<Vec<_>>();

    let p: &[_] = &['<', '>'];
    let typ = line_sep[0].trim_matches(p);

    line = line_sep[1..].join(" ").to_string();

    loop {
        for non_terminal_symbol in line.split_whitespace() {
            bison
                .typ
                .insert(non_terminal_symbol.to_string(), typ.to_string());
        }

        // 空白スタートの場合継続業とみなす
        if deq.front().map_or(false, is_start_whitespace) {
            line = deq.pop_front().unwrap();
        } else {
            break;
        }
    }
}

fn parse_token(bison: &mut Bison, line: &str, deq: &mut VecDeque<String>) {
    let mut line = line["%token ".len()..].to_string();
    let line_sep = line
        .split_ascii_whitespace()
        .map(str::to_string)
        .collect::<Vec<_>>();

    let typ = if line_sep[0].starts_with('<') {
        let p: &[_] = &['<', '>'];
        line = line_sep[1..].join(" ");
        Some(line_sep[0].trim_matches(p).to_string())
    } else {
        line = line_sep.join(" ");
        None
    };

    loop {
        for terminal_symbol in line.split_whitespace() {
            bison.token.insert(terminal_symbol.to_string(), typ.clone());
        }

        // 空白スタートの場合継続業とみなす
        if deq
            .front()
            .map_or(false, |line| is_start_whitespace(line) || line.is_empty())
        {
            line = deq.pop_front().unwrap();
        } else {
            break;
        }
    }
}

fn parse_assoc(
    bison: &mut Bison,
    line: &str,
    deq: &mut VecDeque<String>,
    directive: AssocDirective,
) {
    let mut line = line
        .split_ascii_whitespace()
        .skip(1)
        .collect::<Vec<_>>()
        .join(" ");

    let priority = bison.assoc.len();

    loop {
        for name in line.split_whitespace() {
            let assoc = Assoc {
                name: name.to_string(),
                priority,
                directive,
            };

            bison.assoc.insert(name.to_string(), assoc);
        }

        // 空白スタートの場合継続業とみなす
        if deq.front().map_or(false, is_start_whitespace) {
            line = deq.pop_front().unwrap();
        } else {
            break;
        }
    }
}

#[derive(Debug, Clone)]
pub struct Range {
    start_byte_pos: usize,
    start_row: usize,
    start_col: usize,
    end_byte_pos: usize,
    end_row: usize,
    end_col: usize,
}

#[derive(Debug, Clone)]
pub enum BisonToken {
    Colon { range: Range },
    SemiColon { range: Range },
    Identifier { range: Range, value: String },
    Literal { range: Range, value: String },
    VerticalBar { range: Range },
    Prec { range: Range, value: String },
}

impl BisonToken {
    pub fn identifier(&self) -> Option<String> {
        match self {
            BisonToken::Identifier { value, .. } => Some(value.clone()),
            _ => None,
        }
    }
}

/// scan bison's grammar file
fn scan(body: String) -> Vec<BisonToken> {
    let mut tokens = Vec::new();

    let mut row = 0;
    let mut col = 0;
    let mut byte_pos = 0;

    fn update_range(
        chars: &[char],
        i: &mut usize,
        row: &mut usize,
        col: &mut usize,
        byte_pos: &mut usize,
    ) {
        let c = chars[*i];

        if c == '\r' {
            *i += 1;
            *byte_pos += c.len_utf8();
            return;
        }

        if c == '\n' {
            *i += 1;
            *byte_pos += c.len_utf8();
            *row += 1;
            *col = 0;
            return;
        }

        *i += 1;
        *byte_pos += c.len_utf8();
        *col += 1;
    }

    fn range(s: &str, row: usize, col: usize, byte_pos: usize) -> Range {
        Range {
            start_byte_pos: byte_pos,
            start_row: row,
            start_col: col,
            end_byte_pos: byte_pos + s.len(),
            end_row: row,
            end_col: col + s.len(),
        }
    }

    let chars = body.chars().collect::<Vec<_>>();

    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];

        if c.is_ascii_whitespace() {
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == ':' {
            tokens.push(BisonToken::Colon {
                range: range(&c.to_string(), row, col, byte_pos),
            });
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == ';' {
            tokens.push(BisonToken::SemiColon {
                range: range(&c.to_string(), row, col, byte_pos),
            });
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == '|' {
            tokens.push(BisonToken::VerticalBar {
                range: range(&c.to_string(), row, col, byte_pos),
            });
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == '{' {
            let mut depth = 0;
            loop {
                match chars[i] {
                    '{' => depth += 1,
                    '}' => depth -= 1,
                    _ => (),
                }

                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                if depth == 0 {
                    break;
                }
            }

            continue;
        }

        if c == '/' {
            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);

            let c: char = chars[i];
            if c == '/' {
                while chars[i] != '\n' {
                    update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                }

                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                continue;
            }

            if c == '*' {
                while !chars[i..].starts_with(&['*', '/']) {
                    update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                }

                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
                continue;
            }

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
        }

        if c == '\'' {
            let mut value = String::new();

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);

            while chars[i] != '\'' {
                value.push(chars[i]);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            }

            tokens.push(BisonToken::Literal {
                range: Range {
                    start_byte_pos: byte_pos - value.len(),
                    start_row: row,
                    start_col: col - value.chars().count(),
                    end_byte_pos: byte_pos,
                    end_row: row,
                    end_col: col,
                },
                value,
            });

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            continue;
        }

        if c == '%' {
            let mut value = String::new();

            update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            loop {
                if !chars[i].is_ascii_alphanumeric() && chars[i] != '_' {
                    break;
                }

                value.push(chars[i]);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            }

            tokens.push(BisonToken::Prec {
                range: Range {
                    start_byte_pos: byte_pos - value.len() - 1,
                    start_row: row,
                    start_col: col - value.chars().count() - 1,
                    end_byte_pos: byte_pos,
                    end_row: row,
                    end_col: col,
                },
                value,
            });

            continue;
        }

        if c.is_ascii_alphabetic() {
            let mut value = String::new();

            loop {
                if !chars[i].is_ascii_alphanumeric() && chars[i] != '_' {
                    break;
                }

                value.push(chars[i]);
                update_range(&chars, &mut i, &mut row, &mut col, &mut byte_pos);
            }

            tokens.push(BisonToken::Identifier {
                range: Range {
                    start_byte_pos: byte_pos - value.len(),
                    start_row: row,
                    start_col: col - value.chars().count(),
                    end_byte_pos: byte_pos,
                    end_row: row,
                    end_col: col,
                },
                value,
            });

            continue;
        }

        eprintln!("{}", chars[i..][..100].into_iter().collect::<String>());

        unreachable!();
    }

    tokens
}

fn parse_grammar_rules(bison: &mut Bison, body: String) {
    let tokens = scan(body);

    let mut raw_rules = Vec::new();

    for rules in tokens.split(|t| matches!(t, BisonToken::SemiColon { .. })) {
        if rules.is_empty() {
            continue;
        }

        if !matches!(rules[1], BisonToken::Colon { .. }) {
            panic!("expected colon");
        }

        let rule_name = rules[0].identifier().unwrap();
        bison.rule_names.push(rule_name.clone());

        for rule in rules[2..].split(|t| matches!(t, BisonToken::VerticalBar { .. })) {
            let mut prec = None;
            let mut rule = rule.to_vec();
            for i in (0..rule.len()).rev() {
                if matches!(rule[i], BisonToken::Prec { .. }) {
                    prec = Some(match &rule[i + 1] {
                        BisonToken::Identifier { value, .. } => value.clone(),
                        _ => unreachable!(),
                    });

                    rule.remove(i + 1);
                    rule.remove(i);
                }
            }
            let comp: Vec<RawComponent> = rule
                .iter()
                .filter_map(|t| match t {
                    BisonToken::Identifier { value, .. } => {
                        Some(RawComponent::Identifier(value.clone()))
                    }
                    BisonToken::Literal { value, .. } => Some(RawComponent::Raw(value.clone())),
                    _ => {
                        dbg!(t);
                        unreachable!()
                    }
                })
                .collect();

            raw_rules.push((rule_name.clone(), comp, prec.clone()));
        }
    }

    for (name, comp, prec) in raw_rules {
        let components: Vec<_> = comp
            .into_iter()
            .map(|c| match c {
                RawComponent::Identifier(s) if bison.is_non_terminal(&s) => {
                    Component::NonTerminal(s)
                }
                RawComponent::Identifier(s) => Component::Terminal(TokenKind::from(s)),
                RawComponent::Raw(s) => Component::Terminal(TokenKind::RAW(s)),
            })
            .collect();

        let components = components
            .into_iter()
            .map(|c| bison.to_component_id(c))
            .collect();

        bison.rules.push(Rule {
            name,
            components,
            prec,
        });
    }
}

pub fn parse_bison(s: impl AsRef<str>) -> Bison {
    let mut deq = s
        .as_ref()
        .lines()
        .map(str::to_owned)
        .collect::<VecDeque<_>>();

    let mut bison = Bison {
        typ: HashMap::new(),
        token: HashMap::new(),
        assoc: HashMap::new(),
        rules: Vec::new(),
        rule_names: Vec::new(),
        comments: Vec::new(),
        components: Vec::new(),
        component_map: HashMap::new(),

        name_to_rules: HashMap::new(),

        first_set: HashMap::new(),
        nullable: HashMap::new(),

        state_set: StateSet {
            states: Vec::new(),
            need_update: HashSet::new(),
        },
        action_table: HashMap::new(),
        goto_table: HashMap::new(),

        accept_rule_component: Component::NonTerminal("dummy".to_string()),
        accept_rule_component_id: ComponentId(0),
        end_rule_component: Component::NonTerminal("dummy".to_string()),
        end_rule_component_id: ComponentId(0),
    };

    while let Some(line) = deq.pop_front() {
        if line.starts_with("%%") {
            break;
        }

        match line.split_whitespace().next() {
            Some("%type") => parse_type(&mut bison, &line, &mut deq),
            Some("%token") => parse_token(&mut bison, &line, &mut deq),
            Some("%nonassoc") => parse_assoc(&mut bison, &line, &mut deq, AssocDirective::NonAssoc),
            Some("%left") => parse_assoc(&mut bison, &line, &mut deq, AssocDirective::Left),
            Some("%right") => parse_assoc(&mut bison, &line, &mut deq, AssocDirective::Right),
            _ => (),
        }
    }

    let mut rule_body: String = String::new();
    while let Some(line) = deq.pop_front() {
        if line.starts_with("%%") {
            break;
        }

        rule_body.push_str(&line);
        rule_body.push('\n');
    }

    parse_grammar_rules(&mut bison, rule_body);

    bison.build_lr1_parse_table();

    bison
}

#[cfg(test)]
mod tests {
    use super::parse_bison;

    #[test]
    fn test1() {
        let bison = parse_bison(
            r#"
%%
S: V EQUALS E
 | E
 ;

E: V;
V: X;
V: ASTERISK E;
%%
"#,
        );

        assert_eq!(bison.action_table.len(), 17);
    }

    #[test]
    fn test2() {
        let bison = parse_bison(
            r#"
%%
A: E EQUALS E
 | Id
 ;

E: E '+' T;
E: T;
T: Num;
T: Id;
%%
"#,
        );

        assert_eq!(bison.action_table.len(), 26);
    }

    #[test]
    fn test3() {
        let bison = parse_bison(
            r#"
%%
A: E EQUALS E
 | Id
 ;

E: E '+' E
| E '-' E
| E '*' E;
E: T;
T: Num;
T: Id;
%%
"#,
        );

        assert_eq!(bison.action_table.len(), 54);
    }

    #[test]
    fn test4() {
        let bison = parse_bison(include_str!("../../resources/gram.y"));
        assert_eq!(bison.state_set.states.len(), 6236);
    }
}
