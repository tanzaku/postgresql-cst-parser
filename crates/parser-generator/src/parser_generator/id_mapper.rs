use std::collections::{HashMap, hash_map::Entry};

// use serde::{Deserialize, Serialize};

use super::bison::{Component, ComponentId};

// #[derive(Debug, Serialize, Deserialize)]
#[derive(Debug)]
pub struct IdMapper {
    component_map: HashMap<Component, ComponentId>,
    pub components: Vec<Component>,
}

impl IdMapper {
    pub fn new() -> Self {
        Self {
            component_map: HashMap::new(),
            components: Vec::new(),
        }
    }

    pub fn contains(&mut self, c: &Component) -> bool {
        self.component_map.contains_key(c)
    }

    pub fn len(&self) -> usize {
        self.component_map.len()
    }

    pub fn insert(&mut self, c: Component) {
        if let Entry::Vacant(e) = self.component_map.entry(c.clone()) {
            let id = ComponentId(self.components.len() as u16);
            self.components.push(c);
            e.insert(id);
        }
    }

    pub fn to_component_id(&self, c: &Component) -> ComponentId {
        if let Some(&id) = self.component_map.get(c) {
            id
        } else {
            dbg!(c);
            panic!("Unknown component");
        }
    }
}
