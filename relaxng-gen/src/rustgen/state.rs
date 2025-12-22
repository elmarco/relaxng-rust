use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use crate::utils::strip_r_prefix;

use super::{ConfigRule, GenEnum, GenStruct};

#[derive(Debug)]
pub struct State {
    pub(crate) state: PatState,
    pub(crate) elem_count: HashMap<String, usize>,
    pub(crate) interleave_count: usize,
    pub(crate) group_count: usize,
    pub(crate) choice_count: usize,
    pub(crate) optional_count: usize,
    pub(crate) config: Option<ConfigRule>,
    // should have other counters?
}

impl State {
    fn new(state: PatState) -> Self {
        Self {
            state,
            elem_count: HashMap::new(),
            interleave_count: 0,
            group_count: 0,
            choice_count: 0,
            optional_count: 0,
            config: None,
        }
    }
}

// sort of xpath
pub(crate) fn xpath_from_iter<'a>(
    iter: impl Iterator<Item = &'a State>,
    with_count: bool,
) -> String {
    let mut path = String::new();
    let mut last: Option<&State> = None;
    for c in iter {
        match &c.state {
            PatState::Element {
                name,
                ..
            } => {
                path.push_str(&format!("/element[@name='{name}']"));
                if with_count {
                    let count =
                        last.and_then(|last| last.elem_count.get(name)).copied().unwrap_or(1);
                    path.push_str(&format!("[{count}]"));
                }
            }
            PatState::Attribute(attribute) => {
                path.push_str(&format!("/attribute[@name='{}']", attribute.name))
            }
            PatState::Choice {
                ..
            } => {
                path.push_str("/choice");
                if with_count {
                    path.push_str(&format!(
                        "[{}]",
                        last.map(|last| last.choice_count).unwrap_or(1)
                    ));
                }
            }
            PatState::Group {
                interleave: true,
            } => {
                path.push_str("/interleave");
                if with_count {
                    path.push_str(&format!(
                        "[{}]",
                        last.map(|last| last.interleave_count).unwrap_or(1)
                    ));
                }
            }
            PatState::Group {
                interleave: false,
            } => {
                path.push_str("/group");
                if with_count {
                    path.push_str(&format!("[{}]", last.map(|last| last.group_count).unwrap_or(1)))
                }
            }
            PatState::Optional => {
                path.push_str("/optional");
                if with_count {
                    path.push_str(&format!(
                        "[{}]",
                        last.map(|last| last.optional_count).unwrap_or(1)
                    ))
                }
            }
            PatState::OneOrMore => path.push_str("/oneOrMore"),
            PatState::ZeroOrMore => path.push_str("/zeroOrMore"),
            PatState::Ref {
                name,
                ..
            } => path.push_str(&format!("/ref[@name='{name}']")),
            PatState::Text => path.push_str("/text"),
            PatState::Value {
                value,
            } => path.push_str(&format!("/value[text()='{value}']")),
            PatState::DatatypeName {
                ..
            } => path.push_str("/data"),
        }
        last = Some(c);
    }
    path
}

pub(crate) fn mod_path_from_iter<'a>(iter: impl Iterator<Item = &'a State>) -> PathBuf {
    iter.filter_map(|c| c.state.mod_name(true)).collect()
}

/// Generate an XML XPath from the state stack (e.g., /element/child/@attr)
pub(crate) fn xml_xpath_from_iter<'a>(iter: impl Iterator<Item = &'a State>) -> String {
    let mut path = String::new();
    for c in iter {
        match &c.state {
            PatState::Element {
                name,
                ..
            } => {
                path.push('/');
                path.push_str(name);
            }
            PatState::Attribute(attribute) => {
                path.push_str("/@");
                path.push_str(&attribute.name);
            }
            PatState::Choice { .. }
            | PatState::Group { .. }
            | PatState::Optional
            | PatState::OneOrMore
            | PatState::ZeroOrMore
            | PatState::Ref { .. }
            | PatState::Text
            | PatState::Value { .. }
            | PatState::DatatypeName { .. } => {}
        }
    }
    if path.is_empty() {
        "/".to_string()
    } else {
        path
    }
}

#[derive(Debug)]
pub struct StateStack {
    pub(crate) states: Vec<State>,
    // cache to speed update from traversing the tree looking for Ref { recursive }
    pub(crate) recursive: usize,
}

impl StateStack {
    pub(crate) fn new() -> Self {
        Self {
            states: Vec::new(),
            recursive: 0,
        }
    }

    pub(crate) fn push(&mut self, state: PatState) {
        match state {
            PatState::Element {
                ref name,
                ..
            } => {
                if let Some(last) = self.states.last_mut() {
                    last.elem_count.entry(name.clone()).and_modify(|v| *v += 1).or_insert(1);
                }
            }
            PatState::Attribute(_) => (),
            PatState::Group {
                interleave: true,
            } => {
                if let Some(last) = self.states.last_mut() {
                    last.interleave_count += 1;
                }
            }
            PatState::Group {
                interleave: false,
            } => {
                if let Some(last) = self.states.last_mut() {
                    last.group_count += 1;
                }
            }
            PatState::Choice {
                ..
            } => {
                if let Some(last) = self.states.last_mut() {
                    last.choice_count += 1;
                }
            }
            PatState::Optional => {
                if let Some(last) = self.states.last_mut() {
                    last.optional_count += 1;
                }
            }
            PatState::OneOrMore => (),
            PatState::ZeroOrMore => (),
            PatState::Text => (),
            PatState::Value {
                ..
            } => (),
            PatState::DatatypeName {
                ..
            } => (),
            PatState::Ref {
                recursively,
                ..
            } => {
                if recursively {
                    self.recursive += 1;
                }
            }
        }
        self.states.push(State::new(state));
    }

    pub(crate) fn pop(&mut self) -> Option<(PatState, ConfigRule)> {
        self.states.pop().map(|s| {
            if let PatState::Ref {
                recursively,
                ..
            } = &s.state
                && *recursively
            {
                self.recursive -= 1;
            }
            (s.state, s.config.unwrap_or_default())
        })
    }

    pub(crate) fn iter_with_counter(
        &self,
        from_last_ref: bool,
    ) -> impl DoubleEndedIterator<Item = &State> {
        if from_last_ref
            && let Some(pos) =
                self.states.iter().rposition(|x| matches!(x.state, PatState::Ref { .. }))
        {
            return self.states[pos..].iter();
        }

        self.states.iter()
    }

    pub(crate) fn iter(&self) -> impl DoubleEndedIterator<Item = &PatState> {
        self.states.iter().map(|s| &s.state)
    }

    pub(crate) fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut PatState> {
        self.states.iter_mut().map(|s| &mut s.state)
    }

    pub(crate) fn iter_states_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut State> {
        self.states.iter_mut()
    }

    pub(crate) fn last_mut(&mut self) -> Option<&mut PatState> {
        self.states.last_mut().map(|s| &mut s.state)
    }

    pub(crate) fn last_enum_mut(&mut self) -> Option<&mut GenEnum> {
        for s in self.states.iter_mut().rev() {
            match &mut s.state {
                PatState::Choice {
                    gen_enum,
                    ..
                } => return Some(gen_enum),
                PatState::Ref {
                    ..
                } => continue,
                _ => return None,
            }
        }
        None
    }

    pub(crate) fn recursive_ref(&self) -> bool {
        self.recursive != 0
    }

    pub(crate) fn set_config(&mut self, config: ConfigRule) {
        let last = self.states.last_mut().expect("no current state");
        last.config = Some(config);
    }

    pub(crate) fn get_ref(&self) -> Option<Rc<RefCell<Ref>>> {
        for s in self.states.iter().rev() {
            match &s.state {
                PatState::Ref {
                    rf,
                    ..
                } => {
                    return Some(rf.clone());
                }
                PatState::Element {
                    ..
                }
                | PatState::Choice {
                    ..
                } => {
                    return None;
                }
                _ => {
                    // Continue searching
                }
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct Attribute {
    pub(crate) name: String,
    pub(crate) not_allowed: bool,
}

impl Attribute {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            not_allowed: false,
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum PatState {
    Element {
        name: String,
        gen_struct: GenStruct,
    },
    Attribute(Attribute),
    Group {
        interleave: bool,
    },
    Choice {
        gen_enum: GenEnum,
    },
    Optional,
    OneOrMore,
    ZeroOrMore,
    Ref {
        name: String,
        recursively: bool,
        rf: Rc<RefCell<Ref>>,
    },
    Text,
    Value {
        value: String,
    },
    DatatypeName {
        ty: syn::Path,
    },
}

impl PatState {
    pub(crate) fn is_group(&self) -> bool {
        matches!(self, PatState::Group { .. })
    }

    pub(crate) fn is_choice(&self) -> bool {
        matches!(self, PatState::Choice { .. })
    }

    pub(crate) fn has_name(&self) -> bool {
        match self {
            PatState::Element { gen_struct, .. } => !gen_struct.name.is_empty(),
            PatState::Choice { gen_enum, .. } => !gen_enum.name_is_none(),
            _ => false,
        }
    }

    pub(crate) fn set_name(&mut self, name: &str) {
        match self {
            PatState::Element {
                gen_struct,
                ..
            } => {
                gen_struct.set_name(name);
            }
            PatState::Choice {
                gen_enum,
                ..
            } => {
                gen_enum.set_name(name.to_string());
            }
            _ => (),
        }
    }

    fn mod_name(&self, stripr: bool) -> Option<String> {
        let ret = match self {
            PatState::Element {
                gen_struct,
                ..
            } => gen_struct.mod_name(),
            PatState::Choice {
                gen_enum,
            } => gen_enum.mod_name(),
            _ => return None,
        };
        if stripr {
            Some(strip_r_prefix(&ret))
        } else {
            Some(ret)
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct Ref {
    entered: usize,
    depth: usize,
}

impl Ref {
    // pub(crate) fn shared_ref(&self) -> bool {
    //     self.entered > 1
    // }

    pub(crate) fn depth(&self) -> usize {
        self.depth
    }

    pub(crate) fn enter(&mut self) {
        self.entered += 1;
        self.depth += 1;
    }

    pub(crate) fn exit(&mut self) {
        self.depth -= 1;
    }
}
