use core::fmt;
use datatypes::datatype_to_ty;
use genenum::GenEnumRef;
use genfield::FieldTy;
use genfield::SerializeAs;
use genmod::GenMod;
use heck::ToUpperCamelCase;
use pretty_assertions::Comparison;
use relaxng_model::FsFiles;
use relaxng_model::Syntax;
use relaxng_model::model::DefineRule;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use std::str::FromStr;
use tracing::debug;
use tracing::trace;
use tracing::warn;

use relaxng_model::model::NameClass;
use relaxng_model::model::Pattern;

use relaxng_model::Compiler;

mod genfield;
use genfield::GenField;
mod genstruct;
use genstruct::GenStruct;
mod genenum;
use genenum::GenEnum;
mod genmod;
mod gentest;
use gentest::generate_test;
mod gentree;
use gentree::{GenTree, GenUnit};

use crate::utils::strip_r_prefix;
// sort of xpath
use crate::xpath::XPath;
mod datatypes;

#[derive(Debug, Clone, Deserialize, Default)]
pub(crate) struct Config {
    #[serde(default)]
    pub rule: HashMap<XPath, ConfigRule>,
    #[serde(default)]
    pub field: HashMap<String, ConfigField>,
}

#[derive(Debug, Deserialize, Clone, Default)]
pub struct ConfigRule {
    pub name: Option<String>,
    #[serde(default)]
    pub as_child: bool,
}

#[derive(Debug, Deserialize, Clone, Default)]
pub struct ConfigField {
    pub name: Option<String>,
}

impl ConfigRule {
    fn merge(&mut self, c: &ConfigRule) {
        if c.name.is_some() {
            self.name = c.name.clone();
        }

        self.as_child |= c.as_child;
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Error {
    Reconcile(FieldTy, FieldTy),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Reconcile(ty1, ty2) => {
                write!(f, "Type reconciliation error: {:#?} and {:#?}", ty1, ty2)
            }
        }
    }
}

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

pub(crate) fn generate(schema: PathBuf, out: PathBuf, test: bool, config: Config) {
    let mut compiler = if schema.extension().map(|ext| ext == "rng").unwrap_or(false) {
        Compiler::new(FsFiles, Syntax::Xml)
    } else {
        Compiler::default()
    };
    let model = match compiler.compile(&schema) {
        Ok(m) => m,
        Err(err) => {
            compiler.dump_diagnostic(&err);
            exit(1);
        }
    };
    let define = model.as_ref().borrow();
    let pattern = define.as_ref().expect("A define rule").pattern();
    let mut ctx = Context::new(config);

    visit_pattern(pattern, &mut ctx);
    let (root_mod, root_ty) = ctx.root_name.take().expect("A root name");
    let mods = ctx.units.root_mods();
    let gen_mod = GenMod::new(
        mods,
        root_mod,
        schema.to_str().expect("Schema file name").to_string(),
    );
    ctx.add_unit(GenUnit::Mod(gen_mod), "/");
    ctx.write_rs(&out);

    if test {
        generate_test(&root_ty, &out);
    }
}

#[derive(Debug)]
struct Attribute {
    name: String,
    not_allowed: bool,
}

impl Attribute {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            not_allowed: false,
        }
    }
}

#[derive(Debug)]
enum State {
    Element { name: String, gen_struct: GenStruct },
    Attribute(Attribute),
    Group { interleave: bool },
    Choice { gen_enum: GenEnum },
    Optional,
    OneOrMore,
    ZeroOrMore,
    Ref { name: String, recursively: bool },
}

impl State {
    fn is_group(&self) -> bool {
        matches!(self, State::Group { .. })
    }
}

#[derive(Debug)]
struct StateCounter {
    state: State,
    elem_count: HashMap<String, usize>,
    interleave_count: usize,
    group_count: usize,
    choice_count: usize,
}

impl StateCounter {
    fn new(state: State) -> Self {
        Self {
            state,
            elem_count: HashMap::new(),
            interleave_count: 0,
            group_count: 0,
            choice_count: 0,
        }
    }
}

#[derive(Debug)]
struct StateStack {
    states: Vec<StateCounter>,
    // cache to speed update from traversing the tree looking for Ref { recursive }
    recursive: usize,
}

impl StateStack {
    fn new() -> Self {
        Self {
            states: Vec::new(),
            recursive: 0,
        }
    }

    fn push(&mut self, state: State) {
        match state {
            State::Element { ref name, .. } => {
                if let Some(last) = self.states.last_mut() {
                    last.elem_count
                        .entry(name.clone())
                        .and_modify(|v| *v += 1)
                        .or_insert(1);
                }
            }
            State::Attribute(_) => (),
            State::Group { interleave: true } => {
                if let Some(last) = self.states.last_mut() {
                    last.interleave_count += 1;
                }
            }
            State::Group { interleave: false } => {
                if let Some(last) = self.states.last_mut() {
                    last.group_count += 1;
                }
            }
            State::Choice { .. } => {
                if let Some(last) = self.states.last_mut() {
                    last.choice_count += 1;
                }
            }
            State::Optional => (),
            State::OneOrMore => (),
            State::ZeroOrMore => (),
            State::Ref { recursively, .. } => {
                if recursively {
                    self.recursive += 1;
                }
            }
        }
        self.states.push(StateCounter::new(state));
    }

    fn pop(&mut self) -> Option<State> {
        self.states.pop().map(|s| {
            if let State::Ref { recursively, .. } = &s.state {
                if *recursively {
                    self.recursive -= 1;
                }
            }
            s.state
        })
    }

    fn iter_with_counter(
        &self,
        from_last_ref: bool,
    ) -> impl DoubleEndedIterator<Item = &StateCounter> {
        if from_last_ref {
            if let Some(pos) = self
                .states
                .iter()
                .rposition(|x| matches!(x.state, State::Ref { .. }))
            {
                return self.states[pos..].iter();
            }
        }

        self.states.iter()
    }

    fn iter(&self) -> impl DoubleEndedIterator<Item = &State> {
        self.states.iter().map(|s| &s.state)
    }

    fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut State> {
        self.states.iter_mut().map(|s| &mut s.state)
    }

    fn last_mut(&mut self) -> Option<&mut State> {
        self.states.last_mut().map(|s| &mut s.state)
    }

    fn recursive_ref(&self) -> bool {
        self.recursive != 0
    }
}

struct Context {
    state: StateStack,
    choice_count: usize,
    units: GenTree,
    refs: HashMap<String, Ref>,
    config: Config,
    root_name: Option<(String, String)>,
}

impl Context {
    fn new(config: Config) -> Self {
        Self {
            state: StateStack::new(),
            choice_count: 0,
            units: GenTree::default(),
            refs: Default::default(),
            root_name: None,
            config,
        }
    }

    fn push_state(&mut self, state: State) {
        if state.is_group() {
            if let Some(State::Choice { gen_enum, .. }) = self.state.last_mut() {
                gen_enum.push_group();
            }
        }

        self.state.push(state);
    }

    fn pop_state(&mut self) {
        let xpath_str = self.xpath_from_last_ref();
        debug!(?xpath_str, "pop");

        let mut config = ConfigRule::default();
        let xpath = XPath::from_str(&xpath_str).unwrap();
        for (pat, c) in self.config.rule.iter() {
            if pat.matches(&xpath) {
                debug!("Extra config: {:?}", c);
                config.merge(c);
            }
        }

        match self.state.pop().unwrap() {
            State::Element { gen_struct, .. } => self.pop_struct(gen_struct, &config, &xpath_str),
            State::Choice { gen_enum, .. } => {
                self.pop_choice(gen_enum, &config, &xpath_str);
            }
            State::Group { .. } => {
                if let Some(State::Choice { gen_enum, .. }) = self.state.last_mut() {
                    if config.name.is_none() {
                        warn!("Unnamed variant at {}", xpath_str)
                    }
                    gen_enum.pop_group(config.name);
                }
            }
            State::Attribute(_)
            | State::Optional
            | State::OneOrMore
            | State::ZeroOrMore
            | State::Ref { .. } => {}
        }
    }

    fn add_field_err(&mut self, xml_name: &str, ty: FieldTy) -> Result<()> {
        let mut field = GenField::new(xml_name, ty);
        let mut to_add = None;
        for state in self.state.iter_mut().rev() {
            match state {
                State::Element { .. } => {
                    // field.set_serialize_as(SerializeAs::Element);
                    to_add = Some(state);
                    break;
                }
                State::Choice { .. } => {
                    to_add = Some(state);
                    break;
                }
                State::Attribute(Attribute { name, not_allowed }) => {
                    field.set_xml_name(name);
                    field.set_serialize_as(SerializeAs::Attribute);
                    field.set_not_allowed(*not_allowed);
                }
                State::Optional => field.set_optional(true),
                State::OneOrMore => field.set_multiple(true),
                State::ZeroOrMore => {
                    field.set_optional(true);
                    field.set_multiple(true);
                }
                State::Ref {
                    recursively,
                    name: _name,
                } => {
                    // field.set_name(_name);
                    field.set_ref(true);
                    field.set_recursive(*recursively);
                }
                State::Group { .. } => {
                    // handle ordering?
                }
            }
        }

        let name = match to_add {
            Some(State::Element { gen_struct, .. }) => format!("{}", gen_struct),
            Some(State::Choice { gen_enum, .. }) => format!("{}", gen_enum),
            _ => String::new(),
        };
        let qfield = format!("{}.{}", name, field.name);
        debug!(
            "Adding field: {}{}{} {}",
            qfield,
            if field.multiple {
                "[]"
            } else if field.optional {
                "?"
            } else {
                ""
            },
            if field.in_ref { "+" } else { "" },
            field.serialize_as
        );

        if let Some(config) = self.config.field.get(&qfield) {
            if let Some(name) = &config.name {
                field.name = name.clone();
            }
        }

        let new_unit = match to_add {
            Some(State::Element { gen_struct, .. }) => gen_struct.add_field(field)?,
            Some(State::Choice { gen_enum, .. }) => gen_enum.add_field(field)?,
            _ => {
                let ty_path = field.ty_path().unwrap();
                let path = ty_path.get_ident().unwrap();
                self.root_name = Some((field.name.to_string(), path.to_string()));
                None
            }
        };

        if let Some(_unit) = new_unit {
            todo!()
            // self.add_unit_err(unit, None)?;
        }

        Ok(())
    }

    fn add_field(&mut self, xml_name: &str, ty: FieldTy) {
        if let Err(err) = self.add_field_err(xml_name, ty) {
            panic!(
                "Failed to add field: {} {}",
                err,
                self.xpath_from_last_ref()
            );
        }
    }

    fn pop_struct(&mut self, mut gen_struct: GenStruct, config: &ConfigRule, xpath: &str) {
        let mut xpath = xpath.to_string();
        if let Some(ref name) = config.name {
            let parent_name = gen_struct.mod_name();
            gen_struct.set_name(name);
            if config.as_child {
                self.add_unit(GenUnit::PlaceHolder(parent_name), &xpath);
                xpath.push_str(&format!("/{}", name));
            }
        }
        let name = &gen_struct.xml_name;
        self.add_field(name, FieldTy::Xml(gen_struct.path()));
        self.add_unit(GenUnit::Struct(gen_struct), &xpath);
    }

    fn name_from_state(&self) -> Option<String> {
        for state in self.state.iter().rev() {
            match state {
                State::Ref { name, .. } => return Some(name.to_upper_camel_case()),
                State::Attribute(Attribute { name, .. }) => {
                    return Some(name.to_upper_camel_case());
                }
                State::OneOrMore | State::Optional | State::ZeroOrMore => continue,
                _ => {
                    debug!(?state, "couldn't find a name");
                    return None;
                }
            }
        }
        None
    }

    fn pop_choice(&mut self, mut gen_enum: GenEnum, config: &ConfigRule, xpath: &str) {
        let name = if let Some(ref name) = config.name {
            name.clone()
        } else {
            self.name_from_state().unwrap_or_else(|| {
                self.choice_count += 1;
                warn!("Unnamed enum at {}", xpath);
                format!("Choice{}", self.choice_count)
            })
        };

        gen_enum.set_name(name);
        let field_name = gen_enum.var_name().to_string();
        let gen_enum = GenEnumRef::from(gen_enum);
        self.add_field(&field_name, FieldTy::Choice(gen_enum.clone()));

        if !gen_enum.borrow().is_none() {
            let unit = GenUnit::Enum(gen_enum);
            self.add_unit(unit, xpath);
        }
    }

    fn text(&mut self) {
        self.add_field("value", FieldTy::Text);
    }

    fn empty(&mut self) {
        self.add_field("empty", FieldTy::Empty);
    }

    fn datatype_name(&mut self, ty: String) {
        let ty = syn::parse_str(&ty).unwrap();

        self.add_field("value", FieldTy::Parse(ty));
    }

    fn value(&mut self, val: &str) {
        self.add_field(val, FieldTy::Value(val.to_string()));
    }

    fn write_rs(&self, outdir: &Path) {
        let src = outdir.join("src").join("xml");

        self.units.write_rs(&src);
    }

    fn add_unit_err(&mut self, unit: GenUnit, xpath: &str) -> Result<()> {
        fn conflict(unit: GenUnit, exist: &mut GenUnit) -> Result<Vec<GenUnit>> {
            let mut new_units = Vec::new();
            if unit != *exist {
                trace!("Conflicting unit: {}", Comparison::new(exist, &unit));
                let old = exist.clone();
                new_units = exist.reconcile(unit)?;
                trace!("Resulting unit: {}", Comparison::new(&old, exist));
            }

            Ok(new_units)
        }

        let mut new_units = Vec::new();
        let mut xpath = xpath.to_string();
        if let Some(exist) = self.units.lookup_unit_mut(&xpath) {
            new_units = conflict(unit, exist)?;
        } else if let Some((exist, path)) = self.units.file_conflict_at(&xpath, &unit.name()) {
            new_units = conflict(unit, exist)?;
            self.units.move_children(&xpath, &path);
            xpath = path;
        } else {
            self.units.insert_unit(&xpath, unit);
        }

        for new_unit in new_units {
            let xpath = format!("{}/{}", xpath, strip_r_prefix(&new_unit.mod_name()));
            debug!(?xpath, "New unit");
            self.add_unit_err(new_unit, &xpath)?;
        }

        Ok(())
    }

    fn add_unit(&mut self, unit: GenUnit, xpath: &str) {
        if let Err(err) = self.add_unit_err(unit, xpath) {
            panic!("Failed to add unit: {} at {}", err, xpath);
        }
    }

    fn not_allowed(&mut self) {
        for state in self.state.iter_mut().rev() {
            match state {
                State::Element { gen_struct, .. } => {
                    gen_struct.set_not_allowed(true);
                    break;
                }
                State::Choice { gen_enum, .. } => {
                    gen_enum.set_not_allowed(true);
                    break;
                }
                State::Attribute(attr) => {
                    attr.not_allowed = true;
                    break;
                }
                _ => continue,
            }
        }
    }

    fn xpath_from_last_ref(&self) -> String {
        let iter = self.state.iter_with_counter(true);
        xpath_from_iter(iter, true)
    }

    fn xpath(&self) -> String {
        let iter = self.state.iter_with_counter(false);
        xpath_from_iter(iter, true)
    }
}

fn visit_pattern(pattern: &Pattern, ctx: &mut Context) {
    match pattern {
        Pattern::Element(name_class, pattern) => {
            if matches!(name_class, NameClass::AnyName { .. }) {
                debug!("AnyName element encountered");
                return;
            }
            let name = name_class_to_name(name_class);
            let gen_struct = GenStruct::new(name.clone());
            ctx.push_state(State::Element {
                name: name.clone(),
                gen_struct,
            });
            if !ctx.state.recursive_ref() {
                visit_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Attribute(name_class, pattern) => {
            let name = name_class_to_name(name_class);
            ctx.push_state(State::Attribute(Attribute::new(name)));
            visit_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::Choice(patterns) => {
            ctx.push_state(State::Choice {
                gen_enum: GenEnum::new(),
            });
            for pattern in patterns {
                visit_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Interleave(patterns) | Pattern::Group(patterns) => {
            let interleave = matches!(pattern, Pattern::Interleave(_));
            ctx.push_state(State::Group { interleave });
            for pattern in patterns {
                visit_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Mixed(pattern) => {
            ctx.text();
            visit_pattern(pattern, ctx);
        }
        Pattern::Empty => {
            ctx.empty();
        }
        Pattern::Text => {
            ctx.text();
        }
        Pattern::NotAllowed => {
            ctx.not_allowed();
        }
        Pattern::Optional(pattern) => {
            ctx.push_state(State::Optional);
            visit_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::ZeroOrMore(pattern) => {
            ctx.push_state(State::ZeroOrMore);
            visit_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::OneOrMore(pattern) => {
            ctx.push_state(State::OneOrMore);
            visit_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::Ref(_span, name, pat_ref) => {
            let xpath = ctx.xpath();
            let recursively = if let Some(_ref) = ctx.refs.get_mut(name) {
                debug!("Reference loop: {} at {}", name, xpath);
                if _ref.recursive() {
                    debug!("recursive reference");
                    return;
                }
                _ref.set_recursive(true);
                true
            } else {
                ctx.refs.insert(name.clone(), Ref::new());
                false
            };

            let rule = pat_ref.0.borrow();
            let rule = rule.as_ref().unwrap();
            assert!(matches!(rule, DefineRule::AssignCombine(_, _, _)));

            ctx.push_state(State::Ref {
                name: name.clone(),
                recursively,
            });
            visit_pattern(rule.pattern(), ctx);
            ctx.pop_state();

            if recursively {
                ctx.refs.get_mut(name).unwrap().set_recursive(false);
            } else {
                ctx.refs.remove(name);
            }
        }
        Pattern::DatatypeValue { datatype } => {
            use relaxng_model::datatype::*;
            match datatype {
                DatatypeValues::Relax(value) => match value {
                    relax::BuiltinDatatypeValue::TokenValue(val) => {
                        ctx.value(val);
                    }
                    relax::BuiltinDatatypeValue::StringValue(_val) => todo!(),
                },
                DatatypeValues::Xsd(_xsd_datatype_values) => todo!(),
            }
        }
        Pattern::DatatypeName { datatype, except } => {
            if except.is_some() {
                warn!(
                    "Unimplemented: DatatypeName with exception: {:?} {:?}",
                    datatype, except
                );
            }
            let ty = datatype_to_ty(datatype);
            ctx.datatype_name(ty);
        }
        Pattern::List(_pattern) => {
            warn!("Unimplemented: List pattern");
        }
    }
}

fn name_class_to_name(name_class: &NameClass) -> &String {
    let NameClass::Named {
        namespace_uri: _,
        name,
    } = name_class
    else {
        panic!("Unexpected name class {:#?}", name_class);
    };

    name
}

// sort of xpath
fn xpath_from_iter<'a>(iter: impl Iterator<Item = &'a StateCounter>, with_count: bool) -> String {
    let mut path = String::new();
    let mut last: Option<&StateCounter> = None;
    for c in iter {
        match &c.state {
            State::Element { name, .. } => {
                path.push_str(&format!("/element[@name='{}']", name));
                if with_count {
                    let count = last
                        .and_then(|last| last.elem_count.get(name))
                        .copied()
                        .unwrap_or(1);
                    path.push_str(&format!("[{count}]"));
                }
            }
            State::Attribute(attribute) => {
                path.push_str(&format!("/attribute[@name='{}']", attribute.name))
            }
            State::Choice { .. } => {
                path.push_str("/choice");
                if with_count {
                    path.push_str(&format!(
                        "[{}]",
                        last.map(|last| last.choice_count).unwrap_or(1)
                    ));
                }
            }
            State::Group { interleave: true } => {
                path.push_str("/interleave");
                if with_count {
                    path.push_str(&format!(
                        "[{}]",
                        last.map(|last| last.interleave_count).unwrap_or(1)
                    ));
                }
            }
            State::Group { interleave: false } => {
                path.push_str("/group");
                if with_count {
                    path.push_str(&format!(
                        "[{}]",
                        last.map(|last| last.group_count).unwrap_or(1)
                    ))
                }
            }
            State::Optional => path.push_str("/optional"),
            State::OneOrMore => path.push_str("/oneOrMore"),
            State::ZeroOrMore => path.push_str("/zeroOrMore"),
            State::Ref { name, .. } => path.push_str(&format!("/ref[@name='{}']", name)),
        }
        last = Some(c);
    }
    path
}

#[derive(Debug, Default)]
struct Ref {
    recursive: bool,
}

impl Ref {
    fn new() -> Self {
        Self::default()
    }

    fn recursive(&self) -> bool {
        self.recursive
    }

    fn set_recursive(&mut self, recursive: bool) {
        self.recursive = recursive
    }
}
