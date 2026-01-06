use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::{self, exit};
use std::rc::Rc;
use std::str::FromStr;

use heck::ToUpperCamelCase;
use pretty_assertions::Comparison;
use quote::format_ident;
use relaxng_model::model::{DefineRule, NameClass, Pattern};
use relaxng_model::{Compiler, FsFiles, Syntax};
use syn::Ident;
#[allow(unused_imports)]
use tracing::{Level, debug, error, info, span, trace, warn};

pub(crate) use crate::error::{Error, Result};
use crate::utils::{datatype_value, name_class_to_name, pattern_to_string};
use crate::xpath::XPath;

mod config;
pub(crate) use config::Config;
pub use config::ConfigRule;
mod datatypes;
use datatypes::datatype_to_ty;
mod genfield;
pub(crate) use genfield::FieldTy;
use genfield::{GenField, SerializeAs};
mod genstruct;
use genstruct::GenStruct;
mod genenum;
use genenum::{GenEnum, GenEnumRef};
mod genmod;
use genmod::GenLib;
mod gentest;
use gentest::generate_test;
mod gentree;
use gentree::{GenTree, GenUnit};
mod state;
use state::{PatState, Ref, StateStack, mod_path_from_iter, xpath_from_iter};

pub(crate) fn generate(schema: PathBuf, out: PathBuf, config: &mut Config) -> Context<'_> {
    if config.without_impl && config.with_test {
        eprintln!("Cannot generate test without implementation");
        process::exit(1);
    }

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
    for loaded in compiler.loaded() {
        let path = loaded.with_extension("toml");
        if path.exists() {
            config.load_toml(&path).expect("Failed to load TOML configuration");
        }
    }
    let define = model.as_ref().borrow();
    let pattern = define.as_ref().expect("A define rule").pattern();
    let gen_config = config.general.take();
    let mut ctx = Context::new(config);

    visit_pattern(pattern, &mut ctx);
    let (root_mod, root_ty, root_needs_interleave) = ctx.root_name.take().expect("A root name");
    let mods = ctx.units.root_mods();
    let mut gen_lib =
        GenLib::new(mods, root_mod, schema.to_str().expect("Schema file name").to_string());
    gen_lib.doc = gen_config.unwrap_or_default().doc.take();

    for (pattern, type_name) in ctx.regex_patterns.iter() {
        gen_lib.add_regex_pattern(pattern.clone(), type_name.clone());
    }

    ctx.add_unit(GenUnit::Lib(gen_lib), "", Path::new("lib"));
    ctx.write_rs(&out);
    ctx.warn_unused_doc();

    if config.with_test {
        let project_name = out.file_name().unwrap().to_str().unwrap();
        let project_name = crate::utils::safe_var_name(project_name);
        generate_test(&project_name, &root_ty, root_needs_interleave, &out);
    }

    ctx
}

fn visit_pattern(pattern: &Pattern, ctx: &mut Context) {
    let _span = span!(Level::TRACE, "", p = pattern_to_string(pattern)).entered();

    match pattern {
        Pattern::Element(name_class, pattern) => {
            if matches!(name_class, NameClass::AnyName { .. }) {
                debug!("AnyName element encountered - adding AnyElement field");
                ctx.add_field("any_element", None, FieldTy::AnyElement, None);
                ctx.uses_any_element = true;
                return;
            }
            let name = name_class_to_name(name_class);
            let gen_struct = GenStruct::new(name.to_string());
            ctx.push_state(PatState::Element {
                name: name.to_string(),
                gen_struct,
            });
            if !ctx.stack.recursive_ref() {
                visit_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Attribute(name_class, pattern) => {
            let name = name_class_to_name(name_class);
            ctx.push_state(PatState::Attribute(state::Attribute::new(name)));
            if !ctx.stack.recursive_ref() {
                visit_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Choice(patterns) => {
            ctx.push_state(PatState::Choice {
                gen_enum: GenEnum::new(ctx.xpath()),
            });
            if !ctx.stack.recursive_ref() {
                // this is silly, relang-rust should canonicalize patterns
                let mut patterns = patterns.as_slice();
                while let [Pattern::Choice(p)]
                | [Pattern::NotAllowed, Pattern::Choice(p)]
                | [Pattern::Choice(p), Pattern::NotAllowed] = patterns
                {
                    patterns = p;
                }

                for pattern in patterns {
                    visit_pattern(pattern, ctx);
                }
            }
            ctx.pop_state();
        }
        Pattern::Interleave(patterns) | Pattern::Group(patterns) => {
            let interleave = matches!(pattern, Pattern::Interleave(_));
            ctx.push_state(PatState::Group {
                interleave,
            });
            if !ctx.stack.recursive_ref() {
                for pattern in patterns {
                    visit_pattern(pattern, ctx);
                }
            }
            ctx.pop_state();
        }
        Pattern::Mixed(pattern) => {
            ctx.push_state(PatState::Text);
            ctx.pop_state();
            visit_pattern(pattern, ctx);
        }
        Pattern::Empty => {
            ctx.add_field("empty", None, FieldTy::Empty, None);
        }
        Pattern::Text => {
            ctx.push_state(PatState::Text);
            ctx.pop_state();
        }
        Pattern::NotAllowed => {
            ctx.not_allowed();
        }
        Pattern::Optional(pattern) => {
            ctx.push_state(PatState::Optional);
            visit_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::ZeroOrMore(pattern) => {
            ctx.push_state(PatState::ZeroOrMore);
            visit_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::OneOrMore(pattern) => {
            ctx.push_state(PatState::OneOrMore);
            if !ctx.stack.recursive_ref() {
                visit_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Ref(_span, name, pat_ref) => {
            let rf = ctx.refs.entry(name.to_string()).or_default().clone();
            if rf.borrow().depth() >= 2 {
                error!("inner recursive reference");
                return;
            }

            let rule = pat_ref.0.borrow();
            let rule = rule.as_ref().unwrap();
            assert!(matches!(rule, DefineRule::AssignCombine(_, _, _)));

            rf.borrow_mut().enter();
            ctx.push_state(PatState::Ref {
                name: name.clone(),
                recursively: rf.borrow().depth() >= 2,
                rf: rf.clone(),
            });
            visit_pattern(rule.pattern(), ctx);
            ctx.pop_state();
            rf.borrow_mut().exit();
        }
        Pattern::DatatypeValue {
            datatype,
        } => {
            let value = datatype_value(datatype);
            ctx.value(value);
        }
        Pattern::DatatypeName {
            datatype,
            except,
        } => {
            if except.is_some() {
                warn!("Unimplemented: DatatypeName with exception: {:?} {:?}", datatype, except);
            }
            let ty = datatype_to_ty(datatype, ctx);
            ctx.push_state(PatState::DatatypeName {
                ty: syn::parse2(ty).unwrap(),
            });
            ctx.pop_state();
        }
        Pattern::List(_pattern) => {
            warn!("Unimplemented: List pattern");
        }
    }
}

pub struct Context<'a> {
    stack: StateStack,
    choice_count: usize,
    units: GenTree,
    refs: HashMap<String, Rc<RefCell<Ref>>>,
    config: &'a Config,
    /// (module_name, type_name, needs_interleave_args)
    root_name: Option<(String, String, bool)>,
    /// XPath for missing documentation
    missing_doc: BTreeSet<String>,
    /// Tracks which config rule XPaths with doc have been used
    used_doc_rules: HashSet<XPath>,
    regex_patterns: BTreeMap<String, Ident>,
    /// Whether any AnyElement fields are used
    uses_any_element: bool,
}

impl<'a> Context<'a> {
    fn new(config: &'a Config) -> Self {
        Self {
            stack: StateStack::new(),
            choice_count: 0,
            units: GenTree::default(),
            refs: Default::default(),
            root_name: None,
            missing_doc: BTreeSet::new(),
            used_doc_rules: HashSet::new(),
            regex_patterns: BTreeMap::new(),
            uses_any_element: false,
            config,
        }
    }

    fn write_rs(&mut self, outdir: &Path) {
        let src = outdir.join("src");

        // Update GenLib with the uses_any_element flag
        self.units.set_uses_any_element(self.uses_any_element);

        // Update all_optional for field references based on final struct content
        // This must be done after all reconciliation is complete
        self.units.update_all_optional();

        self.units.write_rs(&src, self.config);
    }

    pub(crate) fn print_missing_doc(&self) {
        for path in self.missing_doc.iter() {
            println!("[rule.\"{path}\"]");
            println!("doc = \"\"");
            println!()
        }
    }

    pub(crate) fn warn_unused_doc(&self) {
        for (pat, rule) in self.config.rule.iter() {
            if rule.doc.is_some() && !self.used_doc_rules.contains(pat) {
                warn!("Unused doc configuration for rule: {pat}");
            }
        }
    }

    fn push_state(&mut self, pat_state: PatState) {
        if pat_state.is_group()
            && let Some(gen_enum) = self.stack.last_enum_mut()
        {
            gen_enum.push_group();
        }
        let is_choice = pat_state.is_choice();

        self.stack.push(pat_state);
        let xpath_ref = self.xpath_from_last_ref();
        let config = self.get_config_from_xpath(&xpath_ref);

        let has_name = self.stack.states.last().is_some_and(|s| s.state.has_name());
        if let Some(name) = config.name.as_deref() {
            self.stack.last_mut().unwrap().set_name(name);
        } else if is_choice && !has_name {
            let name = self.enum_name_from_state().unwrap_or_else(|| {
                self.choice_count += 1;
                warn!("Unnamed enum at {}", xpath_ref);
                format!("Choice{}", self.choice_count)
            });
            self.stack.last_mut().unwrap().set_name(&name);
        };

        self.stack.set_config(config);
    }

    fn pop_state(&mut self) {
        let xpath = self.xpath_from_last_ref();
        let mod_path = self.mod_path();
        let (state, config) = self.stack.pop().unwrap();
        // TODO: allow the xpath to store the unit to be somewhere else on the tree
        // in particular in the common sub-tree: ref['foo']/etc -> parent/foo
        if config.skip {
            return;
        }

        let name = config.name.as_deref();
        let mut doc_needed = false;
        let mut get_doc = || {
            doc_needed = true;
            config.doc.clone()
        };
        match state {
            PatState::Element {
                gen_struct,
                ..
            } => {
                doc_needed = true;
                self.pop_struct(gen_struct, &config, &xpath, mod_path);
            }
            PatState::Choice {
                gen_enum,
                ..
            } => {
                doc_needed = true;
                self.pop_choice(gen_enum, &config, &xpath, mod_path);
            }
            PatState::Group {
                ..
            } => {
                if let Some(gen_enum) = self.stack.last_enum_mut() {
                    if name.is_none() {
                        warn!("Unnamed variant at {}", xpath)
                    }
                    let doc = get_doc().or_else(|| Some(xpath.clone()));
                    gen_enum.pop_group(config.name, doc);
                }
            }
            PatState::DatatypeName {
                ty,
            } => {
                self.add_field("value", name, FieldTy::Parse(ty), get_doc());
            }
            PatState::Text => {
                self.add_field("value", name, FieldTy::Text, get_doc());
            }
            PatState::Value {
                value,
            } => {
                self.add_field(&value, name, FieldTy::Value(value.to_string()), get_doc());
            }
            PatState::Attribute(_)
            | PatState::Optional
            | PatState::OneOrMore
            | PatState::ZeroOrMore
            | PatState::Ref {
                ..
            } => {}
        }

        if doc_needed && config.doc.is_none() {
            self.missing_doc.insert(xpath);
        }
    }

    fn pop_struct(
        &mut self,
        mut gen_struct: GenStruct,
        config: &ConfigRule,
        xpath: &str,
        mod_path: PathBuf,
    ) {
        if gen_struct.fields.is_enum() && !config.allow_structure_is_enum {
            debug!("Structure is an enum {}", xpath);
        }
        let mut field_name = gen_struct.var_name().to_string();
        if let Some(config_field_name) = &config.field_name {
            field_name = config_field_name.to_string();
        }

        let child_units = std::mem::take(&mut gen_struct.units);
        for c in child_units {
            self.add_child_unit(c, xpath, &mod_path);
        }

        let rf = self.stack.get_ref();
        let all_optional = !gen_struct.has_required_fields();
        self.add_field(
            &gen_struct.xml_name,
            Some(&field_name),
            FieldTy::Xml {
                path: gen_struct.path(),
                root: rf.is_some(),
                all_optional,
            },
            config.doc.clone(),
        );

        gen_struct.doc = config.doc.clone();
        self.add_unit(GenUnit::Struct(gen_struct), xpath, &mod_path);
    }

    fn pop_choice(
        &mut self,
        mut gen_enum: GenEnum,
        config: &ConfigRule,
        xpath: &str,
        mod_path: PathBuf,
    ) {
        if config.collapse_with_parent {
            match self.stack.pop().unwrap() {
                (
                    PatState::Element {
                        gen_struct,
                        ..
                    },
                    elem_config,
                ) => {
                    gen_enum.set_as_element(gen_struct);
                    if gen_enum.doc.is_none() {
                        gen_enum.doc = elem_config.doc;
                    }
                }
                _ => panic!("Unexpected state"),
            }
            self.push_state(PatState::Choice {
                gen_enum,
            });
            return;
        }

        if gen_enum.doc.is_none() {
            gen_enum.doc = config.doc.clone();
        }

        let child_units = std::mem::take(&mut gen_enum.units);
        for c in child_units {
            self.add_child_unit(c, xpath, &mod_path);
        }

        if gen_enum.name_is_none() {
            warn!("unnamed or empty choice?");
            return;
        }

        let xml_name = gen_enum.var_name().to_string();
        let doc = if config.doc_propagate == Some(false) {
            None
        } else {
            gen_enum.doc.clone().or_else(|| config.doc.clone())
        };

        if config.replace_with_text {
            self.add_field(&xml_name, config.field_name.as_deref(), FieldTy::Text, doc);
        } else {
            let rf = self.stack.get_ref();
            // Set merged_name from config if specified
            gen_enum.set_merged_name(config.merged_name.clone());
            let gen_enum = GenEnumRef::from(gen_enum);
            self.add_field(
                &xml_name,
                config.field_name.as_deref(),
                FieldTy::Choice {
                    gen_enum: gen_enum.clone(),
                    root: rf.is_some(),
                },
                doc,
            );

            if !gen_enum.borrow().name_is_none() {
                let unit = GenUnit::Enum(gen_enum);
                self.add_unit(unit, xpath, &mod_path);
            }
        }
    }

    fn value(&mut self, val: &str) {
        self.push_state(PatState::Value {
            value: val.to_string(),
        });
        self.pop_state();
    }

    fn not_allowed(&mut self) {
        for state in self.stack.iter_mut().rev() {
            match state {
                PatState::Attribute(attr) => {
                    attr.not_allowed = true;
                    break;
                }
                _ => {
                    trace!("Not allowed - todo?");
                }
            }
        }
    }

    fn add_child_unit(&mut self, c: GenUnit, xpath: &str, mod_path: &Path) {
        let modname = c.mod_name(true);
        let mut child_path = mod_path.to_path_buf();
        child_path.push(&modname);
        self.add_unit(c, &format!("{xpath}/child {modname}"), &child_path);
    }

    fn add_unit(&mut self, unit: GenUnit, xpath: &str, path: &Path) {
        if let Err(err) = self.add_unit_err(unit, xpath, path) {
            panic!("Failed to add unit: {err} at {xpath}");
        }
    }

    fn add_unit_err(&mut self, new_unit: GenUnit, xpath: &str, path: &Path) -> Result<()> {
        if self.stack.recursive_ref() {
            return Ok(());
        }

        fn reconcile(
            path: &Path,
            xpath: &str,
            new_unit: GenUnit,
            unit: &mut GenUnit,
        ) -> Result<Vec<GenUnit>> {
            if new_unit == *unit {
                return Ok(vec![]);
            }

            info!("Conflicting unit at {:?} {}: {}", path, xpath, Comparison::new(unit, &new_unit));
            let old = unit.clone();
            let reconcile_result = unit.reconcile(new_unit);
            if reconcile_result.is_err() {
                warn!("{:?}", unit);
            }
            let new_units = reconcile_result?;
            trace!("Resulting unit: {}", Comparison::new(&old, unit));

            Ok(new_units)
        }

        if let Some(unit) = self.units.get_mut(path) {
            let child_units = reconcile(path, xpath, new_unit, unit)?;
            for unit in child_units {
                self.add_child_unit(unit, xpath, path);
            }
        } else {
            self.units.insert_unit(path, xpath, new_unit);
        }

        Ok(())
    }

    fn get_config_from_xpath(&mut self, xpath_str: &str) -> ConfigRule {
        let mut config = ConfigRule::default();
        let xpath = XPath::from_str(xpath_str).unwrap();
        for (pat, c) in self.config.rule.iter() {
            if pat.matches(&xpath) {
                debug!("Extra config: {:?}", c);
                if c.doc.is_some() {
                    self.used_doc_rules.insert(pat.clone());
                }
                config.merge(c);
            }
        }
        config
    }

    fn add_field(&mut self, xml_name: &str, name: Option<&str>, ty: FieldTy, doc: Option<String>) {
        if let Err(err) = self.add_field_err(xml_name, name, ty, doc) {
            panic!("Failed to add field: {} {}", err, self.xpath_from_last_ref());
        }
    }

    fn add_field_err(
        &mut self,
        xml_name: &str,
        name: Option<&str>,
        ty: FieldTy,
        doc: Option<String>,
    ) -> Result<()> {
        let mut field = GenField::new(xml_name, ty, doc);
        let mut parent_idx = None;
        let mut attr_missing_doc_idx = None;

        // Iterate backwards through the stack using indices
        let len = self.stack.states.len();
        for i in (0..len).rev() {
            let s = &mut self.stack.states[i];
            match &mut s.state {
                PatState::Element {
                    ..
                } => {
                    parent_idx = Some(i);
                    break;
                }
                PatState::Choice {
                    ..
                } => {
                    parent_idx = Some(i);
                    break;
                }
                PatState::Attribute(state::Attribute {
                    name,
                    not_allowed,
                }) => {
                    field.set_xml_name(name);
                    field.set_serialize_as(SerializeAs::Attribute);
                    field.set_not_allowed(*not_allowed);
                    // Override doc if attribute has a doc config
                    if let Some(ref config) = s.config
                        && let Some(ref attr_doc) = config.doc
                    {
                        field.doc = Some(attr_doc.clone());
                    }
                    // Track if this attribute has missing doc
                    if field.doc.is_none() {
                        attr_missing_doc_idx = Some(i);
                    }
                }
                PatState::Optional => field.set_optional(true),
                PatState::OneOrMore => field.set_multiple(true),
                PatState::ZeroOrMore => {
                    field.set_optional(true);
                    field.set_multiple(true);
                }
                PatState::Ref {
                    rf: _rf,
                    recursively,
                    name: _name,
                } => {
                    field.set_recursive(*recursively);
                }
                PatState::Group {
                    ..
                } => {
                    // handle ordering?
                }
                PatState::Text
                | PatState::Value {
                    ..
                }
                | PatState::DatatypeName {
                    ..
                } => {}
            }
        }

        // Insert missing doc for attribute with correct xpath
        if let Some(idx) = attr_missing_doc_idx {
            let xpath = self.xpath_up_to(idx);
            self.missing_doc.insert(xpath);
        }

        if let Some(name) = name {
            field.name = name.to_string();
        }

        let parent_name = match parent_idx {
            Some(idx) => match &self.stack.states[idx].state {
                PatState::Element {
                    gen_struct,
                    ..
                } => format!("{gen_struct}"),
                PatState::Choice {
                    gen_enum,
                    ..
                } => format!("{gen_enum}"),
                _ => String::new(),
            },
            None => String::new(),
        };

        let field_name = format!("{}.{}", parent_name, field.name);
        let cardinality = match (field.optional, field.multiple) {
            (true, true) => "*",
            (false, true) => "+",
            (true, false) => "?",
            (false, false) => "",
        };

        let recursion = if field.recursive {
            " (recursive)"
        } else {
            ""
        };

        debug!("Adding field: {}{}{}", field_name, cardinality, recursion);

        match parent_idx {
            Some(idx) => match &mut self.stack.states[idx].state {
                PatState::Element {
                    gen_struct,
                    ..
                } => gen_struct.add_field(field)?,
                PatState::Choice {
                    gen_enum,
                    ..
                } => gen_enum.add_field(field)?,
                _ => {}
            },
            None => {
                let ty_path = field.ty_path().unwrap();
                let path = ty_path.get_ident().unwrap();
                // Check if the root type is an interleave-based enum (needs 2-arg from_xml)
                let needs_interleave = match &field.ty {
                    FieldTy::Choice {
                        gen_enum,
                        ..
                    } => gen_enum.borrow().as_element.is_none(),
                    _ => false,
                };
                self.root_name = Some((field.name.to_string(), path.to_string(), needs_interleave));
            }
        };

        Ok(())
    }

    fn enum_name_from_state(&self) -> Option<String> {
        for state in self.stack.iter().rev().skip(1) {
            match state {
                PatState::Ref {
                    name,
                    ..
                } => return Some(name.to_upper_camel_case()),
                PatState::Attribute(state::Attribute {
                    name,
                    ..
                }) => return Some(format!("{}Choice", name.to_upper_camel_case())),
                PatState::Element {
                    name,
                    ..
                } => return Some(format!("{}Choice", name.to_upper_camel_case())),
                PatState::OneOrMore | PatState::Optional | PatState::ZeroOrMore => continue,
                _ => {
                    debug!(?state, "couldn't guess a name");
                    return None;
                }
            }
        }

        None
    }

    fn xpath_from_last_ref(&self) -> String {
        let iter = self.stack.iter_with_counter(true);
        xpath_from_iter(iter, true)
    }

    #[allow(unused)]
    fn xpath(&self) -> String {
        let iter = self.stack.iter_with_counter(false);
        xpath_from_iter(iter, true)
    }

    fn xpath_up_to(&self, end_idx: usize) -> String {
        let iter = self.stack.iter_up_to(end_idx);
        xpath_from_iter(iter, true)
    }

    fn mod_path(&self) -> PathBuf {
        let iter = self.stack.iter_with_counter(true);
        mod_path_from_iter(iter)
    }

    fn add_regex_pattern(&mut self, pat: &str) -> Ident {
        if let Some(ident) = self.regex_patterns.get(pat) {
            return ident.clone();
        }

        // Generate unique type name for new pattern
        let pattern_count = self.regex_patterns.len();
        let ident = format_ident!("RegexPattern{}", pattern_count);
        self.regex_patterns.insert(pat.to_string(), ident.clone());
        ident
    }
}
