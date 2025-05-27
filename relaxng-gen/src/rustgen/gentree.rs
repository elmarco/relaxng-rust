use std::collections::HashMap;

use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::ToTokens;
use tracing::{debug, error, warn};

use crate::utils::strip_r_prefix;

use super::{Result, genenum::GenEnumRef, genmod::GenMod, genstruct::GenStruct};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum GenUnit {
    Mod(GenMod),
    Enum(GenEnumRef),
    Struct(GenStruct),
    PlaceHolder(String),
}

impl GenUnit {
    pub(crate) fn name(&self) -> String {
        match self {
            GenUnit::Mod(m) => m.name().to_string(),
            GenUnit::Enum(e) => e.borrow().name().to_string(),
            GenUnit::Struct(s) => s.name().to_string(),
            GenUnit::PlaceHolder(n) => n.clone(),
        }
    }

    pub(crate) fn mod_name(&self) -> String {
        let mod_name = match self {
            GenUnit::Mod(m) => m.name().to_snake_case(),
            GenUnit::Enum(e) => e.borrow().var_name().to_string(),
            GenUnit::Struct(s) => s.var_name().to_string(),
            GenUnit::PlaceHolder(n) => n.clone(),
        };

        mod_name
    }

    pub(crate) fn token_stream(&self) -> TokenStream {
        match self {
            GenUnit::Mod(m) => m.to_token_stream(),
            GenUnit::Struct(st) => st.to_token_stream(),
            GenUnit::Enum(en) => en.borrow().to_token_stream(),
            GenUnit::PlaceHolder(_) => TokenStream::new(),
        }
    }

    pub(crate) fn reconcile(&mut self, other: GenUnit) -> Result<Vec<GenUnit>> {
        match (self, other) {
            (GenUnit::Struct(s), GenUnit::Struct(other)) => s.reconcile(other),
            (GenUnit::Enum(s), GenUnit::Enum(other)) => s.borrow_mut().reconcile(other.take()),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct GenTree {
    pub(crate) unit: Option<GenUnit>,
    pub(crate) children: HashMap<String, GenTree>,
    pub(crate) moved_children: Vec<GenTree>,
}

impl GenTree {
    pub(crate) fn write_rs_xpath(&self, src: &std::path::Path, xpath: &str) {
        let mut path = src.to_path_buf();
        if let Some(unit) = &self.unit {
            let ts = unit.token_stream();
            if !ts.is_empty() {
                path.set_extension("rs");
                if path.exists() {
                    error!(
                        "File already exists at {:?}, rename with xpath config \"{}\"?",
                        path, xpath,
                    );
                } else {
                    debug!("Writing file {:?} {}", path, xpath);
                    crate::utils::write_rs(&path, ts);
                }
            }
        }

        path.set_extension("");
        for (p, child) in self.children.iter() {
            if let Some(ref unit) = child.unit {
                path.push(strip_r_prefix(&unit.mod_name()));
            };
            child.write_rs_xpath(&path, &format!("{}/{}", xpath, p));
            if child.unit.is_some() {
                path.pop();
            }
        }
        for child in self.moved_children.iter() {
            if let Some(ref unit) = child.unit {
                path.push(strip_r_prefix(&unit.mod_name()));
            };
            child.write_rs_xpath(&path, xpath);
            if child.unit.is_some() {
                path.pop();
            }
        }
    }

    pub(crate) fn write_rs(&self, src: &std::path::Path) {
        self.write_rs_xpath(src, "");
    }

    pub(crate) fn root_mods(&self) -> Vec<String> {
        fn collect_root_mods(tree: &GenTree) -> Vec<String> {
            tree.children
                .values()
                .flat_map(|child| {
                    if let Some(ref unit) = child.unit {
                        vec![unit.mod_name()]
                    } else {
                        collect_root_mods(child)
                    }
                })
                .collect()
        }

        collect_root_mods(self)
    }

    pub(crate) fn lookup_mut(&mut self, xpath: &str) -> &mut GenTree {
        let mut loc = self;

        for c in xpath.split('/').skip(1) {
            loc = loc.children.entry(c.to_string()).or_default();
        }

        loc
    }

    pub(crate) fn lookup_unit_mut(&mut self, xpath: &str) -> Option<&mut GenUnit> {
        self.lookup_mut(xpath).unit.as_mut()
    }

    pub(crate) fn file_conflict_at(
        &mut self,
        xpath: &str,
        name: &str,
    ) -> Option<(&mut GenUnit, String)> {
        let split: Vec<_> = xpath.split('/').collect();
        let parent = &split[..split.len() - 1].join("/");
        for (p, c) in self.lookup_mut(&parent).children.iter_mut() {
            if let Some(unit) = &mut c.unit {
                if unit.name() == name {
                    let xpath = format!("{}/{}", parent, p);
                    warn!("File conflict at xpath: {}", xpath);
                    return Some((unit, xpath));
                }
            }
        }
        None
    }

    pub(crate) fn insert_unit(&mut self, xpath: &str, unit: GenUnit) {
        let loc = self.lookup_mut(xpath);
        assert!(loc.unit.is_none());
        loc.unit = Some(unit);
    }

    pub(crate) fn move_children(&mut self, xpath: &str, new_parent: &str) {
        let loc = self.lookup_mut(xpath);
        let children = std::mem::take(&mut loc.children);
        self.lookup_mut(new_parent)
            .moved_children
            .extend(children.into_values());
    }
}
