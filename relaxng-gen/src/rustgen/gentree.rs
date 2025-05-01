use std::collections::HashMap;

use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::ToTokens;
use tracing::error;

use crate::utils::safe_var_name;

use super::{Result, genenum::GenEnumRef, genmod::GenMod, genstruct::GenStruct};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum GenUnit {
    Mod(GenMod),
    Enum(GenEnumRef),
    Struct(GenStruct),
}

impl GenUnit {
    pub(crate) fn name(&self) -> String {
        match self {
            GenUnit::Mod(m) => m.name().to_string(),
            GenUnit::Enum(e) => e.borrow().name().to_string(),
            GenUnit::Struct(s) => s.name().to_string(),
        }
    }

    pub(crate) fn mod_name(&self) -> String {
        self.name().to_snake_case()
    }

    pub(crate) fn token_stream(&self) -> TokenStream {
        match self {
            GenUnit::Mod(m) => m.to_token_stream(),
            GenUnit::Struct(st) => st.to_token_stream(),
            GenUnit::Enum(en) => en.borrow().to_token_stream(),
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
}

impl GenTree {
    pub(crate) fn write_rs(&self, src: &std::path::Path) {
        let mut path = src.to_path_buf();
        if let Some(unit) = &self.unit {
            let ts = unit.token_stream();
            path.set_extension("rs");
            if path.exists() {
                error!(
                    "File already exists at {:?}, rename with xpath config?",
                    path
                );
            } else {
                crate::utils::write_rs(&path, ts);
            }
        }
        path.set_extension("");
        for child in self.children.values() {
            if let Some(ref unit) = child.unit {
                path.push(unit.mod_name());
            };
            child.write_rs(&path);
            if child.unit.is_some() {
                path.pop();
            }
        }
    }

    pub(crate) fn root_mods(&self) -> Vec<String> {
        fn collect_root_mods(tree: &GenTree) -> Vec<String> {
            tree.children
                .values()
                .flat_map(|child| {
                    if let Some(ref unit) = child.unit {
                        vec![safe_var_name(&unit.mod_name())]
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

    pub(crate) fn insert_unit(&mut self, xpath: &str, unit: GenUnit) {
        let loc = self.lookup_mut(xpath);
        assert!(loc.unit.is_none());
        loc.unit = Some(unit);
    }
}
