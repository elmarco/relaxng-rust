use std::path::{Path, PathBuf};

use indexmap::IndexMap;
use proc_macro2::TokenStream;
use tracing::info;
#[allow(unused_imports)]
use tracing::{Level, debug, error, span, warn};

use crate::error::Error;
use crate::rustgen::Config;
use crate::utils::strip_r_prefix;

use super::Result;
use super::genenum::GenEnumRef;
use super::genmod::GenLib;
use super::genstruct::GenStruct;

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct GenTree {
    pub(crate) units: IndexMap<PathBuf, GenUnit>,
}

impl GenTree {
    pub(crate) fn get_mut(&mut self, path: &Path) -> Option<&mut GenUnit> {
        self.units.get_mut(path)
    }

    pub(crate) fn insert_unit(&mut self, path: &Path, xpath: &str, unit: GenUnit) {
        match self.units.entry(path.to_path_buf()) {
            indexmap::map::Entry::Occupied(_entry) => {
                // this must be handled first with unit_conflict_at()
                unreachable!("Unit conflict at {} (for {xpath})", path.display());
            }
            indexmap::map::Entry::Vacant(entry) => {
                entry.insert(unit);
            }
        }
    }

    pub(crate) fn write_rs(&self, dst: &std::path::Path, config: &Config) {
        for (unit_path, unit) in &self.units {
            let ts = unit.to_token_stream(config);
            if !ts.is_empty() {
                let mut path = dst.to_path_buf();
                path.push(unit_path);
                path.set_extension("rs");
                info!("Writing unit {:?}", path);
                if path.exists() && !config.force {
                    panic!("File {:?} already exists, use -f to overwrite", path);
                }
                crate::utils::write_rs(&path, ts);
            }
        }
    }

    pub(crate) fn root_mods(&self) -> Vec<String> {
        let mut mods: Vec<String> = self
            .units
            .iter()
            .filter(|(path, _)| path.components().count() == 1)
            .map(|(_, unit)| unit.mod_name(false))
            .collect();
        mods.sort();
        mods
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum GenUnit {
    Lib(GenLib),
    Enum(GenEnumRef),
    Struct(GenStruct),
}

impl GenUnit {
    pub(crate) fn mod_name(&self, stripr: bool) -> String {
        let ret = match self {
            GenUnit::Lib(m) => m.name().into(),
            GenUnit::Enum(e) => e.borrow().mod_name(),
            GenUnit::Struct(s) => s.mod_name(),
        };
        if stripr {
            strip_r_prefix(&ret)
        } else {
            ret
        }
    }

    pub(crate) fn to_token_stream(&self, config: &Config) -> TokenStream {
        match self {
            GenUnit::Lib(m) => m.to_token_stream(config),
            GenUnit::Struct(st) => st.to_token_stream(config),
            GenUnit::Enum(en) => en.borrow().to_token_stream(config),
        }
    }

    pub(crate) fn reconcile(&mut self, other: GenUnit) -> Result<Vec<GenUnit>> {
        match (self, other) {
            (GenUnit::Struct(s), GenUnit::Struct(other)) => s.reconcile(other),
            (GenUnit::Enum(gen_enum), GenUnit::Enum(other)) => {
                let mut gen_enum = gen_enum.borrow_mut();
                let res = gen_enum.reconcile(other.take());
                let mut other = other.borrow_mut();
                // as others may still reference the old enum
                *other = gen_enum.clone();
                // just for safety
                other.debug = format!("-> {}", gen_enum.debug);
                res
            }
            _ => {
                // handling enum && struct is tricky, since earlier fields could reference the struct Path
                // we may want to allow a configuration to force a struct as an enum?
                // for now, users should rename the conflicting unit
                Err(Error::Other)
            }
        }
    }
}
