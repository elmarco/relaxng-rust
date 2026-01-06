use std::collections::HashMap;
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
use super::genfield::FieldTy;
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

    pub(crate) fn set_uses_any_element(&mut self, uses: bool) {
        for unit in self.units.values_mut() {
            if let GenUnit::Lib(lib) = unit {
                lib.set_uses_any_element(uses);
            }
        }
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
                // Use parent directory from unit_path but filename from unit's mod_name
                // This ensures the file name matches what imports expect
                if let Some(parent) = unit_path.parent() {
                    if parent.components().count() > 0 {
                        path.push(parent);
                    }
                }
                let mod_name = unit.mod_name(true);
                path.push(&mod_name);
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
        mods.dedup();
        mods
    }

    /// Update `all_optional` for all field references based on actual struct content.
    /// This must be called after all reconciliation is done and before code generation.
    pub(crate) fn update_all_optional(&mut self) {
        // Step 1: Build a map from struct ident name -> has_required_fields
        let mut struct_info: HashMap<String, bool> = HashMap::new();
        for unit in self.units.values() {
            if let GenUnit::Struct(st) = unit {
                let ident = st.ident().to_string();
                let has_required = st.has_required_fields();
                struct_info.insert(ident, has_required);
            }
        }

        // Step 2: Update all_optional for field references in all enums
        for unit in self.units.values_mut() {
            if let GenUnit::Enum(en) = unit {
                let mut en = en.borrow_mut();
                // Update fields in all_fields (GenFields)
                for field in en.all_fields.fields.values_mut() {
                    if let FieldTy::Xml {
                        path,
                        all_optional,
                        ..
                    } = &mut field.ty
                    {
                        if let Some(ident) = path.get_ident() {
                            let ident_str = ident.to_string();
                            if let Some(&has_required) = struct_info.get(&ident_str) {
                                let new_all_optional = !has_required;
                                if *all_optional != new_all_optional {
                                    debug!(
                                        "Updating all_optional for field '{}' (type {}): {} -> {}",
                                        field.name, ident_str, all_optional, new_all_optional
                                    );
                                    *all_optional = new_all_optional;
                                }
                            }
                        }
                    }
                }
            }
        }
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
