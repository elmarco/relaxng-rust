use std::{collections::HashMap, fs, path::PathBuf};

use serde::Deserialize;
use tracing::{info, warn};

use crate::xpath::XPath;

#[derive(Debug, Default, Deserialize)]
pub(crate) struct Config {
    #[serde(default, rename = "relaxng-gen")]
    pub general: Option<ConfigGen>,

    #[serde(default)]
    pub without_impl: bool,

    #[serde(default)]
    pub with_test: bool,

    #[serde(default)]
    pub force: bool,

    #[serde(default)]
    pub rule: HashMap<XPath, ConfigRule>,
}

impl Config {
    pub(crate) fn merge(&mut self, c: Config) {
        self.without_impl |= c.without_impl;
        if c.general.is_some() {
            self.general = c.general;
        }
        for (xpath, rule) in c.rule {
            let xpath_clone = xpath.clone();
            self.rule
                .entry(xpath)
                .and_modify(|r| {
                    warn!("Merging rule for {}", xpath_clone);
                    r.merge(&rule);
                })
                .or_insert(rule);
        }
    }

    pub(crate) fn load_toml(&mut self, path: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
        info!("Loading TOML configuration from {}...", path.display());
        let config_str = fs::read_to_string(path)?;
        let c = toml::from_str(&config_str)?;
        self.merge(c);
        Ok(())
    }
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct ConfigGen {
    // the module documentation
    pub doc: Option<String>,
}

#[derive(Debug, Deserialize, Clone, Default)]
pub struct ConfigRule {
    // skip this element
    #[serde(default)]
    pub skip: bool,

    // rename the associated Rust type
    pub name: Option<String>,

    // rename the associated field
    pub field_name: Option<String>,

    // replace with Text
    #[serde(default)]
    pub replace_with_text: bool,

    // The associated documentation.
    pub doc: Option<String>,

    // Whether to propagate the doc to users of this type.
    // Default: true (propagate)
    #[serde(default)]
    pub doc_propagate: Option<bool>,

    // used to indicate that the parent type is this
    // For cases like: <element><choice>...</choice></element> -> choice
    #[serde(default)]
    pub collapse_with_parent: bool,

    // allow the structure to be just an enum,
    // This is okay if the enum is a common reference for ex
    #[serde(default)]
    pub allow_structure_is_enum: bool,

    // Name for the merged enum when multiple choice enums are reconciled.
    // The original variant-specific enums are kept separate, while a merged
    // enum with all variants is created for the builder.
    pub merged_name: Option<String>,
}

impl ConfigRule {
    pub(crate) fn merge(&mut self, c: &ConfigRule) {
        self.skip |= c.skip;
        if c.name.is_some() {
            self.name = c.name.clone();
        }
        if c.field_name.is_some() {
            self.field_name = c.field_name.clone();
        }
        self.replace_with_text |= c.replace_with_text;
        self.collapse_with_parent |= c.collapse_with_parent;
        self.allow_structure_is_enum |= c.allow_structure_is_enum;
        if c.doc.is_some() {
            self.doc = c.doc.clone();
        }
        if c.doc_propagate.is_some() {
            self.doc_propagate = c.doc_propagate;
        }
        if c.merged_name.is_some() {
            self.merged_name = c.merged_name.clone();
        }
    }
}
