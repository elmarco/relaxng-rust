use std::path::Path;

use check_keyword::CheckKeyword;
use heck::{ToSnakeCase, ToUpperCamelCase};
use prettyplease::unparse;
use proc_macro2::TokenStream;
use relaxng_model::{
    datatype::{DatatypeValues, relax::BuiltinDatatypeValue},
    model::{NameClass, Pattern},
};
use syn::parse_quote;
use tracing::debug;

pub(crate) fn ts_to_string(stream: TokenStream) -> String {
    let st = stream.to_string();
    match syn::parse2(stream) {
        Ok(file) => unparse(&file),
        Err(err) => {
            debug!(?st);
            panic!("{}", err);
        }
    }
}

pub(crate) fn write_rs(path: &Path, stream: TokenStream) {
    std::fs::create_dir_all(path.parent().unwrap()).expect("Creating directory");
    std::fs::write(path, ts_to_string(stream)).expect("writing rs code");
}

fn is_default_match_arm(arm: &syn::Arm) -> bool {
    fn is_default_pattern(pat: &syn::Pat) -> bool {
        match pat {
            syn::Pat::Wild(_) => true,
            syn::Pat::Ident(pat_ident) => {
                // Ensure it's not a struct pattern like Struct { x, .. }
                pat_ident.by_ref.is_none() && pat_ident.subpat.is_none()
            }
            // Check each pattern in an "or" pattern
            syn::Pat::Or(pat_or) => pat_or.cases.iter().any(is_default_pattern),

            _ => false,
        }
    }

    is_default_pattern(&arm.pat) && arm.guard.is_none()
}

pub(crate) fn has_default_match_arm(arms: &[TokenStream]) -> bool {
    let m: syn::ExprMatch = parse_quote!(match n { #(#arms),* });

    m.arms.iter().any(is_default_match_arm)
}

pub(crate) fn safe_var_name(name: &str) -> String {
    let mut name = name.to_snake_case();

    // Prepend underscore if name starts with a digit
    if name.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        name = format!("_{name}");
    }

    name.into_safe()
}

pub(crate) fn safe_ty_name(name: &str) -> String {
    let mut name = name.to_upper_camel_case();

    // Prepend underscore if name starts with a digit
    if name.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        name = format!("_{name}");
    }

    name.into_safe()
}

pub(crate) fn strip_r_prefix(input: &str) -> String {
    input.strip_prefix("r#").unwrap_or(input).to_string()
}

pub(crate) fn pattern_to_string(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Choice(_) => "choice".to_string(),
        Pattern::Interleave(_) => "interleave".to_string(),
        Pattern::Group(_) => "group".to_string(),
        Pattern::Mixed(_) => "mixed".to_string(),
        Pattern::Empty => "empty".to_string(),
        Pattern::Text => "text".to_string(),
        Pattern::NotAllowed => "notAllowed".to_string(),
        Pattern::Optional(_) => "optional".to_string(),
        Pattern::ZeroOrMore(_) => "zeroOrMore".to_string(),
        Pattern::OneOrMore(_) => "oneOrMore".to_string(),
        Pattern::Attribute(nc, _) => format!("@{}", name_class_to_name(nc)),
        Pattern::Element(nc, _) => format!("<{}>", name_class_to_name(nc)),
        Pattern::Ref(_, rf, _) => format!("ref({rf})"),
        Pattern::DatatypeValue {
            datatype,
        } => format!("={}", datatype_value(datatype)),
        Pattern::DatatypeName {
            ..
        } => "datatypeName".to_string(),
        Pattern::List(_) => "list".to_string(),
    }
}

pub(crate) fn name_class_to_name(name_class: &NameClass) -> &str {
    match name_class {
        NameClass::Named {
            name,
            ..
        } => name,
        NameClass::NsName {
            ..
        } => todo!(),
        NameClass::AnyName {
            ..
        } => "anyName",
        NameClass::Alt {
            ..
        } => "alt",
    }
}

pub(crate) fn datatype_value(dt: &DatatypeValues) -> &str {
    match dt {
        DatatypeValues::Relax(value) => match value {
            BuiltinDatatypeValue::TokenValue(val) => val.as_str(),
            BuiltinDatatypeValue::StringValue(_val) => todo!(),
        },
        DatatypeValues::Xsd(_xsd_datatype_values) => todo!(),
    }
}
