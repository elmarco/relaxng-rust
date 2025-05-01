use std::path::PathBuf;

use check_keyword::CheckKeyword;
use heck::{ToSnakeCase, ToUpperCamelCase};
use prettyplease::unparse;
use proc_macro2::TokenStream;
use syn::parse_quote;
use tracing::debug;

pub(crate) fn write_rs(path: &PathBuf, stream: TokenStream) {
    std::fs::create_dir_all(path.parent().unwrap()).expect("Creating directory");
    let st = stream.to_string();
    match syn::parse2(stream) {
        Ok(file) => {
            let generated = unparse(&file);
            std::fs::write(path, generated).expect("writing rs code");
        }
        Err(err) => {
            debug!(?st);
            panic!("{}", err);
        }
    }
}

fn is_default_match_arm(arm: &syn::Arm) -> bool {
    fn is_default_pattern(pat: &syn::Pat) -> bool {
        match pat {
            syn::Pat::Wild(_) => true,
            syn::Pat::Ident(pat_ident) => {
                // Ensure it's not a struct pattern like Struct { x, .. }
                !pat_ident.by_ref.is_some() && !pat_ident.subpat.is_some()
            }
            // Check each pattern in an "or" pattern
            syn::Pat::Or(pat_or) => pat_or.cases.iter().any(|p| is_default_pattern(p)),

            _ => false,
        }
    }

    is_default_pattern(&arm.pat)
}

pub(crate) fn has_default_match_arm(arms: &[TokenStream]) -> bool {
    let m: syn::ExprMatch = parse_quote!(match n { #(#arms),* });

    m.arms.iter().any(|arm| is_default_match_arm(arm))
}

pub(crate) fn safe_var_name(name: &str) -> String {
    let mut name = name.to_snake_case();

    // Prepend underscore if name starts with a digit
    if name.chars().next().map_or(false, |c| c.is_digit(10)) {
        name = format!("_{}", name);
    }

    name.into_safe()
}

pub(crate) fn safe_ty_name(name: &str) -> String {
    let mut name = name.to_upper_camel_case();

    // Prepend underscore if name starts with a digit
    if name.chars().next().map_or(false, |c| c.is_digit(10)) {
        name = format!("_{}", name);
    }

    name.into_safe()
}
