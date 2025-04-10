use heck::ToSnakeCase;
use heck::ToUpperCamelCase;
use indexmap::IndexMap;
use prettyplease::unparse;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use std::arch::x86_64::_SIDD_CMP_EQUAL_ORDERED;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::process::exit;
use syn::AttrStyle;
use syn::Ident;

use relaxng_model::model::NameClass;
use relaxng_model::model::Pattern;

use relaxng_model::Compiler;

pub(crate) fn generate(schema: PathBuf) {
    let mut compiler = Compiler::default();
    let model = match compiler.compile(&schema) {
        Ok(m) => m,
        Err(err) => {
            compiler.dump_diagnostic(&err);
            exit(1);
        }
    };
    let p = model.as_ref();
    let p = p.borrow();
    let pattern = p.as_ref().unwrap().pattern();
    dbg!(&pattern);

    let mut ctx = Context::new();
    ctx.deny_unknown = true;
    let _gen_pattern = generate_pattern(pattern, &mut ctx);
    let global = ctx.global;
    dbg!(global.to_string());

    let tokens = quote! {
        use serde::{Serialize, Deserialize};

        #global
    };

    let file = syn::parse2(tokens).unwrap();
    let unparsed = unparse(&file);
    println!("{unparsed}");
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Hint {
    None,
    ZeroOrMore,
    OneOrMore,
    Optional,
}

struct Context {
    global: TokenStream,
    hint: Vec<Hint>,
    deny_unknown: bool,
    /// True if currently generating fields within a Choice pattern branch.
    in_choice: bool,
}

impl Context {
    fn new() -> Self {
        Self {
            global: TokenStream::new(),
            hint: Vec::new(),
            deny_unknown: false,
            in_choice: false,
        }
    }

    fn zero_or_more(&self) -> bool {
        let Some(last) = self.hint.last() else {
            return false;
        };

        *last == Hint::ZeroOrMore
    }

    fn one_or_more(&self) -> bool {
        let Some(last) = self.hint.last() else {
            return false;
        };

        *last == Hint::OneOrMore
    }

    fn optional(&self) -> bool {
        let Some(last) = self.hint.last() else {
            return false;
        };

        *last == Hint::Optional
    }
}

macro_rules! push_hint {
    ($ctx:expr, $hint:expr, $block:expr) => {{
        $ctx.hint.push($hint);
        let gen_pattern = $block;
        $ctx.hint.pop();
        gen_pattern
    }};
}

fn generate_pattern(pattern: &Pattern, ctx: &mut Context) -> GenFields {
    match pattern {
        Pattern::Choice(vec) => {
            let mut choice_fields = GenFields::new();
            ctx.in_choice = true;
            push_hint!(ctx, Hint::None, {
                for p in vec {
                    let gen_pattern = generate_pattern(p, ctx);
                    choice_fields.extend(gen_pattern);
                }
            });
            ctx.in_choice = false;
            choice_fields
        }
        Pattern::Interleave(_vec) => panic!("Unimplemented: Interleave"),
        Pattern::Group(vec) => {
            let mut group_fields = GenFields::new();
            push_hint!(ctx, Hint::None, {
                for p in vec {
                    let gen_pattern = generate_pattern(p, ctx);
                    group_fields.extend(gen_pattern);
                }
            });
            group_fields
        }
        Pattern::Mixed(_pattern) => panic!("Unimplemented: Mixed"),
        Pattern::Empty => GenFields::new(),
        Pattern::Text => {
            let is_already_optional = ctx.optional();
            let mut field = GenField::new("value", "$text", "String");
            field.set_optional(ctx.in_choice && !is_already_optional);

            GenFields::new_with(field)
        }
        Pattern::NotAllowed => panic!("Unimplemented: NotAllowed"),
        Pattern::Optional(pattern) => {
            push_hint!(ctx, Hint::Optional, { generate_pattern(pattern, ctx) })
        }
        Pattern::ZeroOrMore(pattern) => {
            push_hint!(ctx, Hint::ZeroOrMore, { generate_pattern(pattern, ctx) })
        }
        Pattern::OneOrMore(pattern) => {
            push_hint!(ctx, Hint::OneOrMore, { generate_pattern(pattern, ctx) })
        }
        Pattern::Attribute(name_class, pattern) => {
            let NameClass::Named {
                namespace_uri: _,
                name,
            } = name_class
            else {
                panic!("Unexpected name class for attribute");
            };

            // TODO: Handle inner patterns other than Text if needed.
            // For now, assume attributes contain simple text.
            let ty = match **pattern {
                Pattern::Text => "String",
                // Add other cases like DatatypeName, DatatypeValue etc. if required
                _ => panic!("Unsupported inner pattern for Attribute: {:?}", pattern),
            };

            let field_name = name.to_snake_case();
            // Use "@name" convention for XML attributes in serde
            let rename = format!("@{}", name);

            let mut field = GenField::new(&field_name, &rename, ty);
            field.set_optional(ctx.optional() || ctx.in_choice);

            GenFields::new_with(field)
        }
        Pattern::Element(name_class, pattern) => {
            let NameClass::Named {
                namespace_uri: _,
                name,
            } = name_class
            else {
                panic!("Unexpected name class");
            };
            let struct_name = name.to_upper_camel_case();
            let field_name = format!(
                "{}{}",
                name.to_snake_case(),
                if ctx.zero_or_more() || ctx.one_or_more() {
                    "s"
                } else {
                    ""
                }
            );

            let gen_pattern = generate_pattern(pattern, ctx);

            let mut serde_attr = Vec::new();
            if ctx.deny_unknown {
                serde_attr.push(quote! {
                    deny_unknown_fields
                });
            }
            if struct_name != *name {
                serde_attr.push(quote! {
                    rename = #name
                })
            };

            let struct_attr = if !serde_attr.is_empty() {
                Some(quote! {
                    #[serde(#(#serde_attr),*)]
                })
            } else {
                None
            };

            {
                let struct_name = Ident::new(&struct_name, Span::call_site());
                let global = &ctx.global;
                ctx.global = quote! {
                    #global

                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
                    #struct_attr
                    pub struct #struct_name {
                        #gen_pattern
                    }
                };
            }

            let mut field = GenField::new(&field_name, name, &struct_name);
            field.set_optional(ctx.optional() | ctx.zero_or_more() || ctx.in_choice);
            field.set_vec(ctx.zero_or_more() || ctx.one_or_more());
            GenFields::new_with(field)
        }
        Pattern::Ref(_span, _, _pat_ref) => panic!("Unimplemented: Ref"),
        Pattern::DatatypeValue { datatype: _ } => panic!("Unimplemented: DatatypeValue"),
        Pattern::DatatypeName {
            datatype: _,
            except: _,
        } => panic!("Unimplemented: DatatypeName"),
        Pattern::List(_pattern) => panic!("Unimplemented: List"),
    }
}

#[derive(Debug)]
struct GenField {
    name: String,
    rename: String,
    ty: String,
    optional: bool,
    vec: bool,
}

impl GenField {
    fn new(name: &str, rename: &str, ty: &str) -> Self {
        Self {
            name: name.to_string(),
            rename: rename.to_string(),
            ty: ty.to_string(),
            optional: false,
            vec: false,
        }
    }

    fn set_optional(&mut self, optional: bool) {
        self.optional = optional;
    }

    fn set_vec(&mut self, vec: bool) {
        self.vec = vec;
    }

    fn is_attr(&self) -> bool {
        self.name.starts_with('@')
    }
}

impl ToTokens for GenField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut serde_attr = Vec::new();
        if self.name != self.rename {
            let rename = &self.rename;
            serde_attr.push(quote! { rename = #rename });
        };
        if self.optional {
            serde_attr.push(quote! { skip_serializing_if = "Option::is_none" });
        };
        let serde_attr = if !serde_attr.is_empty() {
            Some(quote! {
                #[serde(#(#serde_attr),*)]
            })
        } else {
            None
        };
        let ty = Ident::new(&self.ty, Span::call_site());
        let ty = if self.vec {
            quote! { Vec<#ty> }
        } else {
            quote! { #ty }
        };
        let ty = if self.optional {
            quote! { Option<#ty> }
        } else {
            quote! { #ty }
        };
        let field_name = Ident::new(&self.name, Span::call_site());
        let gen = quote! {
            #serde_attr
            #field_name: #ty
        };
        tokens.extend(gen);
    }
}

#[derive(Debug)]
struct GenFields {
    fields: IndexMap<String, GenField>,
}

impl GenFields {
    fn new() -> Self {
        Self {
            fields: IndexMap::new(),
        }
    }

    fn new_with(field: GenField) -> Self {
        let mut fields = IndexMap::new();
        fields.insert(field.name.clone(), field);

        Self { fields }
    }

    fn extend(&mut self, others: GenFields) {
        for (other_name, mut other) in others.fields.into_iter() {
            let entry = self.fields.entry(other_name);
            match entry {
                indexmap::map::Entry::Occupied(entry) => {
                    if !other.is_attr() && entry.get().is_attr() {
                        let entry = entry.into_mut();
                        std::mem::swap(&mut *entry, &mut other);
                    }

                    other.name = format!("{}_attr", other.name);
                    self.fields.insert(other.name.clone(), other);
                }
                indexmap::map::Entry::Vacant(entry) => {
                    entry.insert(other);
                }
            }
        }
    }
}

impl ToTokens for GenFields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let fields = self.fields.values();
        let gen = quote! { #(#fields),* };
        tokens.extend(gen);
    }
}
