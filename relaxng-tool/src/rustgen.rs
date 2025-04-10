use heck::ToSnakeCase;
use heck::ToUpperCamelCase;
use prettyplease::unparse;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use std::path::PathBuf;
use std::process::exit;
use syn::parse_quote;
use syn::punctuated::Punctuated;
use syn::Field;
use syn::Ident;
use syn::Token;

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

fn generate_pattern(pattern: &Pattern, ctx: &mut Context) -> TokenStream {
    match pattern {
        Pattern::Choice(vec) => {
            let mut fields = Punctuated::<Field, Token![,]>::new();
            let mut choice_fields = TokenStream::new();
            ctx.in_choice = true;
            push_hint!(ctx, Hint::None, {
                for p in vec {
                    let gen_pattern = generate_pattern(p, ctx);
                    // Append the generated fields (as TokenStream) directly.
                    // generate_pattern should already return comma-terminated fields.
                    if !gen_pattern.is_empty() {
                        choice_fields.extend(gen_pattern);
                    }
                }
            });
            ctx.in_choice = false;
            // Return the combined TokenStream of fields from all choice branches.
            choice_fields
        }
        Pattern::Interleave(vec) => panic!("Unimplemented: Interleave"),
        Pattern::Group(vec) => {
            let mut fields = Punctuated::<Field, Token![,]>::new();
            push_hint!(ctx, Hint::None, {
                for p in vec {
                    let gen_pattern = generate_pattern(p, ctx);
                    fields.push(parse_quote! { #gen_pattern });
                }
            });

            quote! { #fields }
        }
        Pattern::Mixed(pattern) => panic!("Unimplemented: Mixed"),
        Pattern::Empty => panic!("Unimplemented: Empty"),
        Pattern::Text => {
            // Generate field for text content. Using "value" is common.
            let base_type = quote! { String };
            let is_already_optional = ctx.optional();
            let needs_option = ctx.in_choice && !is_already_optional;

            let final_type = if needs_option || is_already_optional {
                quote! { Option<#base_type> }
            } else {
                base_type
            };

            let skip_if_attr = if needs_option || is_already_optional {
                Some(quote! { #[serde(skip_serializing_if = "Option::is_none")] })
            } else {
                None
            };

            quote! {
                #[serde(rename = "$text")]
                #skip_if_attr
                value: #final_type,
            }
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
        Pattern::Attribute(name_class, pattern) => panic!("Unimplemented: Attribute"),
        Pattern::Element(name_class, pattern) => {
            let NameClass::Named {
                namespace_uri: _,
                name,
            } = name_class
            else {
                panic!("Unexpected name class");
            };
            let struct_name = name.to_upper_camel_case();
            let struct_name = Ident::new(&struct_name, Span::call_site());
            let field_name = Ident::new(
                &format!(
                    "{}{}",
                    name.to_snake_case(),
                    if ctx.zero_or_more() || ctx.one_or_more() {
                        "s"
                    } else {
                        ""
                    }
                ),
                Span::call_site(),
            );
            let gen_pattern = generate_pattern(pattern, ctx);

            let mut serde_attr = Vec::new();
            if ctx.deny_unknown {
                serde_attr.push(quote! {
                    deny_unknown_fields
                });
            }
            if struct_name != name {
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

            let global = &ctx.global;
            ctx.global = quote! {
                #global

                #[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
                #struct_attr
                pub struct #struct_name {
                    #gen_pattern
                }
            };

            let serde_rename = if field_name != name {
                Some(quote! {
                    #[serde(rename = #name)]
                })
            } else {
                None
            };

            // Determine base type based on hints
            let (base_field_type, is_already_optional) = if ctx.zero_or_more() {
                (quote! { Vec<#struct_name> }, true) // Vec implies optionality for serialization
            } else if ctx.one_or_more() {
                (quote! { Vec<#struct_name> }, false) // Vec<T> must have at least one item
            } else if ctx.optional() {
                (quote! { Option<#struct_name> }, true)
            } else {
                (quote! { #struct_name }, false)
            };

            // Wrap in Option if in choice and not already optional
            let needs_option = ctx.in_choice && !is_already_optional;
            let final_field_type = if needs_option {
                quote! { Option<#base_field_type> }
            } else {
                base_field_type
            };

            // Add skip_serializing_if if the final type is optional
            let skip_if_attr = if needs_option || is_already_optional {
                Some(quote! { #[serde(skip_serializing_if = "Option::is_none")] })
            } else {
                None
            };

            quote! {
                #serde_rename
                #skip_if_attr
                #field_name: #final_field_type,
            }
        }
        Pattern::Ref(span, _, pat_ref) => panic!("Unimplemented: Ref"),
        Pattern::DatatypeValue { datatype } => panic!("Unimplemented: DatatypeValue"),
        Pattern::DatatypeName { datatype, except } => panic!("Unimplemented: DatatypeName"),
        Pattern::List(pattern) => panic!("Unimplemented: List"),
    }
}
