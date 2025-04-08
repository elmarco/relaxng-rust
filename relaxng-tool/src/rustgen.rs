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

#[derive(Debug, PartialEq)]
enum Hint {
    None,
    ZeroOrMore,
    OneOrMore,
}

struct Context {
    global: TokenStream,
    hint: Vec<Hint>,
    deny_unknown: bool,
}

impl Context {
    fn new() -> Self {
        Self {
            global: TokenStream::new(),
            hint: Vec::new(),
            deny_unknown: false,
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
        Pattern::Choice(vec) => todo!(),
        Pattern::Interleave(vec) => todo!(),
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
        Pattern::Mixed(pattern) => todo!(),
        Pattern::Empty => todo!(),
        Pattern::Text => {
            quote! {
                #[serde(rename = "$text")]
                text: String,
            }
        }
        Pattern::NotAllowed => todo!(),
        Pattern::Optional(pattern) => todo!(),
        Pattern::ZeroOrMore(pattern) => {
            push_hint!(ctx, Hint::ZeroOrMore, { generate_pattern(pattern, ctx) })
        }
        Pattern::OneOrMore(pattern) => {
            push_hint!(ctx, Hint::OneOrMore, { generate_pattern(pattern, ctx) })
        }
        Pattern::Attribute(name_class, pattern) => todo!(),
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

            if ctx.zero_or_more() {
                quote! {
                    #serde_rename
                    #[serde(skip_serializing_if = "Option::is_none")]
                    #field_name: Option<Vec<#struct_name>>
                }
            } else if ctx.one_or_more() {
                quote! {
                    #serde_rename
                    #field_name: Vec<#struct_name>
                }
            } else {
                quote! {
                    #serde_rename
                    #field_name: #struct_name
                }
            }
        }
        Pattern::Ref(span, _, pat_ref) => todo!(),
        Pattern::DatatypeValue { datatype } => todo!(),
        Pattern::DatatypeName { datatype, except } => todo!(),
        Pattern::List(pattern) => todo!(),
    }
}
