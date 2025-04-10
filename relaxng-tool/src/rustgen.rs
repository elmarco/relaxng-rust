use heck::ToSnakeCase;
use heck::ToUpperCamelCase;
use indexmap::IndexMap;
use prettyplease::unparse;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;
use relaxng_model::datatype::relax::BuiltinDatatypeValue;
use relaxng_model::datatype::Datatypes;
use std::collections::HashSet;
use std::path::PathBuf;
use std::process::exit;
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

    let mut ctx = Context::new();
    ctx.deny_unknown = true;
    let _gen_pattern = generate_pattern(pattern, &mut ctx);
    let global = ctx.global;
    // dbg!(global.to_string());

    let tokens = quote! {
        use std::str::FromStr;
        use std::fmt;

        use serde::de::{self, Deserializer, Error as DeError};
        use serde::ser::Serializer;
        use serde::{Deserialize, Serialize};

        macro_rules! impl_enum_serialize {
            ($enum_type:ident) => {
                impl Serialize for $enum_type {
                    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                    where
                        S: Serializer,
                    {
                        serializer.serialize_str(self.as_str())
                    }
                }
            };
        }

        macro_rules! impl_enum_deserialize {
            ($enum_type:ident) => {
                impl<'de> Deserialize<'de> for $enum_type {
                    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                    where
                        D: Deserializer<'de>,
                    {
                        struct EnumVisitor;

                        impl<'de> de::Visitor<'de> for EnumVisitor {
                            type Value = $enum_type;

                            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                                formatter.write_str(concat!(
                                    "a string representing a ",
                                    stringify!($enum_type),
                                    " variant"
                                ))
                            }

                            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                            where
                                E: DeError,
                            {
                                $enum_type::from_str(value).map_err(E::custom)
                            }
                        }

                        deserializer.deserialize_str(EnumVisitor)
                    }
                }
            };
        }

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
    Choice,
}

struct Context {
    global: TokenStream,
    hint: Vec<Hint>,
    deny_unknown: bool,
    ref_seen: HashSet<String>,
}

impl Context {
    fn new() -> Self {
        Self {
            global: TokenStream::new(),
            hint: Vec::new(),
            deny_unknown: false,
            ref_seen: HashSet::new(),
        }
    }

    fn in_hint(&self, hint: Hint) -> bool {
        self.hint.last().map_or(false, |last| last == &hint)
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
            // quick-xml/serde doesn't handle untagged enums
            // https://github.com/tafia/quick-xml/issues/203
            push_hint!(ctx, Hint::Choice, {
                for p in vec {
                    let gen_pattern = generate_pattern(p, ctx);
                    choice_fields.extend(gen_pattern);
                }
            });
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
            let mut field = GenField::new("value", "$text", "String");
            field.set_optional(ctx.in_hint(Hint::Optional) || ctx.in_hint(Hint::Choice));

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

            let field_ty = name.to_upper_camel_case();
            let ty = match &**pattern {
                Pattern::Text => "String".to_string(),
                Pattern::DatatypeName {
                    datatype,
                    except: _,
                } => datatype_to_ty(datatype),
                Pattern::Choice(vec) => {
                    let mut e = GenEnum::new(&field_ty);
                    for p in vec {
                        use relaxng_model::datatype::DatatypeValues::*;

                        let Pattern::DatatypeValue { datatype } = p else {
                            panic!("Unexpected pattern in Choice for Attribute");
                        };
                        match datatype {
                            Relax(BuiltinDatatypeValue::TokenValue(value)) => e.insert(value),
                            Xsd(xsd_datatype_values) => todo!(),
                            _ => todo!(),
                        }
                    }
                    ctx.global.append_all(e.to_token_stream());
                    e.name
                }
                // Add other cases like DatatypeName, DatatypeValue etc. if required
                _ => panic!("Unsupported inner pattern for Attribute: {:?}", pattern),
            };

            let field_name = name.to_snake_case();
            // Use "@name" convention for XML attributes in serde
            let rename = format!("@{}", name);

            let mut field = GenField::new(&field_name, &rename, &ty);
            field.set_optional(ctx.in_hint(Hint::Optional) || ctx.in_hint(Hint::Choice));

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
                if ctx.in_hint(Hint::ZeroOrMore) || ctx.in_hint(Hint::OneOrMore) {
                    "s"
                } else {
                    ""
                }
            );

            let gen_pattern = push_hint!(ctx, Hint::None, { generate_pattern(pattern, ctx) });

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
            field.set_optional(
                ctx.in_hint(Hint::Optional) | ctx.in_hint(Hint::ZeroOrMore)
                    || ctx.in_hint(Hint::Choice),
            );
            field.set_vec(ctx.in_hint(Hint::ZeroOrMore) || ctx.in_hint(Hint::OneOrMore));
            GenFields::new_with(field)
        }
        Pattern::Ref(_span, name, pat_ref) => {
            if ctx.ref_seen.insert(name.clone()) {
                let rule = pat_ref.0.borrow();
                let rule = rule.as_ref().unwrap();
                generate_pattern(rule.pattern(), ctx)
            } else {
                panic!("Unhandled circular reference detected");
            }
        }
        Pattern::DatatypeValue { datatype: _ } => panic!("Unimplemented: DatatypeValue"),
        Pattern::DatatypeName { datatype, except } => {
            if except.is_some() {
                panic!("Unimplemented: DatatypeName with exception");
            }
            let ty = datatype_to_ty(datatype);
            let mut field = GenField::new("value", "$text", &ty);
            field.set_optional(ctx.in_hint(Hint::Optional) || ctx.in_hint(Hint::Choice));
            GenFields::new_with(field)
        }
        Pattern::List(_pattern) => panic!("Unimplemented: List"),
    }
}

fn datatype_to_ty(datatype: &Datatypes) -> String {
    use relaxng_model::datatype::*;

    match datatype {
        Datatypes::Relax(builtin_datatype) => todo!(),
        Datatypes::Xsd(xsd_datatypes) => match xsd_datatypes {
            xsd::XsdDatatypes::NormalizedString(string_facets) => todo!(),
            xsd::XsdDatatypes::String(string_facets) => todo!(),
            xsd::XsdDatatypes::Short(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedShort(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::Long(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::Int(min_max_facet, pattern_facet) => "i32".to_string(),
            xsd::XsdDatatypes::Integer(min_max_facet, pattern_facet) => {
                // should be some bigint
                "isize".to_string()
            }
            xsd::XsdDatatypes::PositiveInteger(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedInt(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedLong(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::Decimal {
                min_max,
                pattern,
                fraction_digits,
                total_digits,
            } => todo!(),
            xsd::XsdDatatypes::Double(pattern_facet) => todo!(),
            xsd::XsdDatatypes::NmTokens(length_facet) => todo!(),
            xsd::XsdDatatypes::NmToken(length_facet) => todo!(),
            xsd::XsdDatatypes::NcName(length_facet) => todo!(),
            xsd::XsdDatatypes::Token(length_facet) => todo!(),
            xsd::XsdDatatypes::Duration(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Date(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Datetime(pattern_facet) => todo!(),
            xsd::XsdDatatypes::AnyURI(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Language(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Boolean(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Id(pattern_facet) => todo!(),
            xsd::XsdDatatypes::IdRef(pattern_facet) => todo!(),
        },
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

struct GenEnum {
    name: String,
    variants: IndexMap<String, String>,
}

impl GenEnum {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            variants: IndexMap::new(),
        }
    }

    fn insert(&mut self, value: &str) {
        let name = value.to_upper_camel_case();
        self.variants.insert(name, value.to_owned());
    }
}

impl ToTokens for GenEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &Ident::new(&self.name, Span::call_site());
        let variants = self
            .variants
            .keys()
            .map(|variant| Ident::new(variant, Span::call_site()))
            .collect::<Vec<_>>();
        let values = self.variants.values().collect::<Vec<_>>();
        let gen = quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
            pub enum #name {
                #(#variants),*
            }

            impl #name {
                fn as_str(&self) -> &str {
                    match self {
                        #(Self::#variants => #values),*
                    }
                }
            }

            impl FromStr for #name {
                type Err = String;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    match s {
                        #(#values => Ok(Self::#variants)),*,
                        _ => Err(format!("Invalid {} value: {}", stringify!(#name), s)),
                    }
                }
            }

            impl_enum_serialize!(#name);
            impl_enum_deserialize!(#name);
        };
        tokens.extend(gen);
    }
}
