use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use heck::ToUpperCamelCase;
use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt, format_ident, quote};
use syn::{Ident, Path, parse_quote};

use crate::utils::{has_default_match_arm, safe_ty_name, safe_var_name};

use super::{
    FieldTy, GenField, GenUnit, Result,
    genfield::{GenFields, gen_mods_from_fields},
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct GenEnum {
    // rs type name
    pub(crate) name: Option<String>,
    pub(crate) simple_variants: GenFields,
    pub(crate) group_variants: HashMap<String, GenFields>,
    pub(crate) group: Option<GenFields>,
    pub(crate) is_complex: bool,
    pub(crate) not_allowed: bool,
}

impl Display for GenEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.as_deref().unwrap_or("enum"))
    }
}

impl GenEnum {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn is_none(&self) -> bool {
        self.name.is_none()
    }

    pub(crate) fn name(&self) -> &str {
        self.name.as_ref().expect("missing enum name")
    }

    pub(crate) fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }

    pub(crate) fn ident(&self) -> Ident {
        format_ident!("{}", self.name())
    }

    pub(crate) fn path(&self) -> Path {
        let ident = self.ident();

        parse_quote! { #ident }
    }

    pub(crate) fn builder_ident(&self) -> Ident {
        format_ident!("{}Builder", self.name())
    }

    pub(crate) fn add_field(&mut self, field: GenField) -> Result<Option<GenUnit>> {
        if !(field.is_value() || field.is_text() || field.is_parse()) {
            self.is_complex = true;
        }
        if let Some(group) = &mut self.group {
            self.is_complex = true;
            group.add_field(field, true)?;
        } else {
            self.simple_variants.add_field(field, false)?;
        }

        Ok(None)
    }

    pub(crate) fn push_group(&mut self) {
        // we really don't handle nesting weird cases
        assert!(self.group.is_none());
        self.group = Some(Default::default());
    }

    pub(crate) fn pop_group(&mut self, name: Option<String>) {
        let n = self.group_variants.len();
        let name = name.unwrap_or_else(|| format!("Variant{}", n));
        self.group_variants.insert(name, self.group.take().unwrap());
    }

    pub(crate) fn var_name(&self) -> Ident {
        format_ident!("{}", safe_var_name(self.name()))
    }

    fn iter_fields(&self) -> impl Iterator<Item = &GenField> {
        self.simple_variants
            .iter()
            .chain(self.group_variants.values().flatten())
    }

    fn all_unique_fields(&self) -> Vec<GenField> {
        let mut all_fields = IndexMap::<String, GenField>::new();

        for f in self.iter_fields() {
            let entry = all_fields.entry(f.name());
            entry
                .and_modify(|e| {
                    e.optional |= f.optional;
                    e.multiple |= f.multiple;
                })
                .or_insert(f.clone());
        }

        all_fields.into_values().collect()
    }

    fn gen_from_xml_fn(&self) -> TokenStream {
        let mut from_xml_attrs = Vec::new();
        let mut from_xml_elems = Vec::new();
        let mut from_xml_text = Vec::new();
        let mut from_xml_other = Vec::new();

        let fields = self.all_unique_fields();

        for field in fields {
            field.gen_from_xml(
                &mut from_xml_attrs,
                &mut from_xml_elems,
                &mut from_xml_text,
                &mut from_xml_other,
                true,
            );
        }

        if !has_default_match_arm(&from_xml_text) {
            from_xml_text.push(quote! { _ => {} });
        }
        gen_from_xml_fn(&from_xml_attrs, &from_xml_elems, &from_xml_text)
    }

    fn gen_from_str_fn(&self) -> Option<TokenStream> {
        if self.is_complex {
            return None;
        }

        let mut match_arms = Vec::new();
        for field in self.all_unique_fields() {
            let field_id = field.single_ident();
            let arm = match &field.ty {
                FieldTy::Value(val) => {
                    quote! {
                        #val => {
                            builder.#field_id()?;
                        }
                    }
                }
                FieldTy::Text => {
                    let build_field = field.gen_from_text_build_field(None);
                    quote! {
                        val => {
                            #build_field
                        }
                    }
                }
                FieldTy::Parse(path) => {
                    let build_field = field.gen_from_text_build_field(Some(path));
                    quote! {
                        val => {
                            #build_field
                        }
                    }
                }
                FieldTy::Choice(_gen_enum) => todo!(),
                FieldTy::Xml(_path) => unreachable!(),
                FieldTy::Empty => unreachable!(),
            };
            match_arms.push(arm);
        }

        if !has_default_match_arm(&match_arms) {
            match_arms.push(quote! { other => { dbg!(other); } });
        }

        let res = quote! {
            pub fn from_str(s: &str) -> Result<Self> {
                let mut builder = Self::builder();
                match s {
                    #(#match_arms),*
                }
                builder.build()
            }
        };

        Some(res)
    }

    pub(crate) fn reconcile(&mut self, other: GenEnum) -> Result<Vec<GenUnit>> {
        let mut ret = Vec::new();

        let new_units = self.simple_variants.reconcile(other.simple_variants)?;
        ret.extend(new_units);

        // todo: reconcile / remove dups?
        self.group_variants.extend(other.group_variants);

        Ok(ret)
    }

    pub(crate) fn set_not_allowed(&mut self, not_allowed: bool) {
        self.not_allowed = not_allowed;
    }
}

impl ToTokens for GenEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.name();
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();

        let all_fields = self.all_unique_fields();
        let builder_fns: TokenStream = all_fields.iter().map(GenField::gen_builder_fn).collect();
        let builder_fields = all_fields.iter().cloned().map(|mut f| {
            f.set_optional(true);
            f
        });

        let mut build = quote! {};
        let mut build_err_or_empty = quote! { Err(Error::BuilderVariant(#name)) };
        let mut variants = Vec::new();
        let mut to_xml_attr = Vec::new();
        let mut to_xml = Vec::new();
        let mut to_xml_empty = vec![quote! { true }];
        let from_xml_fn = self.gen_from_xml_fn();

        for field in &self.simple_variants {
            let field_ident = field.ident();
            let (varid, ty) = match &field.ty {
                FieldTy::Empty => {
                    let id = field.variant_name();
                    build_err_or_empty = quote! { Ok(#name_ident::#id) };
                    (id, None)
                }
                FieldTy::Text => {
                    let id = field.variant_name();
                    (id, Some(format_ident!("String")))
                }
                FieldTy::Xml(path) | FieldTy::Parse(path) => {
                    let ty = path.get_ident().unwrap().clone();
                    let id = safe_ty_name(&ty.to_string());
                    let id = format_ident!("{}", id);
                    (id, Some(ty))
                }
                FieldTy::Choice(gen_enum) => {
                    let path = gen_enum.borrow().path();
                    let id = path.get_ident().unwrap().clone();
                    let ty = format_ident!("{}", id.to_string().to_upper_camel_case());
                    (id, Some(ty))
                }
                FieldTy::Value(name) => {
                    let id = format_ident!("{}", safe_ty_name(name));
                    (id, None)
                }
            };
            let (typ, elem) = if ty.is_some() {
                let typ = if field.multiple {
                    quote! { (Vec<#ty>) }
                } else {
                    quote! { (#ty) }
                };
                (Some(typ), Some(quote! { (#field_ident) }))
            } else {
                (None, None)
            };
            let rgen = quote! {
                #varid #typ
            };
            variants.push(rgen);

            // to_xml
            let mut to_xml_attrs = quote! {};
            let mut to_xml_elems = quote! {};
            field.gen_to_xml(
                false,
                &mut to_xml_attrs,
                &mut to_xml_elems,
                &mut to_xml_empty,
            );

            let rgen = quote! {
                Self::#varid #elem => {
                    #to_xml_attrs
                }
            };
            to_xml_attr.push(rgen);

            let rgen = quote! {
                Self::#varid #elem => {
                    #to_xml_elems
                }
            };
            to_xml.push(rgen);

            // build()
            let val = if ty.is_some() {
                let mut val = quote! { &self.#field_ident };
                if !field.multiple {
                    val = quote! { #val.as_ref().unwrap() };
                };
                quote! { #name_ident::#varid(Clone::clone(#val)) }
            } else {
                quote! { #name_ident::#varid }
            };
            // check other fields are none/empty?
            let rgen = if field.multiple {
                quote! {
                    if !self.#field_ident.is_empty() {
                        return Ok(#val)
                    }
                }
            } else {
                quote! {
                    if self.#field_ident.is_some() {
                        return Ok(#val)
                    }
                }
            };
            build.append_all(rgen);
        }

        for (var_name, fields) in self.group_variants.iter() {
            // variant enum
            let ty = fields.iter().map(|f| f.to_token_stream());
            let variant = format_ident!("{}", var_name);
            let rgen = quote! {
                #variant {
                    #(#ty),*
                }
            };
            variants.push(rgen);

            // all field names
            let name: Vec<_> = fields.iter().map(|f| f.ident()).collect();

            // to_xml*()
            let mut to_xml_attrs = quote! {};
            let mut to_xml_elems = quote! {};
            for field in fields {
                field.gen_to_xml(
                    false,
                    &mut to_xml_attrs,
                    &mut to_xml_elems,
                    &mut to_xml_empty,
                );
            }
            let rgen = quote! {
                Self::#variant { #(#name),* } => {
                    #to_xml_attrs
                }
            };
            to_xml_attr.push(rgen);

            let rgen = quote! {
                Self::#variant { #(#name),* } => {
                    #to_xml_elems
                }
            };
            to_xml.push(rgen);

            // build()
            let rgen = quote! {
                // check other fields are none?
                if #(self.#name.is_some())&&* {
                    return Ok(#name_ident::#variant {
                        #(#name: Clone::clone(self.#name.as_ref().unwrap()).into()),*
                    })
                }
            };
            build.append_all(rgen);
        }

        let from_str_fn = self.gen_from_str_fn();
        let fields = self.iter_fields();
        let mods = gen_mods_from_fields(fields);

        let rgen = quote! {
            #mods

            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub enum #name_ident {
                #(#variants),*
            }

            impl ToXml for #name_ident {
                type Builder = #builder_ident;

                fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write,
                {
                    use quick_xml::events::{BytesEnd, BytesStart, BytesText, Event};

                    match self {
                        #(#to_xml),*
                    }
                    Ok(())
                }
            }

            impl #name_ident {
                #from_xml_fn

                #from_str_fn

                pub fn to_xml_attr(&self, mut start: &mut quick_xml::events::BytesStart<'_>) -> Result<()> {
                    match self {
                        #(#to_xml_attr),*
                    }
                    Ok(())
                }

                pub fn is_empty(&self) -> bool {
                    // TODO: to improve to avoid unnecessary serial and O(complexity)
                    self.to_string().is_empty()
                }
            }

            #[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #builder_ident {
                #(#builder_fields),*
            }

            impl #builder_ident {
                #builder_fns

                pub fn build(&self) -> Result<#name_ident> {
                    #build

                    #build_err_or_empty
                }
            }
        };

        tokens.extend(rgen);
    }
}

fn gen_from_xml_fn(
    from_xml_attrs: &[TokenStream],
    from_xml_elems: &[TokenStream],
    from_xml_text: &[TokenStream],
) -> TokenStream {
    quote! {
        pub fn from_xml(node: &roxmltree::Node, from_child: &mut Option<roxmltree::Node>) -> Result<Self>
        {
            let mut builder = Self::builder();

            #(#from_xml_attrs);*

            loop {
                let Some(child) = from_child else {
                    break;
                };
                match child.node_type() {
                    roxmltree::NodeType::Element => match child.tag_name().name() {
                        #(#from_xml_elems),*
                        _ => {
                        }
                    },
                    roxmltree::NodeType::Text => {
                        if let Some(val) = child.text() {
                            match val {
                                #(#from_xml_text),*
                            }
                        }
                    }
                    _ => {}
                }
                *from_child = child.next_sibling();
                if let Ok(build) = builder.build() {
                    return Ok(build);
                }
            }

            builder.build()
        }

    }
}

pub(crate) type GenEnumRef = Rc<RefCell<GenEnum>>;

impl From<GenEnum> for GenEnumRef {
    fn from(gen_enum: GenEnum) -> Self {
        Rc::new(RefCell::new(gen_enum))
    }
}
