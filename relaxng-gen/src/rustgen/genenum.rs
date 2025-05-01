use std::{cell::RefCell, fmt::Display, rc::Rc};

use heck::ToUpperCamelCase;
use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt, format_ident, quote};
use syn::{Ident, Path, parse_quote};
use tracing::debug;

use crate::utils::{has_default_match_arm, safe_ty_name, safe_var_name};

use super::{
    FieldTy, GenField, GenUnit, Result,
    genfield::{GenFields, gen_mods_from_fields},
    genstruct::GenStruct,
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct Fields {
    // all_fields idx -> original field
    pub(crate) fields: Vec<(usize, GenField)>,
}

impl Fields {
    fn push(&mut self, idx: usize, field: GenField) {
        self.fields.push((idx, field));
    }

    fn contains_idx(&self, idx: usize) -> bool {
        self.fields.iter().any(|(i, _)| *i == idx)
    }

    fn contains_field(&self, field: &GenField) -> bool {
        self.fields.iter().any(|(_, f)| f == field)
    }

    fn iter_idx(&self) -> impl Iterator<Item = &usize> {
        self.fields.iter().map(|(idx, _)| idx)
    }

    fn into_iter_fields(self) -> impl Iterator<Item = GenField> {
        self.fields.into_iter().map(|(_, field)| field)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct GenEnum {
    // rs type name
    pub(crate) name: Option<String>,
    pub(crate) all_fields: GenFields,
    pub(crate) simple_variants: Fields,
    pub(crate) group_variants: IndexMap<String, Fields>,
    pub(crate) group: Option<Fields>,
    // can't be serialized as simple string literal
    pub(crate) is_complex: bool,
    pub(crate) not_allowed: bool,
    pub(crate) units: Vec<GenUnit>,
    pub(crate) doc: Option<String>,
    pub(crate) as_element: Option<GenStruct>,
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

    pub(crate) fn set_as_element(&mut self, st: GenStruct) {
        assert!(st.fields.is_empty());
        self.set_name(st.name.clone());
        self.as_element = Some(st);
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

    pub(crate) fn simple_variants(&self) -> Vec<&GenField> {
        let mut ret = Vec::new();

        self.simple_variants.iter_idx().for_each(|idx| {
            let field = self.all_fields.get_index(*idx).unwrap();
            ret.push(field);
        });

        ret
    }

    // -> variant_name, fields, orig_fields
    pub(crate) fn group_variants(&self) -> Vec<(&str, Vec<(&GenField, &GenField)>)> {
        let mut ret = Vec::new();

        for (name, fields) in &self.group_variants {
            let fields = fields
                .fields
                .iter()
                .map(|(idx, field)| (self.all_fields.get_index(*idx).unwrap(), field))
                .collect();
            ret.push((name.as_str(), fields));
        }

        ret
    }

    pub(crate) fn variant_for(&self, value: &str) -> Option<Ident> {
        self.simple_variants()
            .iter()
            .find(|f| f.name == value)
            .map(|f| f.variant_name())
    }

    pub(crate) fn add_field(&mut self, mut field: GenField) -> Result<()> {
        field.public = false;

        if !(field.is_value() || field.is_text() || field.is_parse()) {
            self.is_complex = true;
        }

        // Future
        // // Instead of adding Variant(Option<T>) do Variant(T) + None
        // if field.optional {
        //     field.optional = false;
        //     let mut field = field.clone();
        //     field.ty = FieldTy::Empty;
        //     field.name = "None".to_string();
        //     self.simple_variants.add_field(field, false)?;
        // }

        // Future
        // If the variant is a kind of tagged enum (some set of fixed value fields)
        // those field(s) shouldn't be in the variant itself
        let (idx, new_unit) = self.all_fields.add_field_full(field.clone(), false)?;
        if let Some(new_unit) = new_unit {
            self.add_unit(new_unit);
        }

        if let Some(group) = &mut self.group {
            self.is_complex = true;
            // for group variant fields, retain all original fields for tag enum support?
            if !group.contains_field(&field) {
                group.push(idx, field);
            }
        } else {
            // but for simple variants, only new fields should be added
            if !self.simple_variants.contains_idx(idx) {
                self.simple_variants.push(idx, field);
            }
        };

        Ok(())
    }

    pub(crate) fn push_group(&mut self) {
        // we really don't handle nesting weird cases
        assert!(self.group.is_none());
        self.group = Some(Default::default());
    }

    pub(crate) fn pop_group(&mut self, name: Option<String>) {
        let n = self.group_variants.len();
        let name = name.unwrap_or_else(|| format!("Variant{n}"));
        self.group_variants.insert(name, self.group.take().unwrap());
    }

    pub(crate) fn var_name(&self) -> Ident {
        format_ident!("{}", safe_var_name(self.name()))
    }

    fn iter_all_fields(&self) -> impl Iterator<Item = &GenField> {
        self.all_fields.iter()
    }

    fn all_fields(&self) -> Vec<GenField> {
        let mut all_fields = IndexMap::<String, GenField>::new();

        for f in self.iter_all_fields() {
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

        let fields = self.all_fields();

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

        let mut from_xml = gen_from_xml_fn(
            &self.ident(),
            &from_xml_attrs,
            &from_xml_elems,
            &from_xml_text,
        );
        if self.as_element.is_some() {
            from_xml = quote! {
                pub fn from_xml(node: &roxmltree::Node) -> Result<Self> {
                    #from_xml

                    from_xml(node, &mut node.first_child())
                }
            };
        }
        from_xml
    }

    fn gen_from_str_fn(&self) -> Option<TokenStream> {
        if self.is_complex {
            return None;
        }

        let mut match_arms = Vec::new();
        for field in self.all_fields() {
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
                FieldTy::Choice(gen_enum) => {
                    let gen_enum = gen_enum.borrow();
                    debug!("{}", gen_enum.ident());
                    quote! {
                        _ => { todo!(); }
                    }
                }
                FieldTy::Xml(_path) => unreachable!(),
                FieldTy::Empty => {
                    quote! {
                        "" => {
                            builder.#field_id()?;
                        }
                    }
                }
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
        let (_new_idx, new_units) = self.all_fields.reconcile(other.all_fields)?;

        // we shouldn't be in a group when reconciling
        assert!(self.group.is_none());

        // readding should take care of dedup, even if slightly less efficient
        for (_old_idx, field) in other.simple_variants.fields {
            self.add_field(field)?;
        }

        for (name, fields) in other.group_variants {
            self.push_group();
            for field in fields.into_iter_fields() {
                self.add_field(field)?;
            }
            self.pop_group(Some(name));
        }

        Ok(new_units)
    }

    pub(crate) fn set_not_allowed(&mut self, not_allowed: bool) {
        self.not_allowed = not_allowed;
    }

    fn add_unit(&mut self, new_unit: GenUnit) {
        self.units.push(new_unit);
    }
}

impl ToTokens for GenEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.name();
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();

        let all_fields = self.all_fields();
        let builder_fns: TokenStream = all_fields.iter().map(GenField::gen_builder_fn).collect();
        let builder_fields = all_fields.iter().cloned().map(|mut f| {
            f.set_optional(true);
            f
        });

        let mut build = quote! {};
        let mut build_group = quote! {};
        let mut build_err_or_empty = quote! { Err(Error::BuilderVariant(#name)) };
        let mut variants = Vec::new();
        let mut to_xml_attr = Vec::new();
        let mut to_xml = Vec::new();
        let mut to_xml_empty = vec![quote! { true }];
        let from_xml_fn = self.gen_from_xml_fn();

        let mut to_xml_begin = quote! {};
        let mut to_xml_end = quote! {};
        if let Some(elem) = &self.as_element {
            let name = &elem.xml_name;
            to_xml_begin = quote! { writer.write_event(Event::Start(BytesStart::new(#name)))?; };
            to_xml_end = quote! { writer.write_event(Event::End(BytesEnd::new(#name)))?; };
        }

        for field in self.simple_variants() {
            let field_ident = field.ident();
            let (var_name, ty) = match &field.ty {
                FieldTy::Empty => {
                    let var_name = field.variant_name();
                    build_err_or_empty = quote! { Ok(#name_ident::#var_name) };
                    (var_name, None)
                }
                FieldTy::Text => {
                    let var_name = field.variant_name();
                    (var_name, Some(format_ident!("String")))
                }
                FieldTy::Xml(path) | FieldTy::Parse(path) => {
                    let ty = path.get_ident().unwrap().clone();
                    let var_name = safe_ty_name(&ty.to_string());
                    let var_name = format_ident!("{}", var_name);
                    (var_name, Some(ty))
                }
                FieldTy::Choice(gen_enum) => {
                    let path = gen_enum.borrow().path();
                    let var_name = path.get_ident().unwrap().clone();
                    let ty = format_ident!("{}", var_name.to_string().to_upper_camel_case());
                    (var_name, Some(ty))
                }
                FieldTy::Value(_name) => {
                    let var_name = format_ident!("{}", field.variant_name());
                    (var_name, None)
                }
            };

            let ty = if ty.is_none() {
                if field.optional || field.multiple {
                    Some(quote! { () })
                } else {
                    None
                }
            } else {
                Some(quote! { #ty })
            };

            let (typ, var_field) = if ty.is_some() {
                let typ = if field.multiple {
                    quote! { (Vec<#ty>) }
                } else if field.optional {
                    quote! { (Option<#ty>) }
                } else {
                    quote! { (#ty) }
                };
                (Some(typ), Some(quote! { (#field_ident) }))
            } else {
                (None, None)
            };
            let doc = field.doc.as_ref().map(|doc| quote! { #[doc = #doc] });
            let rgen = quote! {
                #doc
                #var_name #typ
            };
            variants.push(rgen);

            // to_xml
            let mut to_xml_attrs = quote! {};
            if var_field.is_some() {
                to_xml_attrs.extend(quote! {
                    let elem = #field_ident;
                });
            }
            let mut to_xml_elems = to_xml_attrs.clone();
            field.gen_to_xml(
                false,
                &mut to_xml_attrs,
                &mut to_xml_elems,
                &mut to_xml_empty,
            );

            let rgen = quote! {
                Self::#var_name #var_field => {
                    #to_xml_attrs
                }
            };
            to_xml_attr.push(rgen);

            let rgen = quote! {
                Self::#var_name #var_field => {
                    #to_xml_elems
                }
            };
            to_xml.push(rgen);

            // build()
            let val = if ty.is_some() {
                let mut val = quote! { &self.#field_ident };
                if !field.multiple && !field.optional {
                    val = quote! { #val.as_ref().unwrap() };
                };
                quote! { #name_ident::#var_name(Clone::clone(#val)) }
            } else {
                quote! { #name_ident::#var_name }
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

        for (var_name, fields) in self.group_variants() {
            fn iter_skip_fix<'a>(
                fields: &'a Vec<(&GenField, &GenField)>,
            ) -> impl Iterator<Item = &'a GenField> {
                fields
                    .iter()
                    .filter(|&(_, f)| f.fixed_value().is_none())
                    .map(|&(f, _)| f)
            }

            // variant enum
            let ty = iter_skip_fix(&fields).map(|f| f.to_token_stream());
            let variant = format_ident!("{}", var_name);
            let rgen = quote! {
                #variant {
                    #(#ty),*
                }
            };
            variants.push(rgen);

            // all field names
            let name: Vec<_> = iter_skip_fix(&fields).map(|f| f.ident()).collect();

            // to_xml*()
            let mut to_xml_attrs = quote! {};
            let mut to_xml_elems = quote! {};
            fields.iter().for_each(|&(mut field, orig)| {
                let field_ident = field.ident();
                if orig.fixed_value().is_some() {
                    field = orig;
                } else {
                    to_xml_attrs.extend(quote! { let elem = #field_ident; });
                }
                field.gen_to_xml(
                    false,
                    &mut to_xml_attrs,
                    &mut to_xml_elems,
                    &mut to_xml_empty,
                );
            });

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
            let mut cond = vec![];
            let mut refs = vec![];
            for (field, orig) in fields {
                let name = field.ident();
                if field.multiple {
                    cond.push(quote! { !self.#name.is_empty()});
                } else if field.ty != orig.ty {
                    if let Some(value) = orig.fixed_value() {
                        let FieldTy::Choice(genenum) = &field.ty else {
                            // not sure how to handle this case, or if it can happen
                            unimplemented!()
                        };
                        let variant = genenum
                            .borrow()
                            .variant_for(value)
                            .expect("matching variant");
                        let ty_path = field.ty.path();
                        cond.push(quote! { matches!(self.#name, Some(#ty_path::#variant)) });
                    } else {
                        debug!(?field, ?orig);
                        // unimplemented!()
                        // if it's an optional fixed value, it should match it
                    }
                } else {
                    cond.push(quote! { self.#name.is_some() });
                }
                if field.optional || field.multiple {
                    refs.push(quote! { &self.#name });
                } else {
                    refs.push(quote! { self.#name.as_ref().unwrap() });
                }
            }
            let rgen = quote! {
                // check other fields are none?
                if #(#cond)&&* {
                    return Ok(#name_ident::#variant {
                        #(#name: Clone::clone(#refs).into()),*
                    })
                }
            };
            build_group.append_all(rgen);
        }

        let from_str_fn = self.gen_from_str_fn();
        let fields = self.iter_all_fields();
        let mods = gen_mods_from_fields(fields);

        let doc = self.doc.as_ref().map(|doc| {
            quote! {
                #[doc = #doc]
            }
        });
        let rgen = quote! {
            #mods

            #doc
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

                    #to_xml_begin

                    match self {
                        #(#to_xml),*
                    }

                    #to_xml_end

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
                    #build_group
                    #build

                    #build_err_or_empty
                }
            }
        };

        tokens.extend(rgen);
    }
}

fn gen_from_xml_fn(
    name_ident: &Ident,
    from_xml_attrs: &[TokenStream],
    from_xml_elems: &[TokenStream],
    from_xml_text: &[TokenStream],
) -> TokenStream {
    quote! {
        pub fn from_xml(node: &roxmltree::Node, from_child: &mut Option<roxmltree::Node>) -> Result<#name_ident>
        {
            let mut builder = #name_ident::builder();

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
