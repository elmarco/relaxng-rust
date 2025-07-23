use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::{TokenStreamExt, format_ident, quote};
use syn::{Ident, Path, parse_quote};
#[allow(unused_imports)]
use tracing::{Level, debug, instrument, span, trace};

use super::genfield::{GenFields, gen_mods_from_fields};
use super::genstruct::GenStruct;
use super::{FieldTy, GenField, GenUnit, Result};
use crate::rustgen::Config;
use crate::rustgen::genstruct::gen_impl_display;
use crate::utils::{has_default_match_arm, safe_var_name};

#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) struct Fields {
    pub(crate) doc: Option<String>,
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

// -> variant_name, fields, orig_fields, doc
type GroupedVariants<'a> = Vec<(&'a str, Vec<(&'a GenField, &'a GenField)>, Option<&'a str>)>;

#[derive(Debug, Clone, Default)]
pub(crate) struct GenEnum {
    pub(crate) debug: String,
    // rs type name
    pub(crate) name: Option<String>,
    pub(crate) all_fields: GenFields,
    pub(crate) simple_variants: Fields,
    pub(crate) group_variants: IndexMap<String, Fields>,
    pub(crate) group: Option<Fields>,
    pub(crate) units: Vec<GenUnit>,
    pub(crate) doc: Option<String>,
    pub(crate) as_element: Option<GenStruct>,
    /// Name for the merged enum when this enum is reconciled with others.
    /// Set from config's merged_name field.
    pub(crate) merged_name: Option<String>,
}

impl PartialEq for GenEnum {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.all_fields == other.all_fields
            && self.simple_variants == other.simple_variants
            && self.group_variants == other.group_variants
            && self.group == other.group
            && self.units == other.units
            && self.doc == other.doc
            && self.as_element == other.as_element
    }
}

impl Display for GenEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.as_deref().unwrap_or("enum"))
    }
}

impl GenEnum {
    pub fn new(debug: String) -> Self {
        Self {
            debug,
            name: Default::default(),
            all_fields: Default::default(),
            simple_variants: Default::default(),
            group_variants: Default::default(),
            group: Default::default(),
            units: Default::default(),
            doc: Default::default(),
            as_element: Default::default(),
            merged_name: Default::default(),
        }
    }

    pub(crate) fn set_merged_name(&mut self, name: Option<String>) {
        self.merged_name = name;
    }

    pub(crate) fn name_is_none(&self) -> bool {
        self.name.is_none()
    }

    pub(crate) fn name(&self) -> &str {
        if let Some(name) = self.name.as_ref() {
            if name.is_empty() {
                panic!("enum name cannot be empty");
            }
            name
        } else {
            debug!(?self);
            panic!("missing enum name")
        }
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

    pub(crate) fn group_variants(&self) -> GroupedVariants<'_> {
        let mut ret = Vec::new();

        for (name, fields) in &self.group_variants {
            let pair_fields = fields
                .fields
                .iter()
                .map(|(idx, field)| (self.all_fields.get_index(*idx).unwrap(), field))
                .collect();
            ret.push((name.as_str(), pair_fields, fields.doc.as_deref()));
        }

        ret
    }

    pub(crate) fn variant_for(&self, value: &FieldTy) -> Option<&GenField> {
        // only the Value variants?
        self.simple_variants().iter().find(|f| f.ty == *value).copied()
    }

    // can't be parsed from literal string
    pub(crate) fn is_complex(&self) -> bool {
        if !self.group_variants().is_empty() {
            return true;
        }

        for field in self.simple_variants() {
            match &field.ty {
                FieldTy::Choice {
                    gen_enum,
                    ..
                } => {
                    if gen_enum.borrow().is_complex() {
                        return true;
                    }
                }
                FieldTy::Xml {
                    ..
                } => {
                    return true;
                }
                _ => continue,
            }
        }

        false
    }

    pub(crate) fn add_field(&mut self, mut field: GenField) -> Result<()> {
        // don't make variant fields pub
        field.public = false;

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
        trace!("Pushing group");
        assert!(self.group.is_none());
        let fields = Fields::default();
        self.group = Some(fields);
    }

    pub(crate) fn pop_group(&mut self, name: Option<String>, doc: Option<String>) {
        trace!("Popping group {:?}", name);
        let n = self.group_variants.len();
        let mut variant = self.group.take().expect("Not in a group");
        variant.doc = doc;
        let name = name.unwrap_or_else(|| format!("Variant{n}"));
        self.group_variants.insert(name, variant);
    }

    pub(crate) fn var_name(&self) -> Ident {
        format_ident!("{}", safe_var_name(self.name()))
    }

    pub(crate) fn mod_name(&self) -> String {
        self.var_name().to_string()
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

    // sort by fixed values first
    fn all_fields_sorted(&self) -> Vec<GenField> {
        let mut all_fields = self.all_fields();
        all_fields.sort_by_key(|field| match field.ty {
            FieldTy::Empty => 0,
            FieldTy::Value(_) => 0,
            FieldTy::Choice {
                ..
            } => 1,
            FieldTy::Text => 2,
            FieldTy::Xml {
                ..
            } => 2,
            FieldTy::Parse(_) => 2,
        });
        all_fields
    }

    fn gen_from_xml_fn(&self) -> TokenStream {
        let mut from_xml_attrs = Vec::new();
        let mut from_xml_elems = Vec::new();
        let mut from_xml_text = Vec::new();
        let mut from_xml_other = Vec::new();

        let fields = self.all_fields_sorted();

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

        let mut from_xml =
            gen_from_xml_fn(&self.ident(), &from_xml_attrs, &from_xml_elems, &from_xml_text);
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

    fn gen_from_str(&self) -> Option<TokenStream> {
        if self.is_complex() {
            return None;
        }

        let mut match_arms = Vec::new();
        for field in self.all_fields_sorted() {
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
                FieldTy::Choice {
                    gen_enum,
                    ..
                } => {
                    let gen_enum = gen_enum.borrow();
                    let ident = gen_enum.ident();
                    quote! {
                        val if #ident::from_str(val).is_ok() => {
                            let val: #ident = #ident::from_str(val).unwrap();
                            builder.#field_id(val)?;
                        }
                    }
                }
                FieldTy::Xml {
                    ..
                } => unreachable!(),
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

        let name_ident = self.ident();
        let res = quote! {
            impl FromStr for #name_ident {
                type Err = Error;

                fn from_str(s: &str) -> Result<Self> {
                    let mut builder = Self::builder();
                    match s {
                        #(#match_arms),*
                    }
                    builder.build()
                }
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

        for (mut name, mut fields) in other.group_variants {
            if let Some(group) = self.group_variants.get(&name) {
                if *group == fields {
                    continue;
                } else {
                    name = String::from("RenameThisVariant");
                }
            }
            self.push_group();
            let doc = fields.doc.take();
            for field in fields.into_iter_fields() {
                self.add_field(field)?;
            }
            self.pop_group(Some(name), doc);
        }

        Ok(new_units)
    }

    fn add_unit(&mut self, new_unit: GenUnit) {
        self.units.push(new_unit);
    }
}

impl GenEnum {
    pub(crate) fn to_token_stream(&self, config: &Config) -> TokenStream {
        let mut tokens = TokenStream::new();

        let name = self.name();
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();

        let all_fields = self.all_fields();
        let builder_fns: TokenStream = all_fields.iter().map(GenField::gen_builder_fn).collect();
        let builder_fields = all_fields.iter().cloned().map(|mut f| {
            f.set_optional(true);
            f.to_token_stream(config)
        });

        let mut build = quote! {};
        let mut build_group = quote! {};
        let mut build_end = quote! { Err(Error::BuilderVariant(#name)) };
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
                    build_end = quote! { Ok(#name_ident::#var_name) };
                    (var_name, None)
                }
                FieldTy::Text => {
                    let var_name = field.variant_name();
                    let ty = parse_quote! { String };
                    (var_name, Some(ty))
                }
                FieldTy::Xml {
                    path,
                    ..
                }
                | FieldTy::Parse(path) => {
                    let var_name = field.variant_name();
                    let ty = path.clone();
                    let var_name = format_ident!("{}", var_name);
                    (var_name, Some(ty))
                }
                FieldTy::Choice {
                    gen_enum,
                    ..
                } => {
                    let path = gen_enum.borrow().path();
                    let var_name = path.get_ident().unwrap().clone();
                    (var_name, Some(path))
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
            field.gen_to_xml(false, &mut to_xml_attrs, &mut to_xml_elems, &mut to_xml_empty);

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

        let mut unconditional_build = None;
        for (var_name, fields, doc) in self.group_variants() {
            fn iter_skip_fix(
                fields: &Vec<(&GenField, &GenField)>,
            ) -> impl Iterator<Item = GenField> {
                fields.iter().filter(|&(_, f)| f.fixed_value().is_none()).map(|&(f, orig)| {
                    let mut f = f.clone();
                    f.optional = orig.optional;
                    f
                })
            }

            // variant enum
            let ty = iter_skip_fix(&fields).map(|f| f.to_token_stream(config));
            let variant = format_ident!("{}", var_name);
            let doc = doc.map(|doc| {
                quote! {
                    #[doc = #doc]
                }
            });
            let rgen = quote! {
                #doc
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
                let mut clone;
                if orig.fixed_value().is_some() {
                    field = orig;
                } else {
                    clone = field.clone();
                    clone.optional = orig.optional;
                    field = &clone;
                    to_xml_attrs.extend(quote! { let elem = #field_ident; });
                }
                // debug!("{} {}", field.name(), field.optional);
                field.gen_to_xml(false, &mut to_xml_attrs, &mut to_xml_elems, &mut to_xml_empty);
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
                let value = match orig.ty {
                    FieldTy::Empty | FieldTy::Value(_) | FieldTy::Text => Some(&orig.ty),
                    _ => {
                        // not sure if we should allow matching the other types
                        // needs some test cases
                        None
                    }
                };
                if orig.multiple {
                    cond.push(quote! { !self.#name.is_empty()});
                } else if field.ty != orig.ty
                    && let Some(value) = value
                {
                    let FieldTy::Choice {
                        gen_enum,
                        ..
                    } = &field.ty
                    else {
                        // not sure how to handle this case, or if it can happen
                        unreachable!()
                    };
                    let genenum_ref = gen_enum.borrow();
                    let variant = match genenum_ref.variant_for(value) {
                        Some(variant) => variant,
                        None => {
                            panic!("cannot find matching variant for {value:?} in {genenum_ref:#?}")
                        }
                    };
                    let variant_name = variant.variant_name();
                    let variant = if variant.optional || variant.is_text() || variant.is_parse() {
                        quote! { #variant_name(_) }
                    } else {
                        quote! { #variant_name }
                    };
                    let ty_path = field.ty.path();
                    let mut c = quote! { matches!(self.#name, Some(#ty_path::#variant)) };
                    if orig.optional {
                        c = quote! { (self.#name.is_none() || #c) };
                    }
                    cond.push(c);
                } else {
                    cond.push(quote! { self.#name.is_some() });
                }
                if orig.fixed_value().is_some() {
                    continue;
                }
                if orig.optional || orig.multiple {
                    refs.push(quote! { &self.#name });
                } else {
                    refs.push(quote! { self.#name.as_ref().unwrap() });
                }
            }

            let mut rgen = quote! {
                return Ok(#name_ident::#variant {
                    #(#name: Clone::clone(#refs).into()),*
                });
            };
            if cond.is_empty() {
                assert!(unconditional_build.is_none()); // what should it build then?
                unconditional_build = Some(rgen);
            } else {
                rgen = quote! {
                    // check other fields are none?
                    if #(#cond)&&* {
                        #rgen
                    }
                };
                build_group.append_all(rgen);
            }
        }
        if let Some(rgen) = unconditional_build {
            build_end = rgen;
        }

        let from_str = self.gen_from_str();
        let fields = self.iter_all_fields();
        let mods = gen_mods_from_fields(fields);

        let doc = self.doc.as_ref().map(|doc| {
            quote! {
                #[doc = #doc]
            }
        });
        let mut rgen = quote! {
            #mods

            #doc
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub enum #name_ident {
                #(#variants),*
            }
        };

        if !config.without_impl {
            let impl_display = gen_impl_display(&name_ident);

            let impl_toxml = quote! {
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
            };

            rgen.extend(quote! {
                #impl_toxml

                #impl_display

                impl #name_ident {
                    #from_xml_fn

                    pub fn to_xml_attr(
                        &self,
                        mut start: &mut quick_xml::events::BytesStart<'_>,
                    ) -> Result<()> {
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

                #from_str

                #[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
                pub struct #builder_ident {
                    #(#builder_fields),*
                }

                impl #builder_ident {
                    #builder_fns

                    pub fn build(&self) -> Result<#name_ident> {
                        #build_group
                        #build

                        #build_end
                    }
                }
            });
        }

        tokens.extend(rgen);
        tokens
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
