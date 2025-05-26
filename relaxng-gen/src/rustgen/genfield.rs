use std::{collections::HashMap, fmt::Display};

use heck::ToSnakeCase;
use indexmap::IndexMap;
use pluralizer::pluralize;
use proc_macro2::{Literal, TokenStream};
use quote::{ToTokens, TokenStreamExt, format_ident, quote};
use syn::{Ident, Path, parse_quote};
use tracing::{debug, warn};

use crate::{
    rustgen::{Error, genenum::GenEnum},
    utils::{safe_ty_name, safe_var_name},
};

use super::{GenUnit, Result, genenum::GenEnumRef};

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct GenField {
    // name of the rs field/mod, ex "snake_case"
    pub(crate) name: String,
    // name of the xml element, ex "OriginalName"
    pub(crate) xml_name: String,
    // associate rs type, ex "String"
    pub(crate) ty: FieldTy,
    pub(crate) optional: bool,
    pub(crate) multiple: bool,
    pub(crate) serialize_as: SerializeAs,
    pub(crate) not_allowed: bool,
    pub(crate) in_ref: bool,
    pub(crate) recursive: bool,
}

impl GenField {
    pub(crate) fn new(xml_name: &str, ty: FieldTy) -> Self {
        Self {
            xml_name: xml_name.to_string(),
            name: xml_name.to_snake_case(),
            ty,
            optional: false,
            multiple: false,
            serialize_as: SerializeAs::Inline,
            not_allowed: false,
            in_ref: false,
            recursive: false,
        }
    }

    pub(crate) fn new_value(val: &str) -> GenField {
        Self::new(val, FieldTy::Value(val.to_string()))
    }

    pub(crate) fn set_xml_name(&mut self, name: &str) {
        self.xml_name = name.to_string();
        self.name = name.to_snake_case();
    }

    pub(crate) fn set_serialize_as(&mut self, serialize_as: SerializeAs) {
        self.serialize_as = serialize_as;
    }

    pub(crate) fn set_optional(&mut self, optional: bool) {
        self.optional = optional;
    }

    pub(crate) fn set_multiple(&mut self, multiple: bool) {
        self.multiple = multiple;
    }

    pub(crate) fn name(&self) -> String {
        let name = pluralize(&self.name, if self.multiple { 2 } else { 1 }, false);
        safe_var_name(&name)
    }

    pub(crate) fn xml_name_b(&self) -> Literal {
        Literal::byte_string(self.xml_name.as_bytes())
    }

    pub(crate) fn ident(&self) -> Ident {
        format_ident!("{}", self.name())
    }

    pub(crate) fn single_ident(&self) -> Ident {
        format_ident!("{}", safe_var_name(&self.name))
    }

    pub(crate) fn variant_name(&self) -> Ident {
        format_ident!("{}", safe_ty_name(&self.name))
    }

    pub(crate) fn ty_path(&self) -> Option<Path> {
        self.ty.path()
    }

    pub(crate) fn is_text(&self) -> bool {
        self.ty.is_text()
    }

    pub(crate) fn is_parse(&self) -> bool {
        self.ty.is_parse()
    }

    pub(crate) fn is_choice(&self) -> bool {
        self.ty.is_choice()
    }

    pub(crate) fn is_value(&self) -> bool {
        self.ty.is_value()
    }

    pub(crate) fn is_xml(&self) -> bool {
        self.ty.is_xml()
    }

    pub(crate) fn as_attribute(&self) -> bool {
        matches!(self.serialize_as, SerializeAs::Attribute)
    }

    pub(crate) fn as_element(&self) -> bool {
        matches!(self.serialize_as, SerializeAs::Element)
    }

    pub(crate) fn as_inline(&self) -> bool {
        matches!(self.serialize_as, SerializeAs::Inline)
    }

    pub(crate) fn gen_builder_fn(&self) -> TokenStream {
        let field_ident = self.ident();
        let field_ty = self.ty_path();
        let field_single = self.single_ident();

        let mut val = if field_ty.is_some() {
            quote! { #field_single.try_into()? }
        } else {
            quote! { () }
        };
        if self.optional && !self.multiple {
            val = quote! { Some(#val) };
            if !self.is_value() {
                val = quote! {
                    if let Some(#field_single) = #field_single {
                        #val
                    } else {
                        None
                    }
                };
            }
        }
        let body = if self.multiple {
            quote! { self.#field_ident.push(#val); }
        } else if self.optional {
            quote! { self.#field_ident = #val; }
        } else {
            quote! { self.#field_ident = Some(#val); }
        };
        let t = if self.optional && !self.multiple {
            quote! { Option<T> }
        } else {
            quote! { T }
        };

        let field_single_fn = if field_ty.is_some() {
            quote! {
                pub fn #field_single<T>(&mut self, #field_single: #t) -> Result<&mut Self>
                where
                    T: TryInto<#field_ty>,
                    Error: From<<T as TryInto<#field_ty>>::Error>
            }
        } else {
            quote! {
                pub fn #field_single(&mut self) -> Result<&mut Self>
            }
        };

        quote! {
            #field_single_fn
            {
                #body
                Ok(self)
            }
        }
    }

    pub(crate) fn gen_from_xml(
        &self,
        from_xml_attrs: &mut Vec<TokenStream>,
        from_xml_elems: &mut Vec<TokenStream>,
        from_xml_text: &mut Vec<TokenStream>,
        from_xml_other: &mut Vec<TokenStream>,
    ) {
        match &self.ty {
            FieldTy::Xml(ty) => {
                self.gen_from_ty_xml(ty, from_xml_elems);
            }
            FieldTy::Value(value) => {
                self.gen_from_value(value, from_xml_attrs, from_xml_text);
            }
            FieldTy::Parse(ty) => {
                self.gen_from_text(Some(ty), from_xml_attrs, from_xml_text);
            }
            FieldTy::Choice(gen_enum) => {
                let ty = gen_enum.borrow().path();
                self.gen_from_choice(&ty, from_xml_attrs, from_xml_elems, from_xml_other);
            }
            FieldTy::Text => {
                self.gen_from_text(None, from_xml_attrs, from_xml_text);
            }
            FieldTy::Empty => {}
        }
    }

    fn gen_from_ty_xml(&self, ty: &Path, from_xml_elems: &mut Vec<TokenStream>) {
        let field_xml_name = &self.xml_name;
        let field_ident = self.single_ident();

        let mut val = quote! { #ty::from_xml(&child)? };
        if self.optional && !self.multiple {
            val = quote! { Some(#val) };
        }

        from_xml_elems.push(quote! {
            #field_xml_name => { builder.#field_ident(#val)?; }
        });
    }

    fn gen_from_value(
        &self,
        value: &str,
        from_xml_attrs: &mut Vec<TokenStream>,
        from_xml_text: &mut Vec<TokenStream>,
    ) {
        let name = &self.xml_name;
        let field_single = self.single_ident();
        let build_field = quote! { builder.#field_single()?; };

        match self.serialize_as {
            SerializeAs::Attribute => {
                let pat = quote! {
                    if matches!(node.attribute(#name), Some(#value)) {
                        #build_field
                    }
                };
                from_xml_attrs.push(pat)
            }
            SerializeAs::Element => todo!(),
            SerializeAs::Inline => {
                let pat = quote! {
                    #value => {
                        #build_field
                    }
                };
                from_xml_text.push(pat)
            }
        }
    }

    pub(crate) fn gen_from_text_build_field(&self, ty_path: Option<&Path>) -> TokenStream {
        let field_name = self.name();
        let field_single = self.single_ident();

        let mut val = quote! { val };
        if let Some(path) = ty_path {
            val = quote! {
                #val.parse::<#path>().map_err(|e| Error::ParseError(#field_name, Box::new(e)))?
            }
        }
        if self.optional && !self.multiple {
            val = quote! { Some(#val) };
        };

        quote! { builder.#field_single(#val)?; }
    }

    fn gen_from_text(
        &self,
        ty_path: Option<&Path>,
        from_xml_attrs: &mut Vec<TokenStream>,
        from_xml_text: &mut Vec<TokenStream>,
    ) {
        let build_field = self.gen_from_text_build_field(ty_path);

        match self.serialize_as {
            SerializeAs::Attribute => {
                let xml_name = &self.xml_name;
                let pat = quote! {
                    if let Some(val) = node.attribute(#xml_name) {
                        #build_field
                    }
                };
                from_xml_attrs.push(pat)
            }
            SerializeAs::Element => {
                debug!(?self);
                todo!();
            }
            SerializeAs::Inline => {
                let pat = quote! {
                    _ => { #build_field }
                };
                from_xml_text.push(pat);
            }
        }
    }

    fn gen_from_choice(
        &self,
        ty: &Path,
        from_xml_attrs: &mut Vec<TokenStream>,
        from_xml_elems: &mut Vec<TokenStream>,
        from_xml_other: &mut Vec<TokenStream>,
    ) {
        let xml_name = &self.xml_name;
        let field_id = self.single_ident();

        match self.serialize_as {
            SerializeAs::Attribute => {
                let mut val = quote! { #ty::from_str(val)? };
                if self.optional && !self.multiple {
                    val = quote! { Some(#val) };
                };
                let attr = quote! {
                    if let Some(val) = node.attribute(#xml_name) {
                        builder.#field_id(#val)?;
                    }
                };
                from_xml_attrs.push(attr);
            }
            SerializeAs::Element => {
                self.gen_from_ty_xml(ty, from_xml_elems);
            }
            SerializeAs::Inline => {
                let field_ident = self.single_ident();

                let mut val = quote! { #ty::from_xml(node, child)? };
                if self.optional && !self.multiple {
                    val = quote! { Some(#val) };
                }

                from_xml_other.push(quote! {
                    let child = &mut node.first_child();
                    builder.#field_ident(#val)?;
                });
            }
        }
    }

    pub(crate) fn gen_to_xml(
        &self,
        for_self: bool,
        to_xml_attrs: &mut TokenStream,
        to_xml_elems: &mut TokenStream,
        to_xml_empty: &mut Vec<TokenStream>,
    ) {
        if self.is_choice() && self.as_inline() {
            let elem_to_xml = quote! { elem.to_xml_attr(&mut start)?; };
            let elem = self.to_xml_elem_wrap(for_self, elem_to_xml);
            to_xml_attrs.extend(elem);
        }

        let val = match &self.ty {
            FieldTy::Empty => {
                return;
            }
            FieldTy::Xml(_) => {
                quote! { unreachable!() }
            }
            FieldTy::Value(val) => {
                quote! { #val }
            }
            FieldTy::Choice(_) => {
                quote! { &elem.to_string() }
            }
            FieldTy::Text | FieldTy::Parse(_) => {
                quote! { &elem.to_string() }
            }
        };

        let mut elem_to_xml = match self.serialize_as {
            SerializeAs::Attribute => {
                let name_b = self.xml_name_b();
                quote! { start.push_attribute((&#name_b[..], quick_xml::escape::escape(#val).as_bytes())); }
            }
            SerializeAs::Element | SerializeAs::Inline => {
                if self.is_text() || self.is_parse() || self.is_value() {
                    quote! { writer.write_event(Event::Text(BytesText::new(#val)))?; }
                } else {
                    quote! { elem.to_xml(writer)?; }
                }
            }
        };

        if self.as_element() && !self.is_xml() {
            let name = &self.xml_name;
            elem_to_xml = quote! {
                writer.write_event(Event::Start(BytesStart::new(#name)))?;
                #elem_to_xml
                writer.write_event(Event::End(BytesEnd::new(#name)))?;
            };
        }

        let elem = self.to_xml_elem_wrap(for_self, elem_to_xml);
        if self.as_attribute() {
            to_xml_attrs.extend(elem);
        } else {
            let is_empty = self.to_xml_is_empty(for_self);
            to_xml_empty.push(quote! {
                (#is_empty)
            });
            to_xml_elems.extend(elem);
        }
    }

    fn to_xml_is_empty(&self, for_self: bool) -> TokenStream {
        let check = if self.is_choice() {
            quote! { elem.is_empty() }
        } else {
            quote! { false }
        };
        let empty = if self.multiple {
            quote! { elem.iter().all(|elem| #check) }
        } else if self.optional {
            quote! { elem.as_ref().map(|elem| #check).unwrap_or(true) }
        } else {
            quote! { #check }
        };

        self.elem_code(for_self, empty)
    }

    fn to_xml_elem_wrap(&self, for_self: bool, mut elem_to_xml: TokenStream) -> TokenStream {
        if self.multiple {
            elem_to_xml = quote! {
                for elem in elem {
                    #elem_to_xml
                }
            }
        } else if self.optional {
            elem_to_xml = quote! {
                if let Some(elem) = elem {
                    #elem_to_xml
                }
            }
        }

        self.elem_code(for_self, elem_to_xml)
    }

    fn elem_code(&self, for_self: bool, elem_code: TokenStream) -> TokenStream {
        if self.is_value() && !for_self {
            return elem_code;
        }

        let field_ident = self.ident();
        let this = if for_self {
            quote! { &self. }
        } else {
            quote! {}
        };

        quote! {
            {
                let elem = #this #field_ident;
                #elem_code
            }
        }
    }

    pub(crate) fn gen_mod(&self) -> TokenStream {
        let ty = self.ty_path();
        if ty.is_none() || self.is_parse() || self.is_text() {
            return quote! {};
        }
        let mod_name = self.mod_name();
        if self.in_ref {
            quote! {
                use crate::xml::#mod_name::#ty;
            }
        } else {
            quote! {
                pub mod #mod_name;
                use #mod_name::#ty;
            }
        }
    }

    pub(crate) fn reconcile(
        &mut self,
        mut other: GenField,
        reconcile_multiple: bool,
    ) -> Result<Option<GenUnit>> {
        if *self == other {
            return Ok(None);
        }

        warn!(
            "Reconciling fields: {:#?} and {:#?} {}",
            self,
            other,
            if reconcile_multiple { "multi" } else { "" }
        );

        assert!(self.serialize_as == other.serialize_as);
        assert!(self.xml_name == other.xml_name);

        let mut new_unit = None;
        self.optional |= other.optional;
        self.multiple |= other.multiple | reconcile_multiple;
        self.recursive |= other.recursive;

        match (&mut self.ty, &mut other.ty) {
            (FieldTy::Empty, FieldTy::Empty) => {}
            (FieldTy::Text, FieldTy::Text) => {}
            (FieldTy::Value(first), FieldTy::Value(second)) if first == second => {}
            (FieldTy::Xml(path), FieldTy::Xml(other_path)) if path == other_path => {}
            (FieldTy::Parse(path), FieldTy::Parse(other_path)) if path == other_path => {}
            (FieldTy::Choice(first), FieldTy::Choice(second)) if first == second => {}
            (FieldTy::Choice(gen_enum), FieldTy::Xml(other_path)) => {
                let field = GenField::new(
                    &other_path.get_ident().unwrap().to_string(),
                    FieldTy::Xml(other_path.clone()),
                );
                gen_enum.borrow_mut().add_field(field)?;
            }
            (FieldTy::Xml(path), FieldTy::Xml(other_path)) => {
                let mut gen_enum = GenEnum::new();
                let name = safe_ty_name(&self.name);
                gen_enum.set_name(name);
                let field = GenField::new(
                    &path.get_ident().unwrap().to_string(),
                    FieldTy::Xml(path.clone()),
                );
                // field.set_serialize_as(SerializeAs::Inline);
                gen_enum.add_field(field)?;
                let field = GenField::new(
                    &other_path.get_ident().unwrap().to_string(),
                    FieldTy::Xml(other_path.clone()),
                );
                // field.set_serialize_as(SerializeAs::Inline);
                gen_enum.add_field(field)?;
                let gen_enum = GenEnumRef::from(gen_enum);
                self.ty = FieldTy::Choice(gen_enum.clone());
                new_unit = Some(GenUnit::Enum(gen_enum));
            }
            (FieldTy::Value(first), FieldTy::Value(second)) => {
                let mut gen_enum = GenEnum::new();
                let name = safe_ty_name(&self.name);
                gen_enum.set_name(name);
                gen_enum.add_field(GenField::new(first, FieldTy::Value(first.clone())))?;
                gen_enum.add_field(GenField::new(second, FieldTy::Value(second.clone())))?;
                let gen_enum = GenEnumRef::from(gen_enum);
                self.ty = FieldTy::Choice(gen_enum.clone());
                new_unit = Some(GenUnit::Enum(gen_enum));
            }
            (FieldTy::Parse(_path), FieldTy::Parse(_other_path)) => {
                // could be an enum..?
                self.ty = FieldTy::Text;
            }
            (FieldTy::Choice(choice), FieldTy::Value(val))
            | (FieldTy::Value(val), FieldTy::Choice(choice)) => {
                choice.borrow_mut().add_field(GenField::new_value(val))?;
                self.ty = FieldTy::Choice(choice.clone());
            }
            (FieldTy::Choice(gen_enum), FieldTy::Choice(other)) => {
                let mut gen_enum = gen_enum.borrow_mut();
                gen_enum.reconcile(other.take())?;
            }
            _ => {
                return Err(Error::Reconcile(self.ty.clone(), other.ty));
            }
        }

        Ok(new_unit)
    }

    pub(crate) fn set_not_allowed(&mut self, not_allowed: bool) {
        self.not_allowed = not_allowed;
    }

    pub(crate) fn set_ref(&mut self, arg: bool) {
        self.in_ref = arg;
    }

    pub(crate) fn set_recursive(&mut self, recursive: bool) {
        self.recursive = recursive
    }

    fn mod_name(&self) -> Ident {
        let ty = self.ty_path();
        let mod_name = safe_var_name(&ty.as_ref().unwrap().get_ident().unwrap().to_string());
        format_ident!("{}", mod_name)
    }
}

impl ToTokens for GenField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name_ident = self.ident();
        let mut ty = self.ty_path().to_token_stream();
        if ty.is_empty() {
            ty = parse_quote! { () };
        }

        if self.multiple {
            ty = quote! { Vec<#ty> };
        } else if self.optional {
            if self.recursive {
                ty = quote! { Box<#ty> };
            }
            ty = quote! { Option<#ty> };
        };

        tokens.extend(quote! {
            #name_ident: #ty
        });
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum FieldTy {
    Empty,
    Text,
    Xml(Path),
    Parse(Path),
    Choice(GenEnumRef),
    Value(String),
}

impl FieldTy {
    pub(crate) fn is_empty(&self) -> bool {
        matches!(self, FieldTy::Empty)
    }

    pub(crate) fn is_text(&self) -> bool {
        matches!(self, FieldTy::Text)
    }

    pub(crate) fn is_parse(&self) -> bool {
        matches!(self, FieldTy::Parse(_))
    }

    pub(crate) fn is_choice(&self) -> bool {
        matches!(self, FieldTy::Choice(_))
    }

    pub(crate) fn is_value(&self) -> bool {
        matches!(self, FieldTy::Value(_))
    }

    pub(crate) fn is_xml(&self) -> bool {
        matches!(self, FieldTy::Xml(_))
    }

    pub(crate) fn path(&self) -> Option<Path> {
        match self {
            FieldTy::Xml(path) => Some(path.clone()),
            FieldTy::Parse(path) => Some(path.clone()),
            FieldTy::Choice(gen_enum) => Some(gen_enum.borrow().path()),
            FieldTy::Text => Some(parse_quote! { String }),
            FieldTy::Value(_val) => None,
            FieldTy::Empty => None, // OR () ?
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) enum SerializeAs {
    Attribute,
    Element,
    Inline,
}

impl Display for SerializeAs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SerializeAs::Attribute => write!(f, "@"),
            SerializeAs::Element => write!(f, "<>"),
            SerializeAs::Inline => write!(f, ""),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GenFields {
    pub(crate) fields: IndexMap<String, GenField>,
}

impl Default for GenFields {
    fn default() -> Self {
        Self {
            fields: IndexMap::new(),
        }
    }
}

impl<'a> IntoIterator for &'a mut GenFields {
    type Item = &'a mut GenField;

    type IntoIter = indexmap::map::ValuesMut<'a, String, GenField>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.values_mut()
    }
}

impl<'a> IntoIterator for &'a GenFields {
    type Item = &'a GenField;

    type IntoIter = indexmap::map::Values<'a, String, GenField>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.values()
    }
}

impl GenFields {
    pub(crate) fn add_field(
        &mut self,
        field: GenField,
        reconcile_multiple: bool,
    ) -> Result<Option<GenUnit>> {
        let field_name = field.name.clone();
        let attr_name = format!("@{}", field_name);
        let mut new_unit = None;

        let name = if field.as_attribute() {
            attr_name.clone()
        } else {
            field_name.clone()
        };

        if let Some(existing) = self.fields.get_mut(&name) {
            new_unit = existing.reconcile(field, reconcile_multiple)?;
        } else {
            self.fields.insert(name, field);
        }

        if self.fields.contains_key(&field_name) {
            if let Some(attr) = self.fields.get_mut(&attr_name) {
                attr.name = format!("{}_attr", attr.name);
            }
        }

        Ok(new_unit)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &GenField> {
        self.fields.values()
    }

    pub(crate) fn reconcile(&mut self, other: GenFields) -> Result<Vec<GenUnit>> {
        let mut ret = Vec::new();

        for (key, value) in other.fields {
            if let Some(existing) = self.fields.get_mut(&key) {
                if let Some(new) = existing.reconcile(value, false)? {
                    ret.push(new);
                }
            } else {
                self.fields.insert(key, value);
            }
        }

        Ok(ret)
    }
}

pub(crate) fn gen_mods_from_fields<'a>(fields: impl Iterator<Item = &'a GenField>) -> TokenStream {
    let mut set = HashMap::new();

    let mut mods = quote! {
        use crate::xml::{Error, Result, ToXml};
    };

    for field in fields {
        let mod_ = field.gen_mod();
        set.insert(mod_.to_string(), mod_);
    }

    mods.append_all(set.values());
    mods
}
