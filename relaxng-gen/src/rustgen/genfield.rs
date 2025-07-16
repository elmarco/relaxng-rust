use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use heck::ToSnakeCase;
use indexmap::IndexMap;
use pluralizer::pluralize;
use proc_macro2::{Literal, TokenStream};
use quote::{ToTokens, TokenStreamExt, format_ident, quote};
use syn::{Ident, Path, parse_quote};
use tracing::trace;

use crate::{
    rustgen::{Error, genenum::GenEnum},
    utils::{safe_ty_name, safe_var_name},
};

use super::{GenUnit, Ref, Result, genenum::GenEnumRef};

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct GenField {
    // name of the rs field/mod, ex "snake_case"
    pub(crate) name: String,
    // name of the xml element, ex "OriginalName"
    pub(crate) xml_name: String,
    // associate rs type, ex "String"
    pub(crate) ty: FieldTy,
    pub(crate) doc: Option<String>,
    pub(crate) optional: bool,
    pub(crate) multiple: bool,
    pub(crate) serialize_as: SerializeAs,
    pub(crate) not_allowed: bool,
    pub(crate) recursive: bool,
    pub(crate) public: bool,
    pub(crate) rf: Option<Rc<RefCell<Ref>>>,
}

impl GenField {
    pub(crate) fn new(xml_name: &str, ty: FieldTy, doc: Option<String>) -> Self {
        Self {
            xml_name: xml_name.to_string(),
            name: xml_name.to_snake_case(),
            ty,
            doc,
            optional: false,
            multiple: false,
            serialize_as: SerializeAs::Inline,
            not_allowed: false,
            recursive: false,
            public: true,
            rf: None,
        }
    }

    pub(crate) fn new_value(val: &str, doc: Option<String>) -> GenField {
        Self::new(val, FieldTy::Value(val.to_string()), doc)
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
        let name = if self.multiple {
            pluralize(&self.name, 2, false)
        } else {
            self.name.clone()
        };
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
        // matches!(self.serialize_as, SerializeAs::Element)
        false
    }

    pub(crate) fn as_inline(&self) -> bool {
        matches!(self.serialize_as, SerializeAs::Inline)
    }

    pub(crate) fn gen_doc(&self) -> Option<TokenStream> {
        self.doc.as_ref().map(|doc| quote! { #[doc = #doc] })
    }

    pub(crate) fn visibility(&self) -> Option<TokenStream> {
        if self.public {
            Some(quote! { pub })
        } else {
            None
        }
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
        if self.recursive && !self.multiple {
            val = quote! { Box::new(#val) };
        }
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

        let doc = self.gen_doc();
        quote! {
            #doc
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
        for_enum: bool,
    ) {
        match &self.ty {
            FieldTy::Xml(ty) => {
                self.gen_from_ty_xml(ty, from_xml_elems, for_enum);
            }
            FieldTy::Value(value) => {
                self.gen_from_value(value, from_xml_attrs, from_xml_text);
            }
            FieldTy::Parse(ty) => {
                self.gen_from_text(Some(ty), from_xml_attrs, from_xml_text);
            }
            FieldTy::Choice(gen_enum) => {
                let gen_enum = gen_enum.borrow();
                self.gen_from_choice(&gen_enum, from_xml_attrs, from_xml_elems, from_xml_other);
            }
            FieldTy::Text => {
                self.gen_from_text(None, from_xml_attrs, from_xml_text);
            }
            FieldTy::Empty => {}
        }
    }

    fn gen_from_ty_xml(&self, ty: &Path, from_xml_elems: &mut Vec<TokenStream>, for_enum: bool) {
        let mut arm = self.xml_name.to_token_stream();
        let field_ident = self.single_ident();

        let from_xml = quote! { #ty::from_xml(&child) };
        let val = if self.optional && !self.multiple {
            quote! { Some(#from_xml?) }
        } else {
            quote! { #from_xml? }
        };

        if for_enum {
            // fixme: use if_let_guard when stabilized
            arm = quote! { #arm if #from_xml.is_ok() };
        };
        from_xml_elems.push(quote! {
            #arm => { builder.#field_ident(#val)?; }
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
        gen_enum: &GenEnum,
        from_xml_attrs: &mut Vec<TokenStream>,
        _from_xml_elems: &mut [TokenStream],
        from_xml_other: &mut Vec<TokenStream>,
    ) {
        let ty = gen_enum.path();
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
            // SerializeAs::Element => {
            //     self.gen_from_ty_xml(ty, from_xml_elems, false);
            // }
            SerializeAs::Inline => {
                let field_ident = self.single_ident();

                from_xml_other.push(quote! {
                    let child = &mut node.first_child();
                });

                let build = if let Some(st) = &gen_enum.as_element {
                    let xml_name = &st.xml_name;
                    let mut val = quote! { #ty::from_xml(child)? };
                    if self.optional {
                        val = quote! { Some(#val) };
                    }
                    quote! {
                        loop {
                            if child.is_none() {
                                break;
                            }
                            if let Some(child) = child {
                                if child.is_element() && child.tag_name().name() == #xml_name {
                                    builder.#field_ident(#val)?;
                                }
                            }
                            *child = child.and_then(|c| c.next_sibling());
                        }
                    }
                } else {
                    let val = quote! { #ty::from_xml(node, child) };

                    if self.multiple {
                        from_xml_other.push(quote! {
                            loop {
                                if let Ok(elem) = #val {
                                    builder.#field_ident(elem)?;
                                } else {
                                    break;
                                }
                            }
                        });
                        return;
                    }

                    let mut val = quote! { #val? };
                    if self.optional {
                        val = quote! { Some(#val) };
                    }
                    quote! {
                        builder.#field_ident(#val)?;
                    }
                };

                from_xml_other.push(build);
            }
        }
    }

    // for_self: for struct, else for enum variants
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
            SerializeAs::Inline => {
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

    pub(crate) fn fixed_value(&self) -> Option<&str> {
        if self.optional || self.multiple || self.recursive {
            return None;
        }
        if let FieldTy::Value(v) = &self.ty {
            Some(v)
        } else {
            None
        }
    }

    fn elem_code(&self, for_self: bool, elem_code: TokenStream) -> TokenStream {
        if self.fixed_value().is_some() {
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
        if self.shared_ref() {
            quote! {
                use crate::#mod_name::#ty;
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
            if reconcile_multiple {
                self.multiple |= true;
            }
            return Ok(None);
        }

        trace!(
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
                    None,
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
                    None,
                );
                // field.set_serialize_as(SerializeAs::Inline);
                gen_enum.add_field(field)?;
                let field = GenField::new(
                    &other_path.get_ident().unwrap().to_string(),
                    FieldTy::Xml(other_path.clone()),
                    None,
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
                gen_enum.add_field(GenField::new(
                    first,
                    FieldTy::Value(first.clone()),
                    self.doc.clone(),
                ))?;
                gen_enum.add_field(GenField::new(
                    second,
                    FieldTy::Value(second.clone()),
                    other.doc.clone(),
                ))?;
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
                choice
                    .borrow_mut()
                    .add_field(GenField::new_value(val, Some("fixme".to_string())))?;
                self.ty = FieldTy::Choice(choice.clone());
            }
            (FieldTy::Choice(gen_enum), FieldTy::Choice(other)) => {
                let mut gen_enum = gen_enum.borrow_mut();
                gen_enum.reconcile(other.take())?;
                other.borrow_mut().set_name("fixme".to_string());
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

    pub(crate) fn set_ref(&mut self, rf: Rc<RefCell<Ref>>) {
        self.rf = Some(rf);
    }

    pub(crate) fn shared_ref(&self) -> bool {
        self.rf
            .as_ref()
            .map(|rf| rf.borrow().shared_ref())
            .unwrap_or(false)
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
        let mut empty = false;

        if ty.is_empty() {
            empty = true;
            ty = parse_quote! { () };
        }

        if self.multiple {
            empty = false;
            ty = quote! { Vec<#ty> };
        } else if self.recursive {
            empty = false;
            ty = quote! { Box<#ty> };
        }

        if self.optional && !self.multiple {
            empty = false;
            ty = quote! { Option<#ty> };
        };

        if empty {
            return;
        }
        let doc = self.gen_doc();
        let vis = self.visibility();
        tokens.extend(quote! {
            #doc
            #vis #name_ident: #ty
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
    // Element,
    Inline,
}

impl Display for SerializeAs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SerializeAs::Attribute => write!(f, "@"),
            //SerializeAs::Element => write!(f, "<>"),
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
    pub(crate) fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    pub(crate) fn get_index(&self, idx: usize) -> Option<&GenField> {
        self.fields.get_index(idx).map(|(_, f)| f)
    }

    // also return the index of the field
    pub(crate) fn add_field_full(
        &mut self,
        field: GenField,
        reconcile_multiple: bool,
    ) -> Result<(usize, Option<GenUnit>)> {
        let field_name = field.name.clone();
        let attr_name = format!("@{field_name}");
        let mut new_unit = None;

        let key_name = if field.as_attribute() {
            attr_name.clone()
        } else {
            field_name.clone()
        };

        let idx = if let Some((idx, _, existing)) = self.fields.get_full_mut(&key_name) {
            new_unit = existing.reconcile(field, reconcile_multiple)?;
            idx
        } else {
            let (idx, _) = self.fields.insert_full(key_name, field);
            idx
        };

        // if we have both, rename the attribute field
        if self.fields.contains_key(&field_name) {
            if let Some(attr) = self.fields.get_mut(&attr_name) {
                // fixme only rename when conflicting
                attr.name = format!("{field_name}_attr");
            }
        }

        Ok((idx, new_unit))
    }

    pub(crate) fn add_field(
        &mut self,
        field: GenField,
        reconcile_multiple: bool,
    ) -> Result<Option<GenUnit>> {
        self.add_field_full(field, reconcile_multiple)
            .map(|(_, unit)| unit)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &GenField> {
        self.fields.values()
    }

    pub(crate) fn reconcile(&mut self, other: GenFields) -> Result<(Vec<usize>, Vec<GenUnit>)> {
        let mut ret_units = Vec::new();
        let mut ret_idx = Vec::new();

        for (name, field) in self.fields.iter_mut() {
            if !other.fields.contains_key(name) {
                field.set_optional(true);
            }
        }

        for (_, field) in other.fields {
            let (idx, unit) = self.add_field_full(field, true)?;
            if let Some(unit) = unit {
                ret_units.push(unit);
            }
            ret_idx.push(idx);
        }

        Ok((ret_idx, ret_units))
    }

    pub(crate) fn is_enum(&self) -> bool {
        if self.fields.values().len() == 1 {
            if let Some(field) = self.fields.values().next() {
                field.ty.is_choice()
            } else {
                false
            }
        } else {
            false
        }
    }
}

pub(crate) fn gen_mods_from_fields<'a>(fields: impl Iterator<Item = &'a GenField>) -> TokenStream {
    let mut set = HashMap::new();

    let mut mods = quote! {
        use crate::{Error, Result, ToXml};
    };

    for field in fields {
        let mod_ = field.gen_mod();
        set.insert(mod_.to_string(), mod_);
    }

    mods.append_all(set.values());
    mods
}
