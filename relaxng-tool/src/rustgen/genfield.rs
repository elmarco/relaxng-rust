use heck::ToSnakeCase;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

#[derive(Debug, Clone)]
pub(crate) struct GenField {
    // name of the rs field/mod, ex "snake_case"
    pub(crate) name: String,
    // name of the xml element, ex "OriginalName"
    pub(crate) xml_name: String,
    // associate rs type, ex "String"
    pub(crate) ty: String,
    // whether this is a text/body
    pub(crate) text: bool,
    pub(crate) optional: bool,
    pub(crate) multiple: bool,
    pub(crate) attribute: bool,
}

impl GenField {
    pub(crate) fn new(name: &str, ty: &str, text: bool) -> Self {
        Self {
            xml_name: name.to_string(),
            name: name.to_snake_case(),
            ty: ty.to_string(),
            text,
            optional: false,
            multiple: false,
            attribute: false,
        }
    }

    pub(crate) fn set_name(&mut self, name: &str) {
        self.xml_name = name.to_string();
        self.name = name.to_snake_case();
    }

    pub(crate) fn set_attribute(&mut self, attribute: bool) {
        self.attribute = attribute;
    }

    pub(crate) fn set_optional(&mut self, optional: bool) {
        self.optional = optional;
    }

    pub(crate) fn set_multiple(&mut self, multiple: bool) {
        self.multiple = multiple;
    }

    pub(crate) fn name(&self) -> String {
        format!("{}{}", self.name, if self.multiple { "s" } else { "" })
    }

    pub(crate) fn name_b(&self) -> Literal {
        Literal::byte_string(self.xml_name.as_bytes())
    }

    pub(crate) fn ident(&self) -> Ident {
        format_ident!("{}", self.name())
    }

    pub(crate) fn single_ident(&self) -> Ident {
        format_ident!("{}", self.name)
    }

    pub(crate) fn ty_ident(&self) -> Ident {
        format_ident!("{}", self.ty)
    }
}

impl ToTokens for GenField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name_ident = self.ident();
        let mut ty = self.ty_ident().to_token_stream();

        if self.multiple {
            ty = quote! { Vec<#ty> };
        } else if self.optional {
            ty = quote! { Option<#ty> };
        };

        let gen = quote! {
            #name_ident: #ty
        };

        tokens.extend(gen);
    }
}
