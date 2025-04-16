use heck::ToSnakeCase;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

use super::GenEnum;

#[derive(Debug, Clone)]
pub(crate) enum FieldTy {
    Ty(String),
    Choice(GenEnum),
    Text,
}

impl FieldTy {
    pub(crate) fn is_text(&self) -> bool {
        matches!(self, FieldTy::Text)
    }

    pub(crate) fn is_choice(&self) -> bool {
        matches!(self, FieldTy::Choice(_))
    }

    pub(crate) fn ident(&self) -> Ident {
        match self {
            FieldTy::Ty(ty) => format_ident!("{}", ty),
            FieldTy::Choice(choice) => choice.ident(),
            FieldTy::Text => format_ident!("String"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct GenField {
    // name of the rs field/mod, ex "snake_case"
    pub(crate) name: String,
    // name of the xml element, ex "OriginalName"
    pub(crate) xml_name: String,
    // associate rs type, ex "String"
    pub(crate) ty: FieldTy,
    pub(crate) optional: bool,
    pub(crate) multiple: bool,
    pub(crate) attribute: bool,
}

impl GenField {
    pub(crate) fn new(name: &str, ty: FieldTy) -> Self {
        Self {
            xml_name: name.to_string(),
            name: name.to_snake_case(),
            ty,
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
        self.ty.ident()
    }

    pub(crate) fn is_text(&self) -> bool {
        self.ty.is_text()
    }

    pub(crate) fn is_choice(&self) -> bool {
        self.ty.is_choice()
    }

    pub(crate) fn gen_builder_fn(&self) -> TokenStream {
        let field_ident = self.ident();
        let field_ty = self.ty_ident();
        let field_single = self.single_ident();

        let mut val = quote! { #field_single.try_into()? };
        if self.optional && !self.multiple {
            val = quote! {
                if let Some(#field_single) = #field_single {
                    Some(#val)
                } else {
                    None
                }
            };
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

        quote! {
            pub fn #field_single<T>(mut self, #field_single: #t) -> Result<Self>
            where
                T: TryInto<#field_ty>,
                Error: From<<T as TryInto<#field_ty>>::Error>
            {
                #body
                Ok(self)
            }
        }
    }

    pub(crate) fn gen_from_xml(
        &self,
        builder: &Ident,
        from_xml_attrs: &mut TokenStream,
        from_xml_elems: &mut TokenStream,
        xml_events: &mut TokenStream,
    ) {
        match &self.ty {
            FieldTy::Ty(ty) => {
                self.gen_from_ty(builder, ty, from_xml_attrs, from_xml_elems, xml_events);
            }
            FieldTy::Choice(e) => {
                e.gen_from_xml(builder, from_xml_attrs, from_xml_elems, xml_events);
            }
            FieldTy::Text => {
                self.gen_from_xml_text(builder, from_xml_attrs, xml_events);
            }
        }
    }

    fn gen_from_ty(
        &self,
        builder: &Ident,
        ty: &String,
        _from_xml_attrs: &mut TokenStream,
        from_xml_elems: &mut TokenStream,
        _xml_events: &mut TokenStream,
    ) {
        let ty = format_ident!("{ty}");
        let field_name_b = self.name_b();
        let field_single = self.single_ident();

        let mut val = quote! { #ty::from_xml(reader, &e)? };
        if self.optional && !self.multiple {
            val = quote! { Some(#val) };
        }

        from_xml_elems.extend(quote! {
            #field_name_b => { #builder = #builder.#field_single(#val)?; }
        });
    }

    fn gen_from_xml_text(
        &self,
        builder: &Ident,
        from_xml_attrs: &mut TokenStream,
        xml_events: &mut TokenStream,
    ) {
        let field_name_b = self.name_b();
        let field_single = self.single_ident();

        let mut val = if self.attribute {
            quote! {
                attr.unescape_value()?
            }
        } else {
            quote! {
                e.unescape()?
            }
        };
        if self.optional && !self.multiple {
            val = quote! { Some(#val) };
        };
        let build_field = quote! { #builder = #builder.#field_single(#val)?; };
        if self.attribute {
            let pat = quote! {
                #field_name_b => {
                    #build_field
                }
            };
            from_xml_attrs.extend(pat)
        } else {
            let event = quote! {
                Event::Text(e) => {
                    #build_field
                }
            };
            xml_events.extend(event);
        }
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
