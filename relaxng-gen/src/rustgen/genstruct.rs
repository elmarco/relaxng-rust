use std::fmt::Display;

use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::{Ident, parse_quote};

use crate::utils::{has_default_match_arm, safe_ty_name};

use super::{
    GenField, GenUnit, Result,
    genfield::{GenFields, gen_mods_from_fields},
};

// TODO: use newtype for single field structs

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GenStruct {
    // rs type name
    pub(crate) name: String,
    pub(crate) fields: GenFields,
    pub(crate) xml_name: String,
    pub(crate) not_allowed: bool,
}

impl Display for GenStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl GenStruct {
    pub(crate) fn new<S: Into<String>>(xml_name: S) -> Self {
        let xml_name = xml_name.into();
        let name = safe_ty_name(&xml_name);
        Self {
            name,
            xml_name,
            fields: Default::default(),
            not_allowed: false,
        }
    }

    pub(crate) fn set_name(&mut self, name: &str) {
        self.name = safe_ty_name(name);
    }

    pub(crate) fn add_field(&mut self, field: GenField) -> Result<Option<GenUnit>> {
        let res = if field.ty.is_empty() {
            None
        } else {
            self.fields.add_field(field, true)?
        };
        Ok(res)
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn ident(&self) -> Ident {
        format_ident!("{}", self.name)
    }

    pub(crate) fn path(&self) -> syn::Path {
        let ident = self.ident();

        parse_quote! { #ident }
    }

    pub(crate) fn builder_ident(&self) -> Ident {
        format_ident!("{}Builder", self.ident())
    }

    fn gen_to_xml(&self) -> TokenStream {
        let name = &self.xml_name;
        let mut to_xml_attrs = quote! {};
        let mut to_xml_elems = quote! {};
        let mut to_xml_empty = vec![quote! { true }];

        for field in self.iter_fields() {
            field.gen_to_xml(
                true,
                &mut to_xml_attrs,
                &mut to_xml_elems,
                &mut to_xml_empty,
            );
        }

        let body_event = quote! {
            let empty = {
                #(#to_xml_empty)&&*
            };

            if empty {
                writer.write_event(Event::Empty(start))?;
            } else {
                writer.write_event(Event::Start(start))?;
                #to_xml_elems
                writer.write_event(Event::End(BytesEnd::new(#name)))?;
            }
        };

        let body = quote! {
            use quick_xml::events::{Event, BytesStart, BytesEnd, BytesText};

            let mut start = BytesStart::new(#name);

            #to_xml_attrs

            #body_event
        };

        quote! {
            fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
            where
                W: std::io::Write,
            {
                #body
                Ok(())
            }
        }
    }

    pub(crate) fn reconcile(&mut self, other: GenStruct) -> Result<Vec<GenUnit>> {
        self.fields.reconcile(other.fields)
    }

    fn iter_fields(&self) -> impl Iterator<Item = &GenField> {
        self.fields.into_iter()
    }

    pub(crate) fn set_not_allowed(&mut self, not_allowed: bool) {
        self.not_allowed = not_allowed;
    }
}

impl ToTokens for GenStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.xml_name;
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();
        let fields: Vec<&GenField> = self.iter_fields().collect();
        let mut builder_fields: Vec<GenField> = self.iter_fields().cloned().collect();
        builder_fields.iter_mut().for_each(|f| f.optional = true);

        let to_xml = self.gen_to_xml();
        let mut from_xml_attrs = Vec::new();
        let mut from_xml_elems = Vec::new();
        let mut from_xml_text = Vec::new();
        let mut from_xml_other = Vec::new();
        let mut build_fields = Vec::new();
        let builder_fns: TokenStream = self.iter_fields().map(GenField::gen_builder_fn).collect();

        for field in self.iter_fields() {
            let field_name = field.name();
            let field_ident = field.ident();

            // builder
            let build_field = if field.optional {
                quote! {
                    #field_ident: Clone::clone(&self.#field_ident)
                }
            } else if field.multiple {
                quote! {
                    #field_ident: if self.#field_ident.is_empty() {
                        return Err(Error::BuilderMissingField(#name, #field_name));
                    } else {
                        Clone::clone(&self.#field_ident)
                    }
                }
            } else {
                quote! {
                    #field_ident: Clone::clone(
                        self.#field_ident.as_ref().ok_or(Error::BuilderMissingField(#name, #field_name))?
                    )
                }
            };
            build_fields.push(build_field);

            field.gen_from_xml(
                &mut from_xml_attrs,
                &mut from_xml_elems,
                &mut from_xml_text,
                &mut from_xml_other,
            );
        }

        if !has_default_match_arm(&from_xml_text) {
            from_xml_text.push(quote! { _ => {} });
        }

        let from_xml_fn = gen_from_xml_fn(
            &from_xml_attrs,
            &from_xml_elems,
            &from_xml_text,
            &from_xml_other,
        );

        let mods = gen_mods_from_fields(self.iter_fields());
        let rgen = quote! {
            #mods

            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #name_ident {
                #(#fields),*
            }

            #[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #builder_ident {
                #(#builder_fields),*
            }

            impl ToXml for #name_ident {
                type Builder = #builder_ident;

                #to_xml
            }

            impl #name_ident {
                #from_xml_fn
            }

            impl #builder_ident {
                pub fn build(&self) -> Result<#name_ident> {
                    Ok(#name_ident {
                        #(#build_fields),*
                    })
                }

                #builder_fns
            }
        };

        tokens.extend(rgen);
    }
}

fn gen_from_xml_fn(
    from_xml_attrs: &[TokenStream],
    from_xml_elems: &[TokenStream],
    from_xml_text: &[TokenStream],
    from_xml_other: &[TokenStream],
) -> TokenStream {
    quote! {
        pub fn from_xml(node: &roxmltree::Node) -> Result<Self>
        {
            let mut builder = Self::builder();

            #(#from_xml_attrs)*

            for child in node.children() {
                match child.tag_name().name() {
                    #(#from_xml_elems),*
                    _ => continue,
                }
            }

            if let Some(val) = node.text() {
                match val {
                    #(#from_xml_text),*
                }
            }

            #(#from_xml_other);*

            builder.build()
        }

    }
}
