use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use syn::{Ident, Index};

use super::GenField;

#[derive(Debug)]
pub(crate) struct GenEnum {
    // rs type name
    pub(crate) name: String,
    pub(crate) variants: Vec<Vec<GenField>>,
}

impl GenEnum {
    pub(crate) fn new(name: String) -> Self {
        Self {
            name,
            variants: Vec::new(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn ident(&self) -> Ident {
        format_ident!("{}", self.name())
    }

    pub(crate) fn add_field(&mut self, field: GenField) {
        let variant = self.variants.last_mut().expect("an enum variant");

        variant.push(field);
    }

    pub(crate) fn push_variant(&mut self) {
        self.variants.push(Vec::new());
    }
}

impl ToTokens for GenEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name_ident = self.ident();
        let mut variants = Vec::new();
        let mut to_xml = Vec::new();
        for (n, v) in self.variants.iter().enumerate() {
            let ty = v.iter().map(|f| f.ty_ident());
            let ty = quote! {
                (#(#ty),*)
            };
            let variant = format_ident!("Variant{}", n);
            let gen = quote! {
                #variant(#ty)
            };
            variants.push(gen);

            let mut to_xml_variant = quote! {};
            for (n, _ty) in v.iter().enumerate() {
                let n = Index::from(n);
                let gen = quote! {
                    v.#n.to_xml();
                };
                to_xml_variant.append_all(gen);
            }
            let gen = quote! {
                #variant(v) => { #to_xml_variant }
            };
            to_xml.push(gen);
        }
        let gen = quote! {
            pub enum #name_ident {
                #(#variants),*
            }

            impl #name_ident {
                pub fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write,
                {
                    match self {
                        #(#to_xml),*
                    }
                }
            }
        };

        tokens.extend(gen);
    }
}
