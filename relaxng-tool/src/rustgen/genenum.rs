use heck::ToSnakeCase;
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

    pub(crate) fn builder_ident(&self) -> Ident {
        format_ident!("{}Builder", self.name())
    }

    pub(crate) fn add_field(&mut self, field: GenField) {
        let variant = self.variants.last_mut().expect("an enum variant");

        variant.push(field);
    }

    pub(crate) fn push_variant(&mut self) {
        self.variants.push(Vec::new());
    }

    pub(crate) fn var_name(&self) -> Ident {
        format_ident!("{}", self.name.to_snake_case())
    }

    pub(crate) fn all_fields(&self) -> impl Iterator<Item = &GenField> {
        self.variants.iter().flatten()
    }
}

impl ToTokens for GenEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.name();
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();

        let builder_fns: TokenStream = self.all_fields().map(GenField::gen_builder_fn).collect();
        let mut builder_fields: Vec<_> = self.all_fields().cloned().collect();
        builder_fields.iter_mut().for_each(|f| f.optional = true);

        let mut build = quote! {};
        let mut variants = Vec::new();
        let mut to_xml = Vec::new();
        for (n, v) in self.variants.iter().enumerate() {
            let name: Vec<_> = v.iter().map(|f| f.ident()).collect();
            let ty = v.iter().map(|f| f.ty_ident());
            let variant = format_ident!("Variant{}", n);
            let gen = quote! {
                #variant {
                    #(#name: #ty),*
                }
            };
            variants.push(gen);

            let gen = quote! {
                #variant { #(#name),* } => {
                    #(#name.to_xml();)*
                }
            };
            to_xml.push(gen);

            let gen = quote! {
                // todo: if all fields of v are set, then build the #variant with it
            };
            build.append_all(gen);
        }

        let gen = quote! {
            pub enum #name_ident {
                #(#variants),*
            }

            impl #name_ident {
                pub fn builder() -> #builder_ident {
                    Default::default()
                }

                pub fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write,
                {
                    match self {
                        #(#to_xml),*
                    }
                }
            }

            #[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #builder_ident {
                #(#builder_fields),*
            }

            impl #builder_ident {
                #builder_fns

                pub fn build(self) -> Result<#name_ident> {
                    #build
                    Err(Error::BuiderVariant(#name))
                }
            }

        };

        tokens.extend(gen);
    }
}
