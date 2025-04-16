use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use syn::{parse_quote, Ident, Path};

use super::GenField;

#[derive(Debug, Clone)]
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

    pub(crate) fn path(&self) -> Path {
        let ident = self.ident();

        parse_quote! { #ident }
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

    pub(crate) fn prefix_field_ty(&mut self) {
        let prefix = self.var_name().to_string();

        for v in self.variants.iter_mut().flatten() {
            v.prefix_ty(&prefix);
        }
    }

    pub(crate) fn gen_from_xml(
        &self,
        builder: &Ident,
        from_xml_attrs: &mut TokenStream,
        from_xml_elems: &mut TokenStream,
        xml_events: &mut TokenStream,
    ) {
        for v in self.variants.iter().flatten() {
            v.gen_from_xml(builder, from_xml_attrs, from_xml_elems, xml_events);
        }
    }
}

impl ToTokens for GenEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.name();
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();

        let all_fields: Vec<_> = self.all_fields().cloned().collect();
        let all_fields_names = all_fields.iter().map(|f| f.ident());
        let builder_fns: TokenStream = all_fields.iter().map(GenField::gen_builder_fn).collect();
        let builder_fields = all_fields.iter().cloned().map(|mut f| {
            f.set_optional(true);
            f
        });

        let mut build = quote! {};
        let mut variants = Vec::new();
        let mut to_xml_attr = Vec::new();
        let mut to_xml = Vec::new();
        for (n, v) in self.variants.iter().enumerate() {
            let (f_attr, f_elem): (Vec<_>, Vec<_>) = v.iter().partition(|f| f.attribute);
            let name_attr: Vec<_> = f_attr.iter().map(|f| f.ident()).collect();
            // let attr_to_xml: Vec<_> = f_attr.iter().map(|f| f.gen_to_xml()).collect();
            let name_elem: Vec<_> = f_elem.iter().map(|f| f.ident()).collect();
            let name: Vec<_> = v.iter().map(|f| f.ident()).collect();
            let ty = v.iter().map(|f| f.ty_path());
            // let to_xml_fields = v.iter().map(|f| f.gen_to_xml(to_xml_attrs, to_xml_elems));
            let variant = format_ident!("Variant{}", n);
            let gen = quote! {
                #variant {
                    #(#name: #ty),*
                }
            };
            variants.push(gen);

            let gen = quote! {
                Self::#variant { #(#name),* } => {
                    #(let elem = #name_attr;)*
                }
            };
            to_xml_attr.push(gen);

            let gen = quote! {
                Self::#variant { #(#name),* } => {
                    #(let elem = #name_elem;)*
                }
            };
            to_xml.push(gen);

            let gen = quote! {
                // FIXME: check other fields are None
                if #(#name.is_some())&&* {
                    return Ok(#name_ident::#variant {
                        #(#name: #name.unwrap()),*
                    })
                }
            };
            build.append_all(gen);
        }

        let gen = quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub enum #name_ident {
                #(#variants),*
            }

            impl #name_ident {
                pub fn builder() -> #builder_ident {
                    Default::default()
                }

                pub fn to_xml_attr(&self, start: &mut quick_xml::events::BytesStart<'_>) -> Result<()> {
                    match self {
                        #(#to_xml_attr),*
                    }
                    Ok(())
                }

                pub fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write,
                {
                    match self {
                        #(#to_xml),*
                    }
                    Ok(())
                }
            }

            #[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #builder_ident {
                #(#builder_fields),*
            }

            impl #builder_ident {
                #builder_fns

                pub fn build(self) -> Result<#name_ident> {
                    let Self { #(#all_fields_names),* } = self;

                    #build

                    Err(Error::BuilderVariant(#name))
                }
            }
        };

        tokens.extend(gen);
    }
}
