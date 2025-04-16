use heck::{ToSnakeCase, ToUpperCamelCase};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{parse_quote, Ident};

use super::GenField;

#[derive(Debug)]
pub(crate) struct GenStruct {
    pub(crate) xml_name: String,
    pub(crate) fields: Vec<GenField>,
    pub(crate) is_top: bool,
}

impl GenStruct {
    pub(crate) fn new(name: &str, is_top: bool) -> Self {
        Self {
            xml_name: name.to_string(),
            fields: Vec::new(),
            is_top,
        }
    }

    pub(crate) fn add_field(&mut self, field: GenField) {
        self.fields.push(field);
    }

    pub(crate) fn xml_name_b(&self) -> Literal {
        Literal::byte_string(self.xml_name.as_bytes())
    }

    pub(crate) fn var_name(&self) -> Ident {
        format_ident!("{}", self.xml_name.to_snake_case())
    }

    pub(crate) fn ident(&self) -> Ident {
        format_ident!("{}", self.xml_name.to_upper_camel_case())
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

        for field in &self.fields {
            field.gen_to_xml(true, &mut to_xml_attrs, &mut to_xml_elems);
        }

        let body_event = quote! {
            // TODO: improve efficiency.. this is n2
            let empty = {
                let inspect_sink = crate::xml::InspectSink::default();
                let mut inspect = quick_xml::writer::Writer::new(inspect_sink);
                let writer = &mut inspect;
                #to_xml_elems
                !inspect.into_inner().written
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
            use quick_xml::events::{Event, BytesStart, BytesEnd};

            let mut start = BytesStart::new(#name);

            #to_xml_attrs

            #body_event
        };

        quote! {
            pub fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
            where
                W: std::io::Write,
            {
                #body
                Ok(())
            }
        }
    }
}

impl ToTokens for GenStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.xml_name;
        let name_b = self.xml_name_b();
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();
        let fields = &self.fields;
        let mut builder_fields = fields.clone();
        builder_fields.iter_mut().for_each(|f| f.optional = true);
        let field_names = fields.iter().map(|f| f.ident());

        let to_xml = self.gen_to_xml();
        let mut xml_events = quote! {};
        let mut from_xml_attrs = quote! {};
        let mut from_xml_elems = quote! {};
        let mut build_fields = quote! {};
        let builder_fns: TokenStream = self.fields.iter().map(GenField::gen_builder_fn).collect();
        let mut choice_builders = quote! {};
        let mut choice_build = quote! {};

        for field in &self.fields {
            let field_name = field.name();
            let field_ident = field.ident();
            let field_ty = field.ty_path();

            // choice builders
            let builder = if field.is_choice() {
                choice_builders.extend(quote! {
                    let mut #field_ident = #field_ty::builder();
                });
                choice_build.extend(quote! {
                    builder = builder.#field_ident(#field_ident.build()?)?;
                });

                field_ident.clone()
            } else {
                format_ident!("builder")
            };

            // builder
            let build_field = if field.optional {
                quote! {
                    #field_ident,
                }
            } else {
                if field.multiple {
                    quote! {
                        #field_ident: if #field_ident.is_empty() {
                            return Err(Error::BuilderMissingField(#name, #field_name));
                        } else {
                            #field_ident
                        }
                    }
                } else {
                    quote! {
                        #field_ident: #field_ident.ok_or(Error::BuilderMissingField(#name, #field_name))?,
                    }
                }
            };
            build_fields.extend(build_field);

            field.gen_from_xml(
                &builder,
                &mut from_xml_attrs,
                &mut from_xml_elems,
                &mut xml_events,
            );
        }

        let on_eof = if self.is_top {
            quote! { break }
        } else {
            quote! { return Err(Error::UnexpectedEof) }
        };

        let gen = quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #name_ident {
                #(#fields),*
            }

            #[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #builder_ident {
                #(#builder_fields),*
            }

            impl #name_ident {
                pub fn builder() -> #builder_ident {
                    Default::default()
                }

                pub fn from_xml<R>(reader: &mut quick_xml::Reader<R>, start_element: &quick_xml::events::Event) -> Result<Self>
                where
                    R: std::io::BufRead,
                {
                    let (start_element, is_empty) = match start_element {
                        Event::Start(bytes_start) => (bytes_start, false),
                        Event::Empty(bytes_start) => (bytes_start, true),
                        _ => return Err(Error::UnexpectedEvent(format!("{:?}", start_element))),
                    };

                    use quick_xml::events::Event;
                    let mut builder = Self::builder();
                    #choice_builders

                    if start_element.name().local_name().as_ref() != #name_b {
                        return Err(Error::UnexpectedEvent(format!("{:?}", start_element)));
                    }

                    for attr in start_element.attributes() {
                        let attr = attr.map_err(|e| Error::Xml(e.into()))?;
                        match attr.key.as_ref() {
                            #from_xml_attrs
                            other => {
                                dbg!(other);
                            }
                        }
                    }

                    if !is_empty {
                        let mut buf = Vec::new();
                        let mut end: Option<quick_xml::events::BytesEnd<'static>> = None;
                        loop {
                            if let Some(end) = end.take() {
                                reader.read_to_end_into(end.name(), &mut buf)?;
                                buf.clear();
                            }
                            match reader.read_event_into(&mut buf)? {
                                event @ (quick_xml::events::Event::Start(_) | quick_xml::events::Event::Empty(_)) => {
                                    let (e, is_start) = match &event {
                                        quick_xml::events::Event::Start(e) => (e, true),
                                        quick_xml::events::Event::Empty(e) => (e, false),
                                        _ => continue,
                                    };

                                    match e.name().as_ref() {
                                        #from_xml_elems
                                        other => {
                                            //dbg!(std::str::from_utf8(other));
                                            if is_start {
                                                end = Some(e.to_end().into_owned());
                                                continue;
                                            }
                                        }
                                    }
                                }
                                #xml_events
                                quick_xml::events::Event::End(e) => break,
                                quick_xml::events::Event::Eof => #on_eof,
                                other => {
                                    //dbg!(other);
                                }
                            }
                            buf.clear();
                        }
                    }

                    #choice_build
                    builder.build()
                }

                #to_xml
            }

            impl #builder_ident {
                pub fn build(self) -> Result<#name_ident> {
                    let Self { #(#field_names),* } = self;

                    Ok(#name_ident {
                        #build_fields
                    })
                }

                #builder_fns
            }
        };

        tokens.extend(gen);
    }
}
