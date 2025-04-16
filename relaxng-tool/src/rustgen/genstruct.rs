use heck::{ToSnakeCase, ToUpperCamelCase};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

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

    pub(crate) fn builder_ident(&self) -> Ident {
        format_ident!("{}Builder", self.ident())
    }

    fn gen_to_xml(&self) -> TokenStream {
        let name = &self.xml_name;
        let mut to_xml_attrs = quote! {};
        let mut to_xml_elems = quote! {};

        for field in &self.fields {
            let field_ident = field.ident();

            let mut elem_to_xml = if field.attribute {
                let name_b = field.name_b();
                quote! {  start.push_attribute((&#name_b[..], quick_xml::escape::escape("foo").as_bytes())); }
            } else if field.text {
                quote! { writer.write_event(quick_xml::events::Event::Text(quick_xml::events::BytesText::new(&elem.to_string())))?; }
            } else {
                quote! { elem.to_xml(writer)?; }
            };
            if field.multiple {
                elem_to_xml = quote! {
                    for elem in elem {
                        #elem_to_xml
                    }
                }
            };
            let elem = if field.optional && !field.multiple {
                quote! {
                    if let Some(elem) = &self.#field_ident {
                        #elem_to_xml
                    }
                }
            } else {
                quote! {
                    let elem = &self.#field_ident;
                    #elem_to_xml
                }
            };
            if field.attribute {
                to_xml_attrs.extend(elem);
            } else {
                to_xml_elems.extend(elem);
            }
        }

        let body_event = if to_xml_elems.is_empty() {
            quote! {
                writer.write_event(Event::Empty(start))?;
            }
        } else {
            quote! {
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
        let mut builder_fns = quote! {};
        for field in &self.fields {
            let field_name = field.name();
            let field_name_b = field.name_b();
            let field_ident = field.ident();
            let field_single = field.single_ident();
            let field_ty = field.ty_ident();

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
            builder_fns.extend(field.gen_builder_fn());

            // from_xml
            if field.text {
                let mut val = if field.attribute {
                    quote! {
                        attr.unescape_value()?
                    }
                } else {
                    quote! {
                        e.unescape()?
                    }
                };
                if field.optional && !field.multiple {
                    val = quote! { Some(#val) };
                };
                let build_field = quote! { builder = builder.#field_single(#val)?; };
                if field.attribute {
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
            } else {
                let mut val = quote! { #field_ty::from_xml(reader, &e)? };
                if field.optional && !field.multiple {
                    val = quote! { Some(#val) };
                }
                let build_field = quote! { builder = builder.#field_single(#val)?; };
                let elem = quote! { #field_name_b => { #build_field } };
                from_xml_elems.extend(elem);
            }
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

                pub fn from_xml<R>(reader: &mut quick_xml::Reader<R>, start_element: &quick_xml::events::BytesStart) -> Result<Self>
                where
                    R: std::io::BufRead,
                {
                    use quick_xml::events::Event;

                    let mut builder = Self::builder();

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

                    let mut buf = Vec::new();
                    let mut end: Option<quick_xml::events::BytesEnd<'static>> = None;
                    loop {
                        if let Some(end) = end.take() {
                            reader.read_to_end_into(end.name(), &mut buf)?;
                            buf.clear();
                        }
                        match reader.read_event_into(&mut buf)? {
                            event @ (Event::Start(_) | Event::Empty(_)) => {
                                let (e, is_start) = match event {
                                    Event::Start(e) => (e, true),
                                    Event::Empty(e) => (e, false),
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
                            Event::End(e) => break,
                            Event::Eof => #on_eof,
                            other => {
                                //dbg!(other);
                            }
                        }
                        buf.clear();
                    }

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
