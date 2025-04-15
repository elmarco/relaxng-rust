use heck::ToUpperCamelCase;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

use super::GenField;

#[derive(Debug)]
pub(crate) struct GenStruct {
    pub(crate) name: String,
    pub(crate) fields: Vec<GenField>,
}

impl GenStruct {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            fields: Vec::new(),
        }
    }

    pub(crate) fn add_field(&mut self, field: GenField) {
        self.fields.push(field);
    }

    pub(crate) fn name_b(&self) -> Literal {
        Literal::byte_string(self.name.as_bytes())
    }

    pub(crate) fn ident(&self) -> Ident {
        format_ident!("{}", self.name.to_upper_camel_case())
    }

    pub(crate) fn builder_ident(&self) -> Ident {
        format_ident!("{}Builder", self.name.to_upper_camel_case())
    }
}

impl ToTokens for GenStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let name_b = self.name_b();
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();
        let fields = &self.fields;
        let mut builder_fields = fields.clone();
        builder_fields.iter_mut().for_each(|f| f.optional = true);
        let field_names = fields.iter().map(|f| f.ident());

        let mut xml_events = quote! {};
        let mut from_xml_attrs = quote! {};
        let mut from_xml_elems = quote! {};
        let mut to_xml_attrs = quote! {};
        let mut to_xml_elems = quote! {};
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

            let mut val = quote! { #field_single.try_into()? };
            if field.optional && !field.multiple {
                val = quote! {
                    if let Some(#field_single) = #field_single {
                        Some(#val)
                    } else {
                        None
                    }
                };
            }
            let body = if field.multiple {
                quote! { self.#field_ident.push(#val); }
            } else if field.optional {
                quote! { self.#field_ident = #val; }
            } else {
                quote! { self.#field_ident = Some(#val); }
            };
            let t = if field.optional && !field.multiple {
                quote! { Option<T> }
            } else {
                quote! { T }
            };
            let build_fn = quote! {
                pub fn #field_single<T>(mut self, #field_single: #t) -> Result<Self>
                where
                    T: TryInto<#field_ty>,
                    Error: From<<T as TryInto<#field_ty>>::Error>
                {
                    #body
                    Ok(self)
                }
            };
            builder_fns.extend(build_fn);

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

            // to_xml
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
                            Event::Eof => return Err(Error::UnexpectedEof),
                            other => {
                                //dbg!(other);
                            }
                        }
                        buf.clear();
                    }

                    builder.build()
                }

                pub fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write,
                {
                    use quick_xml::events::{Event, BytesStart, BytesEnd};

                    let mut start = BytesStart::new(#name);

                    #to_xml_attrs

                    writer.write_event(Event::Start(start))?;

                    #to_xml_elems

                    writer.write_event(Event::End(BytesEnd::new(#name)))?;
                    Ok(())
                }

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
