use heck::ToSnakeCase;
use heck::ToUpperCamelCase;
use indexmap::IndexMap;
use prettyplease::unparse;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;
use relaxng_model::datatype::Datatypes;
use relaxng_model::FsFiles;
use relaxng_model::Syntax;
use std::path::PathBuf;
use std::process::exit;
use syn::AttrStyle;
use syn::Ident;
use syn::Index;

use relaxng_model::model::NameClass;
use relaxng_model::model::Pattern;

use relaxng_model::Compiler;

pub(crate) fn generate(schema: PathBuf, out: PathBuf, test: bool) {
    let mut compiler = if schema.extension().map(|ext| ext == "rng").unwrap_or(false) {
        Compiler::new(FsFiles, Syntax::Xml)
    } else {
        Compiler::default()
    };
    let model = match compiler.compile(&schema) {
        Ok(m) => m,
        Err(err) => {
            compiler.dump_diagnostic(&err);
            exit(1);
        }
    };
    // dbg!(&model);
    let define = model.as_ref().borrow();
    let pattern = define.as_ref().unwrap().pattern();

    let mut ctx = Context::new();
    generate_pattern(pattern, &mut ctx);
    let stream = &ctx.stream;

    let tokens = quote! {
        #![allow(unused_mut)]
        #![allow(unused_variables)]
        use thiserror::Error;

        #[derive(Error, Debug)]
        pub enum Error {
            #[error("IO error: {0}")]
            Io(#[from] std::io::Error),

            #[error("XML parsing error: {0}")]
            Xml(#[from] quick_xml::Error),

            #[error("Unexpected XML event: {0:?}")]
            UnexpectedEvent(String),

            #[error("Unexpected end of file")]
            UnexpectedEof,

            #[error("Invalid value for attribute '{attr}' on element <{elem}>: {reason}")]
            InvalidAttributeValue {
                elem: &'static str,
                attr: &'static str,
                reason: String,
            },

            #[error("Invalid value for element <{0}>: {1}")]
            InvalidElementValue(&'static str, String),

            #[error("Builder {0} missing mandatory field: {0}")]
            BuilderMissingField(&'static str, &'static str),
        }

        impl From<std::convert::Infallible> for Error {
            fn from(_: std::convert::Infallible) -> Self {
                unreachable!("Infallible error should not occur")
            }
        }

        pub type Result<T, E = Error> = std::result::Result<T, E>;

        #stream
    };

    let file = syn::parse2(tokens).unwrap();
    let generated = unparse(&file);
    // println!("{generated}");

    if test {
        generate_test(ctx, out, generated);
    }
}

fn generate_test(ctx: Context, out: PathBuf, generated: String) {
    let src_dir = out.join("src");
    std::fs::create_dir(&src_dir).expect("creating src directory");

    let cargo_toml_path = out.join("Cargo.toml");
    let cargo_toml_content = r#"
[package]
name = "test"
version = "0.1.0"
edition = "2021"

[dependencies]
quick-xml = "0.37"
thiserror = "2.0"
"#;
    std::fs::write(&cargo_toml_path, cargo_toml_content).expect("writing Cargo.toml");

    let gen_path = src_dir.join("gen.rs");
    std::fs::write(&gen_path, generated).expect("writing gen.rs");

    let root = ctx.last_struct.unwrap();
    let root = root.ident();
    let main_rs_path = src_dir.join("main.rs");
    let main_rs = quote! {
        use quick_xml::{Reader, Writer, events::*};
        use std::io::Cursor;
        use std::path::Path; // Import logger implementation
        use std::{env::args, fs};

        mod gen;

        fn main() {
            let path = args().nth(1).expect("No XML file path provided");
            let xml_path = Path::new(&path);
            let xml = fs::read_to_string(xml_path).expect("Failed to read test XML file");

            let mut reader = Reader::from_str(&xml);

            let Event::Start(e) = reader.read_event().unwrap() else {
                panic!("not xml?");
            };

            let res = gen::#root::from_xml(&mut reader, &e).unwrap();
            let mut writer = Writer::new_with_indent(Cursor::new(Vec::new()), b' ', 2);
            res.to_xml(&mut writer).unwrap();
            let res = writer.into_inner().into_inner();
            let res = String::from_utf8(res).unwrap();
            println!("{}", res);
        }
    };
    let file = syn::parse2(main_rs).unwrap();
    let main_rs = unparse(&file);

    std::fs::write(&main_rs_path, main_rs).expect("writing main.rs");
    println!("Created test Rust project at {:?}", out);
}

enum State {
    Choice(GenEnum),
    Group,
    Optional,
    OneOrMore,
    ZeroOrMore,
    Struct(GenStruct),
    Attribute(String),
}

impl State {
    fn into_struct(self) -> GenStruct {
        match self {
            State::Struct(str) => str,
            _ => panic!("Expected Struct state"),
        }
    }
}

struct Context {
    stream: TokenStream,
    state: Vec<State>,
    last_struct: Option<GenStruct>,
    choice_count: usize,
}

impl Context {
    fn new() -> Self {
        Self {
            stream: TokenStream::new(),
            state: Vec::new(),
            last_struct: None,
            choice_count: 0,
        }
    }

    fn add_stream(&mut self, stream: TokenStream) {
        self.stream.extend(stream);
    }

    fn push_state(&mut self, state: State) {
        if let Some(State::Choice(gen)) = self.state.last_mut() {
            gen.push_variant();
        }
        self.state.push(state);
    }

    fn pop_state(&mut self) {
        match self.state.pop().unwrap() {
            State::Choice(e) => self.pop_enum(e),
            State::Group => {}
            State::Optional => {}
            State::OneOrMore => {}
            State::ZeroOrMore => {}
            State::Struct(str) => self.pop_element(str),
            State::Attribute(_) => {}
        }
    }

    fn add_field(&mut self, name: &str, ty: &str, text: bool) {
        let mut field = GenField::new(name, ty, text);
        for state in &mut self.state.iter_mut().rev() {
            match state {
                State::Attribute(name) => {
                    field.set_name(name);
                    field.set_attribute(true);
                }
                State::Struct(str) => {
                    str.add_field(field);
                    return;
                }
                State::Optional => field.set_optional(true),
                State::OneOrMore => field.set_multiple(true),
                State::ZeroOrMore => {
                    field.set_optional(true);
                    field.set_multiple(true);
                }
                State::Group => continue,
                State::Choice(e) => {
                    e.add_field(field);
                    return;
                }
            }
        }
    }

    fn pop_element(&mut self, str: GenStruct) {
        let name = &str.name;
        self.add_stream(str.to_token_stream());

        self.add_field(name, &str.ident().to_string(), false);
        self.last_struct = Some(str);
    }

    fn pop_enum(&mut self, e: GenEnum) {
        let name = &e.name;
        self.add_stream(e.to_token_stream());

        self.add_field(&name.to_snake_case(), &e.ident().to_string(), false);
    }

    fn text(&mut self) {
        self.add_field("value", "String", true);
    }

    fn new_choice(&mut self) -> GenEnum {
        self.choice_count += 1;

        GenEnum::new(format!("Choice{}", self.choice_count))
    }
}

fn generate_pattern(pattern: &Pattern, ctx: &mut Context) {
    match pattern {
        Pattern::Choice(vec) => {
            let choice = ctx.new_choice();
            ctx.push_state(State::Choice(choice));
            for pattern in vec {
                generate_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Interleave(_vec) => panic!("Unimplemented: Interleave"),
        Pattern::Group(vec) => {
            ctx.push_state(State::Group);
            for pattern in vec {
                generate_pattern(pattern, ctx);
            }
            ctx.pop_state();
        }
        Pattern::Mixed(_pattern) => panic!("Unimplemented: Mixed"),
        Pattern::Empty => {
            panic!("Unimplemented: Empty")
        }
        Pattern::Text => ctx.text(),
        Pattern::NotAllowed => panic!("Unimplemented: NotAllowed"),
        Pattern::Optional(pattern) => {
            ctx.push_state(State::Optional);
            generate_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::ZeroOrMore(pattern) => {
            ctx.push_state(State::ZeroOrMore);
            generate_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::OneOrMore(pattern) => {
            ctx.push_state(State::OneOrMore);
            generate_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::Attribute(name_class, pattern) => {
            let NameClass::Named {
                namespace_uri: _,
                name,
            } = name_class
            else {
                panic!("Unexpected name class for attribute");
            };

            let field_ty = name.to_upper_camel_case();
            // Pattern::Choice(vec) => {
            //     let mut e = GenEnum::new(&field_ty);
            //     for p in vec {
            //         use relaxng_model::datatype::DatatypeValues::*;

            //         let Pattern::DatatypeValue { datatype } = p else {
            //             panic!("Unexpected pattern in Choice for Attribute");
            //         };
            //         match datatype {
            //             Relax(BuiltinDatatypeValue::TokenValue(value)) => e.insert(value),
            //             Xsd(xsd_datatype_values) => todo!(),
            //             _ => todo!(),
            //         }
            //     }

            ctx.push_state(State::Attribute(name.to_string()));
            generate_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::Element(name_class, pattern) => {
            let NameClass::Named {
                namespace_uri: _,
                name,
            } = name_class
            else {
                panic!("Unexpected name class");
            };
            let str = GenStruct::new(name);
            ctx.push_state(State::Struct(str));
            generate_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::Ref(_span, name, pat_ref) => {
            // if ctx.ref_seen.insert(name.clone()) {
            //     let rule = pat_ref.0.borrow();
            //     let rule = rule.as_ref().unwrap();
            //     generate_pattern(rule.pattern(), ctx)
            // } else {
            //     panic!("Unhandled circular reference detected");
            // }
            panic!("unhandled ref")
        }
        Pattern::DatatypeValue { datatype: _ } => panic!("Unimplemented: DatatypeValue"),
        Pattern::DatatypeName { datatype, except } => {
            if except.is_some() {
                panic!("Unimplemented: DatatypeName with exception");
            }
            let ty = datatype_to_ty(datatype);
            panic!("unimplemented dn {}", ty)
        }
        Pattern::List(_pattern) => panic!("Unimplemented: List"),
    }
}

fn datatype_to_ty(datatype: &Datatypes) -> String {
    use relaxng_model::datatype::*;

    match datatype {
        Datatypes::Relax(builtin_datatype) => todo!(),
        Datatypes::Xsd(xsd_datatypes) => match xsd_datatypes {
            xsd::XsdDatatypes::NormalizedString(string_facets) => todo!(),
            xsd::XsdDatatypes::String(string_facets) => todo!(),
            xsd::XsdDatatypes::Short(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedShort(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::Long(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::Int(min_max_facet, pattern_facet) => "i32".to_string(),
            xsd::XsdDatatypes::Integer(min_max_facet, pattern_facet) => {
                // should be some bigint
                "isize".to_string()
            }
            xsd::XsdDatatypes::PositiveInteger(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedInt(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedLong(min_max_facet, pattern_facet) => todo!(),
            xsd::XsdDatatypes::Decimal {
                min_max,
                pattern,
                fraction_digits,
                total_digits,
            } => todo!(),
            xsd::XsdDatatypes::Double(pattern_facet) => todo!(),
            xsd::XsdDatatypes::NmTokens(length_facet) => todo!(),
            xsd::XsdDatatypes::NmToken(length_facet) => todo!(),
            xsd::XsdDatatypes::NcName(length_facet) => todo!(),
            xsd::XsdDatatypes::Token(length_facet) => todo!(),
            xsd::XsdDatatypes::Duration(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Date(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Datetime(pattern_facet) => todo!(),
            xsd::XsdDatatypes::AnyURI(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Language(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Boolean(pattern_facet) => todo!(),
            xsd::XsdDatatypes::Id(pattern_facet) => todo!(),
            xsd::XsdDatatypes::IdRef(pattern_facet) => todo!(),
        },
    }
}

#[derive(Debug, Clone)]
struct GenField {
    // name of the rs field, ex "snake_case"
    name: String,
    // name of the xml element, ex "OriginalName"
    xml_name: String,
    // associate rs type, ex "String"
    ty: String,
    // whether this is a text/body
    text: bool,
    optional: bool,
    multiple: bool,
    attribute: bool,
}

impl GenField {
    fn new(name: &str, ty: &str, text: bool) -> Self {
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

    fn set_name(&mut self, name: &str) {
        self.xml_name = name.to_string();
        self.name = name.to_snake_case();
    }

    fn set_attribute(&mut self, attribute: bool) {
        self.attribute = attribute;
    }

    fn set_optional(&mut self, optional: bool) {
        self.optional = optional;
    }

    fn set_multiple(&mut self, multiple: bool) {
        self.multiple = multiple;
    }

    fn name(&self) -> String {
        format!("{}{}", self.name, if self.multiple { "s" } else { "" })
    }

    fn name_b(&self) -> Literal {
        Literal::byte_string(self.xml_name.as_bytes())
    }

    fn ident(&self) -> Ident {
        format_ident!("{}", self.name())
    }

    fn single_ident(&self) -> Ident {
        format_ident!("{}", self.name)
    }

    fn ty_ident(&self) -> Ident {
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

#[derive(Debug)]
struct GenEnum {
    // rs type name
    name: String,
    variants: Vec<Vec<GenField>>,
}

impl GenEnum {
    fn new(name: String) -> Self {
        Self {
            name,
            variants: Vec::new(),
        }
    }
    fn name(&self) -> &str {
        &self.name
    }

    fn ident(&self) -> Ident {
        format_ident!("{}", self.name())
    }

    fn add_field(&mut self, field: GenField) {
        let variant = self.variants.last_mut().expect("an enum variant");

        variant.push(field);
    }

    fn push_variant(&mut self) {
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

#[derive(Debug)]
struct GenStruct {
    name: String,
    fields: Vec<GenField>,
}

impl GenStruct {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            fields: Vec::new(),
        }
    }

    fn add_field(&mut self, field: GenField) {
        self.fields.push(field);
    }

    fn name_b(&self) -> Literal {
        Literal::byte_string(self.name.as_bytes())
    }

    fn ident(&self) -> Ident {
        format_ident!("{}", self.name.to_upper_camel_case())
    }

    fn builder_ident(&self) -> Ident {
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
