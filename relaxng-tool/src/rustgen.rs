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
use syn::Ident;

use relaxng_model::model::NameClass;
use relaxng_model::model::Pattern;

use relaxng_model::Compiler;

pub(crate) fn generate(schema: PathBuf) {
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
    dbg!(&model);
    let define = model.as_ref().borrow();
    let pattern = define.as_ref().unwrap().pattern();

    let mut ctx = Context::new();
    generate_pattern(pattern, &mut ctx);
    let stream = ctx.into_stream();
    // dbg!(stream.to_string());

    let mut tokens = quote! {
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

        pub type Result<T, E = Error> = std::result::Result<T, E>;
    };
    tokens.append_all(stream);

    let file = syn::parse2(tokens).unwrap();
    let unparsed = unparse(&file);
    println!("{unparsed}");
}

enum State {
    Group,
    Optional,
    OneOrMore,
    ZeroOrMore,
    Struct(GenStruct),
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
}

impl Context {
    fn new() -> Self {
        Self {
            stream: TokenStream::new(),
            state: Vec::new(),
        }
    }

    fn add_stream(&mut self, stream: TokenStream) {
        self.stream.extend(stream);
    }

    fn push_state(&mut self, state: State) {
        self.state.push(state);
    }

    fn pop_state(&mut self) -> State {
        self.state.pop().unwrap()
    }

    fn into_stream(self) -> TokenStream {
        self.stream
    }

    fn add_field(&mut self, name: &str, ty: &str) {
        let mut optional = false;
        let mut multiple = false;
        for state in &mut self.state.iter_mut().rev() {
            match state {
                State::Struct(str) => {
                    let mut field = GenField::new(name, ty);
                    field.set_optional(optional);
                    field.set_multiple(multiple);
                    str.add_field(field);
                    return;
                }
                State::Optional => optional = true,
                State::OneOrMore => multiple = true,
                State::ZeroOrMore => {
                    optional = true;
                    multiple = true;
                }
                State::Group => continue,
            }
        }
    }

    fn element(&mut self, name: &str, str: GenStruct) {
        self.add_stream(str.to_token_stream());

        self.add_field(name, &str.ident().to_string());
    }

    fn text(&mut self) {
        self.add_field("value", "String");
    }
}

fn generate_pattern(pattern: &Pattern, ctx: &mut Context) {
    match pattern {
        Pattern::Choice(vec) => {
            panic!("Unimplemented: Choice")
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

            let field_name = name.to_snake_case();
            panic!("unimplemented attr")
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
            let str = ctx.pop_state().into_struct();
            ctx.element(name, str);
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
    name: String,
    ty: String,
    optional: bool,
    multiple: bool,
}

impl GenField {
    fn new(name: &str, ty: &str) -> Self {
        Self {
            name: name.to_snake_case(),
            ty: ty.to_string(),
            optional: false,
            multiple: false,
        }
    }

    fn name(&self) -> String {
        format!("{}{}", self.name, if self.multiple { "s" } else { "" })
    }

    fn set_optional(&mut self, optional: bool) {
        self.optional = optional;
    }

    fn set_multiple(&mut self, multiple: bool) {
        self.multiple = multiple;
    }

    fn ident(&self) -> Ident {
        format_ident!("{}", self.name())
    }
}

impl ToTokens for GenField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name_ident = Ident::new(&self.name(), Span::call_site());
        let mut ty = Ident::new(&self.ty, Span::call_site()).to_token_stream();

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
        let name_b = Literal::byte_string(self.name.as_bytes());
        let name_ident = self.ident();
        let builder_ident = self.builder_ident();
        let fields = &self.fields;
        let mut builder_fields = fields.clone();
        builder_fields.iter_mut().for_each(|f| f.optional = true);
        let field_names = fields.iter().map(|f| f.ident());

        let mut build_fields = quote! {};
        for field in &self.fields {
            let field_name = field.name();
            let field_ident = field.ident();
            let field = if field.optional {
                quote! {
                    #field_ident,
                }
            } else {
                quote! {
                    #field_ident: #field_ident.ok_or_else(|| Error::BuilderMissingField(#name, #field_name))?,
                }
            };
            build_fields.extend(field);
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
                    let builder = Self::builder();

                    if start_element.name().local_name().as_ref() != #name_b {
                        return Err(Error::UnexpectedEvent(format!("{:?}", start_element)));
                    }

                    builder.build()
                }

                pub fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write,
                {
                    use quick_xml::events::{self, BytesStart, BytesEnd};

                    writer.write_event(events::Event::Start(BytesStart::new(#name)))?;
                    writer.write_event(events::Event::End(BytesEnd::new(#name)))?;
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
            }
        };

        tokens.extend(gen);
    }
}
