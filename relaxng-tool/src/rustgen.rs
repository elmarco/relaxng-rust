use heck::ToSnakeCase;
use heck::ToUpperCamelCase;
use prettyplease::unparse;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use relaxng_model::datatype::Datatypes;
use relaxng_model::FsFiles;
use relaxng_model::Syntax;
use std::path::PathBuf;
use std::process::exit;

use relaxng_model::model::NameClass;
use relaxng_model::model::Pattern;

use relaxng_model::Compiler;

mod genfield;
use genfield::GenField;
mod genstruct;
use genstruct::GenStruct;
mod genenum;
use genenum::GenEnum;

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
