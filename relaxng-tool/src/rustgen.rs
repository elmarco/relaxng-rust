use genfield::FieldTy;
use prettyplease::unparse;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;
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

    let mut ctx = Context::new(out.clone());
    generate_pattern(pattern, &mut ctx);
    generate_mod(&mut ctx);

    if test {
        generate_test(ctx, out);
    }
}

fn generate_mod(ctx: &mut Context) {
    let last = ctx.last_struct.as_ref().unwrap();
    let modname = last.var_name();
    let mods = quote! {
        mod #modname;
        pub use #modname::*;
    };

    let tokens = quote! {
        #![allow(dead_code)]
        #![allow(unused_mut)]
        #![allow(unused_variables)]
        #![allow(unused_imports)]

        use thiserror::Error;

        #mods

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

            #[error("{0} builder is missing mandatory field: {1}")]
            BuilderMissingField(&'static str, &'static str),

            #[error("{0} builder cannot build a variant")]
            BuilderVariant(&'static str),
        }

        impl From<std::convert::Infallible> for Error {
            fn from(_: std::convert::Infallible) -> Self {
                unreachable!("Infallible error should not occur")
            }
        }

        pub type Result<T, E = Error> = std::result::Result<T, E>;
    };

    let path = ctx.out.join("mod.rs");
    write_rs(&path, &tokens);
}

fn generate_test(ctx: Context, out: PathBuf) {
    let src_dir = out.join("src");
    let _ = std::fs::create_dir(&src_dir);

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

    let root = ctx.last_struct.unwrap();
    let root_elem = root.ident();
    let main_rs_path = src_dir.join("main.rs");
    let main_rs = quote! {
        use quick_xml::{Reader, Writer, events::*};
        use std::io::Cursor;
        use std::path::Path; // Import logger implementation
        use std::{env::args, fs};

        mod xml;

        fn main() {
            let path = args().nth(1).expect("No XML file path provided");
            let xml_path = Path::new(&path);
            let xml = fs::read_to_string(xml_path).expect("Failed to read test XML file");

            let mut reader = Reader::from_str(&xml);
            let e = match reader.read_event().unwrap() {
                Event::Start(e) | Event::Empty(e) => e,
                _ => panic!("not xml?"),
            };

            let res = xml::#root_elem::from_xml(&mut reader, &e).unwrap();
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

struct Context {
    out: PathBuf,
    state: Vec<State>,
    last_struct: Option<GenStruct>,
    choice_count: usize,
}

impl Context {
    fn new(out: PathBuf) -> Self {
        let out = out.join("src").join("xml");

        Self {
            out,
            state: Vec::new(),
            last_struct: None,
            choice_count: 0,
        }
    }

    fn is_top(&self) -> bool {
        for state in self.state.iter().rev() {
            if let State::Struct(_) = state {
                return false;
            }
        }

        true
    }

    fn output_path(&self) -> PathBuf {
        let mut path = self.out.clone();

        for state in self.state.iter() {
            match state {
                State::Choice(e) => {
                    path = path.join(&e.var_name().to_string());
                }
                State::Struct(s) => {
                    path = path.join(&s.var_name().to_string());
                }
                _ => continue,
            }
        }

        let _ = std::fs::create_dir_all(path.parent().unwrap());
        path.with_extension("rs")
    }

    fn write_rs(&mut self, stream: TokenStream, path: PathBuf) {
        let stream = quote! {
            use crate::xml::{Result, Error};

            #stream
        };

        write_rs(&path, &stream);
    }

    fn push_state(&mut self, state: State) {
        if let Some(State::Choice(gen)) = self.state.last_mut() {
            gen.push_variant();
        }
        self.state.push(state);
    }

    fn pop_state(&mut self) {
        match self.state.last().unwrap() {
            State::Choice(_) => self.pop_choice(),
            State::Struct(_) => self.pop_struct(),
            State::Attribute(_)
            | State::Group
            | State::Optional
            | State::OneOrMore
            | State::ZeroOrMore => {
                let _ = self.state.pop();
            }
        }
    }

    fn add_field(&mut self, name: &str, ty: FieldTy) {
        let mut field = GenField::new(name, ty);
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

    fn pop_struct(&mut self) {
        let output = self.output_path();
        let Some(State::Struct(str)) = self.state.pop() else {
            panic!();
        };

        let mods = gen_mods(str.fields.iter());
        let gen = str.to_token_stream();
        let gen = quote! {
            #mods

            #gen
        };
        self.write_rs(gen, output);

        let name = &str.xml_name;
        self.add_field(name, FieldTy::Ty(str.path()));
        self.last_struct = Some(str);
    }

    fn pop_choice(&mut self) {
        let output = self.output_path();
        let Some(State::Choice(mut e)) = self.state.pop() else {
            panic!();
        };

        let fields: Vec<GenField> = e.variants.iter().flatten().cloned().collect();
        let mods = gen_mods(fields.iter());
        let gen = e.to_token_stream();
        let gen = quote! {
            #mods

            #gen
        };

        self.write_rs(gen, output);
        e.prefix_field_ty();
        self.add_field(&e.var_name().to_string(), FieldTy::Choice(e));
    }

    fn text(&mut self) {
        self.add_field("value", FieldTy::Text);
    }

    fn new_choice(&mut self) -> GenEnum {
        self.choice_count += 1;

        GenEnum::new(format!("Choice{}", self.choice_count))
    }
}

fn gen_mods(fields: std::slice::Iter<'_, GenField>) -> TokenStream {
    let mut mods = quote! {};
    for field in fields {
        if field.is_text() {
            continue;
        }
        let ty = field.ty_path();
        let name = field.single_ident();
        let gen = quote! {
            pub mod #name;
            pub use #name::#ty;
        };
        mods.append_all(gen);
    }
    mods
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
            // nothing
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

            // let field_ty = name.to_upper_camel_case();
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
            let str = GenStruct::new(name, ctx.is_top());
            ctx.push_state(State::Struct(str));
            generate_pattern(pattern, ctx);
            ctx.pop_state();
        }
        Pattern::Ref(_span, _name, _pat_ref) => {
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
        Datatypes::Relax(_builtin_datatype) => todo!(),
        Datatypes::Xsd(xsd_datatypes) => match xsd_datatypes {
            xsd::XsdDatatypes::NormalizedString(_string_facets) => todo!(),
            xsd::XsdDatatypes::String(_string_facets) => todo!(),
            xsd::XsdDatatypes::Short(_min_max_facet, _pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedShort(_min_max_facet, _pattern_facet) => todo!(),
            xsd::XsdDatatypes::Long(_min_max_facet, _pattern_facet) => todo!(),
            xsd::XsdDatatypes::Int(_min_max_facet, _pattern_facet) => "i32".to_string(),
            xsd::XsdDatatypes::Integer(_min_max_facet, _pattern_facet) => {
                // should be some bigint
                "isize".to_string()
            }
            xsd::XsdDatatypes::PositiveInteger(_min_max_facet, _pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedInt(_min_max_facet, _pattern_facet) => todo!(),
            xsd::XsdDatatypes::UnsignedLong(_min_max_facet, _pattern_facet) => todo!(),
            xsd::XsdDatatypes::Decimal {
                min_max: _,
                pattern: _,
                fraction_digits: _,
                total_digits: _,
            } => todo!(),
            xsd::XsdDatatypes::Double(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::NmTokens(_length_facet) => todo!(),
            xsd::XsdDatatypes::NmToken(_length_facet) => todo!(),
            xsd::XsdDatatypes::NcName(_length_facet) => todo!(),
            xsd::XsdDatatypes::Token(_length_facet) => todo!(),
            xsd::XsdDatatypes::Duration(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::Date(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::Datetime(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::AnyURI(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::Language(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::Boolean(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::Id(_pattern_facet) => todo!(),
            xsd::XsdDatatypes::IdRef(_pattern_facet) => todo!(),
        },
    }
}

fn write_rs(path: &PathBuf, stream: &TokenStream) {
    let stream = quote! {
        #![allow(dead_code)]
        #![allow(unused_mut)]
        #![allow(unused_variables)]
        #![allow(unused_imports)]

        #stream
    };

    let file = syn::parse2(stream).unwrap();
    let generated = unparse(&file);

    std::fs::write(path, generated).expect("writing rs code");
}
