use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GenMod {
    pub(crate) mods: Vec<String>,
    pub(crate) root_mod: String,
    pub(crate) schema_filename: String,
}

impl GenMod {
    pub(crate) fn new(mods: Vec<String>, root_mod: String, schema_filename: String) -> Self {
        Self {
            mods,
            root_mod,
            schema_filename,
        }
    }

    pub(crate) fn name(&self) -> &str {
        "mod"
    }
}

impl ToTokens for GenMod {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let root_mod = format_ident!("{}", self.root_mod);
        let filename = format!("'{}'", &self.schema_filename);

        let mods = self.mods.iter().map(|m| format_ident!("{}", m));
        let mods = quote! {
            #(pub mod #mods;)*
            pub use #root_mod::*;
        };

        let ts = quote! {
            #![doc = concat!("Generated from ", #filename, " schema.")]
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

                #[error("Failed to parse '{0}': {1}")]
                ParseError(&'static str, #[source] Box<dyn std::error::Error + Send + Sync + 'static>),

                #[error("From UTF-8 error: {0}")]
                FromUtf8Error(#[from] std::string::FromUtf8Error),
            }

            impl From<std::convert::Infallible> for Error {
                fn from(_: std::convert::Infallible) -> Self {
                    unreachable!("Infallible error should not occur")
                }
            }

            pub trait ToXml {
                type Builder: Default;

                fn builder() -> Self::Builder {
                    Self::Builder::default()
                }

                fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write;

                fn to_string(&self) -> String {
                    let mut writer =
                        quick_xml::Writer::new_with_indent(std::io::Cursor::new(Vec::new()), b' ', 2);
                    self.to_xml(&mut writer).expect("Failed to generate XML");
                    let res = writer.into_inner().into_inner();
                    String::from_utf8(res).expect("Failed to convert bytes to string")
                }
            }

            pub type Result<T, E = Error> = std::result::Result<T, E>;
        };

        tokens.extend(ts);
    }
}
