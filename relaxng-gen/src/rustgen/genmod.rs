use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GenLib {
    pub(crate) mods: Vec<String>,
    pub(crate) root_mod: String,
    pub(crate) schema_filename: String,
    pub(crate) doc: Option<String>,
}

impl GenLib {
    pub(crate) fn new(mods: Vec<String>, root_mod: String, schema_filename: String) -> Self {
        Self {
            mods,
            root_mod,
            schema_filename,
            doc: None,
        }
    }

    pub(crate) fn name(&self) -> &str {
        "lib"
    }
}

impl ToTokens for GenLib {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let root_mod = format_ident!("{}", self.root_mod);
        let filename = format!("'{}'", &self.schema_filename);

        let mods = self.mods.iter().map(|m| format_ident!("{}", m));
        let mods = quote! {
            #(pub mod #mods;)*
            pub use #root_mod::*;
        };

        let doc = self.doc.as_ref().map(|doc| {
            quote! {
                #![doc = #doc]
            }
        });

        let ts = quote! {
            #doc
            #![doc = concat!("Generated from ", #filename, " schema.")]

            #![allow(dead_code)]
            #![allow(unused_mut)]
            #![allow(unused_variables)]
            #![allow(unused_imports)]

            use thiserror::Error;

            #mods

            /// The error type used by this crate.
            #[derive(Error, Debug)]
            pub enum Error {
                #[error("IO error: {0}")]
                Io(#[from] std::io::Error),

                #[error("From UTF-8 error: {0}")]
                FromUtf8Error(#[from] std::string::FromUtf8Error),

                #[error("{0} builder is missing mandatory field: {1}")]
                BuilderMissingField(&'static str, &'static str),

                #[error("{0} builder cannot build a variant")]
                BuilderVariant(&'static str),

                #[error("Failed to parse '{0}': {1}")]
                ParseError(&'static str, #[source] Box<dyn std::error::Error + Send + Sync + 'static>),
            }

            impl From<std::convert::Infallible> for Error {
                fn from(_: std::convert::Infallible) -> Self {
                    unreachable!("Infallible error should not occur")
                }
            }

            /// Helper trait for this crate XML types.
            pub trait ToXml {
                /// The associated builder type.
                type Builder: Default;

                /// Creates a new builder for this type.
                fn builder() -> Self::Builder {
                    Self::Builder::default()
                }

                /// Writes Events to a XML writer.
                fn to_xml<W>(&self, writer: &mut quick_xml::Writer<W>) -> Result<()>
                where
                    W: std::io::Write;

                /// Helper function to convert this type to a string.
                fn to_string(&self) -> String {
                    let mut writer =
                        quick_xml::Writer::new_with_indent(std::io::Cursor::new(Vec::new()), b' ', 2);
                    self.to_xml(&mut writer).expect("Failed to generate XML");
                    let res = writer.into_inner().into_inner();
                    String::from_utf8(res).expect("Failed to convert bytes to string")
                }
            }

            /// A specialized Result type where the error is hard-wired to [`enum@Error`].
            pub type Result<T, E = Error> = std::result::Result<T, E>;
        };

        tokens.extend(ts);
    }
}
