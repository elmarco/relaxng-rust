use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::rustgen::Config;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GenLib {
    pub(crate) mods: Vec<String>,
    pub(crate) root_mod: String,
    pub(crate) schema_filename: String,
    pub(crate) doc: Option<String>,
    pub(crate) regex_patterns: Vec<(String, Ident)>,
}

impl GenLib {
    pub(crate) fn new(mods: Vec<String>, root_mod: String, schema_filename: String) -> Self {
        Self {
            mods,
            root_mod,
            schema_filename,
            doc: None,
            regex_patterns: Vec::new(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        "lib"
    }

    pub(crate) fn add_regex_pattern(&mut self, pattern: String, type_name: Ident) {
        self.regex_patterns.push((pattern, type_name));
    }
}

impl GenLib {
    pub(crate) fn to_token_stream(&self, _config: &Config) -> TokenStream {
        let mut tokens = TokenStream::new();

        let root_mod = format_ident!("{}", self.root_mod);
        let filename = format!("'{}'", &self.schema_filename);

        let mods = self.mods.iter().map(|m| format_ident!("{}", m));
        let mods = quote! {
            #(pub mod #mods;)*
            pub use #root_mod::*;
        };

        // Generate regex pattern structs
        let regex_patterns = self.regex_patterns.iter().map(|(pattern, type_ident)| {
            quote! {
                #[doc(hidden)]
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                pub struct #type_ident;

                impl RegexPattern for #type_ident {
                    const REGEX: &'static str = #pattern;
                }
            }
        });

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

                #[error("String too short, expected at least {0} characters, got {1}")]
                TooShort(usize, usize),

                #[error("String too long, expected at most {0} characters, got {1}")]
                TooLong(usize, usize),

                #[error("String '{0}' does not match pattern '{1}'")]
                PatternMismatch(&'static str, String),

                #[error("Failed to compile regex: {0}")]
                RegexError(#[from] regex::Error),

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
            }

            #[doc(hidden)]
            pub trait RegexPattern {
                const REGEX: &'static str;
            }

            #[doc(hidden)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            pub struct NoPattern;
            impl RegexPattern for NoPattern {
                const REGEX: &'static str = "";
            }

            #(#regex_patterns)*

            /// A string type with length constraints and optional runtime regex validation.
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct ConstrainedString<const MIN_LEN: usize, const MAX_LEN: usize, P: RegexPattern = NoPattern> {
                value: String,
                _phantom: std::marker::PhantomData<P>,
            }

            impl<const MIN: usize, const MAX: usize, P: RegexPattern> ConstrainedString<MIN, MAX, P> {
                pub const NO_MIN_CONSTRAINT: usize = 0;
                pub const NO_MAX_CONSTRAINT: usize = usize::MAX;

                pub fn try_new(value: impl Into<String>) -> Result<Self> {
                    let s = value.into();
                    Self::validate(&s)?;
                    Ok(Self {
                        value: s,
                        _phantom: std::marker::PhantomData,
                    })
                }

                fn validate(s: &str) -> Result<()> {
                    fn get_compiled_regex(pattern: &'static str) -> Result<regex::Regex> {
                        /// Registry for compiled regex patterns
                        static REGEX_REGISTRY: std::sync::OnceLock<std::sync::Mutex<std::collections::HashMap<&'static str, regex::Regex>>> =
                            std::sync::OnceLock::new();

                        if pattern.is_empty() {
                            // No regex validation needed for empty pattern
                            return Ok(regex::Regex::new(".*").unwrap()); // matches everything
                        }

                        let registry = REGEX_REGISTRY.get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
                        let mut map = registry.lock().unwrap();

                        if let Some(regex) = map.get(pattern) {
                            return Ok(regex.clone());
                        }

                        match regex::Regex::new(pattern) {
                            Ok(regex) => {
                                let result = regex.clone();
                                map.insert(pattern, regex);
                                Ok(result)
                            }
                            Err(e) => Err(Error::RegexError(e)),
                        }
                    }

                    if MIN > Self::NO_MIN_CONSTRAINT && s.len() < MIN {
                        return Err(Error::TooShort(MIN, s.len()));
                    }

                    if MAX < Self::NO_MAX_CONSTRAINT && s.len() > MAX {
                        return Err(Error::TooLong(MAX, s.len()));
                    }

                    if !P::REGEX.is_empty() {
                        let regex = get_compiled_regex(P::REGEX)?;
                        if !regex.is_match(s) {
                            return Err(Error::PatternMismatch(P::REGEX, s.to_string()));
                        }
                    }

                    Ok(())
                }

                /// Returns the string as a `&str`.
                #[must_use]
                pub fn as_str(&self) -> &str {
                    &self.value
                }

                /// Converts the constrained string into a `String`, consuming it.
                #[must_use]
                pub fn into_string(self) -> String {
                    self.value
                }
            }

            impl<const MIN: usize, const MAX: usize, P: RegexPattern> std::ops::Deref for ConstrainedString<MIN, MAX, P> {
                type Target = str;

                fn deref(&self) -> &Self::Target {
                    &self.value
                }
            }

            impl<const MIN: usize, const MAX: usize, P: RegexPattern> std::fmt::Display
                for ConstrainedString<MIN, MAX, P>
            {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.value)
                }
            }

            impl<const MIN: usize, const MAX: usize, P: RegexPattern> AsRef<str> for ConstrainedString<MIN, MAX, P> {
                fn as_ref(&self) -> &str {
                    &self.value
                }
            }

            impl<const MIN: usize, const MAX: usize, P: RegexPattern> std::str::FromStr for ConstrainedString<MIN, MAX, P> {
                type Err = Error;

                fn from_str(s: &str) -> Result<Self> {
                    Self::try_new(s)
                }
            }

            /// A specialized Result type where the error is hard-wired to [`enum@Error`].
            pub type Result<T, E = Error> = std::result::Result<T, E>;
        };

        tokens.extend(ts);
        tokens
    }
}
