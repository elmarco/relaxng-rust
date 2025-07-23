use proc_macro2::TokenStream;
use quote::quote;
use relaxng_model::datatype::Datatypes;

use crate::rustgen::Context;

pub(crate) fn datatype_to_ty(datatype: &Datatypes, ctx: &mut Context) -> TokenStream {
    use relaxng_model::datatype::*;

    // TODO: implement facets with "validator" ?

    match datatype {
        Datatypes::Relax(builtin_datatype) => match builtin_datatype {
            relax::BuiltinDatatype::String | relax::BuiltinDatatype::Token => quote! { String },
        },
        Datatypes::Xsd(xsd_datatypes) => match xsd_datatypes {
            xsd::XsdDatatypes::NormalizedString(_string_facets) => quote! { String },
            xsd::XsdDatatypes::String(string_facets) => {
                let pattern = string_facets.regex().map(|regex| {
                    let pat_type = ctx.add_regex_pattern(regex.as_str());
                    quote! { crate::#pat_type }
                });
                if string_facets.bounded() || pattern.is_some() {
                    let min = string_facets.min_len().unwrap_or(0);
                    let max = string_facets.max_len().unwrap_or(usize::MAX);
                    quote! { crate::ConstrainedString<#min, #max, #pattern> }
                } else {
                    quote! { String }
                }
            }
            xsd::XsdDatatypes::Short(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    let min = min_max_facet.min().unwrap_or(i16::MIN);
                    let max = min_max_facet.max().unwrap_or(i16::MAX);
                    quote! { bounded_integer::BoundedI16<#min, #max> }
                } else {
                    quote! { i16 }
                }
            }
            xsd::XsdDatatypes::UnsignedShort(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    let min = min_max_facet.min().unwrap_or(u16::MIN);
                    let max = min_max_facet.max().unwrap_or(u16::MAX);
                    quote! { bounded_integer::BoundedU16<#min, #max> }
                } else {
                    quote! { u16 }
                }
            }
            xsd::XsdDatatypes::Long(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    let min = min_max_facet.min().unwrap_or(i64::MIN);
                    let max = min_max_facet.max().unwrap_or(i64::MAX);
                    quote! { bounded_integer::BoundedI64<#min, #max> }
                } else {
                    quote! { i64 }
                }
            }
            xsd::XsdDatatypes::Int(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    let min = min_max_facet.min().unwrap_or(i32::MIN);
                    let max = min_max_facet.max().unwrap_or(i32::MAX);
                    quote! { bounded_integer::BoundedI32<#min, #max> }
                } else {
                    quote! { i32 }
                }
            }
            xsd::XsdDatatypes::Integer(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    //bigint
                    todo!()
                } else {
                    // should be some bigint
                    quote! { isize }
                }
            }
            // Note: PositiveInteger technically excludes zero, but u64 is a common mapping
            xsd::XsdDatatypes::PositiveInteger(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    //bigint
                    todo!()
                } else {
                    quote! { usize }
                }
            }
            xsd::XsdDatatypes::UnsignedInt(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    let min = min_max_facet.min().unwrap_or(u32::MIN);
                    let max = min_max_facet.max().unwrap_or(u32::MAX);
                    quote! { bounded_integer::BoundedU32<#min, #max> }
                } else {
                    quote! { u32 }
                }
            }
            xsd::XsdDatatypes::UnsignedLong(min_max_facet, _pattern_facet) => {
                if min_max_facet.bounded() {
                    let min = min_max_facet.min().unwrap_or(u64::MIN);
                    let max = min_max_facet.max().unwrap_or(u64::MAX);
                    quote! { bounded_integer::BoundedU64<#min, #max> }
                } else {
                    quote! { u64 }
                }
            }
            // Note: Using String for Decimal to avoid precision issues with floats.
            // A dedicated decimal crate could be used for better accuracy if needed.
            xsd::XsdDatatypes::Decimal {
                min_max: _,
                pattern: _,
                fraction_digits: _,
                total_digits: _,
            } => quote! { String },
            xsd::XsdDatatypes::Double(_pattern_facet) => quote! { f64 },
            // Represents a list of NMTOKEN values
            xsd::XsdDatatypes::NmTokens(_length_facet) => quote! { Vec<String> },
            xsd::XsdDatatypes::NmToken(_length_facet) => quote! { String },
            xsd::XsdDatatypes::NcName(_length_facet) => quote! { String },
            xsd::XsdDatatypes::Token(_length_facet) => quote! { String },
            // Note: Using String for date/time types for simplicity.
            // Crates like chrono could provide specific types if needed.
            xsd::XsdDatatypes::Duration(_pattern_facet) => quote! { String },
            xsd::XsdDatatypes::Date(_pattern_facet) => quote! { String },
            xsd::XsdDatatypes::Datetime(_pattern_facet) => quote! { String },
            xsd::XsdDatatypes::AnyURI(_pattern_facet) => quote! { String },
            xsd::XsdDatatypes::Language(_pattern_facet) => quote! { String },
            xsd::XsdDatatypes::Boolean(_pattern_facet) => quote! { bool },
            xsd::XsdDatatypes::Id(_pattern_facet) => quote! { String },
            xsd::XsdDatatypes::IdRef(_pattern_facet) => quote! { String },
        },
    }
}
