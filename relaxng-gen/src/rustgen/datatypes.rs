use relaxng_model::datatype::Datatypes;

pub(crate) fn datatype_to_ty(datatype: &Datatypes) -> String {
    use relaxng_model::datatype::*;

    // TODO: implement facets with "validator" ?

    match datatype {
        Datatypes::Relax(builtin_datatype) => match builtin_datatype {
            relax::BuiltinDatatype::String | relax::BuiltinDatatype::Token => "String".to_string(),
        },
        Datatypes::Xsd(xsd_datatypes) => match xsd_datatypes {
            xsd::XsdDatatypes::NormalizedString(_string_facets) => "String".to_string(),
            xsd::XsdDatatypes::String(_string_facets) => "String".to_string(),
            xsd::XsdDatatypes::Short(_min_max_facet, _pattern_facet) => "i16".to_string(),
            xsd::XsdDatatypes::UnsignedShort(_min_max_facet, _pattern_facet) => "u16".to_string(),
            xsd::XsdDatatypes::Long(_min_max_facet, _pattern_facet) => "i64".to_string(),
            xsd::XsdDatatypes::Int(_min_max_facet, _pattern_facet) => "i32".to_string(),
            xsd::XsdDatatypes::Integer(_min_max_facet, _pattern_facet) => {
                // should be some bigint
                "isize".to_string()
            }
            // Note: PositiveInteger technically excludes zero, but u64 is a common mapping
            xsd::XsdDatatypes::PositiveInteger(_min_max_facet, _pattern_facet) => "u64".to_string(),
            xsd::XsdDatatypes::UnsignedInt(_min_max_facet, _pattern_facet) => "u32".to_string(),
            xsd::XsdDatatypes::UnsignedLong(_min_max_facet, _pattern_facet) => "u64".to_string(),
            // Note: Using String for Decimal to avoid precision issues with floats.
            // A dedicated decimal crate could be used for better accuracy if needed.
            xsd::XsdDatatypes::Decimal {
                min_max: _,
                pattern: _,
                fraction_digits: _,
                total_digits: _,
            } => "String".to_string(),
            xsd::XsdDatatypes::Double(_pattern_facet) => "f64".to_string(),
            // Represents a list of NMTOKEN values
            xsd::XsdDatatypes::NmTokens(_length_facet) => "Vec<String>".to_string(),
            xsd::XsdDatatypes::NmToken(_length_facet) => "String".to_string(),
            xsd::XsdDatatypes::NcName(_length_facet) => "String".to_string(),
            xsd::XsdDatatypes::Token(_length_facet) => "String".to_string(),
            // Note: Using String for date/time types for simplicity.
            // Crates like chrono could provide specific types if needed.
            xsd::XsdDatatypes::Duration(_pattern_facet) => "String".to_string(),
            xsd::XsdDatatypes::Date(_pattern_facet) => "String".to_string(),
            xsd::XsdDatatypes::Datetime(_pattern_facet) => "String".to_string(),
            xsd::XsdDatatypes::AnyURI(_pattern_facet) => "String".to_string(),
            xsd::XsdDatatypes::Language(_pattern_facet) => "String".to_string(),
            xsd::XsdDatatypes::Boolean(_pattern_facet) => "bool".to_string(),
            xsd::XsdDatatypes::Id(_pattern_facet) => "String".to_string(),
            xsd::XsdDatatypes::IdRef(_pattern_facet) => "String".to_string(),
        },
    }
}
