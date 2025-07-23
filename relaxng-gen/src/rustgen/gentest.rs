use std::path::PathBuf;

use askama::Template;
use prettyplease::unparse;
use quote::{format_ident, quote};

use crate::utils::safe_var_name;

#[derive(Template)]
#[template(path = "Cargo.toml")]
struct CargoTemplate<'a> {
    name: &'a str,
}

pub(crate) fn generate_test(
    project_name: &str,
    root_elem: &str,
    needs_interleave: bool,
    out: &PathBuf,
) {
    let root_elem = format_ident!("{}", root_elem);
    let src_dir = out.join("src");
    let _ = std::fs::create_dir(&src_dir);

    let cargo_toml_path = out.join("Cargo.toml");
    let cargo_toml = CargoTemplate {
        name: project_name,
    };
    let cargo_toml_content = cargo_toml.render().unwrap();
    std::fs::write(&cargo_toml_path, cargo_toml_content).expect("writing Cargo.toml");

    let project_name = format_ident!("{}", safe_var_name(project_name));
    let main_rs_path = src_dir.join("main.rs");

    // Generate different from_xml call based on whether the root type needs interleave args
    let from_xml_call = if needs_interleave {
        quote! {
            let mut child = Some(root);
            let res = #project_name::#root_elem::from_xml(&root, &mut child).unwrap();
        }
    } else {
        quote! {
            let res = #project_name::#root_elem::from_xml(&root).unwrap();
        }
    };

    let main_rs = quote! {
        use std::path::Path;
        use std::{env::args, fs};

        fn main() {
            let path = args().nth(1).expect("No XML file path provided");
            let xml_path = Path::new(&path);
            let xml = fs::read_to_string(xml_path).expect("Failed to read test XML file");
            let xml = roxmltree::Document::parse(&xml).expect("Failed to parse XML");
            let root = xml.root().first_child().expect("No root element found");

            #from_xml_call
            let res = res.to_string();
            println!("{}", res);
        }
    };
    let file = syn::parse2(main_rs).unwrap();
    let main_rs = unparse(&file);

    std::fs::write(&main_rs_path, main_rs).expect("writing main.rs");
    println!("Created test Rust project at {out:?}");
}
