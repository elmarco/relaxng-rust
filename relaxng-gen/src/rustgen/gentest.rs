use std::path::PathBuf;

use prettyplease::unparse;
use quote::{format_ident, quote};

pub(crate) fn generate_test(root_elem: &str, out: &PathBuf) {
    let root_elem = format_ident!("{}", root_elem);
    let src_dir = out.join("src");
    let _ = std::fs::create_dir(&src_dir);

    let cargo_toml_path = out.join("Cargo.toml");
    let cargo_toml_content = r#"
[package]
name = "test"
version = "0.1.0"
edition = "2024"

[dependencies]
roxmltree = "0.20"
quick-xml = "0.38"
thiserror = "2.0"
"#;
    std::fs::write(&cargo_toml_path, cargo_toml_content).expect("writing Cargo.toml");

    let main_rs_path = src_dir.join("main.rs");
    let main_rs = quote! {
        use std::path::Path; // Import logger implementation
        use std::{env::args, fs};

        use test::ToXml;

        fn main() {
            let path = args().nth(1).expect("No XML file path provided");
            let xml_path = Path::new(&path);
            let xml = fs::read_to_string(xml_path).expect("Failed to read test XML file");
            let xml = roxmltree::Document::parse(&xml).expect("Failed to parse XML");
            let root = xml.root().first_child().expect("No root element found");

            let res = test::#root_elem::from_xml(&root).unwrap();
            let res = res.to_string();
            println!("{}", res);
        }
    };
    let file = syn::parse2(main_rs).unwrap();
    let main_rs = unparse(&file);

    std::fs::write(&main_rs_path, main_rs).expect("writing main.rs");
    println!("Created test Rust project at {out:?}");
}
