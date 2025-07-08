use relaxng_dump::Dumper;
use relaxng_model::{Compiler, FsFiles, Syntax};
use std::fs;

#[test]
fn dump() {
    let paths = fs::read_dir("tests").unwrap();
    for path in paths {
        let path = path.unwrap().path();
        if let Some(extension) = path.extension().and_then(|s| s.to_str()) {
            if (extension == "rng" || extension == "rnc")
                && path.file_stem().unwrap() != "tuto7"
            {
                let syntax = if extension == "rnc" {
                    Syntax::Compact
                } else {
                    Syntax::Xml
                };
                let mut compiler = Compiler::new(FsFiles, syntax);
                let model = match compiler.compile(&path) {
                    Ok(model) => model,
                    Err(e) => {
                        panic!("Failed to compile {:?}: {:?}", path, e);
                    }
                };

                let mut dumper = Dumper::new();
                let output = dumper.dump_to_string(&model.borrow().as_ref().unwrap());

                let snapshot_name = path.file_stem().unwrap().to_str().unwrap();
                insta::assert_snapshot!(snapshot_name, output);
            }
        }
    }
}
