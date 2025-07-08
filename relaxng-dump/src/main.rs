use relaxng_dump::Dumper;
use relaxng_model::{Compiler, FsFiles, Syntax};
use std::env;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Err("Missing file argument".into());
    }
    let file_path = Path::new(&args[1]);
    let syntax = if file_path.extension().is_some_and(|ext| ext == "rnc") {
        Syntax::Compact
    } else {
        Syntax::Xml
    };
    let mut compiler = Compiler::new(FsFiles, syntax);
    match compiler.compile(file_path) {
        Ok(model) => {
            if let Some(rule) = model.borrow().as_ref() {
                let mut dumper = Dumper::new();
                println!("{}", dumper.dump_to_string(rule));
            }
        }
        Err(e) => {
            compiler.dump_diagnostic(&e);
            return Err(format!("Compilation failed: {e:?}").into());
        }
    }

    Ok(())
}
