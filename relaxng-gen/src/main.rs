use std::path::PathBuf;

use rustgen::Config;
use structopt::StructOpt;

mod rustgen;
pub(crate) mod utils;
pub(crate) mod xpath;

#[derive(Debug, StructOpt)]
struct Cli {
    /// A RNG (XML) or RNC (simplified) RelaxNG schema
    schema: PathBuf,
    /// Output directory path
    out: PathBuf,

    /// Optional config path
    #[structopt(long = "config")]
    config_path: Vec<PathBuf>,

    /// Generate a test main.rs
    #[structopt(long, takes_value = false)]
    test: bool,

    /// Print missing doc rules
    #[structopt(long, takes_value = false)]
    print_missing_doc: bool,
}

fn main() {
    tracing_subscriber::fmt::init();

    let Cli {
        schema,
        out,
        test,
        config_path,
        print_missing_doc,
    } = Cli::from_args();

    let mut config = Config::default();
    for path in config_path {
        config.load_toml(&path).expect("Failed to load TOML config");
    }

    let ctx = rustgen::generate(schema, out, test, config);
    if print_missing_doc {
        ctx.print_missing_doc();
    }
}
