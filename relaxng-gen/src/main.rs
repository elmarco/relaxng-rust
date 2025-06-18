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
}

fn main() {
    tracing_subscriber::fmt::init();

    let Cli {
        schema,
        out,
        test,
        config_path,
    } = Cli::from_args();

    let mut config = Config::default();
    for path in config_path {
        config.load_toml(&path).expect("Failed to load TOML config");
    }

    rustgen::generate(schema, out, test, config);
}
