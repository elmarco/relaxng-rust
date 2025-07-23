use std::path::PathBuf;

use rustgen::Config;
use structopt::StructOpt;
use tracing_subscriber::{EnvFilter, fmt, layer::SubscriberExt, util::SubscriberInitExt};

pub(crate) mod error;
pub(crate) mod rustgen;
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
    with_test: bool,

    /// Print missing doc rules
    #[structopt(long, takes_value = false)]
    print_missing_doc: bool,

    /// Generate without impl blocks
    #[structopt(long, takes_value = false)]
    without_impl: bool,

    /// Overwrite existing files in output directory
    #[structopt(short = "f", long = "force", takes_value = false)]
    force: bool,
}

fn main() {
    tracing_subscriber::registry()
        .with(
            // The EnvFilter layer reads the RUST_LOG environment variable.
            EnvFilter::from_default_env(),
        )
        .with(
            fmt::layer()
                .without_time()
                .with_file(true)
                .with_line_number(true)
                .with_target(false),
        )
        .init();

    let Cli {
        schema,
        out,
        with_test,
        config_path,
        print_missing_doc,
        without_impl,
        force,
    } = Cli::from_args();

    let mut config = Config {
        with_test,
        without_impl,
        force,
        ..Config::default()
    };
    for path in config_path {
        config.load_toml(&path).expect("Failed to load TOML config");
    }

    let ctx = rustgen::generate(schema, out, &mut config);
    if print_missing_doc {
        ctx.print_missing_doc();
    }
}
