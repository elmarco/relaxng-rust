#![cfg(feature = "integration-tests")]

use std::path::{Path, PathBuf};
use std::process::{Command, Output, exit};

use assert_cmd::prelude::*;
use glob::glob;
use insta::assert_snapshot;

fn generate_and_run(path: &Path) -> datatest_stable::Result<()> {
    let base_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let path = base_dir.join(path);
    let name = path.file_stem().unwrap().to_str().unwrap();
    let fixtures_dir = path.parent().unwrap();

    let mut project = Project::new(path.to_path_buf());
    project.build()?;

    let pattern1 = fixtures_dir.join(format!("{name}.*.xml"));
    let pattern2 = fixtures_dir.join(format!("{name}.xml"));

    for xml_entry in glob(pattern1.to_str().unwrap())
        .unwrap()
        .chain(glob(pattern2.to_str().unwrap()).unwrap())
    {
        let xml_path = xml_entry?;
        let xml_name = xml_path.file_stem().unwrap().to_str().unwrap();
        let output = project.run(xml_path.clone())?;
        if xml_name.ends_with(".ko") {
            assert!(!output.status.success());
            assert_snapshot!(xml_name, String::from_utf8_lossy(&output.stderr).to_string());
        } else {
            assert!(
                output.status.success(),
                "stdout: {}\nstderr: {}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
            assert_snapshot!(xml_name, String::from_utf8_lossy(&output.stdout).to_string());
        }
    }

    Ok(())
}

datatest_stable::harness! {
    { test = generate_and_run, root = "tests/fixtures", pattern = r".*\.(rnc|rng)$" },
}

struct Project {
    path: PathBuf,
    temp_dir: tempfile::TempDir,
}

impl Project {
    fn new(path: PathBuf) -> Self {
        let temp_dir = tempfile::tempdir().unwrap();
        Self {
            path,
            temp_dir,
        }
    }

    fn build(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let out_path = self.temp_dir.path();

        let mut cmd = Command::cargo_bin("relaxng-gen")?;
        cmd.arg(&self.path).arg(out_path).arg("--with-test").assert().success();

        let mut cargo_cmd = Command::new("cargo");
        let output = cargo_cmd.current_dir(out_path).arg("build").arg("--offline").output()?;
        if !output.status.success() {
            eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
            eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
            self.temp_dir.disable_cleanup(true);
            eprintln!("Build failed: {}", self.path.display());
            exit(1);
        }

        Ok(())
    }

    fn run(&mut self, xml: PathBuf) -> Result<Output, Box<dyn std::error::Error>> {
        let out_path = self.temp_dir.path();

        let mut cargo_cmd = Command::new("cargo");
        let output = cargo_cmd.current_dir(out_path).arg("run").arg(xml).output()?;

        Ok(output)
    }
}
