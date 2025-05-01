use assert_cmd::prelude::*;
use glob::glob;
use insta::assert_snapshot;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, exit};
use test_case::test_case;

#[test_case("attr-solo")]
#[test_case("backupdisktype")]
#[test_case("field-conflict")]
#[test_case("recurse-inline")]
#[test_case("simple")]
#[test_case("tuto1")]
#[test_case("tuto1c")]
#[test_case("tuto1d")]
#[test_case("tuto2")]
#[test_case("tuto3")]
#[test_case("tuto3b")]
#[test_case("tuto3c")]
#[test_case("tuto3d")]
#[test_case("tuto4")]
#[test_case("tuto4b")]
#[test_case("tuto4c")]
#[test_case("tuto4d")]
#[test_case("tuto5")]
#[test_case("tuto5b")]
#[test_case("tuto6")]
#[test_case("tuto6a")]
#[test_case("tuto8")]
#[test_case("tuto9")]
#[test_case("tuto15")]
fn generate(name: &str) -> Result<(), Box<dyn std::error::Error>> {
    let base_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = base_dir.join("tests").join("fixtures");

    let mut rng_path = fixtures_dir.join(format!("{}.rnc", name));
    if !rng_path.exists() {
        rng_path = fixtures_dir.join(format!("{}.rng", name));
    }

    let mut project = Project::new(rng_path.clone());
    project.build()?;

    let pattern1 = fixtures_dir.join(format!("{}.*.xml", name));
    let pattern2 = fixtures_dir.join(format!("{}.xml", name));

    for xml_entry in glob(pattern1.to_str().unwrap())
        .unwrap()
        .chain(glob(pattern2.to_str().unwrap()).unwrap())
    {
        let xml_path = xml_entry?;
        let xml_name = xml_path.file_stem().unwrap().to_str().unwrap();
        let output = project.run(xml_path.clone())?;
        if xml_name.ends_with(".ko") {
            assert!(!output.status.success());
        } else {
            assert!(output.status.success());
            assert_snapshot!(
                xml_name,
                String::from_utf8_lossy(&output.stdout).to_string()
            );
        }
    }

    Ok(())
}

struct Project {
    path: PathBuf,
    temp_dir: tempfile::TempDir,
}

impl Project {
    fn new(path: PathBuf) -> Self {
        let temp_dir = tempfile::tempdir().unwrap();
        Self { path, temp_dir }
    }

    fn build(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let out_path = self.temp_dir.path();

        let mut cmd = Command::cargo_bin("gen")?;
        cmd.arg(&self.path)
            .arg(out_path)
            .arg("--test")
            .assert()
            .success();

        let mut cargo_cmd = Command::new("cargo");
        let output = cargo_cmd
            .current_dir(out_path)
            .arg("build")
            .arg("--offline")
            .output()?;
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
        let output = cargo_cmd
            .current_dir(out_path)
            .arg("run")
            .arg(xml)
            .output()?;

        Ok(output)
    }
}
