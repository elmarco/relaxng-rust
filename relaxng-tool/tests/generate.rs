use assert_cmd::prelude::*;
use insta::assert_snapshot;
use std::path::{Path, PathBuf};
use std::process::Command;

fn locate_test_file(filename: &str) -> std::io::Result<PathBuf> {
    let base_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let full_path = base_dir.join("tests").join("fixtures").join(filename);

    if !full_path.exists() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("Test file {} not found at {:?}", filename, full_path),
        ));
    }

    Ok(full_path)
}

#[test]
fn tuto1() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("rng")?;
    let rng = locate_test_file("tuto1.rng")?;
    let xml = locate_test_file("tuto1.xml")?;

    let temp_dir = tempfile::tempdir()?;
    let out_path = temp_dir.path();

    cmd.arg("generate")
        .arg(rng)
        .arg(out_path)
        .arg("--test")
        .assert()
        .success();

    let mut cargo_cmd = Command::new("cargo");
    cargo_cmd
        .current_dir(out_path)
        .arg("build")
        .assert()
        .success();

    let mut cargo_cmd = Command::new("cargo");
    let output = cargo_cmd
        .current_dir(out_path)
        .arg("run")
        .arg(xml)
        .output()?;

    assert!(output.status.success(), "Command failed: {:?}", output);
    let output = String::from_utf8_lossy(&output.stdout);
    assert_snapshot!(output);

    Ok(())
}
