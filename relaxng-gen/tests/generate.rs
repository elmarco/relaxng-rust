use assert_cmd::prelude::*;
use insta::assert_snapshot;
use std::path::{Path, PathBuf};
use std::process::{Command, exit};

#[test]
fn simple() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("simple.rnc")?;
    let xml = locate_test_file("simple.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto1() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto1.rng")?;
    let xml = locate_test_file("tuto1.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto1b() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto1b.rng")?;
    let xml = locate_test_file("tuto1.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto1c() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto1c.rnc")?;
    let xml = locate_test_file("tuto1c.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto2() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto2.rnc")?;
    let xml = locate_test_file("tuto2.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto3() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto3.rnc")?;
    let xml = locate_test_file("tuto3.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto3b() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto3b.rnc")?;
    let xml = locate_test_file("tuto3b.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto4() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto4.rnc")?;
    let xml = locate_test_file("tuto1.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto4b() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto4b.rnc")?;
    let xml = locate_test_file("tuto4b.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto5() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto5.rnc")?;
    let xml = locate_test_file("tuto5.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto6() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto6.rnc")?;
    let xml = locate_test_file("tuto6.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto6a() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto6a.rnc")?;
    let xml = locate_test_file("tuto6a.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}

#[test]
fn tuto15() -> Result<(), Box<dyn std::error::Error>> {
    let rng = locate_test_file("tuto15.rnc")?;
    let xml = locate_test_file("tuto15.xml")?;

    let output = test(rng, xml)?;
    assert_snapshot!(output);

    Ok(())
}
fn test(rng: PathBuf, xml: PathBuf) -> Result<String, Box<dyn std::error::Error>> {
    let temp_dir = tempfile::tempdir()?;
    let out_path = temp_dir.path();

    let mut cmd = Command::cargo_bin("gen")?;
    cmd.arg(rng).arg(out_path).arg("--test").assert().success();

    let mut cargo_cmd = Command::new("cargo");
    let output = cargo_cmd
        .current_dir(out_path)
        .arg("build")
        .arg("--offline")
        .output()?;
    eprint!("{}", String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        let path = temp_dir.into_path();
        eprintln!("Build failed: {}", path.display());
        exit(1);
    }

    let mut cargo_cmd = Command::new("cargo");
    let output = cargo_cmd
        .current_dir(out_path)
        .arg("run")
        .arg(xml)
        .output()?;
    if !output.status.success() {
        let path = temp_dir.into_path();
        eprintln!("Run failed: {}", path.display());
        exit(1);
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

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
