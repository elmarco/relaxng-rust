use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use assert_cmd::prelude::*;
use glob::glob;
use insta::assert_snapshot;

fn collect_rs_files(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        let mut entries: Vec<_> = entries.filter_map(|e| e.ok()).collect();
        entries.sort_by_key(|e| e.file_name());
        for entry in entries {
            let path = entry.path();
            if path.is_dir() {
                collect_rs_files(&path, files);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                files.push(path);
            }
        }
    }
}

#[test]
fn build_all_fixtures() {
    let base_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = base_dir.join("tests").join("fixtures");

    let temp_dir = tempfile::tempdir().unwrap();
    let workspace_dir = temp_dir.path();

    // Collect all fixture files
    let pattern = fixtures_dir.join("**/*.rnc");
    let rnc_files = glob(pattern.to_str().unwrap()).unwrap();
    let pattern = fixtures_dir.join("**/*.rng");
    let rng_files = glob(pattern.to_str().unwrap()).unwrap();

    let mut fixture_files: Vec<_> = rnc_files.chain(rng_files).filter_map(|e| e.ok()).collect();
    fixture_files.sort();

    let mut members = Vec::new();

    for rng_path in fixture_files {
        let name = rng_path.file_stem().unwrap().to_str().unwrap();

        // Create a unique project name based on path
        let relative = rng_path.strip_prefix(&fixtures_dir).unwrap();
        let project_name = if let Some(parent) = relative.parent() {
            if parent.as_os_str().is_empty() {
                format!("test-{name}")
            } else {
                let parent_str = parent.to_str().unwrap().replace('/', "-");
                format!("test-{parent_str}-{name}")
            }
        } else {
            format!("test-{name}")
        };

        let project_dir = workspace_dir.join(&project_name);

        // Generate the project
        let mut cmd = Command::cargo_bin("relaxng-gen").unwrap();
        let output = cmd
            .arg(&rng_path)
            .arg(&project_dir)
            .arg("--with-test")
            .output()
            .expect("failed to run relaxng-gen");

        if !output.status.success() {
            eprintln!(
                "relaxng-gen failed for {}: {}",
                rng_path.display(),
                String::from_utf8_lossy(&output.stderr)
            );
            continue;
        }

        // Only add if Cargo.toml was created
        if project_dir.join("Cargo.toml").exists() {
            members.push(project_name);
        }
    }

    // Sort members for deterministic output
    members.sort();

    // Create workspace Cargo.toml
    let workspace_toml = format!(
        r#"[workspace]
resolver = "2"
members = [
{}
]
"#,
        members.iter().map(|m| format!("    \"{m}\",")).collect::<Vec<_>>().join("\n")
    );
    fs::write(workspace_dir.join("Cargo.toml"), &workspace_toml).unwrap();

    // Build the entire workspace
    let output = Command::new("cargo")
        .current_dir(workspace_dir)
        .arg("build")
        .arg("--offline")
        .output()
        .expect("failed to run cargo build");

    if !output.status.success() {
        eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        let path = temp_dir.keep();
        panic!("Workspace build failed, kept at: {}", path.display());
    }

    eprintln!("Successfully built {} projects", members.len());

    // Snapshot all generated source files (one snapshot per fixture)
    for member in &members {
        let src_dir = workspace_dir.join(member).join("src");
        let mut rs_files = Vec::new();
        collect_rs_files(&src_dir, &mut rs_files);

        let mut combined = String::new();
        for path in rs_files {
            let relative = path.strip_prefix(&src_dir).unwrap();
            let filename = relative.to_str().unwrap();
            let content = fs::read_to_string(&path).unwrap();
            combined.push_str(&format!("--- {filename} ---\n{content}\n"));
        }
        assert_snapshot!(member.clone(), combined);
    }

    if std::env::var("TEST_KEEP").is_ok() {
        let path = temp_dir.keep();
        eprintln!("Kept workspace at: {}", path.display());
    }
}
