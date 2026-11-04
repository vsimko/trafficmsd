use std::fs;
use std::process::{Command, Output};

fn run(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_trafficmsd"))
        .args(args)
        .output()
        .expect("CLI process should start")
}

fn config(command: &str, input: &str, output: Option<&str>, cutoff: Option<f64>) -> String {
    let output_field = output
        .map(|path| format!(", \"output_csv\": \"{path}\""))
        .unwrap_or_default();
    let cutoff_field = cutoff
        .map(|value| format!("\"cutoff_fraction\": {value}"))
        .unwrap_or_default();
    format!(
        r#"{{
          "schema_version": 1,
          "command": "{command}",
          "input_csv": "{input}"{output_field},
          "signal": {{
            "sampling_interval": 1,
            "time_unit": "s",
            "value_unit": "bytes"
          }},
          "parameters": {{{cutoff_field}}}
        }}"#
    )
}

#[test]
fn help_and_version_are_available() {
    assert!(run(&["--help"]).status.success());
    assert!(run(&["--version"]).status.success());
}

#[test]
fn decompose_and_reconstruct_work_as_external_commands() {
    let directory = tempfile::tempdir().unwrap();
    let input = directory.path().join("input signal.csv");
    let decompose_config = directory.path().join("decompose.json");
    let coefficients = directory.path().join("coefficients.csv");
    let reconstruct_config = directory.path().join("reconstruct.json");
    let reconstructed = directory.path().join("reconstructed.csv");

    let mut csv = String::from("sample_index,value\n");
    for index in 0..16 {
        csv.push_str(&format!("{index},{}\n", index + 1));
    }
    fs::write(&input, csv).unwrap();
    fs::write(
        &decompose_config,
        config(
            "decompose",
            "input signal.csv",
            Some("coefficients.csv"),
            Some(0.004),
        ),
    )
    .unwrap();

    let result = run(&[
        "decompose",
        "--config",
        decompose_config.to_str().unwrap(),
    ]);
    assert!(
        result.status.success(),
        "decompose failed: {}",
        String::from_utf8_lossy(&result.stderr)
    );
    let coefficient_text = fs::read_to_string(&coefficients).unwrap();
    assert!(coefficient_text.starts_with("scale,sample_index,coefficient"));

    fs::write(
        &reconstruct_config,
        config(
            "reconstruct",
            "coefficients.csv",
            Some("reconstructed.csv"),
            None,
        ),
    )
    .unwrap();
    let result = run(&[
        "reconstruct",
        "--config",
        reconstruct_config.to_str().unwrap(),
    ]);
    assert!(
        result.status.success(),
        "reconstruct failed: {}",
        String::from_utf8_lossy(&result.stderr)
    );

    let reconstructed_text = fs::read_to_string(&reconstructed).unwrap();
    assert!(reconstructed_text.starts_with("sample_index,value"));
    assert_eq!(reconstructed_text.lines().count(), 17);
}

#[test]
fn rejects_unsupported_schema_before_reading_input() {
    let directory = tempfile::tempdir().unwrap();
    let config_path = directory.path().join("invalid.json");
    let mut invalid = config("validate", "missing.csv", None, None);
    invalid = invalid.replace("\"schema_version\": 1", "\"schema_version\": 2");
    fs::write(&config_path, invalid).unwrap();

    let result = run(&[
        "validate",
        "--config",
        config_path.to_str().unwrap(),
    ]);
    assert_eq!(result.status.code(), Some(3));
    assert!(String::from_utf8_lossy(&result.stderr).contains("unsupported schema_version"));
}

#[test]
fn refuses_to_overwrite_without_force() {
    let directory = tempfile::tempdir().unwrap();
    let input = directory.path().join("input.csv");
    let config_path = directory.path().join("config.json");
    let output = directory.path().join("output.csv");

    let mut csv = String::from("sample_index,value\n");
    for index in 0..16 {
        csv.push_str(&format!("{index},{}\n", index + 1));
    }
    fs::write(&input, csv).unwrap();
    fs::write(&output, "keep-existing-content").unwrap();
    fs::write(
        &config_path,
        config("decompose", "input.csv", Some("output.csv"), Some(0.004)),
    )
    .unwrap();

    let result = run(&[
        "decompose",
        "--config",
        config_path.to_str().unwrap(),
    ]);
    assert_eq!(result.status.code(), Some(4));
    assert_eq!(fs::read_to_string(output).unwrap(), "keep-existing-content");
}
