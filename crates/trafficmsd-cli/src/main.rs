//! Standalone, file-based command-line interface for TrafficMSD.

use serde::Deserialize;
use std::env;
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

#[derive(Debug)]
struct CliError {
    code: u8,
    message: String,
}

impl CliError {
    fn usage(message: impl Into<String>) -> Self {
        Self {
            code: 2,
            message: message.into(),
        }
    }

    fn invalid(message: impl Into<String>) -> Self {
        Self {
            code: 3,
            message: message.into(),
        }
    }

    fn io(message: impl Into<String>) -> Self {
        Self {
            code: 4,
            message: message.into(),
        }
    }

    fn algorithm(message: impl Into<String>) -> Self {
        Self {
            code: 5,
            message: message.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Operation {
    Validate,
    Decompose,
    Reconstruct,
}

impl Operation {
    fn parse(value: &str) -> Result<Self, CliError> {
        match value {
            "validate" => Ok(Self::Validate),
            "decompose" => Ok(Self::Decompose),
            "reconstruct" => Ok(Self::Reconstruct),
            _ => Err(CliError::usage(format!("unsupported command: {value}"))),
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::Validate => "validate",
            Self::Decompose => "decompose",
            Self::Reconstruct => "reconstruct",
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Config {
    schema_version: u32,
    command: String,
    input_csv: String,
    output_csv: Option<String>,
    signal: SignalConfig,
    parameters: Parameters,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct SignalConfig {
    sampling_interval: f64,
    time_unit: String,
    value_unit: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Parameters {
    cutoff_fraction: Option<f64>,
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("error: {}", error.message);
            ExitCode::from(error.code)
        }
    }
}

fn run() -> Result<(), CliError> {
    let mut args = env::args().skip(1);
    let Some(first) = args.next() else {
        print_help();
        return Ok(());
    };

    if first == "--help" || first == "-h" {
        print_help();
        return Ok(());
    }
    if first == "--version" || first == "-V" {
        println!("trafficmsd {}", trafficmsd_core::version());
        return Ok(());
    }

    let operation = Operation::parse(&first)?;
    let mut config_path: Option<PathBuf> = None;
    let mut force = false;

    while let Some(argument) = args.next() {
        match argument.as_str() {
            "--config" => {
                if config_path.is_some() {
                    return Err(CliError::usage("--config may be supplied only once"));
                }
                let Some(path) = args.next() else {
                    return Err(CliError::usage("--config requires a file path"));
                };
                config_path = Some(PathBuf::from(path));
            }
            "--force" => force = true,
            "--help" | "-h" => {
                print_help();
                return Ok(());
            }
            other => return Err(CliError::usage(format!("unknown option: {other}"))),
        }
    }

    let config_path =
        config_path.ok_or_else(|| CliError::usage("missing required --config path"))?;
    let config_text = fs::read_to_string(&config_path)
        .map_err(|error| CliError::io(format!("cannot read config {}: {error}", config_path.display())))?;
    let config: Config = serde_json::from_str(&config_text)
        .map_err(|error| CliError::invalid(format!("invalid JSON configuration: {error}")))?;
    validate_config(&config, operation)?;

    let config_dir = config_path
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let input_path = resolve_path(config_dir, &config.input_csv);

    match operation {
        Operation::Validate => {
            read_signal_csv(&input_path)?;
        }
        Operation::Decompose => {
            let signal = read_signal_csv(&input_path)?;
            let cutoff = config.parameters.cutoff_fraction.expect("validated cutoff");
            let decomposition = trafficmsd_core::decomposition::decompose(&signal, cutoff)
                .map_err(|error| CliError::invalid(format!("invalid signal for decomposition: {error}")))?;
            let output_path = resolve_path(
                config_dir,
                config.output_csv.as_deref().expect("validated output path"),
            );
            let bytes = encode_coefficients(&decomposition.coefficients)?;
            write_output(&output_path, &bytes, force)?;
        }
        Operation::Reconstruct => {
            let coefficients = read_coefficient_csv(&input_path)?;
            let signal = trafficmsd_core::decomposition::reconstruct(&coefficients)
                .map_err(|error| match error {
                    trafficmsd_core::decomposition::SignalError::NonFinite(index) => {
                        CliError::algorithm(format!(
                            "reconstruction produced a non-finite value at sample {index}"
                        ))
                    }
                    other => CliError::invalid(format!("invalid coefficient input: {other}")),
                })?;
            let output_path = resolve_path(
                config_dir,
                config.output_csv.as_deref().expect("validated output path"),
            );
            let bytes = encode_signal(&signal)?;
            write_output(&output_path, &bytes, force)?;
        }
    }

    Ok(())
}

fn validate_config(config: &Config, operation: Operation) -> Result<(), CliError> {
    if config.schema_version != 1 {
        return Err(CliError::invalid(format!(
            "unsupported schema_version {}; expected 1",
            config.schema_version
        )));
    }
    if config.command != operation.as_str() {
        return Err(CliError::invalid(format!(
            "configuration command {:?} does not match CLI command {:?}",
            config.command,
            operation.as_str()
        )));
    }
    if config.input_csv.trim().is_empty() {
        return Err(CliError::invalid("input_csv must be a non-empty path"));
    }
    if !config.signal.sampling_interval.is_finite() || config.signal.sampling_interval <= 0.0 {
        return Err(CliError::invalid(
            "signal.sampling_interval must be finite and positive",
        ));
    }
    if !matches!(config.signal.time_unit.as_str(), "s" | "ms" | "us" | "ns") {
        return Err(CliError::invalid(
            "signal.time_unit must be one of s, ms, us, or ns",
        ));
    }
    if config.signal.value_unit.trim().is_empty() {
        return Err(CliError::invalid("signal.value_unit must be non-empty"));
    }

    match operation {
        Operation::Validate => {
            if config.output_csv.is_some() {
                return Err(CliError::invalid(
                    "output_csv must be omitted for validate",
                ));
            }
            if config.parameters.cutoff_fraction.is_some() {
                return Err(CliError::invalid(
                    "parameters.cutoff_fraction is not used by validate",
                ));
            }
        }
        Operation::Decompose => {
            require_output(config)?;
            let cutoff = config
                .parameters
                .cutoff_fraction
                .ok_or_else(|| CliError::invalid("decompose requires parameters.cutoff_fraction"))?;
            if !cutoff.is_finite() || cutoff <= 0.0 || cutoff >= 1.0 {
                return Err(CliError::invalid(
                    "parameters.cutoff_fraction must be finite and between 0 and 1",
                ));
            }
        }
        Operation::Reconstruct => {
            require_output(config)?;
            if config.parameters.cutoff_fraction.is_some() {
                return Err(CliError::invalid(
                    "parameters.cutoff_fraction is not used by reconstruct",
                ));
            }
        }
    }

    Ok(())
}

fn require_output(config: &Config) -> Result<(), CliError> {
    match config.output_csv.as_deref() {
        Some(path) if !path.trim().is_empty() => Ok(()),
        _ => Err(CliError::invalid(
            "output_csv is required for this command and must be non-empty",
        )),
    }
}

fn resolve_path(base: &Path, value: &str) -> PathBuf {
    let path = Path::new(value);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        base.join(path)
    }
}

fn read_signal_csv(path: &Path) -> Result<Vec<f64>, CliError> {
    let file = File::open(path)
        .map_err(|error| CliError::io(format!("cannot open input {}: {error}", path.display())))?;
    let mut reader = csv::ReaderBuilder::new().has_headers(true).from_reader(file);
    let headers = reader
        .headers()
        .map_err(|error| CliError::invalid(format!("invalid signal CSV header: {error}")))?;
    if headers.len() != 2 || headers.get(0) != Some("sample_index") || headers.get(1) != Some("value") {
        return Err(CliError::invalid(
            "signal CSV header must be exactly sample_index,value",
        ));
    }

    let mut values = Vec::new();
    for (row, record) in reader.records().enumerate() {
        let record = record.map_err(|error| {
            CliError::invalid(format!("invalid signal CSV row {}: {error}", row + 2))
        })?;
        let index = parse_usize(record.get(0), "sample_index", row + 2)?;
        if index != values.len() {
            return Err(CliError::invalid(format!(
                "sample_index at CSV row {} must be {}; found {}",
                row + 2,
                values.len(),
                index
            )));
        }
        let value = parse_f64(record.get(1), "value", row + 2)?;
        values.push(value);
    }

    if values.is_empty() {
        return Err(CliError::invalid("signal CSV must contain at least one observation"));
    }
    Ok(values)
}

fn read_coefficient_csv(path: &Path) -> Result<Vec<Vec<f64>>, CliError> {
    let file = File::open(path)
        .map_err(|error| CliError::io(format!("cannot open input {}: {error}", path.display())))?;
    let mut reader = csv::ReaderBuilder::new().has_headers(true).from_reader(file);
    let headers = reader
        .headers()
        .map_err(|error| CliError::invalid(format!("invalid coefficient CSV header: {error}")))?;
    if headers.len() != 3
        || headers.get(0) != Some("scale")
        || headers.get(1) != Some("sample_index")
        || headers.get(2) != Some("coefficient")
    {
        return Err(CliError::invalid(
            "coefficient CSV header must be exactly scale,sample_index,coefficient",
        ));
    }

    let mut entries = Vec::new();
    let mut max_scale = 0usize;
    let mut max_sample = 0usize;
    let mut previous: Option<(usize, usize)> = None;
    for (row, record) in reader.records().enumerate() {
        let record = record.map_err(|error| {
            CliError::invalid(format!("invalid coefficient CSV row {}: {error}", row + 2))
        })?;
        let scale = parse_usize(record.get(0), "scale", row + 2)?;
        let sample = parse_usize(record.get(1), "sample_index", row + 2)?;
        let value = parse_f64(record.get(2), "coefficient", row + 2)?;
        let current = (scale, sample);
        if previous.is_some_and(|prior| current <= prior) {
            return Err(CliError::invalid(format!(
                "coefficient rows must be ordered by ascending scale and sample_index (CSV row {})",
                row + 2
            )));
        }
        previous = Some(current);
        if scale >= usize::BITS as usize {
            return Err(CliError::invalid(format!(
                "scale at CSV row {} exceeds the supported index range",
                row + 2
            )));
        }
        max_scale = max_scale.max(scale);
        max_sample = max_sample.max(sample);
        entries.push((scale, sample, value));
    }

    if entries.is_empty() {
        return Err(CliError::invalid("coefficient CSV must contain at least one row"));
    }

    let sample_count = max_sample
        .checked_add(1)
        .ok_or_else(|| CliError::invalid("sample_index range overflows"))?;
    let scale_count = max_scale
        .checked_add(1)
        .ok_or_else(|| CliError::invalid("scale range overflows"))?;
    let mut matrix = vec![vec![None; sample_count]; scale_count];

    for (scale, sample, value) in entries {
        if matrix[scale][sample].replace(value).is_some() {
            return Err(CliError::invalid(format!(
                "duplicate coefficient for scale {scale}, sample_index {sample}"
            )));
        }
    }

    let mut coefficients = Vec::with_capacity(scale_count);
    for (scale, row) in matrix.into_iter().enumerate() {
        let mut values = Vec::with_capacity(sample_count);
        for (sample, value) in row.into_iter().enumerate() {
            values.push(value.ok_or_else(|| {
                CliError::invalid(format!(
                    "missing coefficient for scale {scale}, sample_index {sample}"
                ))
            })?);
        }
        coefficients.push(values);
    }

    Ok(coefficients)
}

fn parse_usize(value: Option<&str>, field: &str, row: usize) -> Result<usize, CliError> {
    value
        .ok_or_else(|| CliError::invalid(format!("missing {field} at CSV row {row}")))?
        .parse::<usize>()
        .map_err(|_| CliError::invalid(format!("invalid {field} at CSV row {row}")))
}

fn parse_f64(value: Option<&str>, field: &str, row: usize) -> Result<f64, CliError> {
    let parsed = value
        .ok_or_else(|| CliError::invalid(format!("missing {field} at CSV row {row}")))?
        .parse::<f64>()
        .map_err(|_| CliError::invalid(format!("invalid {field} at CSV row {row}")))?;
    if !parsed.is_finite() {
        return Err(CliError::invalid(format!(
            "{field} at CSV row {row} must be finite"
        )));
    }
    Ok(parsed)
}

fn encode_signal(values: &[f64]) -> Result<Vec<u8>, CliError> {
    let mut writer = csv::WriterBuilder::new().from_writer(Vec::new());
    writer
        .write_record(["sample_index", "value"])
        .map_err(|error| CliError::io(format!("cannot encode signal CSV: {error}")))?;
    for (index, value) in values.iter().enumerate() {
        writer
            .write_record([index.to_string(), value.to_string()])
            .map_err(|error| CliError::io(format!("cannot encode signal CSV: {error}")))?;
    }
    writer
        .into_inner()
        .map_err(|error| CliError::io(format!("cannot finish signal CSV: {}", error.error())))
}

fn encode_coefficients(coefficients: &[Vec<f64>]) -> Result<Vec<u8>, CliError> {
    let mut writer = csv::WriterBuilder::new().from_writer(Vec::new());
    writer
        .write_record(["scale", "sample_index", "coefficient"])
        .map_err(|error| CliError::io(format!("cannot encode coefficient CSV: {error}")))?;
    for (scale, row) in coefficients.iter().enumerate() {
        for (sample, value) in row.iter().enumerate() {
            writer
                .write_record([scale.to_string(), sample.to_string(), value.to_string()])
                .map_err(|error| CliError::io(format!("cannot encode coefficient CSV: {error}")))?;
        }
    }
    writer
        .into_inner()
        .map_err(|error| CliError::io(format!("cannot finish coefficient CSV: {}", error.error())))
}

fn write_output(path: &Path, bytes: &[u8], force: bool) -> Result<(), CliError> {
    if path.exists() && !force {
        return Err(CliError::io(format!(
            "output {} already exists; pass --force to overwrite",
            path.display()
        )));
    }

    let parent = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let name = path
        .file_name()
        .ok_or_else(|| CliError::io("output path must name a file"))?
        .to_string_lossy();
    let temporary = parent.join(format!(".{name}.{}.tmp", std::process::id()));

    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)
        .map_err(|error| {
            CliError::io(format!(
                "cannot create temporary output {}: {error}",
                temporary.display()
            ))
        })?;
    if let Err(error) = file.write_all(bytes).and_then(|()| file.sync_all()) {
        let _ = fs::remove_file(&temporary);
        return Err(CliError::io(format!("cannot write output: {error}")));
    }
    drop(file);

    #[cfg(windows)]
    if force && path.exists() {
        fs::remove_file(path)
            .map_err(|error| CliError::io(format!("cannot replace output: {error}")))?;
    }

    if let Err(error) = fs::rename(&temporary, path) {
        let _ = fs::remove_file(&temporary);
        return Err(CliError::io(format!(
            "cannot move completed output into place: {error}"
        )));
    }
    Ok(())
}

fn print_help() {
    println!(
        "TrafficMSD {}\n\
         A network traffic analysis and simulation engine.\n\
         \n\
         Usage:\n\
         trafficmsd <command> --config path/to/config.json [--force]\n\
         \n\
         Commands:\n\
         validate      Validate a normalized signal CSV\n\
         decompose     Write multiscale coefficients as CSV\n\
         reconstruct   Reconstruct a signal from coefficient CSV\n\
         \n\
         Options:\n\
         -h, --help    Print help\n\
         -V, --version Print version\n\
         --config      Read a versioned JSON configuration\n\
         --force       Allow replacing an existing output file",
        trafficmsd_core::version()
    );
}

#[cfg(test)]
mod tests {
    use super::{encode_coefficients, encode_signal, parse_f64, parse_usize, validate_config, Config, Operation};

    #[test]
    fn csv_parsers_reject_invalid_values() {
        assert!(parse_usize(Some("x"), "sample_index", 2).is_err());
        assert!(parse_f64(Some("NaN"), "value", 2).is_err());
    }

    #[test]
    fn encoders_write_expected_headers() {
        let signal = encode_signal(&[1.0, 2.0]).unwrap();
        assert!(String::from_utf8(signal).unwrap().starts_with("sample_index,value"));

        let coefficients = encode_coefficients(&[vec![0.0, 1.0], vec![2.0, 2.0]]).unwrap();
        assert!(String::from_utf8(coefficients)
            .unwrap()
            .starts_with("scale,sample_index,coefficient"));
    }

    #[test]
    fn config_requires_valid_command_specific_fields() {
        let config = Config {
            schema_version: 1,
            command: "decompose".to_owned(),
            input_csv: "input.csv".to_owned(),
            output_csv: Some("output.csv".to_owned()),
            signal: super::SignalConfig {
                sampling_interval: 1.0,
                time_unit: "s".to_owned(),
                value_unit: "bytes".to_owned(),
            },
            parameters: super::Parameters {
                cutoff_fraction: Some(0.004),
            },
        };
        assert!(validate_config(&config, Operation::Decompose).is_ok());
        assert!(validate_config(&config, Operation::Validate).is_err());
    }
}
