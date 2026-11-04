//! Validation and deterministic reconstruction for TrafficMSD model format v1.
//!
//! JSON Schema validates document shape. This module validates semantic
//! invariants that require comparing fields and reconstructs the represented
//! sample vector. It does not assess reconstruction error against observations.

use serde_json::Value;
use std::collections::HashSet;

/// Validate cross-field invariants of a TrafficMSD model v1 document.
///
/// Returns all detected semantic errors. Call a JSON Schema validator as well
/// when accepting untrusted serialized documents; this function focuses on
/// invariants that JSON Schema cannot conveniently express.
pub fn validate_model_v1(model: &Value) -> Result<(), Vec<String>> {
    let mut errors = Vec::new();
    let Some(root) = model.as_object() else {
        return Err(vec!["model must be a JSON object".to_owned()]);
    };

    if root.get("schema_version").and_then(Value::as_str) != Some("1.0") {
        errors.push("schema_version must be \"1.0\"".to_owned());
    }
    if root.get("model_kind").and_then(Value::as_str) != Some("multiscale_reconstruction") {
        errors.push("model_kind must be \"multiscale_reconstruction\"".to_owned());
    }

    let coverage = root.get("coverage").and_then(Value::as_object);
    let (observed_count, decomposition_count, padding_count) = match coverage {
        Some(c) => (
            c.get("observed_sample_count").and_then(Value::as_u64),
            c.get("decomposition_sample_count").and_then(Value::as_u64),
            c.get("padding_sample_count").and_then(Value::as_u64),
        ),
        None => {
            errors.push("coverage must be an object".to_owned());
            (None, None, None)
        }
    };

    let (observed, extent) = match (observed_count, decomposition_count) {
        (Some(o), Some(n)) => {
            if o < 16 {
                errors.push("observed_sample_count must be at least 16".to_owned());
            }
            if n < 16 || !n.is_power_of_two() {
                errors.push("decomposition_sample_count must be a power of two >= 16".to_owned());
            }
            if o > n {
                errors.push(
                    "observed_sample_count cannot exceed decomposition_sample_count".to_owned(),
                );
            }
            (Some(o), Some(n))
        }
        _ => {
            errors.push("coverage sample counts must be non-negative integers".to_owned());
            (None, None)
        }
    };

    if let (Some(o), Some(n), Some(p)) = (observed, extent, padding_count) {
        if n.checked_sub(o) != Some(p) {
            errors.push(
                "padding_sample_count must equal decomposition_sample_count - observed_sample_count"
                    .to_owned(),
            );
        }
    } else if padding_count.is_none() {
        errors.push("padding_sample_count must be a non-negative integer".to_owned());
    }

    let Some(n) = extent else {
        return Err(errors);
    };
    if n < 16 || !n.is_power_of_two() {
        return Err(errors);
    }

    let components = match root.get("components").and_then(Value::as_array) {
        Some(items) => items,
        None => {
            errors.push("components must be an array".to_owned());
            return Err(errors);
        }
    };

    let max_scale = n.trailing_zeros() as u64;
    let mut seen = HashSet::new();
    let mut residual_seen = HashSet::new();

    for (index, component) in components.iter().enumerate() {
        let Some(c) = component.as_object() else {
            errors.push(format!("components[{index}] must be an object"));
            continue;
        };
        let scale = c.get("scale_index").and_then(Value::as_u64);
        let start = c.get("window_start_sample").and_then(Value::as_u64);
        let window = c.get("window_length_samples").and_then(Value::as_u64);
        let emission = c.get("emission_sample_index").and_then(Value::as_u64);
        let amplitude = c.get("amplitude").and_then(Value::as_f64);

        let (Some(scale), Some(start), Some(window), Some(emission), Some(amplitude)) =
            (scale, start, window, emission, amplitude)
        else {
            errors.push(format!("components[{index}] has a missing or invalid numeric field"));
            continue;
        };

        if !amplitude.is_finite() || amplitude < 0.0 {
            errors.push(format!("components[{index}].amplitude must be finite and non-negative"));
        }
        if scale > max_scale {
            errors.push(format!(
                "components[{index}].scale_index exceeds the model's maximum scale"
            ));
            continue;
        }
        if start >= n || window == 0 || start.checked_add(window).map_or(true, |end| end > n) {
            errors.push(format!(
                "components[{index}] window lies outside the decomposition extent"
            ));
            continue;
        }
        if !seen.insert((scale, start)) {
            errors.push(format!("components[{index}] duplicates a scale/window component"));
        }

        if scale == 0 {
            if window != 1 || emission != start {
                errors.push(format!(
                    "components[{index}] scale-zero component must use a one-sample window and emit at its start"
                ));
            } else {
                residual_seen.insert(start);
            }
        } else {
            let expected_window = 1_u64 << scale;
            let expected_emission = start + expected_window / 2 - 1;
            if window != expected_window {
                errors.push(format!(
                    "components[{index}] window_length_samples must equal 2^scale_index"
                ));
            }
            if start % expected_window != 0 {
                errors.push(format!(
                    "components[{index}] window_start_sample is not globally aligned"
                ));
            }
            if emission != expected_emission {
                errors.push(format!(
                    "components[{index}] emission_sample_index must be the globally aligned center-left sample"
                ));
            }
        }
    }

    if residual_seen.len() as u64 != n {
        errors.push(
            "scale-zero residual must contain exactly one component for every sample index"
                .to_owned(),
        );
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Reconstruct the deterministic sample vector represented by a valid v1 model.
///
/// Each component contributes its amplitude at its declared emission index.
/// Interval optimization during extraction can be lossy, so the result is not
/// guaranteed to equal the original observation.
pub fn reconstruct_model_v1(model: &Value) -> Result<Vec<f64>, Vec<String>> {
    validate_model_v1(model)?;
    let root = model.as_object().expect("validated object");
    let n = root["coverage"]["decomposition_sample_count"]
        .as_u64()
        .expect("validated sample count");
    let Ok(n) = usize::try_from(n) else {
        return Err(vec![
            "decomposition_sample_count exceeds this platform's addressable size".to_owned(),
        ]);
    };
    let mut output = Vec::new();
    if output.try_reserve_exact(n).is_err() {
        return Err(vec!["decomposition_sample_count is too large to allocate".to_owned()]);
    }
    output.resize(n, 0.0_f64);

    for component in root["components"].as_array().expect("validated components") {
        let emission = component["emission_sample_index"]
            .as_u64()
            .expect("validated emission index") as usize;
        let amplitude = component["amplitude"]
            .as_f64()
            .expect("validated amplitude");
        output[emission] += amplitude;
        if !output[emission].is_finite() {
            return Err(vec![format!("reconstructed value at sample {emission} is not finite")]);
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::{reconstruct_model_v1, validate_model_v1};
    use serde_json::{json, Value};

    fn fixture() -> Value {
        serde_json::from_str(include_str!(
            "../../../examples/models/v1/synthetic-multiscale-16.json"
        ))
        .expect("the checked-in model v1 fixture must be valid JSON")
    }

    #[test]
    fn accepts_aligned_multiscale_model_with_complete_residual() {
        assert_eq!(validate_model_v1(&fixture()), Ok(()));
    }

    #[test]
    fn reconstructs_residual_and_multiscale_emissions() {
        let reconstructed = reconstruct_model_v1(&fixture()).unwrap();
        assert_eq!(reconstructed.len(), 16);
        assert_eq!(reconstructed[1], 10.0);
        assert_eq!(reconstructed.iter().sum::<f64>(), 10.0);
    }

    #[test]
    fn rejects_wrong_midpoint_alignment() {
        let mut model = fixture();
        model["components"][16]["emission_sample_index"] = json!(2);
        let errors = validate_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("center-left")));
    }

    #[test]
    fn rejects_missing_residual_sample() {
        let mut model = fixture();
        model["components"].as_array_mut().unwrap().remove(0);
        let errors = validate_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("every sample index")));
    }

    #[test]
    fn rejects_non_power_of_two_extent() {
        let mut model = fixture();
        model["coverage"]["decomposition_sample_count"] = json!(24);
        model["coverage"]["padding_sample_count"] = json!(8);
        let errors = validate_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("power of two")));
    }

    #[test]
    fn rejects_misaligned_window_start() {
        let mut model = fixture();
        model["components"][16]["window_start_sample"] = json!(1);
        model["components"][16]["emission_sample_index"] = json!(2);
        let errors = validate_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("globally aligned")));
    }

    #[test]
    fn rejects_inconsistent_padding() {
        let mut model = fixture();
        model["coverage"]["padding_sample_count"] = json!(1);
        let errors = validate_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("padding_sample_count")));
    }

    #[test]
    fn rejects_duplicate_scale_window_component() {
        let mut model = fixture();
        let duplicate = model["components"][16].clone();
        model["components"].as_array_mut().unwrap().push(duplicate);
        let errors = validate_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("duplicates a scale/window")));
    }

    #[test]
    fn rejects_negative_amplitude() {
        let mut model = fixture();
        model["components"][0]["amplitude"] = json!(-1.0);
        let errors = validate_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("finite and non-negative")));
    }

    #[test]
    fn rejects_non_finite_reconstructed_sum() {
        let mut model = fixture();
        model["components"][1]["amplitude"] = json!(f64::MAX);
        model["components"][16]["amplitude"] = json!(f64::MAX);
        let errors = reconstruct_model_v1(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("not finite")));
    }
}
