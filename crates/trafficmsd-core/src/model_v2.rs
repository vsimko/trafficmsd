//! Validation and deterministic reconstruction for sparse TrafficMSD model v2.
//!
//! Missing scale-zero residual components mean an implicit zero residual.
//! Higher-scale components retain the v1 center-left emission semantics.

use serde_json::Value;
use std::collections::HashSet;

/// Validate semantic invariants of a sparse model v2 document.
pub fn validate_model_v2(model: &Value) -> Result<(), Vec<String>> {
    let mut errors = Vec::new();
    let Some(root) = model.as_object() else {
        return Err(vec!["model must be a JSON object".to_owned()]);
    };
    if root.get("schema_version").and_then(Value::as_str) != Some("2.0") {
        errors.push("schema_version must be \"2.0\"".to_owned());
    }
    if root.get("model_kind").and_then(Value::as_str) != Some("multiscale_reconstruction") {
        errors.push("model_kind must be \"multiscale_reconstruction\"".to_owned());
    }

    let coverage = root.get("coverage").and_then(Value::as_object);
    let (observed, extent, padding) = match coverage {
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
    let n = match (observed, extent, padding) {
        (Some(o), Some(n), Some(p)) => {
            if o < 16 || o > n {
                errors.push(
                    "observed_sample_count must be in [16, decomposition_sample_count]".to_owned(),
                );
            }
            if n < 16 || !n.is_power_of_two() {
                errors.push("decomposition_sample_count must be a power of two >= 16".to_owned());
            }
            if n.checked_sub(o) != Some(p) {
                errors.push("padding_sample_count must equal decomposition_sample_count - observed_sample_count".to_owned());
            }
            n
        }
        _ => {
            errors.push("coverage sample counts must be non-negative integers".to_owned());
            return Err(errors);
        }
    };
    if n < 16 || !n.is_power_of_two() {
        return Err(errors);
    }

    let Some(components) = root.get("components").and_then(Value::as_array) else {
        errors.push("components must be an array".to_owned());
        return Err(errors);
    };
    let max_scale = n.trailing_zeros() as u64;
    let mut seen = HashSet::new();

    for (i, item) in components.iter().enumerate() {
        let Some(c) = item.as_object() else {
            errors.push(format!("components[{i}] must be an object"));
            continue;
        };
        let (scale, start, window, emission, amplitude) = (
            c.get("scale_index").and_then(Value::as_u64),
            c.get("window_start_sample").and_then(Value::as_u64),
            c.get("window_length_samples").and_then(Value::as_u64),
            c.get("emission_sample_index").and_then(Value::as_u64),
            c.get("amplitude").and_then(Value::as_f64),
        );
        let (Some(scale), Some(start), Some(window), Some(emission), Some(amplitude)) =
            (scale, start, window, emission, amplitude)
        else {
            errors.push(format!(
                "components[{i}] has a missing or invalid numeric field"
            ));
            continue;
        };
        if !amplitude.is_finite() || amplitude < 0.0 {
            errors.push(format!(
                "components[{i}].amplitude must be finite and non-negative"
            ));
        }
        if amplitude == 0.0 {
            errors.push(format!(
                "components[{i}] zero-amplitude components must be omitted in sparse v2"
            ));
        }
        if scale > max_scale {
            errors.push(format!("components[{i}].scale_index exceeds maximum scale"));
            continue;
        }
        if start >= n || window == 0 || start.checked_add(window).map_or(true, |end| end > n) {
            errors.push(format!(
                "components[{i}] window lies outside the decomposition extent"
            ));
            continue;
        }
        if !seen.insert((scale, start)) {
            errors.push(format!(
                "components[{i}] duplicates a scale/window component"
            ));
        }
        if scale == 0 {
            if window != 1 || emission != start {
                errors.push(format!("components[{i}] scale-zero residual must use a one-sample window and emit at its start"));
            }
        } else {
            let expected_window = 1_u64 << scale;
            let expected_emission = start + expected_window / 2 - 1;
            if window != expected_window {
                errors.push(format!(
                    "components[{i}] window_length_samples must equal 2^scale_index"
                ));
            }
            if start % expected_window != 0 {
                errors.push(format!(
                    "components[{i}] window_start_sample is not globally aligned"
                ));
            }
            if emission != expected_emission {
                errors.push(format!(
                    "components[{i}] emission_sample_index must be the globally aligned center-left sample"
                ));
            }
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Reconstruct the deterministic signal; absent scale-zero entries contribute zero.
pub fn reconstruct_model_v2(model: &Value) -> Result<Vec<f64>, Vec<String>> {
    validate_model_v2(model)?;
    let n = model["coverage"]["decomposition_sample_count"]
        .as_u64()
        .expect("validated sample count") as usize;
    let mut output = vec![0.0_f64; n];
    for component in model["components"]
        .as_array()
        .expect("validated components")
    {
        let index = component["emission_sample_index"]
            .as_u64()
            .expect("validated emission") as usize;
        let amplitude = component["amplitude"]
            .as_f64()
            .expect("validated amplitude");
        output[index] += amplitude;
        if !output[index].is_finite() {
            return Err(vec![format!(
                "reconstructed value at sample {index} is not finite"
            )]);
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::{reconstruct_model_v2, validate_model_v2};
    use serde_json::{json, Value};

    fn fixture() -> Value {
        let mut model: Value = serde_json::from_str(include_str!(
            "../../../examples/models/v1/synthetic-multiscale-16.json"
        ))
        .unwrap();
        model["schema_version"] = json!("2.0");
        let items = model["components"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|c| c["scale_index"] != 0 || c["amplitude"] != 0)
            .cloned()
            .collect::<Vec<_>>();
        model["components"] = json!(items);
        model
    }

    #[test]
    fn accepts_sparse_residual_with_implicit_zeros() {
        assert_eq!(validate_model_v2(&fixture()), Ok(()));
        let reconstructed = reconstruct_model_v2(&fixture()).unwrap();
        assert_eq!(reconstructed.len(), 16);
        assert_eq!(reconstructed[1], 10.0);
        assert_eq!(reconstructed.iter().sum::<f64>(), 10.0);
    }

    #[test]
    fn rejects_duplicate_sparse_residual_index() {
        let mut model = fixture();
        let duplicate = model["components"][0].clone();
        model["components"].as_array_mut().unwrap().push(duplicate);
        let errors = validate_model_v2(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("duplicates")));
    }

    #[test]
    fn rejects_wrong_version() {
        let mut model = fixture();
        model["schema_version"] = json!("1.0");
        assert!(validate_model_v2(&model).is_err());
    }

    #[test]
    fn rejects_explicit_zero_residual_component() {
        let mut model = fixture();
        model["components"].as_array_mut().unwrap().push(json!({
            "scale_index": 0,
            "window_start_sample": 0,
            "window_length_samples": 1,
            "emission_sample_index": 0,
            "amplitude": 0
        }));
        let errors = validate_model_v2(&model).unwrap_err();
        assert!(errors.iter().any(|e| e.contains("zero-amplitude components")));
    }
}
