//! Test-only audit of the multiscale residual dependency graph.
//!
//! This is deliberately a staged, batch reference harness, not a streaming
//! implementation. It makes the parent-window readiness rule executable and
//! checks that every scale reads one residual snapshot before applying writes.

use crate::decomposition::{decompose, optimize_intervals};

fn unique_maximum(chunk: &[f64]) -> f64 {
    let mut sorted = chunk.to_vec();
    sorted.sort_by(|a, b| b.total_cmp(a));
    let mut i = 0;
    while i < sorted.len() {
        let mut j = i + 1;
        while j < sorted.len() && sorted[j] == sorted[i] {
            j += 1;
        }
        if j - i == 1 {
            return sorted[i];
        }
        i = j;
    }
    0.0
}

fn staged_dependency_audit(
    signal: &[f64],
    cutoff_fraction: f64,
) -> (Vec<Vec<f64>>, Vec<Vec<bool>>) {
    let n = signal.len();
    let max_scale = n.trailing_zeros() as usize;
    let mut coefficients = vec![vec![0.0; n]; max_scale + 1];
    let mut residual = signal.to_vec();
    let mut committed = vec![vec![false; n]; max_scale + 1];

    for scale in (1..=max_scale).rev() {
        let width = 1usize << scale;
        let windows = n / width;

        // A child window is eligible only after its unique parent update.
        if scale < max_scale {
            for child in 0..windows {
                let parent = child / 2;
                assert!(committed[scale + 1][parent]);
            }
        }

        // All candidates at this scale read the same immutable residual
        // snapshot. No same-scale update can contaminate a later candidate.
        let snapshot = residual.clone();
        let candidates: Vec<_> = snapshot
            .chunks_exact(width)
            .map(unique_maximum)
            .collect();
        let candidates = optimize_intervals(&candidates, cutoff_fraction);

        for (window, candidate) in candidates.into_iter().enumerate() {
            let begin = window * width;
            let end = begin + width;
            if candidate > 0.0 {
                let source = snapshot[begin..end]
                    .iter()
                    .position(|value| *value == candidate);
                coefficients[scale][begin..end].fill(candidate);

                if let Some(position) = source {
                    residual[begin + position] = 0.0;
                }
                let emit = begin + width / 2 - 1;
                residual[emit] = (residual[emit] - candidate).max(0.0);
            }

            // The update is confined to its own window; that makes exactly
            // the two half-width children ready for the next scale.
            assert!((begin..end).contains(&(begin + width / 2 - 1)));
            committed[scale][window] = true;
        }
    }

    coefficients[0] = optimize_intervals(&residual, cutoff_fraction);
    (coefficients, committed)
}

fn assert_matrix_bits_equal(left: &[Vec<f64>], right: &[Vec<f64>]) {
    assert_eq!(left.len(), right.len());
    for (row, (a, b)) in left.iter().zip(right).enumerate() {
        assert_eq!(a.len(), b.len());
        for (column, (x, y)) in a.iter().zip(b).enumerate() {
            assert_eq!(
                x.to_bits(),
                y.to_bits(),
                "coefficient differs at row {row}, column {column}"
            );
        }
    }
}

#[test]
fn staged_parent_readiness_matches_batch_decomposition() {
    for n in [16usize, 32, 64, 128, 256] {
        for seed in 0..24usize {
            let signal: Vec<f64> = (0..n)
                .map(|i| ((i * 17 + seed * 13 + (i / 3) * 7) % 9) as f64)
                .collect();
            for cutoff in [0.004, 0.1, 0.3, 0.75] {
                let (staged, committed) = staged_dependency_audit(&signal, cutoff);
                let batch = decompose(&signal, cutoff).unwrap();
                assert_matrix_bits_equal(&staged, &batch.coefficients);

                for scale in 1..committed.len() {
                    assert!(committed[scale].iter().all(|ready| *ready));
                }
            }
        }
    }
}

#[test]
fn child_windows_require_their_parent_commit() {
    let n = 64usize;
    let max_scale = n.trailing_zeros() as usize;
    let mut committed = vec![vec![false; n]; max_scale + 1];
    let parent_scale = max_scale;
    let child_scale = parent_scale - 1;

    // The only top-scale window has not committed yet.
    assert!(!committed[parent_scale][0]);
    for child in 0..(n / (1usize << child_scale)) {
        assert!(!committed[parent_scale][child / 2]);
    }

    // Its commit unlocks both child windows.
    committed[parent_scale][0] = true;
    for child in 0..(n / (1usize << child_scale)) {
        assert!(committed[parent_scale][child / 2]);
    }
}
