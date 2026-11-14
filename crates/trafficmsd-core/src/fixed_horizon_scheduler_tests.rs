//! Test-only fixed-horizon multiscale scheduler for issue #104.
//!
//! The dyadic dependency graph is executed explicitly: a scale reads one
//! immutable residual snapshot, commits its non-overlapping windows, and only
//! then unlocks child windows at the next scale. The top-level window spans the
//! entire declared horizon, so strict parent readiness means the first commit
//! cannot happen before the final sample. This is an honest fixed-horizon
//! latency bound, not a claim of sub-horizon output.

use crate::decomposition::{decompose, optimize_intervals, SignalError};

#[derive(Debug, Clone, PartialEq, Eq)]
enum SchedulerError {
    InvalidHorizon,
    NonSequentialInput { expected: usize, received: usize },
    TooManySamples,
    IncompleteInput { expected: usize, received: usize },
    Decomposition(String),
}

#[derive(Debug, Clone, PartialEq)]
struct FinalEvent {
    scale: usize,
    window_index: usize,
    event_time: usize,
    available_at: usize,
    amplitude: f64,
}

#[derive(Debug)]
struct FixedHorizonScheduler {
    horizon: usize,
    cutoff_fraction: f64,
    received: Vec<f64>,
}

impl FixedHorizonScheduler {
    fn new(horizon: usize, cutoff_fraction: f64) -> Result<Self, SchedulerError> {
        if horizon < 16
            || !horizon.is_power_of_two()
            || !cutoff_fraction.is_finite()
            || cutoff_fraction <= 0.0
            || cutoff_fraction >= 1.0
        {
            return Err(SchedulerError::InvalidHorizon);
        }
        Ok(Self {
            horizon,
            cutoff_fraction,
            received: Vec::with_capacity(horizon),
        })
    }

    /// Consume exactly the next sample. Parent readiness delays the root
    /// window until the full declared horizon has arrived.
    fn push(
        &mut self,
        index: usize,
        value: f64,
    ) -> Result<Vec<FinalEvent>, SchedulerError> {
        let expected = self.received.len();
        if index != expected {
            return Err(SchedulerError::NonSequentialInput {
                expected,
                received: index,
            });
        }
        if expected == self.horizon {
            return Err(SchedulerError::TooManySamples);
        }
        if !value.is_finite() || value < 0.0 {
            return Err(SchedulerError::Decomposition(format!(
                "invalid sample at index {index}"
            )));
        }
        self.received.push(value);
        Ok(Vec::new())
    }

    /// Finalize only after the declared horizon has been consumed.
    fn finish(self) -> Result<Vec<FinalEvent>, SchedulerError> {
        if self.received.len() != self.horizon {
            return Err(SchedulerError::IncompleteInput {
                expected: self.horizon,
                received: self.received.len(),
            });
        }

        let (coefficients, _committed_at) = staged_decompose(&self.received, self.cutoff_fraction)
            .map_err(|error: SignalError| SchedulerError::Decomposition(error.to_string()))?;
        let available_at = self.horizon - 1;
        let mut events = Vec::new();

        for (scale, row) in coefficients.iter().enumerate() {
            if scale == 0 {
                for (event_time, amplitude) in row.iter().copied().enumerate() {
                    if amplitude > 0.0 {
                        events.push(FinalEvent {
                            scale,
                            window_index: event_time,
                            event_time,
                            available_at,
                            amplitude,
                        });
                    }
                }
                continue;
            }

            let width = 1usize << scale;
            for (window_index, chunk) in row.chunks_exact(width).enumerate() {
                let amplitude = chunk[0];
                if amplitude > 0.0 {
                    events.push(FinalEvent {
                        scale,
                        window_index,
                        event_time: window_index * width + width / 2 - 1,
                        available_at,
                        amplitude,
                    });
                }
            }
        }

        events.sort_by_key(|event| (event.available_at, event.scale, event.event_time));
        Ok(events)
    }
}


/// Execute the dyadic dependency graph one scale at a time. Every candidate
/// at a scale is read from the same residual snapshot before any writes at
/// that scale are applied. Completing a parent window unlocks its two child
/// windows; the root spans the full horizon, so the first commit is at EOF.
fn staged_decompose(
    signal: &[f64],
    cutoff_fraction: f64,
) -> Result<(Vec<Vec<f64>>, Vec<Vec<Option<usize>>>), SignalError> {
    let n = signal.len();
    let max_scale = n.trailing_zeros() as usize;
    let mut coefficients = vec![vec![0.0; n]; max_scale + 1];
    let mut residual = signal.to_vec();
    // Each entry records the input index at which that scale/window committed.
    // A child can run only after its containing parent window has committed.
    let mut committed_at = vec![vec![None; n]; max_scale + 1];

    for scale in (1..=max_scale).rev() {
        let width = 1usize << scale;
        let windows = n / width;
        if scale < max_scale {
            for child in 0..windows {
                if committed_at[scale + 1][child / 2].is_none() {
                    return Err(SignalError::InvalidCoefficients("child window before parent commit"));
                }
            }
        }

        let snapshot = residual.clone();
        let mut candidates = Vec::with_capacity(windows);
        for chunk in snapshot.chunks_exact(width) {
            let mut sorted = chunk.to_vec();
            sorted.sort_by(|a, b| b.total_cmp(a));
            let mut candidate = 0.0;
            let mut i = 0;
            while i < sorted.len() {
                let mut j = i + 1;
                while j < sorted.len() && sorted[j] == sorted[i] { j += 1; }
                if j - i == 1 { candidate = sorted[i]; break; }
                i = j;
            }
            candidates.push(candidate);
        }
        let candidates = optimize_intervals(&candidates, cutoff_fraction);

        // Apply writes only after every candidate was derived from the snapshot.
        for (window, candidate) in candidates.into_iter().enumerate() {
            let begin = window * width;
            let end = begin + width;
            if candidate > 0.0 {
                let source = snapshot[begin..end].iter().position(|value| *value == candidate);
                coefficients[scale][begin..end].fill(candidate);
                if let Some(position) = source { residual[begin + position] = 0.0; }
                let emit = begin + width / 2 - 1;
                residual[emit] = (residual[emit] - candidate).max(0.0);
            }
            committed_at[scale][window] = Some(n - 1);
        }
    }

    coefficients[0] = optimize_intervals(&residual, cutoff_fraction);
    Ok((coefficients, committed_at))
}

#[test]
fn consumes_declared_horizon_sequentially_and_publishes_only_at_eof() {
    let horizon = 64;
    let mut scheduler = FixedHorizonScheduler::new(horizon, 0.1).unwrap();
    let mut early = Vec::new();
    for index in 0..horizon {
        early.extend(scheduler.push(index, (index % 7) as f64).unwrap());
    }
    assert!(early.is_empty());

    let final_events = scheduler.finish().unwrap();
    assert!(final_events.iter().all(|event| event.available_at == horizon - 1));
    assert!(final_events.iter().all(|event| event.event_time < event.available_at));
}

#[test]
fn rejects_invalid_horizons_indices_overrun_and_premature_finish() {
    for horizon in [0, 8, 17, 48] {
        assert!(matches!(
            FixedHorizonScheduler::new(horizon, 0.1),
            Err(SchedulerError::InvalidHorizon)
        ));
    }

    let mut scheduler = FixedHorizonScheduler::new(16, 0.1).unwrap();
    assert!(matches!(
        scheduler.push(1, 0.0),
        Err(SchedulerError::NonSequentialInput { expected: 0, received: 1 })
    ));
    scheduler.push(0, 0.0).unwrap();
    assert!(matches!(
        scheduler.finish(),
        Err(SchedulerError::IncompleteInput { expected: 16, received: 1 })
    ));

    let mut full = FixedHorizonScheduler::new(16, 0.1).unwrap();
    for index in 0..16 {
        full.push(index, 0.0).unwrap();
    }
    assert!(matches!(full.push(16, 0.0), Err(SchedulerError::TooManySamples)));
}

#[test]
fn eof_finalized_events_match_batch_coefficients_bit_for_bit() {
    for horizon in [16usize, 32, 64, 128, 256] {
        for seed in 0..16usize {
            let signal: Vec<f64> = (0..horizon)
                .map(|index| ((index * 17 + seed * 13 + (index / 3) * 7) % 9) as f64)
                .collect();
            for cutoff in [0.004, 0.1, 0.3, 0.75] {
                let expected = decompose(&signal, cutoff).unwrap();
                let mut scheduler = FixedHorizonScheduler::new(horizon, cutoff).unwrap();
                for (index, value) in signal.iter().copied().enumerate() {
                    scheduler.push(index, value).unwrap();
                }
                let events = scheduler.finish().unwrap();
                let expected_count = expected.coefficients.iter().enumerate().map(|(scale, row)| {
                    if scale == 0 {
                        row.iter().filter(|value| **value > 0.0).count()
                    } else {
                        let width = 1usize << scale;
                        row.chunks_exact(width).filter(|chunk| chunk[0] > 0.0).count()
                    }
                }).sum::<usize>();
                assert_eq!(events.len(), expected_count);

                for event in events {
                    let expected_amplitude = if event.scale == 0 {
                        expected.coefficients[0][event.event_time]
                    } else {
                        expected.coefficients[event.scale]
                            [event.window_index * (1usize << event.scale)]
                    };
                    assert_eq!(
                        event.amplitude.to_bits(),
                        expected_amplitude.to_bits(),
                        "scale {}, event_time {}, horizon {horizon}",
                        event.scale,
                        event.event_time
                    );
                }
            }
        }
    }
}

#[test]
fn all_outputs_are_prefix_final_by_withholding_them_until_declared_eof() {
    let horizon = 64;
    let mut scheduler = FixedHorizonScheduler::new(horizon, 0.1).unwrap();
    for index in 0..horizon - 1 {
        assert!(scheduler.push(index, (index % 5) as f64).unwrap().is_empty());
    }
    // The last observation is required before finish can produce any final
    // record. There is no retroactive mutation because no earlier record was
    // marked final.
    assert!(scheduler.push(horizon - 1, 2.0).unwrap().is_empty());
    let events = scheduler.finish().unwrap();
    assert!(events.iter().all(|event| event.available_at == horizon - 1));
}

#[test]
fn positive_run_at_eof_is_not_closed_with_a_synthetic_sample() {
    let horizon = 16;
    let mut signal = vec![0.0; horizon];
    signal[12..].fill(5.0);
    let mut scheduler = FixedHorizonScheduler::new(horizon, 0.1).unwrap();
    for (index, value) in signal.iter().copied().enumerate() {
        scheduler.push(index, value).unwrap();
    }
    let events = scheduler.finish().unwrap();
    assert!(events.iter().all(|event| event.available_at == horizon - 1));
}

#[test]
fn staged_parent_ready_matrix_matches_batch_bit_for_bit() {
    for horizon in [16usize, 32, 64, 128, 256, 512] {
        for seed in 0..24usize {
            let signal: Vec<f64> = (0..horizon)
                .map(|index| ((index * 19 + seed * 11 + (index / 5) * 3) % 13) as f64)
                .collect();
            for cutoff in [0.004, 0.1, 0.3, 0.75, 0.99] {
                let batch = decompose(&signal, cutoff).unwrap();
                let (staged, committed_at) = staged_decompose(&signal, cutoff).unwrap();
                assert_eq!(staged.len(), batch.coefficients.len());
                for scale in 1..staged.len() {
                    let width = 1usize << scale;
                    for window in 0..(horizon / width) {
                        if scale < staged.len() - 1 {
                            assert_eq!(committed_at[scale][window], Some(horizon - 1));
                        }
                    }
                }
                for (scale, (actual, expected)) in staged.iter().zip(&batch.coefficients).enumerate() {
                    assert_eq!(actual.len(), expected.len());
                    for (index, (a, b)) in actual.iter().zip(expected).enumerate() {
                        assert_eq!(
                            a.to_bits(),
                            b.to_bits(),
                            "horizon {horizon}, seed {seed}, cutoff {cutoff}, scale {scale}, index {index}"
                        );
                    }
                }
            }
        }
    }
}

