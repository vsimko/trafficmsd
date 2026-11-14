//! Test-only fixed-horizon scheduler baseline.
//!
//! This is an EOF-finalized control implementation for issue #104. It consumes
//! samples sequentially, never reads future input, and deliberately publishes
//! no provisional records. The conservative availability timestamp (N - 1)
//! makes prefix-finality trivial but exposes the worst-case latency bound.
//! It is not evidence of an incremental low-latency multiscale scheduler.
//!
//! The next implementation step is to replace the EOF barrier with per-window
//! readiness while preserving the same batch coefficient matrix.

use crate::decomposition::{decompose, SignalError};

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

    /// Consume exactly the next sample. No output is final before EOF in this
    /// baseline; the returned vector is therefore always empty.
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

        let decomposition = decompose(&self.received, self.cutoff_fraction)
            .map_err(|error: SignalError| SchedulerError::Decomposition(error.to_string()))?;
        let available_at = self.horizon - 1;
        let mut events = Vec::new();

        for (scale, row) in decomposition.coefficients.iter().enumerate() {
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
