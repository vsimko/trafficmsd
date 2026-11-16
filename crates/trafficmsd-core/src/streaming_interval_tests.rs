//! Test-only prototype for fixed-horizon incremental interval optimization.
//!
//! This deliberately remains outside the production API. It preserves the
//! batch optimizer's semantics while exposing when each output becomes final.

use std::collections::VecDeque;

use crate::decomposition::optimize_intervals;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StreamError {
    TooManyValues,
    IncompleteInput { expected: usize, received: usize },
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct FinalizedInterval {
    event_time: usize,
    available_at: usize,
    value: f64,
}

#[derive(Debug)]
struct StreamingOptimizer {
    horizon: usize,
    cutoff: f64,
    received: usize,
    pending_positive_run: Vec<(usize, f64)>,
    filtered: VecDeque<(usize, f64)>,
    filtered_count: usize,
    last_optimized: Option<f64>,
    peak_pending_candidates: usize,
}

impl StreamingOptimizer {
    fn new(horizon: usize, cutoff_fraction: f64) -> Self {
        Self {
            horizon,
            cutoff: horizon as f64 * cutoff_fraction,
            received: 0,
            pending_positive_run: Vec::new(),
            filtered: VecDeque::with_capacity(2),
            filtered_count: 0,
            last_optimized: None,
            peak_pending_candidates: 0,
        }
    }

    fn push(&mut self, value: f64) -> Result<Vec<FinalizedInterval>, StreamError> {
        if self.received == self.horizon {
            return Err(StreamError::TooManyValues);
        }

        let index = self.received;
        self.received += 1;

        if self.horizon < 3 {
            return Ok(vec![FinalizedInterval {
                event_time: index,
                available_at: index,
                value,
            }]);
        }

        let mut emitted = Vec::new();
        if self.cutoff > 1.0 {
            if value > 0.0 {
                self.pending_positive_run.push((index, value));
                self.peak_pending_candidates = self
                    .peak_pending_candidates
                    .max(self.pending_positive_run.len() + self.filtered.len());
            } else {
                self.close_positive_run(index, &mut emitted);
                self.accept_filtered(value, index, &mut emitted);
            }
        } else {
            self.accept_filtered(value, index, &mut emitted);
        }
        Ok(emitted)
    }

    fn close_positive_run(
        &mut self,
        available_at: usize,
        emitted: &mut Vec<FinalizedInterval>,
    ) {
        let keep = (self.pending_positive_run.len() as f64) > self.cutoff;
        let run = std::mem::take(&mut self.pending_positive_run);
        self.peak_pending_candidates = self
            .peak_pending_candidates
            .max(run.len() + self.filtered.len());

        for (_, value) in run {
            self.accept_filtered(if keep { value } else { 0.0 }, available_at, emitted);
        }
    }

    fn accept_filtered(
        &mut self,
        value: f64,
        available_at: usize,
        emitted: &mut Vec<FinalizedInterval>,
    ) {
        let index = self.filtered_count;
        self.filtered_count += 1;
        self.filtered.push_back((index, value));

        if index == 0 {
            self.filtered.pop_front().expect("first filtered candidate exists");
            self.last_optimized = Some(value);
            emitted.push(FinalizedInterval {
                event_time: 0,
                available_at,
                value,
            });
            return;
        }

        // The next middle candidate is final once its right filtered neighbor
        // is known. Keep only that two-value lookahead and the previous output.
        while self.filtered.len() >= 2 {
            let (event_time, middle) = self.filtered[0];
            let right = self.filtered[1].1;
            let left = self
                .last_optimized
                .expect("the first boundary sample is committed first");
            let lo = left.min(right);
            let hi = left.max(right);
            let value = if middle < lo {
                lo
            } else if middle > lo {
                hi
            } else {
                middle
            };

            self.filtered.pop_front().expect("middle candidate exists");
            self.last_optimized = Some(value);
            emitted.push(FinalizedInterval {
                event_time,
                available_at,
                value,
            });
        }

        self.peak_pending_candidates = self
            .peak_pending_candidates
            .max(self.pending_positive_run.len() + self.filtered.len());
    }

    fn finish(mut self) -> Result<Vec<FinalizedInterval>, StreamError> {
        if self.received != self.horizon {
            return Err(StreamError::IncompleteInput {
                expected: self.horizon,
                received: self.received,
            });
        }

        let mut emitted = Vec::new();
        if self.horizon >= 3 {
            let eof = self.horizon - 1;
            self.close_positive_run(eof, &mut emitted);

            // The final sample is a boundary and is not interpolated.
            while let Some((event_time, value)) = self.filtered.pop_front() {
                emitted.push(FinalizedInterval {
                    event_time,
                    available_at: eof,
                    value,
                });
                self.last_optimized = Some(value);
            }
        }
        Ok(emitted)
    }

    fn peak_pending_candidates(&self) -> usize {
        self.peak_pending_candidates
    }
}

fn run_stream(values: &[f64], cutoff_fraction: f64) -> Vec<FinalizedInterval> {
    let mut stream = StreamingOptimizer::new(values.len(), cutoff_fraction);
    let mut output = Vec::new();
    for value in values {
        output.extend(stream.push(*value).unwrap());
    }
    output.extend(stream.finish().unwrap());
    output
}

fn assert_same_bits(left: &[f64], right: &[f64]) {
    assert_eq!(left.len(), right.len());
    for (index, (a, b)) in left.iter().zip(right).enumerate() {
        assert_eq!(a.to_bits(), b.to_bits(), "value differs at index {index}");
    }
}

fn assert_emission_matches_batch(
    emission: FinalizedInterval,
    batch: &[f64],
    observed_at: usize,
) {
    assert!(emission.event_time <= emission.available_at);
    assert_eq!(emission.available_at, observed_at);
    assert_eq!(
        emission.value.to_bits(),
        batch[emission.event_time].to_bits(),
        "final output differs at event_time {}",
        emission.event_time
    );
}

#[test]
fn exhaustive_small_vectors_match_batch_and_all_prefix_continuations() {
    let cutoffs = [0.004, 0.1, 0.25, 0.3, 0.5, 0.75, 0.99];
    for horizon in 1..=8 {
        let cases = 3usize.pow(horizon as u32);
        for encoded in 0..cases {
            let mut code = encoded;
            let mut values = vec![0.0; horizon];
            for value in &mut values {
                *value = (code % 3) as f64;
                code /= 3;
            }

            for cutoff in cutoffs {
                let batch = optimize_intervals(&values, cutoff);
                let mut stream = StreamingOptimizer::new(horizon, cutoff);
                let mut seen = vec![false; horizon];

                for (observed_at, value) in values.iter().enumerate() {
                    for emission in stream.push(*value).unwrap() {
                        assert_emission_matches_batch(emission, &batch, observed_at);
                        assert!(!seen[emission.event_time], "duplicate final output");
                        seen[emission.event_time] = true;
                    }
                }

                for emission in stream.finish().unwrap() {
                    assert_emission_matches_batch(emission, &batch, horizon - 1);
                    assert!(!seen[emission.event_time], "duplicate final output at EOF");
                    seen[emission.event_time] = true;
                }

                assert!(seen.into_iter().all(|value| value));
                let streamed = run_stream(&values, cutoff);
                let streamed_values: Vec<_> =
                    streamed.into_iter().map(|record| record.value).collect();
                assert_same_bits(&streamed_values, &batch);
            }
        }
    }
}

#[test]
fn interpolation_preserves_legacy_left_to_right_update_order() {
    let values = [4.0, 0.0, 2.0, 0.0, 4.0];
    let mut stream = StreamingOptimizer::new(values.len(), 0.004);
    let mut emitted = Vec::new();

    for (index, value) in values.iter().enumerate() {
        emitted.extend(stream.push(*value).unwrap());
        if index == 2 {
            assert!(emitted.iter().any(|record| {
                record.event_time == 1 && record.available_at == 2 && record.value == 2.0
            }));
        }
        if index == 3 {
            assert!(emitted.iter().any(|record| {
                record.event_time == 2 && record.available_at == 3 && record.value == 2.0
            }));
        }
    }
    emitted.extend(stream.finish().unwrap());

    let batch = optimize_intervals(&values, 0.004);
    let streamed: Vec<_> = emitted.iter().map(|record| record.value).collect();
    assert_same_bits(&streamed, &batch);
    assert_eq!(batch, vec![4.0, 2.0, 2.0, 2.0, 4.0]);
}

#[test]
fn positive_run_is_final_only_after_zero_or_declared_eof() {
    let horizon = 16;
    let mut values = vec![0.0; horizon];
    values[2..8].fill(2.0);
    let mut stream = StreamingOptimizer::new(horizon, 0.25);
    let mut emitted = Vec::new();

    for (index, value) in values.iter().enumerate() {
        emitted.extend(stream.push(*value).unwrap());
        if index < 8 {
            assert!(!emitted.iter().any(|record| (2..8).contains(&record.event_time)));
        }
    }

    assert!(emitted.iter().any(|record| {
        (2..8).contains(&record.event_time) && record.available_at == 8
    }));
    let tail = stream.finish().unwrap();
    emitted.extend(tail);

    let batch = optimize_intervals(&values, 0.25);
    let streamed: Vec<_> = emitted.iter().map(|record| record.value).collect();
    assert_same_bits(&streamed, &batch);
}

#[test]
fn eof_closes_a_positive_run_without_appending_a_synthetic_zero() {
    let mut values = vec![0.0; 16];
    values[2..].fill(2.0);
    let streamed = run_stream(&values, 0.25);
    let batch = optimize_intervals(&values, 0.25);

    assert_same_bits(
        &streamed.iter().map(|record| record.value).collect::<Vec<_>>(),
        &batch,
    );
    assert!(streamed.iter().filter(|record| record.event_time >= 2).all(
        |record| record.available_at == 15
    ));
}

#[test]
fn fixed_power_of_two_horizons_keep_their_own_cutoff() {
    for horizon in [512usize, 1024] {
        let mut values = vec![0.0; horizon];
        values[100..103].fill(5.0);
        let streamed = run_stream(&values, 0.004);
        let streamed_values: Vec<_> = streamed.iter().map(|record| record.value).collect();
        let batch = optimize_intervals(&values, 0.004);
        assert_same_bits(&streamed_values, &batch);

        let expected = if horizon == 512 { 5.0 } else { 0.0 };
        assert_eq!(batch[100], expected);
        assert_eq!(batch[101], expected);
        assert_eq!(batch[102], expected);
    }
}

#[test]
fn finish_requires_the_declared_horizon_and_push_rejects_overrun() {
    let mut incomplete = StreamingOptimizer::new(4, 0.1);
    incomplete.push(0.0).unwrap();
    assert_eq!(
        incomplete.finish(),
        Err(StreamError::IncompleteInput {
            expected: 4,
            received: 1
        })
    );

    let mut complete = StreamingOptimizer::new(1, 0.1);
    complete.push(1.0).unwrap();
    assert_eq!(complete.push(2.0), Err(StreamError::TooManyValues));
}

#[test]
fn pending_state_is_bounded_by_the_open_run_and_interpolation_lookahead() {
    let mut stream = StreamingOptimizer::new(64, 0.25);
    for value in [0.0, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0] {
        stream.push(value).unwrap();
    }

    assert!(stream.peak_pending_candidates() <= 7);
}
