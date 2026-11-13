//! Test-only prototype for fixed-horizon incremental interval optimization.
//!
//! This deliberately remains outside the production API. It proves the
//! optimizer's local streaming contract before any multiscale integration.

use crate::decomposition::optimize_intervals;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StreamError {
    TooManyValues,
    IncompleteInput { expected: usize, received: usize },
}

#[derive(Debug)]
struct StreamingOptimizer {
    horizon: usize,
    cutoff: f64,
    received: usize,
    pending_positive_run: Vec<(usize, f64)>,
    filtered: Vec<f64>,
    optimized: Vec<f64>,
    next_commit: usize,
}

impl StreamingOptimizer {
    fn new(horizon: usize, cutoff_fraction: f64) -> Self {
        Self {
            horizon,
            cutoff: horizon as f64 * cutoff_fraction,
            received: 0,
            pending_positive_run: Vec::new(),
            filtered: Vec::with_capacity(horizon),
            optimized: Vec::with_capacity(horizon),
            next_commit: 0,
        }
    }

    fn push(&mut self, value: f64) -> Result<Vec<(usize, f64)>, StreamError> {
        if self.received == self.horizon {
            return Err(StreamError::TooManyValues);
        }

        let index = self.received;
        self.received += 1;

        if self.horizon < 3 {
            self.optimized.push(value);
            return Ok(vec![(index, value)]);
        }

        let mut emitted = Vec::new();
        if self.cutoff > 1.0 {
            if value > 0.0 {
                self.pending_positive_run.push((index, value));
            } else {
                self.close_positive_run(&mut emitted);
                self.accept_filtered(value, &mut emitted);
            }
        } else {
            self.accept_filtered(value, &mut emitted);
        }
        Ok(emitted)
    }

    fn close_positive_run(&mut self, emitted: &mut Vec<(usize, f64)>) {
        let keep = (self.pending_positive_run.len() as f64) > self.cutoff;
        let run = std::mem::take(&mut self.pending_positive_run);
        for (_, value) in run {
            self.accept_filtered(if keep { value } else { 0.0 }, emitted);
        }
    }

    fn accept_filtered(&mut self, value: f64, emitted: &mut Vec<(usize, f64)>) {
        self.filtered.push(value);
        let index = self.filtered.len() - 1;

        if index == 0 {
            self.optimized.push(value);
            emitted.push((0, value));
            self.next_commit = 1;
            return;
        }

        // A middle sample i is final once filtered sample i+1 is known.
        while self.next_commit + 1 < self.filtered.len() {
            let i = self.next_commit;
            let a = self.optimized[i - 1];
            let b = self.filtered[i];
            let c = self.filtered[i + 1];
            let lo = a.min(c);
            let hi = a.max(c);
            let value = if b < lo {
                lo
            } else if b > lo {
                hi
            } else {
                b
            };
            self.optimized.push(value);
            emitted.push((i, value));
            self.next_commit += 1;
        }
    }

    fn finish(mut self) -> Result<Vec<(usize, f64)>, StreamError> {
        if self.received != self.horizon {
            return Err(StreamError::IncompleteInput {
                expected: self.horizon,
                received: self.received,
            });
        }

        let mut emitted = Vec::new();
        if self.horizon >= 3 {
            self.close_positive_run(&mut emitted);
            // The last sample is a boundary and is never interpolated.
            while self.next_commit < self.horizon {
                let i = self.next_commit;
                let value = self.filtered[i];
                self.optimized.push(value);
                emitted.push((i, value));
                self.next_commit += 1;
            }
        }
        Ok(emitted)
    }
}

fn run_stream(values: &[f64], cutoff_fraction: f64) -> Vec<(usize, f64)> {
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

#[test]
fn exhaustive_small_vectors_match_batch_bit_for_bit() {
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
                let streamed = run_stream(&values, cutoff);
                let indices: Vec<_> = streamed.iter().map(|(index, _)| *index).collect();
                assert_eq!(indices, (0..horizon).collect::<Vec<_>>());

                let streamed_values: Vec<_> =
                    streamed.into_iter().map(|(_, value)| value).collect();
                let batch = optimize_intervals(&values, cutoff);
                assert_same_bits(&streamed_values, &batch);
            }
        }
    }
}

#[test]
fn fixed_horizon_prefix_outputs_never_change() {
    let horizon = 64;
    let mut values = vec![0.0; horizon];
    values[3..11].fill(2.0);
    values[18..25].fill(1.0);
    values[40..47].fill(2.0);

    let mut stream = StreamingOptimizer::new(horizon, 0.1);
    let mut committed = Vec::new();
    for value in &values[..32] {
        committed.extend(stream.push(*value).unwrap());
    }

    let batch = optimize_intervals(&values, 0.1);
    for (index, value) in committed {
        assert_eq!(value.to_bits(), batch[index].to_bits());
    }
}

#[test]
fn fixed_power_of_two_horizons_keep_their_own_cutoff() {
    for horizon in [512usize, 1024] {
        let mut values = vec![0.0; horizon];
        values[100..103].fill(5.0);
        let streamed = run_stream(&values, 0.004);
        let streamed_values: Vec<_> =
            streamed.into_iter().map(|(_, value)| value).collect();
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
fn positive_run_is_closed_by_zero_or_by_eof_without_synthetic_sample() {
    for values in [vec![0.0, 2.0, 2.0, 2.0, 0.0], vec![0.0, 2.0, 2.0, 2.0]] {
        let streamed = run_stream(&values, 0.2);
        let streamed_values: Vec<_> =
            streamed.into_iter().map(|(_, value)| value).collect();
        assert_same_bits(&streamed_values, &optimize_intervals(&values, 0.2));
    }
}
