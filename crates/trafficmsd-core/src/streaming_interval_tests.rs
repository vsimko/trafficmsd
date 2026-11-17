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
                self.accept_filtered(value, index, 0, &mut emitted);
            }
        } else {
            self.accept_filtered(value, index, 0, &mut emitted);
        }
        Ok(emitted)
    }

    fn close_positive_run(&mut self, available_at: usize, emitted: &mut Vec<FinalizedInterval>) {
        let keep = (self.pending_positive_run.len() as f64) > self.cutoff;
        let run = std::mem::take(&mut self.pending_positive_run);
        self.peak_pending_candidates = self
            .peak_pending_candidates
            .max(run.len() + self.filtered.len());

        let mut remaining = run.len();
        for (_, value) in run {
            remaining -= 1;
            self.accept_filtered(
                if keep { value } else { 0.0 },
                available_at,
                remaining,
                emitted,
            );
        }
    }

    fn accept_filtered(
        &mut self,
        value: f64,
        available_at: usize,
        pending_run_remaining: usize,
        emitted: &mut Vec<FinalizedInterval>,
    ) {
        let index = self.filtered_count;
        self.filtered_count += 1;
        self.filtered.push_back((index, value));
        self.peak_pending_candidates = self
            .peak_pending_candidates
            .max(pending_run_remaining + self.filtered.len());

        if index == 0 {
            self.filtered
                .pop_front()
                .expect("first filtered candidate exists");
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
            .max(pending_run_remaining + self.filtered.len());
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

fn assert_emission_matches_batch(emission: FinalizedInterval, batch: &[f64], observed_at: usize) {
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
            assert!(!emitted
                .iter()
                .any(|record| (2..8).contains(&record.event_time)));
        }
    }

    assert!(emitted
        .iter()
        .any(|record| { (2..8).contains(&record.event_time) && record.available_at == 8 }));
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
        &streamed
            .iter()
            .map(|record| record.value)
            .collect::<Vec<_>>(),
        &batch,
    );
    assert!(streamed
        .iter()
        .filter(|record| record.event_time >= 2)
        .all(|record| record.available_at == 15));
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

// A deliberately different, horizon-independent policy used for research:
// retain a positive run as soon as it reaches min_run candidates.
#[derive(Debug)]
struct FixedThresholdOptimizer {
    horizon: usize,
    min_run: usize,
    received: usize,
    pending_positive_run: Vec<(usize, f64)>,
    retaining_positive_run: bool,
    filtered: VecDeque<(usize, f64)>,
    last_optimized: Option<f64>,
}

impl FixedThresholdOptimizer {
    fn new(horizon: usize, min_run: usize) -> Self {
        assert!(min_run > 0);
        Self {
            horizon,
            min_run,
            received: 0,
            pending_positive_run: Vec::new(),
            retaining_positive_run: false,
            filtered: VecDeque::with_capacity(2),
            last_optimized: None,
        }
    }

    fn push(&mut self, value: f64) -> Result<Vec<FinalizedInterval>, StreamError> {
        if self.received == self.horizon {
            return Err(StreamError::TooManyValues);
        }
        let index = self.received;
        self.received += 1;
        let mut emitted = Vec::new();

        if value > 0.0 {
            if self.retaining_positive_run {
                self.accept_filtered(value, index, index, &mut emitted);
            } else {
                self.pending_positive_run.push((index, value));
                if self.pending_positive_run.len() == self.min_run {
                    let run = std::mem::take(&mut self.pending_positive_run);
                    self.retaining_positive_run = true;
                    for (event_time, run_value) in run {
                        self.accept_filtered(run_value, event_time, index, &mut emitted);
                    }
                }
            }
        } else {
            self.close_short_run(index, &mut emitted);
            self.accept_filtered(0.0, index, index, &mut emitted);
        }
        Ok(emitted)
    }

    fn close_short_run(&mut self, available_at: usize, emitted: &mut Vec<FinalizedInterval>) {
        self.retaining_positive_run = false;
        let run = std::mem::take(&mut self.pending_positive_run);
        for (event_time, _) in run {
            self.accept_filtered(0.0, event_time, available_at, emitted);
        }
    }

    fn accept_filtered(
        &mut self,
        value: f64,
        event_time: usize,
        available_at: usize,
        emitted: &mut Vec<FinalizedInterval>,
    ) {
        self.filtered.push_back((event_time, value));
        if self.last_optimized.is_none() {
            let (first_time, first_value) = self.filtered.pop_front().unwrap();
            self.last_optimized = Some(first_value);
            emitted.push(FinalizedInterval {
                event_time: first_time,
                available_at,
                value: first_value,
            });
        }

        while self.filtered.len() >= 2 {
            let (middle_time, middle) = self.filtered[0];
            let right = self.filtered[1].1;
            let left = self.last_optimized.unwrap();
            let lo = left.min(right);
            let hi = left.max(right);
            let optimized = if middle < lo {
                lo
            } else if middle > lo {
                hi
            } else {
                middle
            };
            self.filtered.pop_front();
            self.last_optimized = Some(optimized);
            emitted.push(FinalizedInterval {
                event_time: middle_time,
                available_at,
                value: optimized,
            });
        }
    }

    fn finish(mut self) -> Result<Vec<FinalizedInterval>, StreamError> {
        if self.received != self.horizon {
            return Err(StreamError::IncompleteInput {
                expected: self.horizon,
                received: self.received,
            });
        }
        let eof = self.horizon.saturating_sub(1);
        let mut emitted = Vec::new();
        self.close_short_run(eof, &mut emitted);
        while let Some((event_time, value)) = self.filtered.pop_front() {
            emitted.push(FinalizedInterval {
                event_time,
                available_at: eof,
                value,
            });
        }
        Ok(emitted)
    }
}

fn run_fixed_threshold(values: &[f64], min_run: usize) -> Vec<FinalizedInterval> {
    let mut stream = FixedThresholdOptimizer::new(values.len(), min_run);
    let mut output = Vec::new();
    for value in values {
        output.extend(stream.push(*value).unwrap());
    }
    output.extend(stream.finish().unwrap());
    output
}

#[test]
fn fixed_threshold_is_prefix_final_and_horizon_independent() {
    let fixtures = [
        vec![0.0, 3.0, 0.0, 2.0, 2.0, 0.0, 4.0, 4.0, 4.0, 0.0],
        vec![5.0, 5.0, 0.0, 1.0, 0.0, 6.0, 6.0, 0.0, 0.0, 0.0],
        vec![0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 7.0, 0.0, 0.0, 0.0],
    ];
    for values in fixtures {
        for min_run in [1, 2, 3, 4] {
            let mut stream = FixedThresholdOptimizer::new(values.len(), min_run);
            let mut finalized = Vec::new();
            for (index, value) in values.iter().enumerate() {
                let full = run_fixed_threshold(&values, min_run);
                let newly_emitted = stream.push(*value).unwrap();
                for record in &newly_emitted {
                    assert_eq!(record.available_at, index);
                    assert!(record.event_time <= record.available_at);
                    assert_eq!(
                        record.value.to_bits(),
                        full.iter()
                            .find(|candidate| candidate.event_time == record.event_time)
                            .map(|candidate| candidate.value)
                            .unwrap()
                            .to_bits()
                    );
                }
                finalized.extend(newly_emitted);
            }
            finalized.extend(stream.finish().unwrap());
            let full = run_fixed_threshold(&values, min_run);
            let mut actual = finalized.clone();
            actual.sort_by_key(|record| record.event_time);
            assert_eq!(actual, full);
        }
    }
}

#[test]
fn fixed_threshold_keeps_the_entire_run_after_reaching_threshold() {
    let values = [0.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0];
    let output = run_fixed_threshold(&values, 3);
    let mut ordered = output;
    ordered.sort_by_key(|record| record.event_time);
    assert_eq!(
        ordered
            .iter()
            .map(|record| record.value)
            .collect::<Vec<_>>(),
        values
    );
    assert!(ordered
        .iter()
        .filter(|record| (1..7).contains(&record.event_time))
        .all(|record| record.value == 2.0));
}

#[test]
fn fixed_threshold_retains_at_boundary_and_discards_shorter_runs() {
    let values = [0.0, 2.0, 0.0, 3.0, 3.0, 0.0, 4.0, 4.0, 4.0, 0.0];
    let output = run_fixed_threshold(&values, 2);
    let by_time: Vec<_> = {
        let mut records = output;
        records.sort_by_key(|record| record.event_time);
        records
    };
    assert_eq!(by_time[1].value, 0.0);
    assert_eq!(by_time[3].value, 3.0);
    assert_eq!(by_time[4].value, 3.0);
    assert_eq!(by_time[6].value, 4.0);
}

#[test]
fn fixed_threshold_prefix_emissions_are_invariant_to_declared_horizon() {
    let prefix = [0.0, 3.0, 0.0, 2.0, 2.0, 0.0, 4.0, 4.0, 4.0];
    let suffix = [0.0, 0.0, 1.0, 1.0, 1.0];
    for min_run in [1, 2, 3, 4] {
        let mut short = FixedThresholdOptimizer::new(prefix.len() + 1, min_run);
        let mut long = FixedThresholdOptimizer::new(prefix.len() + suffix.len(), min_run);
        let mut short_emitted = Vec::new();
        let mut long_emitted = Vec::new();

        for value in prefix {
            short_emitted.extend(short.push(value).unwrap());
            long_emitted.extend(long.push(value).unwrap());
        }
        assert_eq!(short_emitted, long_emitted);

        let immutable_prefix = long_emitted.clone();
        for value in suffix {
            long_emitted.extend(long.push(value).unwrap());
        }
        long_emitted.extend(long.finish().unwrap());
        for prior in immutable_prefix {
            assert_eq!(
                long_emitted
                    .iter()
                    .find(|record| record.event_time == prior.event_time)
                    .unwrap()
                    .value
                    .to_bits(),
                prior.value.to_bits()
            );
        }
    }
}

#[test]
fn fixed_threshold_exhaustively_finalizes_all_small_vectors() {
    for horizon in 1..=8 {
        let cases = 3usize.pow(horizon as u32);
        for encoded in 0..cases {
            let mut values = vec![0.0; horizon];
            let mut digits = encoded;
            for value in &mut values {
                *value = (digits % 3) as f64;
                digits /= 3;
            }

            for min_run in 1..=4 {
                let expected = run_fixed_threshold(&values, min_run);
                let mut stream = FixedThresholdOptimizer::new(horizon, min_run);
                let mut seen = vec![false; horizon];

                for (available_at, value) in values.iter().enumerate() {
                    for record in stream.push(*value).unwrap() {
                        assert_eq!(record.available_at, available_at);
                        assert!(record.event_time <= available_at);
                        assert!(!seen[record.event_time], "duplicate final event");
                        seen[record.event_time] = true;
                        assert_eq!(
                            record.value.to_bits(),
                            expected
                                .iter()
                                .find(|candidate| candidate.event_time == record.event_time)
                                .unwrap()
                                .value
                                .to_bits()
                        );
                    }
                }

                for record in stream.finish().unwrap() {
                    assert_eq!(record.available_at, horizon - 1);
                    assert!(!seen[record.event_time], "duplicate final event at EOF");
                    seen[record.event_time] = true;
                    assert_eq!(
                        record.value.to_bits(),
                        expected
                            .iter()
                            .find(|candidate| candidate.event_time == record.event_time)
                            .unwrap()
                            .value
                            .to_bits()
                    );
                }

                assert!(seen.into_iter().all(|was_seen| was_seen));
            }
        }
    }
}

#[test]
fn fixed_threshold_prefix_emissions_are_invariant_for_all_small_suffixes_and_horizons() {
    for prefix_len in 1..=6 {
        let cases = 3usize.pow(prefix_len as u32);
        for encoded in 0..cases {
            let mut prefix = vec![0.0; prefix_len];
            let mut digits = encoded;
            for value in &mut prefix {
                *value = (digits % 3) as f64;
                digits /= 3;
            }

            for min_run in 1..=4 {
                let mut baseline = FixedThresholdOptimizer::new(prefix_len + 1, min_run);
                let mut baseline_emitted = Vec::new();
                for value in &prefix {
                    baseline_emitted.extend(baseline.push(*value).unwrap());
                }

                for horizon in (prefix_len + 1)..=8 {
                    let mut extended = FixedThresholdOptimizer::new(horizon, min_run);
                    let mut extended_emitted = Vec::new();
                    for value in &prefix {
                        extended_emitted.extend(extended.push(*value).unwrap());
                    }
                    assert_eq!(
                        extended_emitted, baseline_emitted,
                        "prefix length {prefix_len}, horizon {horizon}, min_run {min_run}"
                    );
                }
            }
        }
    }
}

/// Real-trace microbenchmark. Set TRAFFICMSD_STREAMING_TRACE to the verified
/// trace.csv path and run the ignored test explicitly.
///
/// This treats the one-second Westermo aggregate as a single candidate vector.
/// It isolates interval filtering/interpolation and does not claim to benchmark
/// multiscale candidate generation or residual updates.
#[test]
#[ignore = "requires the checksum-verified Westermo trace artifact"]
fn fixed_threshold_westermo_packet_trace_benchmark() {
    use std::time::Instant;

    let trace_path = std::env::var("TRAFFICMSD_STREAMING_TRACE")
        .expect("set TRAFFICMSD_STREAMING_TRACE to the verified trace.csv path");
    let text = std::fs::read_to_string(&trace_path).expect("read Westermo trace");
    let mut lines = text.lines();
    assert_eq!(lines.next(), Some("sample_index,value"));
    let values: Vec<f64> = lines
        .map(|line| {
            let (_, value) = line.split_once(',').expect("CSV row has two columns");
            value.parse().expect("finite numeric trace value")
        })
        .collect();
    assert_eq!(values.len(), 5_445, "unexpected Westermo trace length");
    assert!(values
        .iter()
        .all(|value| value.is_finite() && *value >= 0.0));

    let offline_started = Instant::now();
    let offline = optimize_intervals(&values, 0.004);
    let offline_elapsed_ns = offline_started.elapsed().as_nanos();

    let error_metrics = |estimate: &[f64], reference: &[f64]| {
        assert_eq!(estimate.len(), reference.len());
        let mut absolute_sum = 0.0;
        let mut squared_sum = 0.0;
        let mut max_absolute: f64 = 0.0;
        let mut estimate_volume = 0.0;
        let mut reference_volume = 0.0;
        for (actual, expected) in estimate.iter().zip(reference) {
            let error = actual - expected;
            absolute_sum += error.abs();
            squared_sum += error * error;
            max_absolute = max_absolute.max(error.abs());
            estimate_volume += actual;
            reference_volume += expected;
        }
        (
            absolute_sum / estimate.len() as f64,
            (squared_sum / estimate.len() as f64).sqrt(),
            max_absolute,
            if reference_volume > 0.0 {
                (estimate_volume - reference_volume) / reference_volume * 100.0
            } else {
                0.0
            },
        )
    };

    let interval_count = |series: &[f64]| {
        series
            .iter()
            .enumerate()
            .filter(|(index, value)| **value > 0.0 && (*index == 0 || series[*index - 1] <= 0.0))
            .count()
    };

    let mut policies = Vec::new();
    for min_run in [1usize, 2, 4, 8, 16, 22, 32, 64] {
        let started = Instant::now();
        let mut stream = FixedThresholdOptimizer::new(values.len(), min_run);
        let mut records = Vec::with_capacity(values.len());
        let mut peak_pending = 0usize;
        for value in &values {
            records.extend(stream.push(*value).unwrap());
            peak_pending =
                peak_pending.max(stream.pending_positive_run.len() + stream.filtered.len());
        }
        records.extend(stream.finish().unwrap());
        let streaming_elapsed_ns = started.elapsed().as_nanos();
        assert_eq!(records.len(), values.len());

        let mut finalized = vec![0.0; values.len()];
        let mut delays = Vec::with_capacity(records.len());
        for record in records {
            finalized[record.event_time] = record.value;
            delays.push(record.available_at - record.event_time);
        }
        delays.sort_unstable();
        let percentile = |fraction: f64| -> usize {
            let index = ((delays.len() as f64 * fraction).ceil() as usize)
                .saturating_sub(1)
                .min(delays.len() - 1);
            delays[index]
        };

        let (mae, rmse, max_abs, volume_error_pct) = error_metrics(&finalized, &values);
        let (offline_mae, offline_rmse, offline_max_abs, offline_volume_error_pct) =
            error_metrics(&offline, &values);
        let (mae_vs_offline, rmse_vs_offline, max_vs_offline, volume_vs_offline_pct) =
            error_metrics(&finalized, &offline);

        policies.push(serde_json::json!({
            "min_run_candidates": min_run,
            "streaming_elapsed_ns": streaming_elapsed_ns,
            "streaming_candidates_per_second": values.len() as f64
                / (streaming_elapsed_ns as f64 / 1_000_000_000.0),
            "peak_pending_candidates_observed": peak_pending,
            "positive_candidate_count": finalized.iter().filter(|value| **value > 0.0).count(),
            "positive_interval_count": interval_count(&finalized),
            "finality_delay_candidates": {
                "p50": percentile(0.50),
                "p95": percentile(0.95),
                "p99": percentile(0.99),
                "max": *delays.last().unwrap()
            },
            "reconstruction_vs_trace": {
                "mae": mae,
                "rmse": rmse,
                "max_absolute_error": max_abs,
                "volume_error_percent": volume_error_pct
            },
            "difference_vs_offline_reference": {
                "mae": mae_vs_offline,
                "rmse": rmse_vs_offline,
                "max_absolute_error": max_vs_offline,
                "volume_error_percent": volume_vs_offline_pct
            },
            "offline_reference_reconstruction_vs_trace": {
                "mae": offline_mae,
                "rmse": offline_rmse,
                "max_absolute_error": offline_max_abs,
                "volume_error_percent": offline_volume_error_pct
            }
        }));
    }

    let report = serde_json::json!({
        "benchmark": "westermo-fixed-threshold-interval-prototype-v1",
        "scope": "single-scale candidate-vector proxy; no multiscale candidate generation or residual update",
        "dataset": "Westermo Network Traffic Dataset v1.0, right observation point",
        "trace_sha256": "6429e3d6a84efdc13edc14e44b2de2e6cbd31452e1958116c271745069b3fbb7",
        "sample_count": values.len(),
        "sampling_interval_seconds": 1,
        "quantity": "captured_frame_length_bytes_per_bin",
        "offline_cutoff_fraction": 0.004,
        "offline_elapsed_ns": offline_elapsed_ns,
        "offline_candidates_per_second": values.len() as f64
            / (offline_elapsed_ns as f64 / 1_000_000_000.0),
        "policies": policies,
        "limitations": [
            "The trace is industrial-network traffic from one observation point.",
            "The fixed-count prototype is applied directly to the aggregate series as a candidate-vector proxy.",
            "No scale-specific candidate construction, residual updates, or multiscale readiness is measured.",
            "Pending-candidate count is an algorithmic state proxy, not process RSS.",
            "The threshold grid is exploratory and is not calibrated or selected on held-out data."
        ]
    });

    let output_path = std::env::var("TRAFFICMSD_STREAMING_BENCHMARK_OUTPUT")
        .unwrap_or_else(|_| "artifacts/streaming-fixed-threshold-westermo/benchmark.json".into());
    let output_path = std::path::Path::new(&output_path);
    std::fs::create_dir_all(output_path.parent().unwrap()).expect("create report directory");
    std::fs::write(output_path, serde_json::to_vec_pretty(&report).unwrap())
        .expect("write benchmark report");
    println!("{}", serde_json::to_string_pretty(&report).unwrap());
}
