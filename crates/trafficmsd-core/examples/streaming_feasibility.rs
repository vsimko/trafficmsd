use std::hint::black_box;
use std::time::Instant;

use trafficmsd_core::decomposition::{decompose, optimize_intervals};

const CUTOFF: f64 = 0.004;
const ITERATIONS: usize = 5;
const BENCHMARK_SIZES: [usize; 3] = [1_024, 8_192, 32_768];
const PREFIX_SIZES: [usize; 5] = [512, 1_024, 2_048, 4_096, 8_192];

#[derive(Clone, Copy)]
enum Fixture {
    SparseBursts,
    RepeatedTiedMaxima,
    LongPositiveRuns,
    OneSampleGaps,
    QuietTail,
}

impl Fixture {
    const ALL: [Self; 5] = [
        Self::SparseBursts,
        Self::RepeatedTiedMaxima,
        Self::LongPositiveRuns,
        Self::OneSampleGaps,
        Self::QuietTail,
    ];

    fn name(self) -> &'static str {
        match self {
            Self::SparseBursts => "sparse_bursts",
            Self::RepeatedTiedMaxima => "repeated_tied_maxima",
            Self::LongPositiveRuns => "long_positive_runs",
            Self::OneSampleGaps => "one_sample_gaps",
            Self::QuietTail => "quiet_tail",
        }
    }

    fn signal(self, _size: usize) -> Vec<f64> {
        (0..size)
            .map(|i| match self {
                Self::SparseBursts => {
                    if i % 257 < 8 {
                        10.0 + (i % 8) as f64
                    } else {
                        0.0
                    }
                }
                Self::RepeatedTiedMaxima => {
                    let position = i % 32;
                    if position == 5 || position == 21 {
                        12.0
                    } else if position < 4 {
                        2.0
                    } else {
                        0.0
                    }
                }
                Self::LongPositiveRuns => {
                    if (128..1_024).contains(&i) {
                        5.0
                    } else if (2_048..4_096).contains(&i) {
                        3.0
                    } else {
                        0.0
                    }
                }
                Self::OneSampleGaps => {
                    let position = i % 33;
                    if position < 16 || position > 16 {
                        7.0
                    } else {
                        0.0
                    }
                }
                Self::QuietTail => {
                    if i < 2_048 && i % 193 < 12 {
                        4.0 + (i % 12) as f64
                    } else {
                        0.0
                    }
                }
            })
            .collect()
    }
}

fn elapsed_ns<F>(iterations: usize, mut operation: F) -> u128
where
    F: FnMut(),
{
    let start = Instant::now();
    for _ in 0..iterations {
        operation();
    }
    start.elapsed().as_nanos()
}

fn emit(
    fixture: &str,
    size: usize,
    operation: &str,
    iterations: usize,
    total_ns: u128,
    changed_samples: usize,
    changed_fraction: f64,
) {
    println!(
        "{fixture},{size},{operation},{iterations},{total_ns},{},{changed_samples},{changed_fraction:.8}",
        total_ns / iterations as u128
    );
}

fn main() {
    println!(
        "fixture,size,operation,iterations,total_ns,mean_ns,changed_samples,changed_fraction"
    );

    for fixture in Fixture::ALL {
        for size in BENCHMARK_SIZES {
            let signal = fixture.signal(size);

            // Warm up before collecting measurements.
            black_box(optimize_intervals(black_box(&signal), CUTOFF));
            black_box(decompose(black_box(&signal), CUTOFF).expect("valid fixture"));

            let optimizer_ns = elapsed_ns(ITERATIONS, || {
                black_box(optimize_intervals(black_box(&signal), black_box(CUTOFF)));
            });
            emit(
                fixture.name(),
                size,
                "optimize_intervals",
                ITERATIONS,
                optimizer_ns,
                0,
                0.0,
            );

            let decomposition_ns = elapsed_ns(ITERATIONS, || {
                black_box(decompose(black_box(&signal), black_box(CUTOFF)).expect("valid fixture"));
            });
            emit(
                fixture.name(),
                size,
                "decompose",
                ITERATIONS,
                decomposition_ns,
                0,
                0.0,
            );
        }

        // Compare each offline prefix result with the same positions after
        // extending the observation horizon. Any changed value is a
        // retrospective revision under the current whole-vector optimizer.
        for pair in PREFIX_SIZES.windows(2) {
            let previous_size = pair[0];
            let extended_size = pair[1];
            let previous = fixture.signal(previous_size);
            let extended = fixture.signal(extended_size);
            let start = Instant::now();
            let previous_output = optimize_intervals(&previous, CUTOFF);
            let extended_output = optimize_intervals(&extended, CUTOFF);
            let elapsed = start.elapsed().as_nanos();

            let changed_samples = previous_output
                .iter()
                .zip(extended_output.iter())
                .filter(|(left, right)| left != right)
                .count();
            let changed_fraction = changed_samples as f64 / previous_size as f64;
            emit(
                fixture.name(),
                extended_size,
                "prefix_extension",
                1,
                elapsed,
                changed_samples,
                changed_fraction,
            );
        }
    }

    // Explicit regression witness for the horizon-relative run cutoff:
    // the same three-sample run survives at n=512 and is removed at n=1024.
    let mut short_horizon = vec![0.0; 512];
    short_horizon[100..103].fill(1.0);
    let mut extended_horizon = vec![0.0; 1_024];
    extended_horizon[100..103].fill(1.0);
    let short_output = optimize_intervals(&short_horizon, CUTOFF);
    let extended_output = optimize_intervals(&extended_horizon, CUTOFF);
    let changed_samples = short_output
        .iter()
        .zip(extended_output.iter())
        .filter(|(left, right)| left != right)
        .count();
    emit(
        "cutoff_counterexample",
        1_024,
        "prefix_extension",
        1,
        0,
        changed_samples,
        changed_samples as f64 / short_horizon.len() as f64,
    );
}
