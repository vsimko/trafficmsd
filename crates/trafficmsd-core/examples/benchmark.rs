use std::hint::black_box;
use std::time::Instant;

fn signal(size: usize) -> Vec<f64> {
    (0..size)
        .map(|index| {
            let base = ((index * 17) % 101) as f64;
            let burst = if index % 257 < 13 { 75.0 } else { 0.0 };
            base + burst
        })
        .collect()
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

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.len() != 2 {
        eprintln!("usage: benchmark <power-of-two-size> <iterations>");
        std::process::exit(2);
    }

    let size = args[0].parse::<usize>().unwrap_or_else(|_| {
        eprintln!("size must be a positive power of two");
        std::process::exit(2);
    });
    let iterations = args[1].parse::<usize>().unwrap_or_else(|_| {
        eprintln!("iterations must be a positive integer");
        std::process::exit(2);
    });
    if size < 16 || !size.is_power_of_two() || iterations == 0 {
        eprintln!("size must be a power of two >= 16 and iterations must be positive");
        std::process::exit(2);
    }

    let input = signal(size);
    let cutoff = 0.004;

    // Warm up both operations before collecting timings.
    let decomposition = trafficmsd_core::decomposition::decompose(&input, cutoff)
        .expect("benchmark input must be valid");
    let _ = trafficmsd_core::decomposition::reconstruct(&decomposition.coefficients)
        .expect("benchmark coefficients must be valid");

    let decompose_ns = elapsed_ns(iterations, || {
        let result =
            trafficmsd_core::decomposition::decompose(black_box(&input), black_box(cutoff))
                .expect("decomposition failed");
        black_box(result);
    });
    let reconstruct_ns = elapsed_ns(iterations, || {
        let result =
            trafficmsd_core::decomposition::reconstruct(black_box(&decomposition.coefficients))
                .expect("reconstruction failed");
        black_box(result);
    });

    println!("operation,size,iterations,total_ns,mean_ns");
    println!(
        "decompose,{size},{iterations},{decompose_ns},{}",
        decompose_ns / iterations as u128
    );
    println!(
        "reconstruct,{size},{iterations},{reconstruct_ns},{}",
        reconstruct_ns / iterations as u128
    );
}
