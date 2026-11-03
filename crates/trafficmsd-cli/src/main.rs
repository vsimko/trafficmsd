//! Standalone command-line entry point for TrafficMSD.

use std::process::ExitCode;

fn main() -> ExitCode {
    let mut args = std::env::args().skip(1);

    match args.next().as_deref() {
        Some("--version" | "-V") => {
            println!("trafficmsd {}", trafficmsd_core::version());
            ExitCode::SUCCESS
        }
        Some("--help" | "-h") | None => {
            print_help();
            ExitCode::SUCCESS
        }
        Some(argument) => {
            eprintln!("error: unknown argument: {argument}");
            eprintln!("Try 'trafficmsd --help' for usage.");
            ExitCode::from(2)
        }
    }
}

fn print_help() {
    println!(
        "TrafficMSD {}\n\
         A network traffic simulation engine.\n\
         \n\
         Usage: trafficmsd [OPTIONS]\n\
         \n\
         Options:\n\
         -h, --help       Print help\n\
         -V, --version    Print version",
        trafficmsd_core::version()
    );
}

#[cfg(test)]
mod tests {
    #[test]
    fn core_dependency_is_available() {
        assert!(!trafficmsd_core::version().is_empty());
    }
}
