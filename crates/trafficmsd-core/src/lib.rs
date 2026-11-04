//! Domain and simulation core for TrafficMSD.
//!
//! This crate is intentionally independent of the command-line interface and
//! language-specific adapters. Algorithms preserve the documented R reference
//! semantics while being introduced incrementally.

pub mod decomposition;

/// Returns the version of the core crate.
pub const fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

#[cfg(test)]
mod tests {
    use super::version;

    #[test]
    fn exposes_package_version() {
        assert_eq!(version(), env!("CARGO_PKG_VERSION"));
    }
}
