//! Multiscale decomposition and reconstruction, ported from the R reference.

use std::{error::Error, fmt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SignalError {
    TooShort,
    NonFinite(usize),
    Negative(usize),
    NotPowerOfTwo,
    InvalidCutoff,
    InvalidCoefficients(&'static str),
}

impl fmt::Display for SignalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for SignalError {}

#[derive(Debug, Clone, PartialEq)]
pub struct Decomposition {
    pub length: usize,
    pub max_scale: u32,
    pub coefficients: Vec<Vec<f64>>,
}

fn validate(x: &[f64]) -> Result<(), SignalError> {
    if x.len() < 16 {
        return Err(SignalError::TooShort);
    }
    for (i, v) in x.iter().enumerate() {
        if !v.is_finite() {
            return Err(SignalError::NonFinite(i));
        }
        if *v < 0.0 {
            return Err(SignalError::Negative(i));
        }
    }
    Ok(())
}

pub fn pad_power2(x: &[f64]) -> Result<Vec<f64>, SignalError> {
    validate(x)?;
    let n = x
        .len()
        .checked_next_power_of_two()
        .ok_or(SignalError::InvalidCoefficients("length overflow"))?;
    let mut y = x.to_vec();
    y.resize(n, 0.0);
    Ok(y)
}

/// Port of R optimize_intervals(): remove short positive runs, then fill
/// one-sample gaps. cutoff_fraction must be in (0, 1).
pub fn optimize_intervals(x: &[f64], cutoff_fraction: f64) -> Vec<f64> {
    let n = x.len();
    if n < 3 {
        return x.to_vec();
    }

    let mut y = x.to_vec();
    let cutoff = n as f64 * cutoff_fraction;

    if cutoff > 1.0 {
        let mut run = vec![0usize; n + 1];
        for i in (0..n).rev() {
            if y[i] > 0.0 {
                run[i] = run[i + 1] + 1;
            }
        }

        let mut propagated = run[..n].to_vec();
        for i in 1..n {
            if propagated[i] > 0 {
                propagated[i] = propagated[i].max(propagated[i - 1]);
            }
        }

        for i in 0..n {
            if (propagated[i] as f64) <= cutoff {
                y[i] = 0.0;
            }
        }
    }

    // Interpolate small gaps: | a b a | -> | a a a |.
    for i in 1..n - 1 {
        let a = y[i - 1];
        let b = y[i];
        let c = y[i + 1];
        let lo = a.min(c);
        let hi = a.max(c);

        if b < lo {
            y[i] = lo;
        } else if b > lo {
            y[i] = hi;
        }
    }

    y
}

/// Decompose a non-negative finite power-of-two signal. Row zero is the
/// residual; scale s occupies row s and emits at period 2^s's center-left.
pub fn decompose(signal: &[f64], cutoff_fraction: f64) -> Result<Decomposition, SignalError> {
    validate(signal)?;
    if !cutoff_fraction.is_finite() || cutoff_fraction <= 0.0 || cutoff_fraction >= 1.0 {
        return Err(SignalError::InvalidCutoff);
    }

    let n = signal.len();
    if !n.is_power_of_two() {
        return Err(SignalError::NotPowerOfTwo);
    }

    let max_scale = n.trailing_zeros();
    let mut coef = vec![vec![0.0; n]; max_scale as usize + 1];
    let mut residual = signal.to_vec();

    for scale in (1..=max_scale).rev() {
        let w = 1usize << scale;
        let mut candidates = Vec::with_capacity(n / w);

        for chunk in residual.chunks_exact(w) {
            let mut sorted = chunk.to_vec();
            sorted.sort_by(|a, b| b.total_cmp(a));

            let mut candidate = 0.0;
            let mut i = 0;
            while i < sorted.len() {
                let mut j = i + 1;
                while j < sorted.len() && sorted[j] == sorted[i] {
                    j += 1;
                }
                if j - i == 1 {
                    candidate = sorted[i];
                    break;
                }
                i = j;
            }
            candidates.push(candidate);
        }

        for (k, c) in optimize_intervals(&candidates, cutoff_fraction)
            .into_iter()
            .enumerate()
        {
            if c <= 0.0 {
                continue;
            }

            let begin = k * w;
            let end = begin + w;
            let pos = residual[begin..end]
                .iter()
                .position(|v| *v == c)
                .expect("candidate came from window");

            coef[scale as usize][begin..end].fill(c);
            residual[begin + pos] = 0.0;

            let emit = begin + w / 2 - 1;
            residual[emit] = (residual[emit] - c).max(0.0);
        }
    }

    coef[0] = optimize_intervals(&residual, cutoff_fraction);

    Ok(Decomposition {
        length: n,
        max_scale,
        coefficients: coef,
    })
}

/// Reconstruct from scale rows, using globally aligned center-left emission.
pub fn reconstruct(coef: &[Vec<f64>]) -> Result<Vec<f64>, SignalError> {
    if coef.len() < 2 {
        return Err(SignalError::InvalidCoefficients(
            "at least two rows required",
        ));
    }

    let n = coef[0].len();
    if n == 0 || !n.is_power_of_two() || coef.iter().any(|r| r.len() != n) {
        return Err(SignalError::InvalidCoefficients(
            "rows must have equal, power-of-two length",
        ));
    }

    let mut out = vec![0.0; n];
    for (i, v) in coef[0].iter().enumerate() {
        check(*v, i)?;
        out[i] += v;
    }

    for (s, row) in coef.iter().enumerate().skip(1) {
        let w = 1usize
            .checked_shl(s as u32)
            .ok_or(SignalError::InvalidCoefficients("scale overflow"))?;
        if w > n {
            return Err(SignalError::InvalidCoefficients("scale exceeds extent"));
        }

        for begin in (0..n).step_by(w) {
            let i = begin + w / 2 - 1;
            check(row[i], i)?;
            out[i] += row[i];
        }
    }

    if let Some(i) = out.iter().position(|v| !v.is_finite()) {
        return Err(SignalError::NonFinite(i));
    }
    Ok(out)
}

fn check(v: f64, i: usize) -> Result<(), SignalError> {
    if !v.is_finite() {
        Err(SignalError::NonFinite(i))
    } else if v < 0.0 {
        Err(SignalError::Negative(i))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pad_preserves_samples() {
        let x = vec![1.0; 17];
        let y = pad_power2(&x).unwrap();
        assert_eq!(y.len(), 32);
        assert_eq!(&y[..17], &x);
        assert!(y[17..].iter().all(|v| *v == 0.0));
    }

    #[test]
    fn validates_input() {
        assert_eq!(decompose(&[1.0; 15], 0.004), Err(SignalError::TooShort));
        assert_eq!(
            decompose(&[1.0; 17], 0.004),
            Err(SignalError::NotPowerOfTwo)
        );

        let mut x = vec![0.0; 16];
        x[2] = -1.0;
        assert_eq!(decompose(&x, 0.004), Err(SignalError::Negative(2)));

        let mut x = vec![0.0; 16];
        x[3] = f64::NAN;
        assert_eq!(decompose(&x, 0.004), Err(SignalError::NonFinite(3)));
    }

    #[test]
    fn rejects_invalid_cutoff() {
        for cutoff in [0.0, 1.0, f64::NAN, f64::INFINITY] {
            assert_eq!(
                decompose(&[1.0; 16], cutoff),
                Err(SignalError::InvalidCutoff)
            );
        }
    }

    #[test]
    fn constant_and_zero_round_trip() {
        for x in [vec![8.0; 16], vec![0.0; 32]] {
            let d = decompose(&x, 0.004).unwrap();
            assert_eq!(reconstruct(&d.coefficients).unwrap(), x);
        }
    }

    #[test]
    fn higher_scale_uses_global_center_left() {
        let mut c = vec![vec![0.0; 16]; 5];
        c[2].fill(3.0);
        let y = reconstruct(&c).unwrap();
        let ix: Vec<_> = y
            .iter()
            .enumerate()
            .filter_map(|(i, v)| (*v > 0.0).then_some(i))
            .collect();
        assert_eq!(ix, vec![1, 5, 9, 13]);
    }

    #[test]
    fn interpolates_gaps() {
        assert_eq!(
            optimize_intervals(&[2.0, 0.0, 4.0], 0.004),
            vec![2.0, 2.0, 4.0]
        );
        assert_eq!(
            optimize_intervals(&[4.0, 8.0, 2.0], 0.004),
            vec![4.0, 4.0, 2.0]
        );
    }

    #[test]
    fn removes_short_positive_runs() {
        assert_eq!(
            optimize_intervals(&[0.0, 2.0, 0.0, 0.0, 3.0, 3.0, 3.0], 0.3),
            vec![0.0, 0.0, 0.0, 0.0, 3.0, 3.0, 3.0]
        );
    }
}
