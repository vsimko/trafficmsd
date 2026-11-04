\name{decompose_traffic_rust}
\alias{decompose_traffic_rust}
\title{Decompose a Signal with the Rust CLI}
\usage{
decompose_traffic_rust(ts, cutoff.fraction = 0.004,
  sampling_interval = 1, time_unit = "s", value_unit = "unknown")
}
\arguments{
\item{ts}{Finite, non-negative numeric vector with power-of-two length.}
\item{cutoff.fraction}{Interval optimization fraction in (0, 1).}
\item{sampling_interval}{Positive sample interval in \code{time_unit}.}
\item{time_unit}{One of \code{"s"}, \code{"ms"}, \code{"us"}, or \code{"ns"}.}
\item{value_unit}{Non-empty unit label; use \code{"unknown"} if not verified.}
}
\value{
A list containing \code{length}, \code{maxscale}, \code{cutoff.fraction},
and the coefficient matrix \code{coef}.
}
\description{
Runs the standalone \code{trafficmsd} executable. The executable is discovered
through the \code{TRAFFICMSD_CLI} environment variable or \code{PATH}.
The Rust engine requires finite, non-negative input, at least 16 observations,
and a power-of-two length.
}
