\name{reconstruct_traffic_rust}
\alias{reconstruct_traffic_rust}
\title{Reconstruct a Signal with the Rust CLI}
\usage{
reconstruct_traffic_rust(D, sampling_interval = 1,
  time_unit = "s", value_unit = "unknown")
}
\arguments{
\item{D}{Coefficient matrix or decomposition object containing \code{coef}.}
\item{sampling_interval}{Positive sample interval in \code{time_unit}.}
\item{time_unit}{One of \code{"s"}, \code{"ms"}, \code{"us"}, or \code{"ns"}.}
\item{value_unit}{Non-empty unit label; use \code{"unknown"} if not verified.}
}
\value{
A reconstructed finite numeric vector.
}
\description{
Runs the standalone \code{trafficmsd} executable. The executable is discovered
through the \code{TRAFFICMSD_CLI} environment variable or \code{PATH}. This
function accepts the same matrix or decomposition-object input shape as
\code{reconstruct_traffic}.
}
