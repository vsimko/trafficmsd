\name{install_trafficmsd_cli}
\alias{install_trafficmsd_cli}
\title{Install the Prebuilt TrafficMSD Rust CLI}
\usage{
install_trafficmsd_cli(version = "latest",
  repo = "vsimko/trafficmsd", cache_dir = NULL, force = FALSE)
}
\arguments{
\item{version}{Release version such as \code{"0.1.0"}, or \code{"latest"}.}
\item{repo}{GitHub repository in \code{owner/name} form.}
\item{cache_dir}{Optional user-writable cache directory.}
\item{force}{If \code{TRUE}, download and replace an existing cached executable.}
}
\value{
The absolute path to the installed executable, returned invisibly.
}
\description{
Downloads the matching release binary for the current operating system and
architecture, verifies its SHA-256 checksum against the release manifest,
stores it in the user's cache, and sets \code{TRAFFICMSD_CLI} for the current
R session. No Rust toolchain or administrator permissions are required.
A published GitHub Release with binary assets is required.
}
