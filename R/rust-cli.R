# Rust CLI adapter ---------------------------------------------------------

.trafficmsd_cli_path <- function() {
  override <- Sys.getenv("TRAFFICMSD_CLI", unset = "")
  if (nzchar(override)) {
    if (!file.exists(override) || dir.exists(override)) {
      stop("TRAFFICMSD_CLI does not name an existing executable file.", call. = FALSE)
    }
    if (.Platform$OS.type != "windows" && file.access(override, 1L) != 0L) {
      stop("TRAFFICMSD_CLI is not executable by the current user.", call. = FALSE)
    }
    return(normalizePath(override, winslash = "/", mustWork = TRUE))
  }
  executable <- Sys.which("trafficmsd")
  if (!nzchar(executable)) {
    stop("TrafficMSD Rust CLI was not found. Put trafficmsd on PATH or set TRAFFICMSD_CLI.",
         call. = FALSE)
  }
  executable <- unname(executable)
  version_output <- suppressWarnings(
    system2(executable, args = "--version", stdout = TRUE, stderr = TRUE)
  )
  status <- attr(version_output, "status")
  expected <- paste0("trafficmsd ", as.character(utils::packageVersion("trafficmsd")))
  if ((!is.null(status) && status != 0L) ||
      !any(trimws(version_output) == expected)) {
    reported <- paste(version_output, collapse = " ")
    stop(sprintf(
      "Incompatible TrafficMSD CLI. The R package requires '%s', but the executable reported '%s'. Install the matching CLI release.",
      expected, reported
    ), call. = FALSE)
  }
  executable
}

.trafficmsd_cli_metadata <- function(sampling_interval, time_unit, value_unit) {
  if (length(sampling_interval) != 1L || !is.numeric(sampling_interval) ||
      !is.finite(sampling_interval) || sampling_interval <= 0) {
    stop("sampling_interval must be a finite positive number.", call. = FALSE)
  }
  if (!is.character(time_unit) || length(time_unit) != 1L || is.na(time_unit) ||
      !time_unit %in% c("s", "ms", "us", "ns")) {
    stop("time_unit must be one of s, ms, us, or ns.", call. = FALSE)
  }
  if (!is.character(value_unit) || length(value_unit) != 1L || is.na(value_unit) ||
      !nzchar(trimws(value_unit))) {
    stop("value_unit must be a non-empty string.", call. = FALSE)
  }
  list(sampling_interval = as.numeric(sampling_interval),
       time_unit = time_unit, value_unit = value_unit)
}

.trafficmsd_cli_call <- function(command, input, output, metadata, parameters) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required for the Rust CLI adapter.", call. = FALSE)
  }
  executable <- .trafficmsd_cli_path()
  work_dir <- dirname(input)
  config_path <- file.path(work_dir, "trafficmsd-config.json")
  config <- list(
    schema_version = 1L, command = command,
    input_csv = basename(input), output_csv = basename(output),
    signal = metadata, parameters = parameters
  )
  jsonlite::write_json(config, config_path, auto_unbox = TRUE,
                       pretty = TRUE, null = "null", digits = NA)
  args <- c(command, "--config", shQuote(config_path))
  output_text <- suppressWarnings(
    system2(executable, args = args, stdout = TRUE, stderr = TRUE)
  )
  status <- attr(output_text, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) {
    detail <- paste(output_text, collapse = "\n")
    if (!nzchar(detail)) detail <- "no diagnostic output"
    stop(sprintf("TrafficMSD CLI command '%s' failed (exit %d):\n%s",
                 command, status, detail), call. = FALSE)
  }
  if (!file.exists(output)) {
    stop(sprintf("TrafficMSD CLI command '%s' succeeded but did not create its output file.",
                 command), call. = FALSE)
  }
  invisible(output)
}

#' Decompose a signal using the standalone Rust engine
#'
#' Runs the trafficmsd executable and returns the same list structure as
#' decompose_traffic(). The Rust engine requires finite, non-negative input,
#' at least 16 observations, and a power-of-two length. The executable is
#' discovered through TRAFFICMSD_CLI or PATH.
#'
#' @param ts Finite, non-negative numeric vector with power-of-two length.
#' @param cutoff.fraction Interval optimization fraction in (0, 1).
#' @param sampling_interval Positive sample interval in time_unit.
#' @param time_unit One of s, ms, us, or ns.
#' @param value_unit Non-empty unit label; use unknown if not verified.
#' @return A list containing length, maxscale, cutoff.fraction, and coef.
#' @export
decompose_traffic_rust <- function(ts, cutoff.fraction = 0.004,
                                   sampling_interval = 1,
                                   time_unit = "s", value_unit = "unknown") {
  .validate_signal(ts, min_length = 16L)
  if (any(ts < 0)) stop("ts must be non-negative for the Rust engine.", call. = FALSE)
  if (length(ts) != 2^round(log2(length(ts)))) {
    stop("ts length must be a power of two.", call. = FALSE)
  }
  if (length(cutoff.fraction) != 1L || !is.numeric(cutoff.fraction) ||
      !is.finite(cutoff.fraction) || cutoff.fraction <= 0 ||
      cutoff.fraction >= 1) {
    stop("cutoff.fraction must be finite and between 0 and 1.", call. = FALSE)
  }
  metadata <- .trafficmsd_cli_metadata(sampling_interval, time_unit, value_unit)
  work_dir <- tempfile("trafficmsd-rust-")
  if (!dir.create(work_dir)) stop("Cannot create temporary directory for the Rust CLI.",
                                  call. = FALSE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  input <- file.path(work_dir, "signal.csv")
  output <- file.path(work_dir, "coefficients.csv")
  utils::write.csv(
    data.frame(sample_index = seq_along(ts) - 1L, value = as.numeric(ts)),
    input, row.names = FALSE, quote = TRUE
  )
  .trafficmsd_cli_call("decompose", input, output, metadata,
                       list(cutoff_fraction = as.numeric(cutoff.fraction)))
  coefficients <- utils::read.csv(output, check.names = FALSE)
  expected_rows <- (log2(length(ts)) + 1L) * length(ts)
  if (nrow(coefficients) != expected_rows ||
      !identical(names(coefficients),
                 c("scale", "sample_index", "coefficient"))) {
    stop("Rust CLI returned an invalid coefficient CSV.", call. = FALSE)
  }
  coef <- matrix(0, nrow = log2(length(ts)) + 1L, ncol = length(ts))
  coef[cbind(coefficients$scale + 1L,
             coefficients$sample_index + 1L)] <- coefficients$coefficient
  list(length = length(ts), maxscale = log2(length(ts)),
       cutoff.fraction = cutoff.fraction, coef = coef)
}

#' Reconstruct a signal using the standalone Rust engine
#'
#' Accepts a coefficient matrix or an object containing coef, matching
#' reconstruct_traffic(). The executable is discovered through TRAFFICMSD_CLI
#' or PATH.
#'
#' @param D Coefficient matrix or decomposition object containing coef.
#' @param sampling_interval Positive sample interval in time_unit.
#' @param time_unit One of s, ms, us, or ns.
#' @param value_unit Non-empty unit label; use unknown if not verified.
#' @return Reconstructed finite numeric vector.
#' @export
reconstruct_traffic_rust <- function(D, sampling_interval = 1,
                                     time_unit = "s", value_unit = "unknown") {
  if (!is.matrix(D)) D <- D$coef
  if (!is.matrix(D) || !is.numeric(D) || nrow(D) < 2L ||
      ncol(D) < 2L || ncol(D) != 2^round(log2(ncol(D))) ||
      anyNA(D) || any(!is.finite(D)) || any(D < 0)) {
    stop("D must be a finite, non-negative coefficient matrix with at least two rows and power-of-two columns.",
         call. = FALSE)
  }
  metadata <- .trafficmsd_cli_metadata(sampling_interval, time_unit, value_unit)
  work_dir <- tempfile("trafficmsd-rust-")
  if (!dir.create(work_dir)) stop("Cannot create temporary directory for the Rust CLI.",
                                  call. = FALSE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  input <- file.path(work_dir, "coefficients.csv")
  output <- file.path(work_dir, "signal.csv")
  coefficient_rows <- data.frame(
    scale = rep(seq_len(nrow(D)) - 1L, each = ncol(D)),
    sample_index = rep(seq_len(ncol(D)) - 1L, times = nrow(D)),
    coefficient = as.vector(t(D))
  )
  utils::write.csv(coefficient_rows, input, row.names = FALSE, quote = TRUE)
  .trafficmsd_cli_call("reconstruct", input, output, metadata,
                       list(cutoff_fraction = NULL))
  signal <- utils::read.csv(output, check.names = FALSE)
  if (!identical(names(signal), c("sample_index", "value")) ||
      nrow(signal) != ncol(D) ||
      !identical(signal$sample_index, seq_len(ncol(D)) - 1L) ||
      any(!is.finite(signal$value))) {
    stop("Rust CLI returned an invalid reconstructed signal CSV.", call. = FALSE)
  }
  as.numeric(signal$value)
}
