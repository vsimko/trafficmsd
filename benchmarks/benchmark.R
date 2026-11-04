args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("usage: Rscript benchmarks/benchmark.R <trafficmsd-cli> <core-benchmark> <output-dir> [iterations]")
}
cli <- normalizePath(args[[1]], mustWork = TRUE)
core_benchmark <- normalizePath(args[[2]], mustWork = TRUE)
output_dir <- args[[3]]
iterations <- if (length(args) >= 4L) as.integer(args[[4]]) else 5L
if (is.na(iterations) || iterations < 1L) stop("iterations must be a positive integer")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required to write benchmark metadata")
}
if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) {
  stop("cannot create benchmark output directory")
}

library(trafficmsd)

make_signal <- function(n) {
  index <- 0:(n - 1L)
  as.numeric((index * 17) %% 101 + ifelse(index %% 257 < 13, 75, 0))
}

profile_allocated_bytes <- function(fun) {
  profile <- tempfile("trafficmsd-rprofmem-")
  on.exit(unlink(profile), add = TRUE)
  supported <- tryCatch({
    Rprofmem(profile)
    value <- fun()
    Rprofmem(NULL)
    value
  }, error = function(error) {
    try(Rprofmem(NULL), silent = TRUE)
    NULL
  })
  if (is.null(supported) || !file.exists(profile)) return(NA_real_)
  lines <- readLines(profile, warn = FALSE)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  sum(bytes, na.rm = TRUE)
}

elapsed_median <- function(fun, n) {
  values <- replicate(n, unname(system.time(fun())[["elapsed"]]))
  stats::median(values)
}

write_cli_config <- function(path, command, input, output, parameters) {
  config <- list(
    schema_version = 1L,
    command = command,
    input_csv = basename(input),
    output_csv = basename(output),
    signal = list(
      sampling_interval = 0.001,
      time_unit = "s",
      value_unit = "synthetic_units"
    ),
    parameters = parameters
  )
  jsonlite::write_json(
    config, path, auto_unbox = TRUE, pretty = TRUE,
    null = "null", digits = NA
  )
}

run_cli <- function(command, config_path) {
  output <- suppressWarnings(system2(
    cli,
    args = c(command, "--config", shQuote(config_path), "--force"),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) {
    stop(sprintf("CLI %s failed (exit %d): %s",
                 command, status, paste(output, collapse = "\n")))
  }
  invisible(output)
}

run_with_rss <- function(command, config_path = NULL, size = NULL) {
  time_bin <- Sys.which("time")
  rss_file <- tempfile("trafficmsd-rss-")
  on.exit(unlink(rss_file), add = TRUE)
  if (nzchar(time_bin)) {
    if (!is.null(config_path)) {
      args <- c("-f", "%M", "-o", shQuote(rss_file), shQuote(cli),
                command, "--config", shQuote(config_path), "--force")
    } else {
      args <- c("-f", "%M", "-o", shQuote(rss_file), shQuote(core_benchmark),
                as.character(size), as.character(iterations))
    }
    output <- suppressWarnings(system2(time_bin, args = args,
                                        stdout = TRUE, stderr = TRUE))
  } else if (!is.null(config_path)) {
    output <- suppressWarnings(system2(
      cli, c(command, "--config", shQuote(config_path), "--force"),
      stdout = TRUE, stderr = TRUE
    ))
  } else {
    output <- suppressWarnings(system2(
      core_benchmark, c(as.character(size), as.character(iterations)),
      stdout = TRUE, stderr = TRUE
    ))
  }
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) {
    stop("benchmark subprocess failed: ", paste(output, collapse = "\n"))
  }
  rss <- if (file.exists(rss_file)) {
    suppressWarnings(as.numeric(readLines(rss_file, warn = FALSE)[1L]))
  } else {
    NA_real_
  }
  list(output = output, max_rss_kb = rss)
}

all_results <- list()
sizes <- c(1024L, 8192L, 32768L)
for (size in sizes) {
  message("Benchmarking n=", size)
  signal <- make_signal(size)
  reference <- decompose_traffic(signal, cutoff.fraction = 0.004)

  r_decompose_time <- elapsed_median(
    function() decompose_traffic(signal, cutoff.fraction = 0.004),
    iterations
  )
  r_decompose_alloc <- profile_allocated_bytes(
    function() decompose_traffic(signal, cutoff.fraction = 0.004)
  )
  r_reconstruct_time <- elapsed_median(
    function() reconstruct_traffic(reference), iterations
  )
  r_reconstruct_alloc <- profile_allocated_bytes(
    function() reconstruct_traffic(reference)
  )

  work_dir <- tempfile("trafficmsd-benchmark-")
  if (!dir.create(work_dir)) stop("cannot create temporary benchmark directory")
  signal_path <- file.path(work_dir, "signal.csv")
  coefficients_path <- file.path(work_dir, "coefficients.csv")
  reconstructed_path <- file.path(work_dir, "reconstructed.csv")
  decompose_config <- file.path(work_dir, "decompose.json")
  reconstruct_config <- file.path(work_dir, "reconstruct.json")

  utils::write.csv(
    data.frame(sample_index = seq_along(signal) - 1L, value = signal),
    signal_path, row.names = FALSE, quote = TRUE
  )
  write_cli_config(
    decompose_config, "decompose", signal_path, coefficients_path,
    list(cutoff_fraction = 0.004)
  )
  cli_decompose_time <- elapsed_median(
    function() run_cli("decompose", decompose_config), iterations
  )
  cli_coefficients <- utils::read.csv(coefficients_path, check.names = FALSE)
  if (nrow(cli_coefficients) != nrow(reference$coef) * ncol(reference$coef)) {
    stop("CLI coefficient output has an unexpected row count")
  }
  cli_matrix <- matrix(
    cli_coefficients$coefficient,
    nrow = nrow(reference$coef), ncol = ncol(reference$coef),
    byrow = TRUE
  )
  if (!isTRUE(all.equal(cli_matrix, reference$coef, tolerance = 1e-12))) {
    stop("R and Rust CLI decomposition results differ at n=", size)
  }

  coefficient_rows <- data.frame(
    scale = rep(seq_len(nrow(reference$coef)) - 1L,
                each = ncol(reference$coef)),
    sample_index = rep(seq_len(ncol(reference$coef)) - 1L,
                       times = nrow(reference$coef)),
    coefficient = as.vector(t(reference$coef))
  )
  utils::write.csv(coefficient_rows, coefficients_path,
                   row.names = FALSE, quote = TRUE)
  write_cli_config(
    reconstruct_config, "reconstruct", coefficients_path,
    reconstructed_path, list(cutoff_fraction = NULL)
  )
  cli_reconstruct_time <- elapsed_median(
    function() run_cli("reconstruct", reconstruct_config), iterations
  )
  cli_signal <- utils::read.csv(reconstructed_path, check.names = FALSE)$value
  if (!isTRUE(all.equal(
    as.numeric(cli_signal), reconstruct_traffic(reference), tolerance = 1e-12
  ))) {
    stop("R and Rust CLI reconstruction results differ at n=", size)
  }

  core_measurement <- run_with_rss("core", size = size)
  core_table <- utils::read.csv(text = paste(core_measurement$output, collapse = "\n"))
  if (!all(c("operation", "size", "iterations", "total_ns", "mean_ns") %in%
           names(core_table))) {
    stop("Rust core benchmark returned an invalid CSV result")
  }
  core_decompose <- core_table[core_table$operation == "decompose", ]
  core_reconstruct <- core_table[core_table$operation == "reconstruct", ]

  cli_decompose_rss <- run_with_rss("decompose", decompose_config)$max_rss_kb
  cli_reconstruct_rss <- run_with_rss("reconstruct", reconstruct_config)$max_rss_kb

  all_results[[length(all_results) + 1L]] <- data.frame(
    size = size, operation = "decompose", iterations = iterations,
    r_median_seconds = r_decompose_time,
    r_allocated_bytes = r_decompose_alloc,
    rust_core_total_ns = core_decompose$total_ns,
    rust_core_mean_ns = core_decompose$mean_ns,
    rust_core_max_rss_kb = core_measurement$max_rss_kb,
    rust_cli_median_seconds = cli_decompose_time,
    rust_cli_max_rss_kb = cli_decompose_rss
  )
  all_results[[length(all_results) + 1L]] <- data.frame(
    size = size, operation = "reconstruct", iterations = iterations,
    r_median_seconds = r_reconstruct_time,
    r_allocated_bytes = r_reconstruct_alloc,
    rust_core_total_ns = core_reconstruct$total_ns,
    rust_core_mean_ns = core_reconstruct$mean_ns,
    rust_core_max_rss_kb = core_measurement$max_rss_kb,
    rust_cli_median_seconds = cli_reconstruct_time,
    rust_cli_max_rss_kb = cli_reconstruct_rss
  )
  unlink(work_dir, recursive = TRUE)
}

results <- do.call(rbind, all_results)
utils::write.csv(results, file.path(output_dir, "benchmark-results.csv"),
                 row.names = FALSE)
environment <- list(
  schema_version = 1L,
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  repository_commit = Sys.getenv("GITHUB_SHA", unset = NA_character_),
  os = Sys.info()[["sysname"]],
  machine = Sys.info()[["machine"]],
  r_version = R.version.string,
  rustc_version = paste(system2("rustc", "--version", stdout = TRUE),
                        collapse = " "),
  trafficmsd_version = as.character(utils::packageVersion("trafficmsd")),
  cutoff_fraction = 0.004,
  iterations = iterations,
  sizes = sizes,
  memory_notes = paste(
    "R allocated bytes are cumulative Rprofmem allocations for one operation.",
    "Rust and CLI RSS are maximum resident set sizes from GNU time where available.",
    "RSS includes process/runtime overhead; compare like-for-like only."
  ),
  results = results
)
jsonlite::write_json(
  environment, file.path(output_dir, "benchmark-results.json"),
  pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA
)
message("Wrote benchmark results to ", normalizePath(output_dir))
