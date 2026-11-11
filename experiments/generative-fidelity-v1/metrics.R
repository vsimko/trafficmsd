# Metrics for generative fidelity v1
# These functions compare independent generated realizations with one reference
# time series. They deliberately do not calculate pointwise reconstruction MAE.

read_series <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!identical(names(x), c("sample_index", "value")) ||
      nrow(x) < 16L ||
      any(!is.finite(x$value)) || any(x$value < 0) ||
      !identical(as.integer(x$sample_index), seq.int(0L, nrow(x) - 1L))) {
    stop("Expected CSV columns sample_index,value with >=16 contiguous finite non-negative samples: ", path,
         call. = FALSE)
  }
  as.numeric(x$value)
}

acf_at_lag <- function(x, lag) {
  centered <- x - mean(x)
  denominator <- sum(centered^2)
  if (denominator == 0) return(NA_real_)
  sum(centered[seq_len(length(x) - lag)] *
        centered[(lag + 1L):length(x)]) / denominator
}

run_lengths <- function(x, threshold) {
  runs <- rle(x > threshold)
  as.integer(runs$lengths[runs$values])
}

wasserstein_1 <- function(x, y) {
  if (length(x) != length(y)) stop("Wasserstein comparison requires equal sample counts.")
  mean(abs(sort(x) - sort(y)))
}

ks_distance <- function(x, y) {
  points <- sort(unique(c(x, y)))
  max(abs(vapply(points, function(v) mean(x <= v), numeric(1)) -
          vapply(points, function(v) mean(y <= v), numeric(1))))
}

optional_quantile <- function(x, probability) {
  if (length(x) == 0L) return(NULL)
  as.numeric(stats::quantile(x, probability, names = FALSE, type = 7))
}

optional_max <- function(x) {
  if (length(x) == 0L) return(NULL)
  as.integer(max(x))
}

series_summary <- function(x) {
  q <- stats::quantile(x, c(.50, .90, .95, .99), names = FALSE, type = 7)
  list(
    mean = mean(x),
    sd = stats::sd(x),
    cv = if (mean(x) == 0) NULL else stats::sd(x) / mean(x),
    quantiles = as.list(stats::setNames(as.numeric(q), c("p50", "p90", "p95", "p99"))),
    zero_fraction = mean(x == 0),
    total = sum(x)
  )
}

compare_realization <- function(reference, generated, lags = c(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L)) {
  if (length(reference) != length(generated))
    stop("Reference and generated series must have identical sample counts.")
  if (length(reference) < 16L || any(!is.finite(reference)) ||
      any(!is.finite(generated)) || any(reference < 0) || any(generated < 0))
    stop("Series must contain at least 16 finite non-negative samples.")

  eligible <- lags[lags < length(reference) / 4]
  ref_acf <- vapply(eligible, function(lag) acf_at_lag(reference, lag), numeric(1))
  gen_acf <- vapply(eligible, function(lag) acf_at_lag(generated, lag), numeric(1))
  acf_delta <- abs(ref_acf - gen_acf)
  acf_rows <- lapply(seq_along(eligible), function(i) list(
    lag = as.integer(eligible[[i]]),
    reference = unname(ref_acf[[i]]),
    generated = unname(gen_acf[[i]]),
    absolute_difference = unname(acf_delta[[i]])
  ))

  # The threshold is estimated once from the reference, never from a replicate.
  threshold <- as.numeric(stats::quantile(reference, .95, names = FALSE, type = 7))
  ref_runs <- run_lengths(reference, threshold)
  gen_runs <- run_lengths(generated, threshold)
  ref_summary <- series_summary(reference)
  gen_summary <- series_summary(generated)

  list(
    reference = ref_summary,
    generated = gen_summary,
    distribution = list(
      wasserstein_1 = wasserstein_1(reference, generated),
      ks_distance = ks_distance(reference, generated),
      absolute_mean_difference = abs(mean(generated) - mean(reference)),
      relative_mean_difference = if (mean(reference) == 0) NULL else
        abs(mean(generated) - mean(reference)) / abs(mean(reference)),
      absolute_sd_difference = abs(stats::sd(generated) - stats::sd(reference)),
      absolute_quantile_differences = Map(function(a, b) abs(a - b),
        ref_summary$quantiles, gen_summary$quantiles)
    ),
    temporal = list(
      acf_differences = acf_rows,
      mean_absolute_acf_difference = if (any(!is.finite(acf_delta))) NULL else
        mean(acf_delta)
    ),
    bursts = list(
      threshold_from_reference_p95 = threshold,
      reference_exceedance_fraction = mean(reference > threshold),
      generated_exceedance_fraction = mean(generated > threshold),
      absolute_exceedance_fraction_difference =
        abs(mean(generated > threshold) - mean(reference > threshold)),
      reference_run_count = length(ref_runs),
      generated_run_count = length(gen_runs),
      reference_run_median = optional_quantile(ref_runs, .50),
      generated_run_median = optional_quantile(gen_runs, .50),
      reference_run_p95 = optional_quantile(ref_runs, .95),
      generated_run_p95 = optional_quantile(gen_runs, .95),
      reference_run_max = optional_max(ref_runs),
      generated_run_max = optional_max(gen_runs)
    )
  )
}

summarize_replicates <- function(results) {
  fields <- c("wasserstein_1", "ks_distance", "relative_mean_difference",
              "absolute_sd_difference", "mean_absolute_acf_difference",
              "absolute_exceedance_fraction_difference")
  values <- lapply(fields, function(field) {
    vapply(results, function(result) {
      section <- if (field == "mean_absolute_acf_difference") result$temporal else
        if (field == "absolute_exceedance_fraction_difference") result$bursts else
          result$distribution
      value <- section[[field]]
      if (is.null(value)) NA_real_ else as.numeric(value)
    }, numeric(1))
  })
  names(values) <- fields
  lapply(values, function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(list(n = 0L, median = NULL, p05 = NULL, p95 = NULL, iqr = NULL))
    list(n = length(x), median = stats::median(x),
         p05 = as.numeric(stats::quantile(x, .05, names = FALSE, type = 7)),
         p95 = as.numeric(stats::quantile(x, .95, names = FALSE, type = 7)),
         iqr = stats::IQR(x))
  })
}
