# Internal helpers ---------------------------------------------------------

.validate_signal <- function(x, name = "ts", min_length = 1L) {
  if (!(is.numeric(x) && is.null(dim(x)) && is.atomic(x))) {
    stop(sprintf("`%s` must be a numeric vector or time-series.", name), call. = FALSE)
  }
  if (length(x) < min_length) {
    stop(sprintf("`%s` must contain at least %d observations.", name, min_length), call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("`%s` must contain only finite, non-missing values.", name), call. = FALSE)
  }
  invisible(x)
}

.with_local_seed <- function(seed, expr) {
  if (is.null(seed)) return(force(expr))
  if (length(seed) != 1L || !is.numeric(seed) || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed != as.integer(seed)) {
    stop("`seed` must be NULL or one non-negative integer.", call. = FALSE)
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(expr)
}

#' Reads traffic data from a CSV file.
#' 
#' The input file should be a CSV files with the first row containing column names.
#' The file must contain numeric \\code{Time} and \\code{Length} columns.
#' Missing, non-finite, or non-numeric values are rejected. When
#' \\code{use.power2len = TRUE}, zeroes are appended to the next power-of-two
#' length; observations are never truncated.
#' 
#' @param filename Path to an existing CSV file.
#' @param use.power2len If TRUE, append zeroes to reach a power-of-two length.
#' @param use.expfactor Positive finite factor by which timestamps are stretched.
#' @param aggregation_function Function applied to samples corresponding to
#'   the same second.
#' @return A regularized time-series object.
#' 
#' @import zoo
#' @export
read_traffic <- function(filename,
                         use.power2len = TRUE,
                         use.expfactor = 1,
                         aggregation_function = sum
                         ) {

  # preconditions
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !file.exists(filename)) {
    stop("`filename` must name an existing CSV file.", call. = FALSE)
  }
  if (!is.logical(use.power2len) || length(use.power2len) != 1L ||
      is.na(use.power2len)) {
    stop("`use.power2len` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(use.expfactor) || length(use.expfactor) != 1L ||
      !is.finite(use.expfactor) || use.expfactor <= 0) {
    stop("`use.expfactor` must be a finite positive number.", call. = FALSE)
  }
  if (!is.function(aggregation_function)) {
    stop("`aggregation_function` must be a function.", call. = FALSE)
  }

  traffic <- read.csv(filename)
  required_columns <- c("Time", "Length")
  if (!all(required_columns %in% names(traffic))) {
    stop("CSV input must contain Time and Length columns.", call. = FALSE)
  }
  .validate_signal(traffic$Time, "Time")
  .validate_signal(traffic$Length, "Length")
  if (length(traffic$Time) != length(traffic$Length)) {
    stop("Time and Length columns must have equal lengths.", call. = FALSE)
  }

  # hack that should avoid duplicate keys (1000 duplicates max.)
  index.hack <- (10 ** -9) * index(traffic$Time) %% 1000

  z <- zoo( traffic$Length,
            order.by = as.numeric(traffic$Time) * use.expfactor + index.hack )

  # possible aggregation are: mean / max / sum
  rz <- aggregate(z, as.integer(index(z)), regular = TRUE,
                  aggregation_function) # this must be sum!!!!

  # regularize the time series
  ts <- as.ts(rz)

  if(use.power2len) {
    ts <- pad_power2(ts)
  }

  # replace NA with 0
  ts[is.na(ts)] <- 0L

  # posconditions
  assert_that(length(ts) > 0)

  return(ts)
}

#' Multi-scale decomposition of time series.
#' (most important function in this package)
#' 
#' The result is a structure \code{list(length, maxscale, coef)} where:
#' \describe{
#'  \item{length}{Size of the input vector (number of samples)}
#'  \item{maxscale}{What is the maximum scale produced by the decomposition.}
#'  \item{coef}{
#'    The output matrix containing the decomposition coeficients.
#'    Rows represent scales (row 1 represents scale 0).
#'    Columns represent time (sample with index same as column number).
#'  }
#' }
#'            
#' @param ts Finite numeric vector representing a regularized time series.
#'   At least 16 observations are required and the length must be a power of two.
#' @param cutoff.fraction Fraction used to optimize intervals; must be between
#'   zero and one (exclusive).
#' @details Non-power-of-two inputs are rejected. Use \\code{pad_power2()} to
#'   append zeroes explicitly before decomposition.
#' @return A structure \code{list(length, maxscale, coef)}
#' @export
decompose_traffic <- function(ts, cutoff.fraction = 0.004) {

  # some assertions about input parameters
  .validate_signal(ts, min_length = 16L)
  if (length(ts) != 2^round(log2(length(ts)))) {
    stop("`ts` length must be a power of two; use pad_power2() explicitly.", call. = FALSE)
  }
  if (length(cutoff.fraction) != 1L || !is.numeric(cutoff.fraction) ||
      !is.finite(cutoff.fraction) || cutoff.fraction <= 0 || cutoff.fraction >= 1) {
    stop("`cutoff.fraction` must be a finite number between 0 and 1.", call. = FALSE)
  }

  ts.len <- length(ts)
  ts.maxscale <- log2(ts.len)

  # scales 1..n stored in rows 2..n+1
  # remainder will be stored as row 1
  COEF <- matrix(data = 0, nrow = ts.maxscale + 1, ncol = ts.len)

  # this vector will be changed during the decomposition
  # the remaining data will be scale 0
  x <- as.numeric(ts)

  for (scale in seq(from = ts.maxscale, to = 1)) {
    wlen <- 2 ** scale
    wtimes <- ts.len / wlen

    # prepare a vector of candidte hits
    candidates <- numeric(length = wtimes)
    for (i in 1:wtimes) {

      begin <- (i - 1) * wlen + 1
      end   <- begin + wlen - 1
      subx  <- x[begin:end] # this is the interval to be processed

      # We look inside the interval and ask, which value appears only once.
      # Therefore, all values are sorted and lengths of runs of equal value
      # is comupted useing rle() function.
      # decreasing = TRUE is necessary later
      counts  <- rle(sort(subx, decreasing = TRUE))

      # now, we can pick the values which appear only once inside the interval
      counts1 <- counts$values[counts$lengths == 1] #

      # If there are multiple values that appear only once inside the interval
      # we pick the largest value as a candidate
      # (therefore the vector must be sorted: decreasing = TRUE)
      candidates[i] <- counts1[1]
    }

    # fill NA with 0
    candidates[is.na(candidates)] <- 0

    # optimize the vector of candidate hits
    if(cutoff.fraction > 0) {
      candidates <- optimize_intervals(candidates, cutoff.fraction)
    }

    # update the coeficients using the updated candidates
    for (i in 1:wtimes) {

      begin <- (i - 1) * wlen + 1
      end   <- begin + wlen - 1
      subx  <- x[begin:end]

      c <- candidates[i]
      if (c > 0) {

        subx.pos <- match(c, subx)

        COEF[scale + 1, begin:end] <- c

        # remove from x
        x.pos <- begin + subx.pos - 1
        x[x.pos] <- 0

        # Also remove the value from the "emit" position where the generator
        # will actually emit its packet. This is necessary, because the packet
        # will be unnecessarily duplicated by the generator at scale 0.
        x.emitpos    <- begin + as.integer(wlen / 2) - 1
        x[x.emitpos] <- max(0, x[x.emitpos] - c)
      }
    }
  }

  # remainder is stored on row #1
  COEF[1, ] <- optimize_intervals(x, cutoff.fraction)

  # Postconditions
  are_equal(ts.len, length(ts))

  # this list structure will be returned
  list(
    length   = ts.len,
    maxscale = ts.maxscale,
    cutoff.fraction = cutoff.fraction,
    coef     = COEF
  )
}

#' Pad the signal with zeroes to the next power-of-two length.
#' @param ts Finite numeric vector with at least 16 observations.
#' @return A numeric vector with zeroes appended as needed. Existing values
#'   are preserved and no observations are truncated.
#' @export
pad_power2 <- function(ts) {

  # preconditions
  .validate_signal(ts, min_length = 16L)

  # original length of the time series
  ts.len <- length(ts)

  # compute the nearest larger power of 2
  ts.p2len <- 2 ** as.integer(log2(ts.len - 1) + 1)

  # extend to power of 2 length
  if (ts.p2len > ts.len) {
    ts <- c(ts, rep(0, ts.p2len - ts.len))
  }

  # postconditions
  assert_that( length(ts) >= 16 )
  assert_that( log2(length(ts)) %% 1 == 0 ) # log2 should be round number
  return(ts)
}

#' Count non-empty intervals in a vector.
#' @param vec Input vector of numbers.
#' @return The number of intervals found inside the vector.
#' @export
get_interval_count <- function(vec) {

  if (length(vec) == 0L) return(0L)

  count <- 0
  last  <- 0

  for (i in 1:length(vec)) {
    if (last != vec[i] && vec[i] != 0) {
      count <- count + 1
    }
    last <- vec[i]
  }
  return(count)
}

#' Estimate number of clusters.
#' 
#' @param ts Input vector of numbers (preferably integers), can also be a 
#'   Time-series object created using \code{ts()} function.
#' @param max.cl The maximum number of clusters that the algorithm evaluates.
#' @param cutoff Determines how the right number of clusters is chosen.
#' @return The estimated number of clusters. (More clusters will have
#'   \code{sum(witness) < cutoff})
#'   
#' @export
estimate_clusters <- function(ts, max.cl = 25, cutoff = 0.1,
                              seed = 1L, nstart = 25L) {

  .validate_signal(ts, min_length = 1L)
  if (length(max.cl) != 1L || !is.numeric(max.cl) || !is.finite(max.cl) ||
      max.cl != as.integer(max.cl) || max.cl <= 2 || max.cl >= 30) {
    stop("`max.cl` must be an integer between 3 and 29.", call. = FALSE)
  }
  if (length(cutoff) != 1L || !is.numeric(cutoff) ||
      !is.finite(cutoff) || cutoff <= 0 || cutoff >= 1) {
    stop("`cutoff` must be a finite number between 0 and 1.", call. = FALSE)
  }
  if (length(nstart) != 1L || !is.numeric(nstart) || !is.finite(nstart) ||
      nstart != as.integer(nstart) || nstart < 1) {
    stop("`nstart` must be a positive integer.", call. = FALSE)
  }

  distinct <- length(unique(ts))
  if (distinct <= 1L || length(ts) <= 16L) return(1L)
  max.clusters <- min(as.integer(max.cl), distinct, length(ts) - 1L)
  if (max.clusters < 2L) return(1L)

  wss <- rep(0, max.clusters)
  .with_local_seed(seed, {
    for (i in 2:max.clusters) {
      fit <- stats::kmeans(ts, centers = i, nstart = as.integer(nstart))
      wss[i] <- sum(fit$withinss)
    }
  })

  max_wss <- max(wss)
  if (!is.finite(max_wss) || max_wss <= 0) return(1L)
  wss <- wss / max_wss
  max(1L, length(wss[wss > cutoff]))
}

#' Cluster the time series according to the y-axis using k-means algorithm.
#'
#' @param ts Input vector or a time-series object to be clustered.
#' @param num.cl Number of clusters (e.g. estimated by \link{estimate_clusters} function)
#' @return A structure \code{list(data, num.cl, cfit, thdata, abserror, MAE)} where:
#' \describe{
#'  \item{data}{Copied input parameter \code{ts}.}
#'  \item{num.cl}{Copied input parameter \code{num.cl}.}
#'  \item{cfit}{Result from the \link{kmeans} function.}
#'  \item{thdata}{The new time-series object with values replaced with the cluster centers.}
#'  \item{abserror}{A vector of absolute errors, i.e., \code{abs(data - thdata)}}
#'  \item{MAE}{Mean of all absolute errors}
#' }
#' 
#' @import zoo
#' @export
cluster_by_amplitude <- function(ts, num.cl, seed = 1L, nstart = 25L) {

  .validate_signal(ts, min_length = 1L)
  if (length(num.cl) != 1L || !is.numeric(num.cl) || !is.finite(num.cl) ||
      num.cl != as.integer(num.cl) || num.cl < 1) {
    stop("`num.cl` must be a positive integer.", call. = FALSE)
  }
  if (length(nstart) != 1L || !is.numeric(nstart) || !is.finite(nstart) ||
      nstart != as.integer(nstart) || nstart < 1) {
    stop("`nstart` must be a positive integer.", call. = FALSE)
  }

  ts.len <- length(unique(ts))
  num.cl <- min(as.integer(num.cl), ts.len)

  out <- list()
  out$data   <- ts
  out$num.cl <- num.cl

  out$cfit <- .with_local_seed(seed, {
    stats::kmeans(out$data, centers = num.cl, nstart = as.integer(nstart))
  })
  # Keep centers numeric: coercing to integer would silently lose precision.

  out$thdata <- out$data

  # discretizing the data according to the clusters
  for (c in 1:num.cl) {
    out$thdata[ out$cfit$cluster == c ] <- out$cfit$centers[c]
  }

  out$abserrors <- abs(out$data - out$thdata)
  out$MAE       <- mean(out$abserrors)

  assert_that(out$MAE >= 0)
  assert_that(length(out$abserrors) == length(out$data))
  return(out)
}

#' Optimization used as a part of the MSD algorithm.
#' @param x Input Vector
#' @param cutoff.fraction How strongly to optimize the coeficients.
#' @return Optimized version of the input vector.
optimize_intervals <- function(x, cutoff.fraction = 0.004) {

  # preconditions
  assert_that(is.vector(x))
  assert_that(cutoff.fraction > 0)
  assert_that(cutoff.fraction < 1)

  x.len <- length(x)
  if (x.len < 3) {
    return(x)
  }

  cutoff <- x.len * cutoff.fraction

  if (cutoff > 1) {
    # check surroundings and remove small intervals (keep sizes >= 3)
    # this prevents isolated peaks to become coeficients at higher scales
    bx <- 1 * (x > 0)
    y <- integer(length = x.len + 1)

    for (i in x.len:1) {
      y[i] <- bx[i] * (y[i + 1] + bx[i])
    }

    y <- y[1:x.len]

    for (i in 2:x.len) {
      y[i] <- ifelse(y[i] == 0, 0, max(y[i], y[i - 1]))
    }

    x <- (y > cutoff) * x
  }

  # interpolate small gaps |aba| -> |aaa|
  for (i in 2:(x.len - 1)) {

    a <- x[i - 1]
    b <- x[i]
    c <- x[i + 1]

    ac.max <- max(a, c)
    ac.min <- min(a, c)

    if (b < ac.min) {
      x[i] <- ac.min
    } else if (b > ac.min) {
      x[i] <- ac.max
    }
  }

  return(x)
}
