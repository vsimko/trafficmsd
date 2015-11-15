#' Simulate the reconstruction of the original signal.
#' 
#' Simulate the reconstruction of the original signal by using the matrix of 
#' decomposed coeficients. The matrix needs to be filtered in order to simulate 
#' the "generators" emitting the packets. Let's assume a generator on
#' \code{scale=3}. It would emit a packet every 8 samples. However, the matrix
#' will contain 8 coeficients for every packet that should be emitted. Therefore
#' the input matrix needs to be filtered.
#'
#' @param D Either the matrix of coeficients or a \code{list(coef)} that contains the
#'       matrix as a \code{coef}.
#'
#' @return Vector representing the orignal signal in a form of regularized time
#'   series.
#'   
#' @export
reconstruct_traffic <- function(D) {
  # For convenience...
  if (!is.matrix(D)) {
    D <- D$coef
  }

  # reconstruct the original signal
  C <- compute_emits(D)
  C[is.na(C)] <- 0 # remove NA values

  return(colSums(C))
}

#' Count non-empty intervals in the matrix of coeficients.
#' @param coef Input matrix, where each row represent a single vector of coeficients.
#' @return The number of intervals found.
#' @export
get_total_intervals <- function(coef) {

  # preconditions
  assert_that(is.matrix(coef))
  assert_that(nrow(coef) > 1)
  assert_that(ncol(coef) > 1)

  total <- 0
  for(row in 1:nrow(coef)) {
    total <- total + get_interval_count(coef[row, ])
  }

  return(total)
}

#' Print statistics about the multiscale decomposition
#' @param D Matrix containing coeficients.
#' @export
print_stats <- function(D) {

  if (!is.matrix(D)) {
    D <- D$coef
  }

  cat(sep = "\n",
      paste("Length of the time series:", ncol(D)),
      paste("Number of scales:", nrow(D)),
      paste("Number of intervals per scale:")
  )

  # counting the intervals
  total.count <- 0
  for(row in 1:nrow(D)) {

    icount <- get_interval_count(D[row, ])
    total.count <- total.count + icount

    scale <- row - 1
    if (scale == 0) {
      scale <- paste( scale, "(remainder)" )
    }
    cat(" - scale ", scale, " : ", icount, " intervals\n")
  }
  cat("Total number of intervals: ", total.count, "\n")
}
