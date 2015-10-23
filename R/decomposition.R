#' Reads traffic data from a CSV file.
#' 
#' The input file should be a CSV files with the first row containing column names.
#' The file should contain at least two columns: Time, Length
#' 
#' @param filename The traffic data will be read from this file.
#' @param use.power2len Length padded with zeroes to power of 2.
#' @param use.expfactor How should the time be stretched (for debugging)
#' @param aggregation_function Function to be applied on all samples that
#'   correspond to the same second.
#' @return Time-series object which contains the regularized time series.
#' 
#' @import zoo
#' @export
read_traffic <- function(filename,
                         use.power2len = TRUE,
                         use.expfactor = 1,
                         aggregation_function = sum
                         ) {
  
  # preconditions
  assert_that(is.character(filename))
  assert_that(file.exists(filename))
  assert_that(is.logical(use.power2len))
  assert_that(use.expfactor > 0)
  
  traffic <- read.csv(filename)
  
  # hack that should avoid duplicate keys (1000 duplicates max.)
  index.hack = (10 ** -9) * index(traffic$Time) %% 1000
  
  z <- zoo( traffic$Length,
            order.by = as.numeric(traffic$Time) * use.expfactor + index.hack )
  
  # possible aggregation are: mean / max / sum
  rz <- aggregate(z, as.integer(index(z)), regular = TRUE, aggregation_function) # this must be sum!!!!
  
  # regularize the time series
  ts <- as.ts(rz)
  
  # compute the length
  ts.len <- (end(ts) - start(ts))[1] + 1
  
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
#' @param ts Input vector which represents regularized time-series.
#' @param cutoff.fraction How strongly to optimize the coeficients.
#' @return A structure \code{list(length, maxscale, coef)}
#' @export
decompose_traffic <- function(ts, cutoff.fraction=0.004) {
  
  # some assertions about input parameters
  assert_that(is.vector(ts) || is.ts(ts) || is.zoo(ts))
  assert_that(length(ts) >= 16)
  assert_that(cutoff.fraction > 0)
  assert_that(cutoff.fraction < 1)
  
  ts.len <- length(ts)
  ts.maxscale <- log2(ts.len)
  
  # scales 1..n stored in rows 2..n+1
  # remainder will be stored as row 1
  COEF <- matrix(data = 0, nrow = ts.maxscale + 1, ncol = ts.len)
  
  # this vector will be changed during the decomposition
  # the remaining data will be scale 0
  x <- as.integer(ts)
  
  for (scale in seq(from = ts.maxscale, to = 1)) {
    wlen <- 2 ** scale
    wtimes <- ts.len / wlen
    #print(c(s, wlen, wtimes))
    
    
    # prepare a vector of candidte hits
    candidates <- integer(length = wtimes)
    for(i in 1:wtimes) {
      
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
    coef     = COEF
  )
}

#' Pad the signal with zeroes to the nearest higher power of two.
#' @param ts Input vector of numbers (preferably integers)
#' @return New vector that is padded with zeroes
#' @export
pad_power2 <- function(ts) {
  
  # preconditions
  assert_that(is.vector(ts) || is.ts(ts))
  assert_that(length(ts) >= 16)
  
  # original length of the time series
  ts.len <- length(ts)
  
  # compute the nearest larger power of 2
  ts.p2len <- 2 ** as.integer(log2(ts.len - 1) + 1)
  
  # extend to power of 2 length
  ts <- ts[1:ts.p2len]
  
  # replace NA with 0
  ts[is.na(ts)] <- 0L
  
  # print.log(paste("Changed the length from", ts.len, "to", ts.p2len, "by padding with 0 at the end."))
  
  # postconditions
  assert_that( length(ts) >= 16 )
  assert_that( log2(length(ts))%%1 == 0 ) # log2 should be round number
  return(ts)
}

#' Count non-empty intervals in a vector.
#' @param vec Input vector of numbers.
#' @return The number of intervals found inside the vector.
#' @export
get_interval_count <- function(vec) {

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
estimate_clusters <- function(ts, max.cl = 25, cutoff = 0.1) {

  # preconditions
  stopifnot(cutoff > 0, max.cl > 2, max.cl < 30)
  
  max.clusters <- min(max.cl, length(unique(ts)) )
  if (max.cl < 2 || length(ts) <= 16) {
    return(1)
  }
  
  # wss <- integer(length = max.cl)
  wss <- integer(length = max.clusters)
  
  # for (i in 2:max.cl) {
  for (i in 2:max.clusters) {
    fit = kmeans(ts, centers=i)
    wss[i] <- sum(fit$withinss)
  }
  
  # normalize wss
  wss <- wss / max(wss)
  
  # assuming the sequence is monotonically decreasing
  length(wss[wss > cutoff])
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
cluster_by_amplitude <- function(ts, num.cl) {
  
  ts.len <- length(unique(ts))
  num.cl <- min(num.cl, ts.len)
  
  out <- list()
  out$data   <- ts
  out$num.cl <- num.cl
  
  out$cfit <- kmeans(out$data, num.cl)  # K-Means Cluster Analysis
  out$cfit$centers <- as.integer(out$cfit$centers)  # packet sizes are integers
  
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
optimize_intervals <- function(x, cutoff.fraction=0.004) {
  
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
    
    for(i in x.len:1) {
      y[i] <- bx[i] * (y[i+1] + bx[i])
    }
    
    y <- y[1:x.len]
    
    for(i in 2:x.len) {
      y[i] <- ifelse(y[i] == 0, 0, max(y[i], y[i-1]))
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
