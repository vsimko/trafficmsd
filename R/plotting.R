# Main functions for plotting ############################

#' Plotting network traffic as a single time series or as a comparison of two time series.
#' 
#' @param ts First time series
#' @param newts Optional time series for comparison
#' @param area.color Color for the area under the curve.
#' 
#' @export
plot_traffic <- function(ts, newts = c(0), area.color = "black") {

  ts <- as.vector(ts)
  newts <- as.vector(newts)

  xlim <- c(
    min(index(ts), index(newts)),
    max(index(ts), index(newts))
  )

  ylim <- c(0, max(ts, newts))

  # because polygon() is dumb and wants a pre-existing plot
  plot(ts, type = "n", xaxs = "i",
       xlim = xlim, ylim = ylim,
       xlab = "Time [s]",
       ylab = "Transferred [bytes]")

  ts.stairs <- compute_stairs(ts)
  polygon(ts.stairs, border = NA, col = alpha(area.color, 0.5))
  lines(ts, type = "s")

  newts.stairs <- compute_stairs(newts)
  polygon(newts.stairs, border = NA, col = rgb(1, 0, 0, 0.1))
  lines(newts, type = "s", col = rgb(1, 0, 0, 0.4), lwd = 3)
}

#' Plotting density of a traffic time series.
#' 
#' @param ts.list List of time series
#' @param area.colors List of colors
#' 
#' @export
plot_traffic_densities <- function(ts.list, area.colors = index(ts.list) ) {
  plot(c(0), type = "n",
       xlim = c(0,1), ylim = c(0,1),
       xlab = "Normalized transfer sizes",
       ylab = "Norm. Density" )

  color.index <- 0

  for (ts in ts.list) {
    color.index <- color.index + 1
    ts <- ts[ts != 0] # keep only non-zero packets that were emitted
    ts <- ts / max(ts) # normalized transfer sizes
    d <- density(ts, bw = .02)
    d <- list( x = d$x, y = d$y / max(d$y) )
    d$x[1] <- 0
    d$y[1] <- 0
    area.color <- alpha(area.colors[color.index], 0.4)
    polygon(d, col = area.color, lwd = 2)
  }
}

#' Plot spectrum of the decomposition.
#'
#' @param D The input matrix with decomposed coeficients.
#' @param show.emits If TRUE, renders a separate image depicting places where 
#'   packets are emitted.
#' @param show.spectrum If TRUE, renders a nice image showing the spectrum
#'   based. The spectrum is constructed from the coeficients.
#' @param show.contour If TRUE, contours are rendered within the spectrum.
#' @param show.istats If TRUE, renders additional information about intervals.
#' @param show.hmlegend Renders the "heatmap legend" in the spectrogram.
#' 
#' @examples
#'  plot_spectrum( decompose_traffic(sample32) )
#' 
#' @import fields
#' @export
plot_spectrum <- function(D,
                          show.emits = TRUE,
                          show.spectrum = TRUE,
                          show.contour = TRUE,
                          show.istats = TRUE,
                          show.hmlegend = TRUE) {
  if( ! is.matrix(D) ) {
    D <- D$coef
  }

  # image
  if(show.emits) {

    C <- compute_emits(D)

    # draw the activity of generators
    colors.normal <- shade_color(tim.colors(24), 1.2)
    colors.shaded <- shade_color(colors.normal, .4)

    image(t(D), col = c("black", colors.shaded),
          useRaster = TRUE, axes = FALSE)

    image(t(C), col = c(rgb(0,0,0,0), colors.normal),
          useRaster = TRUE, axes = FALSE, add = TRUE)

    pretty_axis(1, 1:ncol(C), 16 )
    pretty_axis(2, (nrow(C) - 1):0 )

    # statistics about intervals
    if(show.istats) {
      total.icount <- 0
      for(row in 1:nrow(D)) {
        scale <- row - 1
        icount <- get_interval_count(D[row, ])
        total.icount <- total.icount + icount
        if (icount > 0) {
          text(0.005, scale * 1 / (nrow(D) - 1) - 0.03,
               icount, col = "yellow", font = 2, adj = c(0, 0), cex=.8
          )
        }
      }

      draw_image_legend(
        sprintf("Decomposed into %d intervals", total.icount),
        nrow(C), ncol(C))
      box()
    }
  }

  # image of the spectrum
  if(show.spectrum) {
    A <- melt(D)

    # This avoids some bug in smooth.2d which would otherwise produce strange
    # stripes in the image.
    tiles <- list(x = 128, y = 64)
    Smooth <- smooth.2d(Y        = log2(A$value + 1),
                       x        = cbind(A$Var1, A$Var2),
                       theta    = 1.5,
                       ncol     = tiles$x + 1,  # This must be odd number.
                       nrow     = tiles$y,
                       surface  = FALSE )[, 2:tiles$x]

    Smooth.rng <- range(Smooth)
    dmax <- log2(max(D) + 1)

    plab <- list(
      at = seq(from = Smooth.rng[1], to = Smooth.rng[2], length.out = 10),
      labels = round(2 ** seq(from = 0, to = dmax, length.out = 10),
                     digits = -(log10(dmax) - 1)) - 1
    )

    z <- t(Smooth)
    image(z, useRaster = TRUE, axes = FALSE, col=tim.colors())
    pretty_axis( 2, (nrow(D) - 1):0 )

    # draw contour of the spectrum
    if(show.contour) {
      clev <- seq(from = 0, to = max(z), length.out = 8)
      clev <- clev[(length(clev) - 4):length(clev)]
      contour(z, add = TRUE, drawlabels = FALSE, lwd = 2, levels = clev)
    }

    draw_image_legend("Activity of generators", tiles$y, tiles$x)
    box()

    # display the legend
    if(show.hmlegend) {
      image.plot(z, axis.args = plab, useRaster = TRUE, horizontal = TRUE,
                 legend.mar = 5, legend.shrink = 0.4, legend.only = TRUE)
    }
  }
}

#' Scaleogram is equivalent of a spectrogram for wavelets.
#' 
#' Here we compute the scaleogram with the help of biwavelet package.
#' 
#' @param ts The input regular time series
#' @param cwt.mother Mother wavelet used in the continuous wavelet transform.
#' @param force.oscilate Introduce additional oscilations by setting each even
#'   sample to zero.
#' @param title Title of the scaleogram
#' 
#' @import biwavelet
#' @export
plot_scaleogram <- function(ts,
                            cwt.mother = "morlet",
                            force.oscilate = TRUE,
                            title = "Scaleogram with '%s' cwt") {

  if(force.oscilate) {
    ts <- ts * c(0,1) # turning stationary signal into oscilations
  }

  # continuous wavelet transform
  w <- wt(cbind(index(ts), ts), s0 = 1, dj = 0.1, mother = cwt.mother)

  jet.colors <- colorRampPalette(c("#00007F", "blue",   "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red",
                                   "#7F0000"))
  w2 <- t( log(1 + w$power.corr))
  image(w2, col = jet.colors(100), axes = FALSE, useRaster = TRUE)
  contour(w2, levels = c(max(w2) * 0.85), add = TRUE,
          drawlabels = FALSE, lwd = 5)

  draw_image_legend(
    sprintf(title, cwt.mother),
    nrow(w$power), ncol(w$power))

  box()
}

# Helper functions for plotting ############################

#' Helper function - TODO comment
#' @param ts Input time series
#' @return \code{list(x,y)}
compute_stairs <- function(ts) {
  x <- index(ts)
  y <- ts

  y2 <- rep(y, each=2)
  y2 <- y2[-length(y2)]
  x2 <- rep(x, each=2)[-1]
  x3 <- c(min(x2), x2, max(x2))
  y3 <- c(0, y2, 0)

  list(x = x3, y = y3)
}

#' Simulate generators' activity.
#' @param D Matrix of coeficients from the MSD, e.g. \code{dts$coef}
#' @return Matrix of coeficients
compute_emits <- function(D) {

  # Here, we construct the filter applied on the coneficients
  # the filter will keep only those places where a particular generator
  # would emit the packet
  F <- matrix(
    nrow = nrow(D),
    ncol = 2 ** as.integer(log2(ncol(D) - 1) + 1)
  )

  F[1,] <- 1 # first row is scale 0
  for(row in 2:nrow(F)) {
    scale <- row - 1
    wlen <- 2 ** scale

    wfilter <- rep(0, wlen)
    wfilter[ wlen / 2 ] <- 1

    F[row, ] <- wfilter
  }

  # reconstruct the original signal
  return(D * F[,1:ncol(D)]) # apply the filter simulating generators' activity
}

#' Create pretty axis using pretty labels
#' @param side See the "size" parameter in "axis()" function.
#' @param v See function \link{pretty_labels}
#' @param howmany See function \link{pretty_labels}
#' @param project.on See function \link{pretty_labels}
pretty_axis <- function(side, v, howmany = length(v), project.on = c(0,1)) {
  lab <- pretty_labels(v, howmany)
  axis(side, at = lab$at, labels = lab$labels)
}

#' Create pretty labels.
#' @param v Vector used as a range from which we draw our labels.
#' @param howmany Howmany labels are preferred to be drawn on the axis.
#' @param project.on The original interval of labels default [0,1] to which we
#'   want to project our newly created lables.
#'   
#' @return
#'   A list of two elements \code{list(at, labels)}.
#'   
#' @seealso \link{axis} function for the sematics of the "at" and "labels" elements.
pretty_labels <- function(v, howmany = length(v), project.on = c(0,1)) {

  project.on <- range(project.on)
  p <- pretty(range(v, na.rm = TRUE), n = howmany)
  at <- (p - min(v)) * (project.on[2] - project.on[1]) / (max(v) - min(v))

  list(at = at, labels = p)
}

#' Creates a shaded version of given colors.
#' @param colors vector of any of the three kinds of R color specifications.
#' @param percent If \code{percent > 1} the colors will be lighter, if \code{percent < 1} then darker.
shade_color <- function(colors, percent) {
  c <- col2rgb(colors) * percent / 255
  c <- pmin(c, 1)
  c <- pmax(c, 0)
  rgb(c[1,], c[2,], c[3,])
}

#' Helper function to plot a nice legend inside an image.
#' @param text Text to be plotted
#' @param nrow Y-position of the text
#' @param ncol X-position of the text
#' @return see \link{legend}
draw_image_legend <- function(text, nrow, ncol) {
  legend(-.5 / ncol, 1 + .5 / nrow, text.width = 1 + .5 / ncol,
         text,
         col = NA, seg.len=0, x.intersp = 0, y.intersp = 0,
         cex = 1.4, xjust=0, lwd = 2, yjust = 1, title.adj = 0,
         text.col = "black", bg = alpha("white", .7),
         box.col = NA)
}
