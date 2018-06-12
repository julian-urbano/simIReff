# color to use for the data in plot.eff
col.data <- "grey70"

#' Plotting tools for effectiveness distributions
#'
#' Plot the density, distribution and quantile functions of an effectiveness distribution. Function
#' \code{plot} plots all three functions in the same graphics device.
#'
#' @param .eff the effectiveness distribution to plot.
#' @param plot.data logical: whether to plot the data used to fit the distribution, if any.
#' @seealso \code{\link{plot.eff.cont}} and \code{\link{plot.eff.disc}} for more details.
#' @export
plot.eff <- function(.eff, plot.data = TRUE, ...) {
  prevpar <- par(mfrow = c(1, 3))

  dplot(.eff, plot.data, ...)
  pplot(.eff, plot.data, ...)
  qplot(.eff, plot.data, ...)

  par(prevpar) # reset previous par
}
#' @rdname plot.eff
#' @export
dplot <- function(.eff, plot.data = TRUE, ...) {
  UseMethod("dplot", .eff)
}
#' @rdname plot.eff
#' @export
pplot <- function(.eff, plot.data = TRUE, ...) {
  UseMethod("pplot", .eff)
}
#' @rdname plot.eff
#' @export
qplot <- function(.eff, plot.data = TRUE, ...) {
  UseMethod("qplot", .eff)
}
