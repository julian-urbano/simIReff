#' Plotting tools for Continuous effectiveness distributions
#'
#' @rdname plot.eff.cont
#' @export
dplot.eff.cont <- function(.eff, plot.data = FALSE, col.data = "grey70",
                           xlab = "x", ylab = "f(x)", main = "density",
                           subdivisions = 200, ...) {
  if(plot.data && is.null(.eff$data))
    stop("no data to plot.")

  x <- seq(1e-4, 1-1e-4, length.out = subdivisions)
  y <- deff(x, .eff)
  ylim = c(0, max(y))
  if(plot.data) {
    h <- hist(.eff$data, plot = FALSE)
    ylim = range(ylim, h$density)
  }

  plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = ylim)
  abline(h = 0, col = col.data, lty = 2)

  if(plot.data) {
    hist(.eff$data, probability = TRUE, border = col.data, add = TRUE, axes = FALSE)
    rug(mean(.eff$data), side = 1, col = col.data)
  }

  lines(x, y, type = "l")
  rug(.eff$mean, side = 1)
}
#' @rdname plot.eff.cont
#' @export
pplot.eff.cont <- function(.eff, plot.data = FALSE, col.data = "grey70",
                           xlab = "q", ylab = "F(q)", main = "distribution",
                           subdivisions = 200, ...) {
  if(plot.data && is.null(.eff$data))
    stop("no data to plot.")

  x <- seq(1e-4, 1-1e-4, length.out = subdivisions)
  y <- peff(x, .eff)

  plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1)
  abline(h = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    lines(ecdf(.eff$data), col = col.data, pch = 19, cex = 1, col.01line = NA)
    rug(mean(.eff$data), side = 1, col = col.data)
  }

  lines(x, y)
  rug(.eff$mean, side = 1)
}
#' @rdname plot.eff.cont
#' @export
qplot.eff.cont <- function(.eff, plot.data = FALSE, col.data = "grey70",
                           xlab = "p", ylab = expression(F^-1*(p)), main = "quantile",
                           subdivisions = 200, ...) {
  if(plot.data && is.null(.eff$data))
    stop("no data to plot.")

  x <- seq(0, 1, length.out = subdivisions)
  y <- qeff(x, .eff)

  plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1)
  abline(v = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    x_data <- sort(unique(.eff$data))
    y_data <- ecdf(.eff$data)(x_data)
    qfun <- stepfun(y_data[-length(y_data)], x_data, right = TRUE, ties = "ordered")
    lines(qfun, col = col.data, pch = 19, cex = 1, verticals = FALSE)
    rug(mean(.eff$data), side = 2, col = col.data)
  }

  lines(x, y)
  rug(.eff$mean, side = 2)
}
