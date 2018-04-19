#' Plotting tools for Discrete effectiveness distributions
#'
#' @rdname plot.eff.disc
#' @export
dplot.eff.disc <- function(.eff, plot.data = FALSE, col.data = "grey70",
                           xlab = "x", ylab = "f(x)", main = "mass", ...) {
  if(plot.data && is.null(.eff$data))
    stop("no data to plot.")

  x <- .eff$support
  y <- deff(x, .eff)
  ylim = c(0, max(y))
  if(plot.data) {
    y_data <- as.numeric(table(factor(.eff$data, levels = .eff$support))) / length(.eff$data)
    ylim = range(ylim, y_data)
  }

  plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = ylim)
  abline(h = 0, col = col.data, lty = 2)

  if(plot.data) {
    lines(x, y_data, type = "b", col = col.data, pch = 19)
    # sapply(seq_along(x), function(i) {
    #
    #   lines(rep(x[i], 2), c(0, y_data[i]), col = col.data)
    #   points(x[i], y_data[i], pch = 19, col = col.data)
    # })
    rug(mean(.eff$data), side = 1, col = col.data)
  }

  lines(x, y, type = "b", pch = 19)
  # sapply(seq_along(x), function(i) {
  #   lines(rep(x[i],2), c(0, y[i]))
  #   points(x[i],y[i],pch=19)
  # })
  rug(.eff$mean, side = 1)
}
#' @rdname plot.eff.disc
#' @export
pplot.eff.disc <- function(.eff, plot.data = FALSE, col.data = "grey70",
                           xlab = "q", ylab = "F(q)", main = "distribution", ...) {
  if(plot.data && is.null(.eff$data))
    stop("no data to plot.")

  x <- .eff$support
  y <- peff(x, .eff)
  pfun <- stepfun(x, c(0, y), ties = "ordered")

  plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1)
  abline(h = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    lines(ecdf(.eff$data), col = col.data, pch = 19, cex = 1, col.01line = NA)
    rug(mean(.eff$data), side = 1, col = col.data)
  }

  lines(pfun, verticals = FALSE, pch = 19, cex = 1)
  rug(.eff$mean, side = 1)
}
#' @rdname plot.eff.disc
#' @export
qplot.eff.disc <- function(.eff, plot.data = FALSE, col.data = "grey70",
                           xlab = "p", ylab = expression(F^-1*(p)), main = "quantile", ...) {
  if(plot.data && is.null(.eff$data))
    stop("no data to plot.")

  x <- .eff$support
  y <- peff(x, .eff)
  qfun <- stepfun(y[-length(y)], x, right = TRUE, ties = "ordered")

  plot(NA, xlab = xlab, ylab = ylab, main = main, xlim = 0:1, ylim = 0:1)
  abline(v = 0:1, col = col.data, lty = 2)

  if(plot.data) {
    x_data <- sort(unique(.eff$data))
    y_data <- ecdf(.eff$data)(x_data)
    qfun_data <- stepfun(y_data[-length(y_data)], x_data, right = TRUE, ties = "ordered")
    lines(qfun_data, col = col.data, pch = 19, cex = 1, verticals = FALSE)
    rug(mean(.eff$data), side = 1, col = col.data)
  }

  lines(qfun, pch = 19, cex = 1, verticals = FALSE)
  rug(.eff$mean, side = 2)
}
