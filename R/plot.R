#' Plotting tools for effectiveness distributions
#'
#' @rdname plot.eff
#' @export
plot.eff <- function(eff, plot.data = FALSE, col.data = "grey70", ...) {
  prevpar <- par(mfrow = c(1, 3))

  dplot(eff, plot.data, col.data, ...)
  pplot(eff, plot.data, col.data, ...)
  qplot(eff, plot.data, col.data, ...)

  par(prevpar) # reset previous par
}
#' @rdname plot.eff
#' @export
dplot <- function(eff, plot.data = FALSE, col.data = "grey70",
                  xlab = "x", ylab = "f(x)", main = "density", ...) {
  UseMethod("dplot", eff)
}
#' @rdname plot.eff
#' @export
pplot <- function(eff, plot.data = FALSE, col.data = "grey70",
                  xlab = "q", ylab = "F(q)", main = "distribution", ...) {
  UseMethod("pplot", eff)
}
#' @rdname plot.eff
#' @export
qplot <- function(eff, plot.data = FALSE, col.data = "grey70",
                  xlab = "p", ylab = expression(F^-1*(p)), main = "quantile", ...) {
  UseMethod("qplot", eff)
}