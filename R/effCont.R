#' Continuous Effectiveness Distributions
#'
#' @param mu the expected value of the distibution.
#' @param df the effective degrees of freedom of the distribution.#'
#' @return an object of class \code{effCont}
#'
#' @author Julián Urbano
#' @export
effCont_new <- function(mu, df) {
  e <- eff_new(mu, df)
  class(e) <- c("effCont", class(e))
  e
}

#' @export
plot.effCont <- function(x, ...) {
  prevpar <- par(mfrow = c(1, 3))

  # density
  x0 <- seq(0, 1, .01)
  y0 <- deff(x0, x)
  plot(x0, y0, type = "l", xlab = "x", ylab = "f(x)")
  points(x$mu, deff(x$mu, x))
  # distribution
  y0 <- peff(x0, x)
  plot(x0, y0, type = "l", xlab = "x", ylab = "F(x)")
  points(x$mu, peff(x$mu, x))
  # quantile
  y0 <- qeff(x0, x)
  plot(x0, y0, type = "l", xlab = "p", ylab = expression(F^-1*(p)))
  points(peff(x$mu, x), x$mu)

  par(prevpar) # reset previous par
}

#' Expected value of a continuous effectiveness distribution.
#'
#' Computes the expected value of a distribution by numerical integration of the supplied quantile
#' function between 0 and 1.
#'
#' @param qeff the quantile function of the distribution.
#' @param abs.tol absolute accuracy requested, passed to \code{\link{integrate}}.
#' @param subdivisions the maximum number of subintervals, passed to \code{\link{integrate}}.#'
#' @return the estimate of the expected value.
#'
#' @examples
#' effContMu(function(p) dnorm(p, mean = 4))
#' effContMu(function(p) qbeta(p, 1, 2))
#'
#' @author Julián Urbano
#' @export
effContMu <- function(qeff, abs.tol = 1e-6, subdivisions = 500) {
  integrate(qeff, lower = 0, upper = 1, abs.tol = abs.tol, subdivisions = subdivisions)$value
}
