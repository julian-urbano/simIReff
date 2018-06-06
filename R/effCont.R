#' Continuous Effectiveness Distributions
#'
#' @param mean the expected value of the distibution.
#' @param var the variance of the distribution.
#' @param df the effective degrees of freedom of the distribution.
#' @param x the sample of effectiveness scores used to fit the distribution (defaults to
#'   \code{NULL}).
#' @return an object of class \code{eff.cont}, which inherits from \code{eff}.
#' @seealso \code{\link{effCont_fit}} to fit continuous distributions. For discrete distributions,
#'   see \code{\link{effDisc}}.
#' @export
#' @name effCont
effCont_new <- function(mean, var, df, x = NULL) {
  e <- eff_new(mean, var, df, x)
  class(e) <- c("eff.cont", class(e))
  e
}

#' Truncate a variable from below and above.
cap <- function(x, xmin = 1e-6, xmax = 1-xmin) {
  pmin(xmax, pmax(xmin, x))
}

#' Approximate the expected value of a continuous effectiveness distribution.
#'
#' Computes the expected value of a distribution by numerical integration of the given quantile
#' function between 0 and 1.
#'
#' @param qeff the quantile function of the distribution.
#' @param abs.tol absolute accuracy requested, passed to \code{\link{integrate}}.
#' @param subdivisions the maximum number of subintervals, passed to \code{\link{integrate}}.
#' @return the estimate of the expected value.
#' @examples
#' effContMean(function(p) qnorm(p, mean = 4))
#' effContMean(function(p) qbeta(p, 1, 2))
effContMean <- function(qeff, abs.tol = 1e-6, subdivisions = 500) {
  integrate(qeff, lower = 0, upper = 1, abs.tol = abs.tol, subdivisions = subdivisions)$value
}

#' Approximate the variance of a continuous effectiveness distribution.
#'
#' Computes the variance of a distribution by numerical integration of the given density
#' function between 0 and 1.
#'
#' @param deff the density function of the distribution.
#' @param mu the expected value of the distribution (see \code{\link{effContMean}}).
#' @param abs.tol absolute accuracy requested, passed to \code{\link{integrate}}.
#' @param subdivisions the maximum number of subintervals, passed to \code{\link{integrate}}.
#' @return the estimate of the expected value.
#' @examples
#' effContVar(function(p) dnorm(p, mean = 4), 4)
#' effContVar(function(p) dbeta(p, 1, 2), 1/3)
effContVar <- function(qeff, mu, abs.tol = 1e-6, subdivisions = 500) {
  # integrate(function(x) deff(x) * (x - mu)^2, lower = 0, upper = 1,
  #           abs.tol = abs.tol, subdivisions = subdivisions)$value
  integrate(function(x) qeff(x)^2, lower = 0, upper = 1,
            abs.tol = abs.tol, subdivisions = subdivisions)$value - mu^2
}

#' Truncation of Distributions
#'
#' Computes the density, distribution and quantile functions of the distribution resulting from
#' truncating a given distribution between 0 and 1.
#'
#' @param d the original density function.
#' @param p the original distribution function.
#' @param q the original quantile function.
#' @param ... additional arguments passed to the original functions, if any.
#' @return A list with components \tabular{ll}{
#'   \code{td} \tab the truncated density function. \cr
#'   \code{tp} \tab the truncated distribution function. \cr
#'   \code{tq} \tab the truncated quantile function.
#' }
#' @examples
#' tr <- effContTrunc(dnorm, pnorm, qnorm, mean = .8, sd = .3)
#' x01 <- seq(0, 1, .01)
#' plot(x01, tr$d(x01), type = "l")
#' plot(x01, tr$p(x01), type = "l")
#' plot(x01, tr$q(x01), type = "l")
#' @export
effContTrunc <- function(d, p, q, ...) {
  F01 <- p(c(0,1), ...) # distribution at endpoints 0 and 1

  td <- function(x) { # truncated density
    y <- d(x, ...) / (F01[2] - F01[1])
    y[x<0 | x>1] <- 0
    y
  }
  tp <- function(q) { # truncated distribution
    y <- (p(q, ...) - F01[1]) / (F01[2] - F01[1])
    cap(y, 0, 1)
  }
  tq <- function(p) { # truncated quantile
    y <- q(p*(F01[2] - F01[1]) + F01[1], ...)
    cap(y, 0, 1)
  }

  list(d = td, p = tp, q = tq)
}

