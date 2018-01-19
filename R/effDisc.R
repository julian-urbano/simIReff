#' Discrete Effectiveness Distributions
#'
#' @param p the values of the distribution function at the support points.
#' @param df the effective degrees of freedom of the distribution.
#' @param support the support of the distribution
#' @param x the sample of effectiveness scores used to fit the distribution (defaults to
#'   \code{NULL}).
#' @return an object of class \code{eff.disc}, which inherits from \code{eff}.
#'
#' @examples
#' @todo
#' @export
effDisc_new <- function(p, support, df, x = NULL) {
  d <- c(p[1], diff(p))
  dfun <- function(x) {
    i <- matchTol(x, support)
    return(d[i])
  }
  pfun <- stepfun(support, c(0, p), ties = "ordered")
  qfun <- stepfun(p[-length(p)], support, right = TRUE, ties = "ordered")
  # qfun <- Vectorize(function(x) {
  #   i <- which(p >= x)[1]
  #   return(support[i])
  # })

  E <- sum(support * dfun(support))
  Var <- sum(support^2 * dfun(support)) - E^2

  e <- eff_new(E, Var, df, x)
  e$support <- support
  e$dfun <- dfun
  e$pfun <- pfun
  e$qfun <- qfun
  class(e) <- c("eff.disc", class(e))
  e
}

#' Match an observation x to its corresponding support value, within tolerance.
#' @export
matchTol <- Vectorize(function(x, table, tol = 1e-4) {
  err <- abs(table - x)
  i <- which.min(err)
  if(err[i] <= tol)
    return(i)
  else
    return(NA)
}, vectorize.args = "x", SIMPLIFY = TRUE)

#' @export
deff.eff.disc <- function(x, eff) {
  eff$dfun(x)
}
#' @export
peff.eff.disc <- function(q, eff) {
  eff$pfun(q)
}
#' @export
qeff.eff.disc <- function(p, eff) {
  eff$qfun(p)
}
#' @export
reff.eff.disc <- function(n, eff) {
  u <- runif(n)
  eff$qfun(u)
}

