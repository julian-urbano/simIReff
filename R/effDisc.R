#' Discrete Effectiveness Distributions
#'
#' @param mean the expected value of the distibution.
#' @param var the variance of the distribution.
#' @param df the effective degrees of freedom of the distribution.
#' @param support the support of the distribution
#' @param x the sample of effectiveness scores used to fit the distribution (defaults to
#'   \code{NULL}).
#' @return an object of class \code{eff.disc}, which inherits from \code{eff}.
#'
#' @examples
#' @todo
#' @export
effDisc_new <- function(mean, var, df, support, x = NULL) {
  e <- eff_new(mean, var, df, x)
  e$support <- support
  class(e) <- c("eff.disc", class(e))
  e
}

#' Match an observation x to its corresponding support value, within tolerance.
matchTol <- Vectorize(function(x, table, tol = 1e-4) {
  err <- abs(table - x)
  i <- which.min(err)
  if(err[i] <= tol)
    return(i)
  else
    return(NA)
}, vectorize.args = "x", SIMPLIFY = TRUE)

effDiscMean <- function(deff, support) {
  sum(support * deff(support))
}

effDiscVar <- function(deff, mu, support) {
  sum(support^2 * deff(support)) - mu^2
}