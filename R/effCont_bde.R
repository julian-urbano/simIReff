#' Continuous Effectiveness as Bounded Kernel-smoothed Distribution.
#'
#' Fits a bounded kernel-smoothed distribution to the given sample of scores. In particular, the
#' beta kernel by Chen (1999) is used, as in \code{\link[bde]{chen99Kernel}}.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @return an object of class \code{eff.cont.bde}, which inherits from \code{eff.cont}.
#'
#' @examples
#' @todo
#'
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @author Julián Urbano, Thomas Nagler
#' @references Chen, S. X. (1999). Beta kernel estimators for density functions. Computational
#'   Statistics & Data Analysis, 31, 131-145.
#' @export
effCont_bde <- function(x) {
  x_cap <- cap(x)
  # estimate
  k <- bde::bde(x_cap, estimator = "betakernel")

  # degrees of freedom
  fhat <- bde::density(k, x_cap)
  K0 <- sapply(x_cap, function(xx) dbeta(xx, xx / k@b + 1, (1 - xx) / k@b + 1))
  df <- mean(K0 / fhat)

  E <- effContMean(function(p) bde::quantile(k, p)) # expected value
  Var <- effContVar(function(x) bde::quantile(k, p), E) # variance

  sum1 <- integrate(function(x) bde::density(k, x), lower = 0, upper = 1)$value
  F1 <- bde::distribution(k, 1)

  # prepare eff object and return
  e <- effCont_new(E, Var, df, x)
  e$model <- list(type = "bde", bde = k, sum1 = sum1, F1 = F1)
  class(e) <- c("eff.cont.bde", class(e))
  e
}

#' @export
deff.eff.cont.bde <- function(x, eff) {
  bde::density(eff$model$bde, x) / eff$model$sum1
}
#' @export
peff.eff.cont.bde <- function(q, eff) {
  bde::distribution(eff$model$bde, q) / eff$model$F1
}
#' @export
qeff.eff.cont.bde <- function(p, eff) {
  bde::quantile(eff$model$bde, p)
}
#' @export
reff.eff.cont.bde <- function(n, eff) {
  bde::rsample(eff$model, n)
}

