#' Continuous Effectiveness as Bounded Kernel-smoothed Distribution.
#'
#' Fits a bounded kernel-smoothed distribution to the given sample of scores. In particular, the
#' beta kernel by Chen (1990) is used, as in \code{\link[bde]{chen99Kernel}}.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @return an object of class \code{effCont_bde}, which inherits from \code{effCont}.
#'
#' @examples
#' @todo
#'
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @author Julián Urbano, Thomas Nagler
#' @export
effCont_bde <- function(x) {
  # estimate
  k <- bde::bde(x, estimator = "betakernel")

# degrees of freedom
  fhat <- sapply(x, function(xx) mean(dbeta(x, xx / k@b + 1, (1 - xx) / k@b + 1)))
  K0 <- sapply(x, function(xx) dbeta(xx, xx / k@b + 1, (1 - xx) / k@b + 1))
  df <- mean(K0 / fhat)

  mu <- effMu(function(p) bde::quantile(k, p)) # expected value

  # prepare eff object and return
  e <- effCont_new(mu, df)
  e$model <- k
  class(e) <- c("effCont_bde", class(e))
  e
}

#' @export
deff.effCont_bde <- function(x, eff) {
  bde::density(eff$model, x)
}
#' @export
peff.effCont_bde <- function(q, eff) {
  bde::distribution(eff$model, q)
}
#' @export
qeff.effCont_bde <- function(p, eff) {
  bde::quantile(eff$model, p)
}
#' @export
reff.effCont_bde <- function(n, eff) {
  bde::rsample(eff$model, n)
}

