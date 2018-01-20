#' Continuous Effectiveness as Bounded Kernel-smoothed Distribution.
#'
#' Fits a kernel-smoothed distribution to the given sample of scores, truncated between 0 and 1, and
#' using a gaussian kernel.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @return an object of class \code{eff.cont.kde}, which inherits from \code{eff.cont}.
#'
#' @examples
#' @todo
#'
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @author Julián Urbano, Thomas Nagler
#' @export
effCont_kde <- function(x) {
  # estimate
  k <- ks::kde(x)
  tk <- effContTrunc(ks::dkde, ks::pkde, ks::qkde, fhat = k)
  df <- mean(dnorm(0, sd = k$h) / ks::dkde(x, fhat = k))

  E <- effContMean(tk$q) # expected value
  Var <- effContVar(tk$d, E) # variance

  # prepare eff object and return
  e <- effCont_new(E, Var, df, x)
  e$model <- list(type = "kde", kde = k, d = tk$d, p = tk$p, q = tk$q)
  class(e) <- c("eff.cont.kde", class(e))
  e
}

#' @export
deff.eff.cont.kde <- function(x, eff) {
  eff$model$d(x)
}
#' @export
peff.eff.cont.kde <- function(q, eff) {
  eff$model$p(q)
}
#' @export
qeff.eff.cont.kde <- function(p, eff) {
  eff$model$q(p)
}
#' @export
reff.eff.cont.kde <- function(n, eff) {
  r <- runif(n)
  eff$model$q(r)
}
