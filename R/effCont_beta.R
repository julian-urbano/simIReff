#' Continuous Effectiveness as Beta Distribution.
#'
#' Fits a Beta distribution to the given sample of scores.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @return an object of class \code{effCont_beta}, which inherits from \code{effCont}.
#'
#' @examples
#' @todo
#'
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @author Julián Urbano
#' @export
effCont_beta <- function(x) {
  x <- cap(x)

  # estimate parameters numerically, from initial values
  mu_0 <- mean(x)
  sigma2_0 <- var(x)
  shape1 <- mu_0 * (mu_0 * (1-mu_0) / sigma2_0 -1)
  shape2 <- (mu_0) * (mu_0 * (1-mu_0) / sigma2_0 -1)

  fit <- MASS::fitdistr(x, densfun = "beta", start = list(shape1 = shape1, shape2 = shape2))
  shape1 <- as.numeric(fit$estimate[1])
  shape2 <- as.numeric(fit$estimate[2])

  mu <- shape1 / (shape1 + shape2) # expected value

  # prepare eff object and return
  e <- effCont_new(mu, 2)
  e$model <- list(shape1 = shape1, shape2 = shape2)
  class(e) <- c("effCont_beta", class(e))
  e
}

#' @export
deff.effCont_beta <- function(x, eff) {
  x <- cap(x)
  dbeta(x, eff$model$shape1, eff$model$shape2)
}
#' @export
peff.effCont_beta <- function(q, eff) {
  pbeta(q, eff$model$shape1, eff$model$shape2)
}
#' @export
qeff.effCont_beta <- function(p, eff) {
  qbeta(p, eff$model$shape1, eff$model$shape2)
}
#' @export
reff.effCont_beta <- function(n, eff) {
  rbeta(n, eff$model$shape1, eff$model$shape2)
}

