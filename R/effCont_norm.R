#' Continuous Effectiveness as Truncated Normal Distribution.
#'
#' Fits a Normal distribution, truncated between 0 and 1, to the given sample of scores.
#'
#' @param x a sample of effectiveness scores between 0 and 1.
#' @return an object of class \code{eff.cont.norm}, which inherits from \code{eff.cont}.
#'
#' @examples
#' @todo
#'
#' @seealso \code{\link{deff}}, \code{\link{peff}}, \code{\link{qeff}} and \code{\link{reff}}.
#' @author Julián Urbano
#' @export
effCont_norm<- function(x) {
  # estimate parameters and truncated functions
  mu0 <- mean(x)
  sigma0 <- sd(x)

  fit <- MASS::fitdistr(x, densfun = function(xx, mu, sigma) {
    truncnorm::dtruncnorm(xx, a = 0, b = 1, mean = mu, sd = sigma)
  }, start = list(mu = mu0, sigma = sigma0),
  lower = list(mu = -Inf, sigma = .02),
  upper = list(mu = Inf, sigma = Inf))
  mu <- as.numeric(fit$estimate[1])
  sigma <- as.numeric(fit$estimate[2])

  E <- truncnorm::etruncnorm(a = 0, b = 1, mean = mu, sd = sigma) # expected value
  Var <- truncnorm::vtruncnorm(a = 0, b = 1, mean = mu, sd = sigma) # variance

  # prepare eff object and return
  e <- effCont_new(E, Var, 2, x)
  e$model <- list(mean = mu, sd = sigma)
  class(e) <- c("eff.cont.norm", class(e))
  e
}

#' @export
deff.eff.cont.norm <- function(x, eff) {
  truncnorm::dtruncnorm(as.numeric(x), a = 0, b = 1, mean = eff$model$mean, sd = eff$model$sd)
}
#' @export
peff.eff.cont.norm <- function(q, eff) {
  truncnorm::ptruncnorm(as.numeric(q), a = 0, b = 1, mean = eff$model$mean, sd = eff$model$sd)
}
#' @export
qeff.eff.cont.norm <- function(p, eff) {
  truncnorm::qtruncnorm(as.numeric(p), a = 0, b = 1, mean = eff$model$mean, sd = eff$model$sd)
}
#' @export
reff.eff.cont.norm <- function(n, eff) {
  truncnorm::rtruncnorm(n, a = 0, b = 1, mean = eff$model$mean, sd = eff$model$sd)
}
