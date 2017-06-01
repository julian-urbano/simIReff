eff_new <- function(mu, df) {
  e <- list(mu = mu, df = df)
  class(e) <- "eff"
  e
}

#' Effectiveness Distributions
#'
#' Density, distribution function, quantile function and random generation for an effectiveness
#' distribution.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param eff the \code{eff} object representing the effectiveness distribution.
#' @return \code{deff} gives the density, \code{peff} gives the distribution function, \code{qeff}
#'   gives the quantile function, and \code{reff} generates random variates.
#'
#' @examples
#' @todo
#'
#' @author Julián Urbano
#' @name eff
NULL

#' @rdname eff
#' @export
deff <- function(x, eff){
  UseMethod("deff", eff)
}

#' @rdname eff
#' @export
peff <- function(q, eff) {
  UseMethod("peff", eff)
}

#' @rdname eff
qeff <- function(p, eff) {
  UseMethod("qeff", eff)
}

#' @rdname eff
#' @export
reff <- function(n, eff) {
  UseMethod("reff", eff)
}
