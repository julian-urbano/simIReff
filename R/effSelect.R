#' Model Selection for Effectiveness Distributions
#'
#' Functions to compute various \code{logLik} computes the log-likelihood for an effectiveness
#' distribution, \code{AIC} computes the Akaike Information Criterion, and \code{BIC} computes the
#' Bayesian Information Criterion.
#'
#' \code{effSelect} is a helper function for automatic selection from a given list of candidate
#' models.
#'
#' @param effs the list of candidate distributions to select from.
#' @param x the sample data. Defaults to the data from the first distribution.
#' @param method selection method. One of \code{"AIC"} (default), \code{"BIC"}, or \code{"logLik"}.
#' @param return \code{"object"} to return the selected distribution object, or \code{"index"} to
#'   return its index within the given list (default).
#' @param ... other parameters to the selection function.
#' @return the index within \code{effs} of the selected disttribution, with an attribute
#'   \code{score} containing the score of the selection method used.
#'
#' @examples
#' @todo
#'
#' @export
effSelect <- function(effs, x = effs[[1]]$data, method = "AIC", return = "index", ...) {
  method <- match.arg(method, c("AIC", "BIC", "logLik"))
  ret <- match.arg(return, c("index", "object"))

  FUN <- switch(method,
                AIC = AIC.eff,
                BIC = BIC.eff,
                logLik = logLik.eff)

  scores <- sapply(effs, FUN, x = x, ...)
  i <- ifelse(method == "logLik", which.max(scores), which.min(scores))
  if(ret == "index")
    structure(i, score = scores[i])
  else
    effs[[i]]
}

#' @rdname effSelect
#' @export
logLik.eff <- function(eff, x = eff$data, ...) {
  stopifnot(!is.null(x))

  ll <- sum(log(deff(x, eff)))
  structure(ll, class = "logLik", df = eff$df, nobs = length(x))
}

#' @rdname effSelect
#' @export
AIC.eff <- function(eff, x = eff$data, k = 2, ...) {
  ll <- logLik(eff, x)
  k * attr(ll, "df") - 2 * as.numeric(ll)
}

#' @rdname effSelect
#' @export
BIC.eff <- function(eff, x = eff$data, ...) {
  ll <- logLik(eff, x)
  log(attr(ll, "nobs")) * attr(ll, "df") - 2 * as.numeric(ll)
}
