#' Model Selection for Effectiveness Distributions
#'
#' Functions to compute the log-likelihood, the Akaike Information Criterion, and the Bayesian
#' Information Criterion for an effectiveness distribution. \code{effSelect} and
#' \code{which.effSelect} are helper function for automatic selection from a given list of
#' candidates.
#'
#' @param effs the list of candidate distributions to select from.
#' @param x the sample data. Defaults to the data from the first distribution.
#' @param method selection method. One of \code{"AIC"} (default), \code{"BIC"}, or \code{"logLik"}.
#' @param ... other parameters to the selection function.
#' @return the selected disttribution (\code{effSelect}), or its index within \code{effs}
#'   (\code{which.effSelect}).
#' @seealso \code{\link{logLik}}, \code{\link{AIC}}, \code{\link{BIC}} for details on model
#'   selection. See \code{\link{effFitAndSelect}} to fit and select automatically.
#' @examples
#' ee <- effContFit(web2010ap[,1])
#' e <- effSelect(ee, method = "BIC")
#' e2 <- ee[[which.effSelect(ee, method = "BIC")]] # same as e
#'
#' logLik(e)
#' logLik(e, web2010ap[,2])
#' @export
effSelect <- function(effs, x, method = "AIC", ...) {
  i <- which.effSelect(effs, x, method, ...)
  effs[[i]]
}

#' @export
#' @rdname effSelect
which.effSelect <- function(effs, x, method = "AIC", ...) {
  if(missing(x))
    x <- effs[[1]]$data

  method <- match.arg(method, c("AIC", "BIC", "logLik"))

  FUN <- switch(method,
                AIC = AIC.eff,
                BIC = BIC.eff,
                logLik = logLik.eff)

  scores <- sapply(effs, FUN, x = x, ...)
  i <- ifelse(method == "logLik", which.max(scores), which.min(scores))
  structure(i, score = scores[i])
}

#' @rdname effSelect
#' @export
logLik.eff <- function(.eff, x, ...) {
  if(missing(x))
    x <- .eff$data
  stopifnot(!is.null(x))

  ll <- sum(log(deff(x, .eff)))
  structure(ll, class = "logLik", df = .eff$df, nobs = length(x))
}

#' @rdname effSelect
#' @export
AIC.eff <- function(.eff, x, k = 2, ...) {
  ll <- logLik(.eff, x)
  k * attr(ll, "df") - 2 * as.numeric(ll)
}

#' @rdname effSelect
#' @export
BIC.eff <- function(.eff, x, ...) {
  ll <- logLik(.eff, x)
  log(attr(ll, "nobs")) * attr(ll, "df") - 2 * as.numeric(ll)
}
