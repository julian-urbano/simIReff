#' Fit Continuous
#'
#' @export
effContFit <- function(x, silent = TRUE) {
  effs <- as.list(rep(NA, 4))

  try(effs[[1]] <- effCont_norm(x), silent = silent)
  try(effs[[2]] <- effCont_beta(x), silent = silent)
  try(effs[[3]] <- effCont_nks(x), silent = silent)
  try(effs[[4]] <- effCont_bks(x), silent = silent)

  effs <- effs[!is.na(effs)]
  if(length(effs) == 0)
    stop("Unable to fit a distribution")
  effs
}

#' Fit Discrete
#'
#' @export
effDiscFit <- function(x, support, silent = TRUE) {
  effs <- as.list(rep(NA, 5))

  try(effs[[1]] <- effDisc_bbinom(x, support), silent = silent)
  try(effs[[2]] <- effDisc_dks(x, support), silent = silent)
  try(effs[[3]] <- effDisc_dks(x, support, mult = 2), silent = silent)
  try(effs[[4]] <- effDisc_dks(x, support, mult = 5), silent = silent)
  try(effs[[5]] <- effDisc_dks(x, support, mult = 10), silent = silent)

  effs <- effs[!is.na(effs)]
  if(length(effs) == 0)
    stop("Unable to fit a distribution")
  effs
}
