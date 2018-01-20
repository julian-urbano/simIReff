#' @export
effContFit <- function(x, silent = TRUE) {
  effs <- vector("list", 4)

  try(effs[[1]] <- effCont_norm(x), silent = silent)
  try(effs[[2]] <- effCont_beta(x), silent = silent)
  try(effs[[3]] <- effCont_kde(x), silent = silent)
  try(effs[[4]] <- effCont_bde(x), silent = silent)

  effs <- effs[!sapply(effs, is.null)]
  if(length(effs) == 0)
    stop("Unable to fit a distribution!")
  effs
}

#' @export
effDiscFit <- function(x, support, silent = TRUE) {
  effs <- vector("list", 5)

  try(effs[[1]] <- effDisc_bbinom(x, support), silent = silent)
  try(effs[[2]] <- effDisc_np(x, support), silent = silent)
  try(effs[[3]] <- effDisc_np(x, support, mult = 2), silent = silent)
  try(effs[[4]] <- effDisc_np(x, support, mult = 5), silent = silent)
  try(effs[[5]] <- effDisc_np(x, support, mult = 10), silent = silent)

  effs <- effs[!sapply(effs, is.null)]
  if(length(effs) == 0)
    stop("Unable to fit a distribution!")
  effs
}