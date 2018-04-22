#' Fit Continuous
#'
#' @export
effContFitAndSelect <- function(x, method = "AIC", silent = TRUE){
  if(is.matrix(x) || is.data.frame(x)) {
    effs <- as.list(rep(NA, ncol(x)))
    for(i in 1:ncol(x))
      effs[[i]] <- effContFitAndSelect_inner(x[,i], method, silent)
    effs
  } else {
    effContFitAndSelect_inner(x, method, silent)
  }
}
effContFitAndSelect_inner <- function(x, method = "AIC", silent = TRUE) {
  effs <- effContFit(x, silent)
  effSelect(effs, x, method)
}

#' Fit Discrete
#'
#' @export
effDiscFitAndSelect <- function(x, support, method = "AIC", silent = TRUE) {
  if(is.matrix(x) || is.data.frame(x)) {
    effs <- as.list(rep(NA, ncol(x)))
    for(i in 1:ncol(x))
      effs[[i]] <- effDiscFitAndSelect_inner(x[,i], support, silent)
    effs
  } else {
    effDiscFitAndSelect_inner(x, support, method, silent)
  }
}
effDiscFitAndSelect_inner <- function(x, support, method = "AIC", silent = TRUE) {
  effs <- effDiscFit(x, support, silent)
  effSelect(effs, x, method)
}