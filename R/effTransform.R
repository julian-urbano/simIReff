#' @export
effTransform <- function(eff, mean, abs.tol = 1e-5) {
  m <- base::mean(eff$data)
  if(!missing(mean))
    m <- mean

  opt <- optim(par = c(1, 1),
               fn = function(par) {
                 teff <- effTransform_inner(eff, par[1], par[2])
                 return((teff$mean - m)^2)
               },
               method = "L-BFGS-B",
               lower = c(1, 1),
               control = list(factr = abs.tol^2 / .Machine$double.eps))

  if(opt$convergence != 0)
    stop("No successful transformation")
  effTransform_inner(eff, opt$par[1], opt$par[2])
}

#' @export
effTransformAll <- function(effs, means, silent = TRUE, ...) {
  if(missing(means))
    means <- sapply(effs, function(e) mean(e$data))

  means <- means + rep(0, length(effs))
  teffs <- as.list(rep(NA, length(effs)))

  for(i in seq_along(effs)) {
    e <- try(effTransform(effs[[i]], mean = means[i], ...), silent = silent)

    if(class(e) != "try-error")
      teffs[[i]] <- e
  }

  teffs
}

effTransform_inner <- function(eff, par1, par2) {
  if(inherits(eff, "eff.cont")) { # continuous

    dfun <- function(x) {
      #x <- cap(x)
      dbeta(peff(x, eff), par1, par2) * deff(x, eff)
    }
    pfun <- function(q) {
      pbeta(peff(q, eff), par1, par2)
    }
    qfun <- function(p) {
      qeff(qbeta(p, par1, par2), eff)
    }

    E <- effContMean(qfun)
    Var <- effContVar(qfun, E)

    teff <- effCont_new(E, Var, eff$df, eff$data)
    teff$model <- list(type = paste0("t(", eff$model$type, ")"),
                       original = eff, par1 = par1, par2 = par2,
                       dfun = dfun, pfun = pfun, qfun = qfun)

    class(teff) <- c("eff.cont.trans", class(teff))
    teff
  } else { # discrete
    # compute the new cdf values, and from there the full object
    p <- pbeta(peff(eff$support, eff), par1, par2)
    p <- cummax(p) # ensure it's monotonically increasing

    teff <- effDisc_new(p, eff$support, eff$df, eff$data)
    teff$model <- list(type = paste0("t(", eff$model$type, ")"),
                       original = eff, par1 = par1, par2 = par2)

    class(teff) <- c("eff.disc.trans", class(teff))
    teff
  }
}

#' @export
deff.eff.cont.trans <- function(x, .eff) {
  .eff$model$dfun(x)
}
#' @export
peff.eff.cont.trans <- function(q, .eff) {
  .eff$model$pfun(q)
}
#' @export
qeff.eff.cont.trans <- function(p, .eff) {
  .eff$model$qfun(p)
}
#' @export
reff.eff.cont.trans <- function(n, .eff) {
  u <- runif(n)
  .eff$model$qfun(u)
}
