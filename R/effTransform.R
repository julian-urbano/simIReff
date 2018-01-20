#' @export
effTransform <- function(eff, mean = base::mean(eff$data), abs.tol = 1e-6) {
  # For some reason, optim fails when the starting values yield a valid solution, so try it first
  # tdist <- effdistTransform(dist,1,1)
  #if(abs(tdist$mu-mu)<abs.tol)
  # return(tdist)

  opt <- optim(par = c(1, 1),
               fn = function(par) {
                 teff <- effTransform_inner(eff, par[1], par[2])
                 return((teff$mean - mean)^2)
               },
               method = "L-BFGS-B",
               lower = c(1, 1),
               control = list(factr = abs.tol / .Machine$double.eps))

  if(opt$convergence != 0)
    stop("No successful transformation")
  return(effTransform_inner(eff, opt$par[1], opt$par[2]))
}

#' @export
effTransformAll <- function(effs, mean = base::mean(effs[[1]]$data), silent = TRUE, ...) {
  teffs <- vector("list", length(effs))

  for(i in seq_along(effs)) {
    e <- try(effTransform(effs[[i]], mean = mean, ...), silent = silent)

    if(class(e) != "try-error")
      teffs[[i]] <- e
    else
      teffs[[i]] <- NULL
  }

  teffs
}

effTransform_inner <- function(eff, par1, par2) {
  if(inherits(eff, "eff.cont")) { # continuous

    dfun <- function(x) {
      x <- cap(x)
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
    p <- cummax(p) # ensure it's monotonically

    teff <- effDisc_new(p, eff$support, eff$df, eff$data)
    teff$model <- list(type = paste0("t(", eff$model$type, ")"),
                       original = eff, par1 = par1, par2 = par2)

    class(teff) <- c("eff.disc.trans", class(teff))
    teff
  }
}

deff.eff.cont.trans <- function(x, eff) {
  eff$model$dfun(x)
}
peff.eff.cont.trans <- function(q, eff) {
  eff$model$pfun(q)
}
qeff.eff.cont.trans <- function(p, eff) {
  eff$model$qfun(p)
}
reff.eff.cont.trans <- function(n, eff) {
  u <- runif(n)
  eff$model$qfun(u)
}
