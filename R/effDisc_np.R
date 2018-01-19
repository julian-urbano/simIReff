#' @export
effDisc_np <- function(x, support, mult = 1) {
  support <- sort(support)

  x_i <- matchTol(x, support)
  support_i <- seq_along(support)
  bw <- np::npudensbw(ordered(x_i, support_i),
                  bwmethod = "cv.ls",
                  okertype = "wangvanryzin")
  bw$bw <- bw$bw * mult
  bw$bandwidth$x <- bw$bandwidth$x * mult

  if (bw$bw > 1)
    stop("(multiplied) bandwidth too large, use a smaller multiplier")

  d <- predict(np::npudens(bw), se.fit = FALSE, as.ordered(support_i))
  d <- d / sum(d)
  p <- cumsum(d)

  e <- effDisc_new(p, support, mean((1 - bw$bw) / d[x_i]), x)
  e$model <- list(bw = bw, mult = mult)
  class(e) <- c("eff.disc.np", class(e))
  e
}
