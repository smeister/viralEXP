#' Function that builds lnL function based on dataMPN
#'
#' Builds a Negative log likelihood (lnL) function
#' @param d Data frame that is passed to the log likelihood function
#' @export
buildlnL <- function (d) {
  function (x, n, v, k, t, b, mu0) {
    suMM=0
    for (m in 1:length(unique(d$rep))) {
      d2<-subset(d, rep == unique(d$rep)[m])
      x = d2$x
      n = d2$n
      v = d2$v
      t = d2$t
      suMM = suMM + sum((n - x) * (mu0 * exp(-k * t + b)) *v - x * log(1 - exp(-(mu0 * exp(-k * t + b)) *v)), na.rm = T)
    }
    return(suMM)
  }
}
