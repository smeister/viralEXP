#' Function that builds lnL function based on dataMPN
#'
#' Builds a Negative log likelihood (lnL) function
#' @export
buildlnL <- function (d) {
  function (x, n, v, k, t, b, mu0) {
    sum=0
    for (m in 1:length(unique(d$rep))) {
      x = d$x
      n = d$n
      v = d$v
      t = d$t
      sum = sum + sum((n - x) * (mu0 * exp(-k * t + b)) *v - x * log(1 - exp(-(mu0 * exp(-k * t + b)) *v)), na.rm = T)
    }
    return(sum)
  }
}
