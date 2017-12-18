#' getK function
#'
#' Calculate the inactivation constant k by combining MPN values and its uncertainties
#' Calculate the inactivation constant k by combining k values of different biological replicates
#' @param ... One of more MPN or K objects
#' @param timeVECT vector containing the time/dose points (necessary if MPN objects)
#' @export
getK <- function (...) UseMethod("getK")

