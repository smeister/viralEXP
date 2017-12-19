#' print.K function
#'
#' S3 function to print only the results without the raw.data
#' @param x a K object output
#' @return a dataframe of the results
#' @export

print.K <- function(x) {
  print(x$Results)
}
