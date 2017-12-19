#' print.MPN function
#'
#' S3 function to print only the results without the raw.data
#' @param x a MPN object output
#' @return a dataframe of the results
#' @export
print.MPN <- function(x) {
  print(x$Results)
}
