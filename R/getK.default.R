#' getK.default function
#'
#' Default function of getK, error message if the class does not corresponds
#' @param x checking the class
#' @param ... following objects
#' @return an error message
#' @export
getK.default <- function(x,...){
  warning(
    paste(
      "getK does not know how to handle object of class ",
      class(x),
      "and can only be used on classes MPN or K"
    )
  )
}
