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
