#' A mean
#'
#' This function allows you to get the mean of a vector/matrix
#' @param x vector or matrix.
#' @export
#' @examples
#' getMean(c(1,2,3))

getMean <- function(x) {
  ToReturn <- mean(x)
  return(ToReturn)
  print("Done")
}