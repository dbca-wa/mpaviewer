#' Function for calculating standard error minimum
#'
#' @param x
#'
#' @return standard error minimum
#' @export
se.min <- function(x) {
  (mean(x)) - se(x)
}
