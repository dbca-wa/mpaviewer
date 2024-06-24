#' Calculate standard error minimum
#'
#' @param x A vector
#'
#' @return standard error minimum
#' @export
se.min <- function(x) {
  (mean(x)) - se(x)
}
