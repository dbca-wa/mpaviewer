#' Calculate standard error maximum
#'
#' @param x A vector
#'
#' @return standard error maxmimum
#' @export
se.max <- function(x) {
  (mean(x)) + se(x)
}
