#' Calculate standard error maximum
#'
#' @param x
#'
#' @return standard error maxmimum
#' @export
se.max <- function(x) {
  (mean(x)) + se(x)
}
