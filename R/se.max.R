#' Function for calculating standard error maximum
#'
#' @param x
#'
#' @return standard error maxmimum
#' @export
#'
#' @examples
se.max <- function(x) {
  (mean(x)) + se(x)
}
