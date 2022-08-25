#' Function for calculating standard error
#'
#' @param x
#'
#' @return standard error
#' @export
#'
#' @examples
se <- function(x) {
  sd(x) / sqrt(length(x))
}
