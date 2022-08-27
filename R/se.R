#' Calculate standard error
#'
#' @param x
#'
#' @return standard error
#' @export
se <- function(x) {
  sd(x) / sqrt(length(x))
}
