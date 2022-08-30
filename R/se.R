#' Calculate standard error
#'
#' @param x A vector
#'
#' @return standard error
#' @export
se <- function(x) {
  sd(x) / sqrt(length(x))
}
