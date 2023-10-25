#' Generate one data object to use in server.R
#'
#'
#' @param save Boolean whether to save data to rds
#' @param dest The target file to save rds as,
#'   default: here::here("inst/data/mpa_data.rds")
#'
#' @return An object of class "mpa_data" containing all data tibbles and objects
#'   used by server.R
#' @export
#' @import purrr
#'
#' @examples
#' \dontrun{
#' x <- generate_data(save = FALSE) # only returns data
#' x <- generate_data() # returns data and saves data to local file
#' }
generate_plots <- function()) {


}
