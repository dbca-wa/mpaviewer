#' Read DBCA TXT files
#'
#' @param flnm A filename
#' @param data_dir The data directory, default: `here::here("inst/data")`.
#'
#' @return The file contents as tibble
#' @export
read_dbca_files_txt <- function(flnm, data_dir) {
  readr::read_tsv(flnm, col_types = readr::cols(.default = "c")) %>%
    dplyr::mutate(folder.structure = stringr::str_replace_all(flnm, paste(data_dir, "/", sep = ""), "")) %>%
    tidyr::separate(folder.structure, into = c("marine_park", "indicator", "method", "campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    CheckEM::clean_names()
}
