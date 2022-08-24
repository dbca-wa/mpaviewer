#' Read DBCA TXT files
#'
#' @param flnm A filename
#'
#' @return The file contents as tibble
#' @export
read_dbca_files_txt <- function(flnm) {
  readr::read_tsv(flnm, col_types = cols(.default = "c")) %>%
    dplyr::mutate(folder.structure = stringr::str_replace_all(flnm, paste(data.dir, "/", sep = ""), "")) %>%
    tidyr::separate(folder.structure, into = c("marine.park", "method", "campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    GlobalArchive::ga.clean.names()
}
