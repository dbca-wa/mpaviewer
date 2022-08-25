#' Download source data files from CKAN to the local data directory
#'
#' This method can be configured to point to the data file through environment
#' variables. It will work with defaults unless the location of the data file
#' changes.
#'
#' @param ckan_url The data catalogue URL,
#'   default: `Sys.getenv("CKAN_URL")` or the current DBCA CKAN server.
#' @param data_dir A local directory to download files to,
#'   default: `here::here("inst/data")`.
#'
#' @return None.
#' @export
download_source_data <- function(ckan_url = Sys.getenv("CKAN_URL", unset = "https://data.dbca.wa.gov.au"),

                                 data_dir = here::here("inst/data")){

  ckanr::ckanr_setup(url = ckan_url)
  message("Downloading data from DBCA data catalogue...")

  # # For each CKAN data resource with resource ID res_id:
  # res_url <- ckanr::resource_show(res_id)$url
  # dest_fn <- here::here(data_dir, "FILENAME.EXT")
  # httr::GET(res_url, httr::write_disk(dest_fn, overwrite = TRUE))

  message("Download finished successfully.")
}
