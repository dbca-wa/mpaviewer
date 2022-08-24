#' Download the data file from CKAN
#'
#' @param ckan_url The data catalogue URL,
#'   default: `Sys.getenv("CKAN_URL")`
#' @param api_key The resource ID of the data file,
#'   default: `Sys.getenv("MPAVIEWER_RID")`.
#' @param data_dir A local directory to download files to,
#'   default: `here::here("inst/data")`.
#'
#' @return None.
#' @import googledrive
#' @export
download_data <- function(ckan_url = Sys.getenv("CKAN_URL"),
                          data_rid = Sys.getenv("MPAVIEWER_RID"),
                          data_dir = here::here("inst/data")) {

  ckanr::ckanr_setup(url=ckan_url)
  res_url <- ckanr::resource_show(data_rid)$url
  dest_fn <- here::here(data_dir, "mpa_data.rds")
  httr::GET(res_url, httr::write_disk(dest_fn, overwrite=TRUE))

  # # Gatechecks
  # if (missing(drive_folder)) {
  #   stop("Missing drive_folder")
  # }
  # if (missing(api_key)) {
  #   stop("Missing api_key")
  # }
  #
  # # Authentication
  # drive_auth_configure(api_key = api_key)
  #
  # drive_ls(drive_folder)
  # # List files
  # files <- drive_ls(path = drive_folder, recursive = TRUE)
  #
  # # Gatecheck
  # if (nrow(files) == 0) {
  #   abort("No files were found on Google Drive.")
  # }
  #
  # # Download files
  # purrr::walk(files$id, ~ drive_download(as_id(.x), overwrite = TRUE))
}
