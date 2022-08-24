#' Download all files from a Google Drive folder to a local folder
#'
#' NOTE this function does not work yet
#'
#' @param drive_folder The URL of the Google Drive folder,
#'   default: `Sys.getenv("GD_MPAVIEWER")`
#' @param api_key A read-permitted Google API key,
#'   default: `Sys.getenv("GOOGLE_API_KEY")`.
#'   Note the Google Drive API must be enabled.
#' @param data_dir A local directory to download files to,
#'   default: `here::here("inst/data")`.
#'
#' @return None.
#' @import googledrive
#' @export
download_data <- function(ckan_url = Sys.getenv("CKAN_URL"),
                          ckan_key = Sys.getenv("CKAN_API_KEY"),
                          data_rid = Sys.getenv("MPAVIEWER_RID"),
                          data_dir = here::here("inst/data")) {

  # ckanr::ckanr_setup(url=ckan_url, key=ckan_key)
  # res <- ckanr::resource_show(data_rid)
  # curl::curl_download(res$url, here::here("inst/data/mpa_data.rds"))
  # httr::GET(res$url, httr::write_disk(here::here("inst/data/mpa_data.rds"), overwrite=TRUE))

  url <- "https://data.dbca.wa.gov.au/dataset/e3e8e90d-f9b5-4e6f-9607-3e50d101f689/resource/29183116-2409-4213-90c7-69f00888f280/download/mpa_data.rds"
  download.file(url, here::here("inst/data/mpa_data.rds"))

  # This does not want to work today.
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
