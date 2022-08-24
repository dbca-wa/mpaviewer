#' Donwload all files from a Google Drive folder to a local folder
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
download_data <- function(drive_folder = Sys.getenv("GD_MPAVIEWER"),
                          api_key = Sys.getenv("GOOGLE_API_KEY"),
                          data_dir = here::here("inst/data")) {
  # Gatechecks
  if (missing(drive_folder)){ stop("Missing drive_folder") }
  if (missing(api_key)){ stop("Missing api_key") }

  # Authentication
  drive_auth_configure(api_key = api_key)

  drive_ls(drive_folder)
  # List files
  files <- drive_ls(path=drive_folder, recursive = TRUE)

  # Gatecheck
  if (nrow(files) == 0) {abort("No files were found on Google Drive.")}

  # Download files
  purrr::walk(files$id, ~ drive_download(as_id(.x), overwrite = TRUE))
}
