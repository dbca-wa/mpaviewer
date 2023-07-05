#' Download the application data file mpa_data.rds from CKAN
#'
#' This method can be configured to point to the data file through environment
#' variables. It will work with defaults unless the location of the data file
#' changes.
#'
#' @param ckan_url The data catalogue URL,
#'   default: `Sys.getenv("CKAN_URL")` or the current DBCA CKAN server.
#' @param data_rid The resource ID of the data file,
#'   default: `Sys.getenv("MPAVIEWER_RID")` or the current resource ID.
#' @param data_dir A local directory to download files to,
#'   default: `here::here("inst/data")`.
#'
#' @return None.
#' @export
googledrive_download_data <- function() {

  library(dplyr)

  options(gargle_oauth_cache = ".secrets")
  # check the value of the option, if you like
  gargle::gargle_oauth_cache()
  googlesheets4::gs4_auth()
  2

  main.dir <- "inst/data/raw"
  popup.dir <- "inst/app/www/popups"

  # Main folder with all marine parks
  drive.folder <- "https://drive.google.com/drive/folders/1GDcemLHxOvAjyecWfv83hhH-ZZh1hqfT"
  folder.id <- googledrive::drive_get(googledrive::as_id(drive.folder))
2
  # find all folders in marine park folder
  files <- googledrive::drive_ls(folder.id, type = "folder")

  # files <- files %>% filter(name %in% "NMP")

  # loop through folders and create a directory
  for (parent in seq_along(files$name)) {

    # parent <- "NMP"

    # Print the current directory
    print(files$name[parent])

    # Make directory
    dir.create(file.path(main.dir, files$name[parent]), recursive = TRUE)

    # Find all children folders in the current folder
    current.folder <- files$id[parent]
    children.folders <- googledrive::drive_ls(current.folder, type = "folder")

    for (child in seq_along(children.folders$name)) {

      # Print the current directory
      print(children.folders$name[child])

      # Make directory
      dir.create(paste(main.dir, files$name[parent], children.folders$name[child], sep = "/"))

      if(nrow(children.folders) > 0){

        # Find all children folders in the current folder
        current.child.folder <- children.folders$id[child]

        print("view baby names")

        baby.folders <- googledrive::drive_ls(current.child.folder, type = "folder") %>%
          dplyr::glimpse()

        for (baby in seq_along(baby.folders$name)) {

          # Print the current directory
          print(baby.folders$name[baby])

          # Make directory
          dir.create(paste(main.dir, files$name[parent], children.folders$name[child], baby.folders$name[baby], sep = "/"))

          # list files
          i_dir <- googledrive::drive_ls(baby.folders[baby, ])

          #download files
          for (file_i in seq_along(i_dir$name)) {
            #fails if already exists
            try({

              print(i_dir$name[file_i])

              # print(getwd())

              googledrive::drive_download(googledrive::as_id(i_dir$id[file_i]),
                                          path = stringr::str_c(main.dir, files$name[parent], children.folders$name[child], baby.folders$name[baby], i_dir$name[file_i], sep = "/"))
            })
          }
        }
      }
    }
  }


  # TODO implement this in the metadata reading in - just change the codes rather than changing the folder names
  codes <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?usp=sharing",
                                     sheet = "park_codes")

  setwd(main.dir)

  for(i in unique(codes$code)){
    code <- i
    new.name <- unique((codes %>% dplyr::filter(code %in% i))$full.name)

    print(new.name)

    file.rename(code, new.name)
  }

  setwd("C:/GitHub/mpaviewer") # NEED to find a way to fix this

  # Folder with images
  drive.folder <- "https://drive.google.com/drive/folders/1PeEcdENN0BhXpkzryqsBbq-0kFn_1N7z"
  folder.id <- googledrive::drive_get(googledrive::as_id(drive.folder))

  # find all folders in marine park folder
  files <- googledrive::drive_ls(folder.id)

  #download files
  for (i in seq_along(files$name)) {
    try({
      googledrive::drive_download(googledrive::as_id(files$id[i]),
                                  path = stringr::str_c("inst/app/www/images", files$name[i], sep = "/"),
                                  overwrite = TRUE)
    })
  }

  # Folder with pop-ups
  drive.folder <- "https://drive.google.com/drive/folders/1laVfBAmFlnrxGInyOEx-2Sp8-s5Dw4rT"
  folder.id <- googledrive::drive_get(googledrive::as_id(drive.folder))

  # find all folders in marine park folder
  files <- googledrive::drive_ls(folder.id)

  #download files
  for (i in seq_along(files$name)) {
    try({
      googledrive::drive_download(googledrive::as_id(files$id[i]),
                                  path = stringr::str_c("inst/app/www/popups", files$name[i], sep = "/"),
                                  overwrite = TRUE)
    })
  }



}
