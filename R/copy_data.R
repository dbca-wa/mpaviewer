#' Copy data to use within DBCA sharepoint folders
#'
#' @param dir File path for the iLab
#'
#' @return Copies data into folders
#' @export
copy_data <- function(dir = iLab::get_dir("iLab_fish")) {

  main_dir <- dir

  dashboard_dir <- file.path(main_dir, "!Essential_Files", "Dashboard_Data")

  # Set Dashboard Data Folder and folder where clean data is ----
  proj_doc_dirs <- grep('/Cleaned Data', list.dirs(main_dir), value = TRUE)

  # Download park codes from google sheet (eg NMP, MBIMP, NCMP) ----
  park_codes <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?gid=864824658#gid=864824658", sheet = 'park_codes') %>%
    dplyr::select(code, full.name)

  file_list <- data.frame(file_full_dir = list.files(proj_doc_dirs, pattern = '.csv|.txt', full.names = TRUE)) %>%
    dplyr::mutate(file_full_dir = stringr::str_replace_all(file_full_dir, "\\\\", "/")) # Added this, backslashes were breaking the split

  # Use file list to extract park code - if there are no obs in data file check that right header is renamed below
  park_name <- data.frame(stringr::str_split_fixed(file_list$file_full_dir, "/", n = 10)) %>%
    dplyr::rename(park = X6) %>%
    dplyr::left_join(., park_codes, by = c("park" = "code")) %>%
    #filter(!park == "DMP") %>%
    dplyr::select(park, full.name)

  # test <- file_list %>%
  #   dplyr::mutate(park = str_match(file_full_dir, '^(?:[^/]+/){5}([^/]+).*')[, 2])

  # Changing park names and creating directory paths ----
  save_list <- file_list %>%
    dplyr::mutate(method = case_when(stringr::str_detect(file_full_dir, "BRUV") ~ "BRUVs",
                                     stringr::str_detect(file_full_dir, "DOV") ~ "DOVs",
                                     stringr::str_detect(file_full_dir, "ROV") ~ "ROVs",
                                     stringr::str_detect(file_full_dir, "UVC") ~ "DOVs"), # Seems wrong but matches what was happening with old ifelse
                  park = stringr::str_match(file_full_dir, '^(?:[^/]+/){5}([^/]+).*')[, 2]) %>%
    dplyr::left_join(park_codes, by = c("park" = "code")) %>%
    dplyr::mutate(park_path = paste0(dashboard_dir, "/", full.name, "/"),
                  method_path = paste0(park_path, "Fish/", method, "/")) %>%
    dplyr::filter(!park == "DMP") %>%
    dplyr::select(file_full_dir, park_path, method_path)

  # test <- save_list %>%
  #   dplyr::filter(str_detect(file_full_dir, "Exmouth"))
  # head(test)

  # Creating sub directory lists ----
  park_folder <- save_list %>%
    dplyr::distinct(park_path)

  method_folder <- save_list %>%
    dplyr::distinct(method_path)

  # Creating sub directories ----
  # Added some more checking for errors

  for(j in seq_along(park_folder$park_path)) {
    p <- park_folder$park_path[j]

    dir.create(p, recursive = T, showWarnings = F)

    if (!dir.exists(p)) {
      warning(sprintf("Failed to create directory: %s", p))
    }
  }

  for(j in seq_along(method_folder$method_path)) {

    p <- method_folder$method_path [j]

    dir.create(p,
               recursive = T, # Needs to be recursive to create the 'Fish' subdirectory
               showWarnings = F)

    if (!dir.exists(p)) {
      warning(sprintf("Failed to create directory: %s", p))
    }
  }


  # Copy data to correct directories ----
  file.copy(from = save_list$file_full_dir, to = file.path(save_list$method_path,
                                                           basename(save_list$file_full_dir)),
            overwrite = TRUE)

}

