#' Consolidate all data to a single set of files
#'
#' @param dir File path for the iLab dashboard
#'
#' @return Saves data
#' @export
consolidate_data <- function(dir) {

  # Metadata ----
  folders <- list.files(path = dir, recursive = T, pattern = "_Metadata.csv", full.names = T) %>%
    as.data.frame() %>%
    dplyr::mutate(folder_structure = stringr::str_replace_all(., paste(dir, "/", sep = ""), "")) %>%
    tidyr::separate(folder_structure, into = c("marine_park","indicator", "method", "campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    dplyr::mutate(read_method = forcats::fct_recode(method,
                                                    "point" = "BRUVs",
                                                    #"point" = "BRUVS",
                                                    "transect" = "DOVs",
                                                    "transect" = "ROVs",
                                                    "transect" = "UVC")) %>%
    dplyr::distinct(marine_park, indicator, read_method, method)

  metadata_joined <- data.frame()

  for(i in 1:nrow(folders)){


    folder <- folders[i,]
    path <- paste(dir, unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

    message(path)

    read_method <- unique(folder$read_method)
    marine_park <- unique(folder$marine_park)
    method <- unique(folder$method)

    if(read_method %in% "point") {

      metadata_temp <- CheckEM::read_metadata(dir = path, method = "BRUVs") %>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    } else {

      metadata_temp <- CheckEM::read_metadata(dir = path, method = "DOVs")%>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    }

    metadata_joined <- dplyr::bind_rows(metadata_joined, metadata_temp)

  }

  # DBCA Zone is missing from quite a few campaigns, think best approach is to extract from shapefile
  # Will create a new column and compare
  zones <- sf::st_read(here::here('inst/data/spatial/western-australia_marine-parks_all.gpkg'))

  metadata_sf <- sf::st_as_sf(metadata_joined, coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
    sf::st_intersection(zones) %>%                                              # This runs really slowly
    as.data.frame() %>%
    dplyr::select(campaignid, sample, dbca_zone = zone.1, agency) %>%
    dplyr::distinct()

  metadata <- metadata_joined %>%
    dplyr::mutate(latitude_dd = as.numeric(latitude_dd),
                  longitude_dd = as.numeric(longitude_dd)) %>%
    dplyr::mutate(status = stringr::str_replace_all(.$status, c("Sanctuary" = "No-take",
                                                                "No Take" = "No-take",
                                                                "MPA" = "No-take",
                                                                "Reserve" = "No-take",
                                                                "No-Take" = "No-take",
                                                                "Protected" = "No-take"))) %>%
    dplyr::mutate(dbca_zone = stringr::str_replace_all(.$dbca_zone, c("Sanctuary Zone" = "Sanctuary"))) %>%
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               #"stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "UVC" = "UVC")) %>%
    dplyr::mutate(dbca_zone = dplyr::case_when(stringr::str_detect(dbca_zone, "Benthic") ~ "SP Benthic Protection",
                                               stringr::str_detect(dbca_zone, "Recreational") ~ "Recreation",
                                               stringr::str_detect(dbca_zone, "Marine Management Area") ~ "General Use",
                                               stringr::str_detect(dbca_zone, "Scientific Reference") ~ "SP Scientific Reference",
                                               stringr::str_detect(dbca_zone, "Scientific Reference") ~ "SP Scientific Reference",
                                               stringr::str_detect(dbca_zone, "Wildlife Conservation") ~ "SP Wildlife Conservation",
                                               stringr::str_detect(dbca_zone, "Wildlife Viewing") ~ "SP Wildlife Viewing",
                                               stringr::str_detect(dbca_zone, "Commonwealth") ~ "Outside Park",
                                               stringr::str_detect(dbca_zone, "Outside") ~ "Outside Park",
                                               is.na(dbca_zone) ~ "Outside Park", # Should NAs be outside park, or general use???
                                               .default = dbca_zone)) %>%
    dplyr::select(campaignid, sample, latitude_dd, longitude_dd, date_time,
                  location, status, site,
                  successful_count, successful_length,
                  depth_m, observer_count, dbca_zone, dbca_sanctuary) %>%
    dplyr::filter(!campaignid %in% c("2021-05_JurienBay.MP.Monitoring_UVC")) # removed due to double up with 2021 ROVs

  test <- metadata %>%
    dplyr::filter(dbca_zone %in% "Outside Park")

  write.csv(test, "inst/data/spatial/test.csv", row.names = F, )

  # Count/MaxN

  # Length
}
