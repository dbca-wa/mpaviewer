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
generate_data <- function(raw_dir, save = TRUE, dest = here::here("inst/data/mpa_data.rds")) {
  message("This function takes ~ 10 minutes to run")

  # data_dir <- here::here("G:/mpaviewer_data")
  data_dir <- raw_dir

  # Benthic data
  # New coral data in dashboard format
  coral_cover <- list.files(path = data_dir, recursive = T, pattern = "_coral.csv", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_csv(., data_dir = data_dir)) %>%
    dplyr::filter(level2class %in% c("Hard coral", "Octocorals - Hard")) %>%
    dplyr::mutate(year = as.numeric(year),
                  percent_cover = as.numeric(percent_cover),
                  plot_year = as.numeric(year),
                  latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:")) # get rid of old files

  unique(coral_cover$marine_park)

  coral_cover_metadata <- coral_cover %>%
    dplyr::select(zone, sector, site, site_code, latitude, longitude, replicate, survey, year, date, plot_year, analysis, software, marine_park, method) %>%
    dplyr::distinct()

  coral_cover_transect <- plyr::ddply(coral_cover, plyr::.(marine_park, method, survey, plot_year, sector, site), plyr::summarize, percent_cover = sum(percent_cover))

  rec_3b <- list.files(path = data_dir, recursive = T, pattern = "REC3b.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
    purrr::map_df(~ read_dbca_files_csv(., data_dir = data_dir)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(year = as.numeric(year),
                  mean = as.numeric(mean))

  rec_3c2 <- list.files(path = data_dir, recursive = T, pattern = "REC3c2.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
    purrr::map_df(~ read_dbca_files_csv(., data_dir = data_dir)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(year = as.numeric(year),
                  mean = as.numeric(mean),
                  sd = as.numeric(sd),
                  se = as.numeric(se))

  ### ► Life history sheet ----
  ## Will need to replace with DBCA's own version eventually but this will work for time being
  # TODO use the caab common names here instead

  synonyms <- CheckEM::aus_synonyms %>% dplyr::distinct()

  common_names <- CheckEM::australia_life_history %>%
    dplyr::select(scientific_name, family, genus, species, australian_common_name)

  dbca_googlesheet_url <- "https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?usp=sharing"

  # Old life hsitory sheet
  # life_history <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "life_history") %>%
  #   CheckEM::clean_names() %>%
  #   dplyr::rename(code = region_code)

  life_history <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "functional_traits") %>%
    CheckEM::clean_names() %>%
    dplyr::select(!complex_functional_group) %>%
    dplyr::rename(trophic_group = simple_functional_group) #%>%
    #dplyr::glimpse()

  # unique(life_history$trophic_group)

  complete_sites <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "temporal_years_sites") %>%
    CheckEM::clean_names() %>%
    dplyr::mutate(year = strsplit(as.character(include_years), split = ", "))%>%
    tidyr::unnest(year) %>%
    dplyr::mutate(site = strsplit(as.character(include_sites), split = ", "))%>%
    tidyr::unnest(site) %>%
    dplyr::distinct() %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::select(marine_park, method, year, site) %>%
    dplyr::mutate(complete = "Consistently sampled")

  complete_needed_campaigns <- complete_sites %>%
    dplyr::distinct(marine_park, method) %>%
    dplyr::mutate(complete_needed = "Consistently sampled")

  codes <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "park_codes") %>%
    dplyr::rename(marine_park = full.name) %>%
    dplyr::select(marine_park, code)

  fished_species <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "target_species") %>%
    CheckEM::clean_names()

  trophic_groups <- life_history %>%
    # dplyr::select(-c(target.code, feeding.guild, trophic.guild)) %>% # TODO use the regions for this
    # dplyr::mutate(trophic_group = stringr::str_replace_all(.$community, c("NANA" = "Unknown",
    #                                                                           "NA" = "Unknown",
    #                                                                           "planktivore" = "Planktivore",
    #                                                                           "Algal Feeder" = "Algal feeder"))) %>%
    # tidyr::replace_na(list(trophic_group = "Unknown")) %>%
    dplyr::distinct() %>%
    # dplyr::full_join(codes)%>%
    # dplyr::filter(!is.na(marine_park)) %>% # To get rid of ones that don't have data yet
    dplyr::filter(!is.na(genus))

  test <- trophic_groups %>%
    dplyr::group_by(family, genus, species) %>%
    dplyr::summarise(n = dplyr::n())

  # There are a few duplicate trophic groups that will cause errors
  # TODO Use the region matching for trophic and fish!!!

  # park_popups <- here::here("inst/data/parks.popups.csv") |> # BG TO DO -  CHANGE THIS
  #   read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
  #   CheckEM::clean_names()

  zoning <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "park_gazettal") %>%
    CheckEM::clean_names()

  foa_codes <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "fishes_of_australia") %>%
    CheckEM::clean_names() %>%
    dplyr::select(-c(number)) %>%
    dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
    dplyr::left_join(common_names) %>%
    dplyr::mutate(scientific_name = paste0(scientific_name, " (", australian_common_name, ")"))

  interpretation_trends <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "interpretation_trends") %>%
    CheckEM::clean_names()

  # _______________________________________________________ ----
  #                        READ IN DATA                     ----
  # _______________________________________________________ ----

  ## ► Metadata (same for every method and data type) ----

  folders <- list.files(path = data_dir, recursive = T, pattern = "_Metadata.csv", full.names = T) %>%
    as.data.frame() %>%
    dplyr::mutate(folder_structure = stringr::str_replace_all(., paste(data_dir, "/", sep = ""), "")) %>%
    tidyr::separate(folder_structure, into = c("marine_park","indicator", "method", "campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    dplyr::mutate(read_method = forcats::fct_recode(method,
                                                    "point" = "BRUVs",
                                                    "point" = "BRUVS",
                                                    "transect" = "DOVs",
                                                    "transect" = "ROVs",
                                                    "transect" = "UVC_ROV")) %>%
    dplyr::distinct(marine_park, indicator, read_method, method)

  unique(folders$marine_park)
  unique(folders$indicator)
  unique(folders$read_method)
  unique(folders$method)

  metadata <- data.frame()

  for(i in 1:nrow(folders)){


    folder <- folders[i,]
    path <- paste(data_dir,unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

    message(path)

    read_method <- unique(folder$read_method)
    marine_park <- unique(folder$marine_park)
    method <- unique(folder$method)

    if(read_method %in% "point"){

      metadata_temp <- CheckEM::read_metadata(dir = path, method = "BRUVs") %>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    } else {

      metadata_temp <- CheckEM::read_metadata(dir = path, method = "DOVs")%>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    }

    metadata <- dplyr::bind_rows(metadata, metadata_temp)

  }

  metadata <- metadata %>%
    dplyr::mutate(latitude_dd = as.numeric(latitude_dd),
                  longitude_dd = as.numeric(longitude_dd)) %>%
    dplyr::left_join(zoning) %>%

    dplyr::mutate(status = stringr::str_replace_all(.$status, c("Sanctuary" = "No-take",
                                                                "No Take" = "No-take",
                                                                "MPA" = "No-take",
                                                                "Reserve" = "No-take",
                                                                "No-Take" = "No-take",
                                                                "Protected" = "No-take"))) %>%
    dplyr::mutate(dbca_zone = stringr::str_replace_all(.$dbca_zone, c("Sanctuary Zone" = "Sanctuary"))) %>%
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs+UVC" = "UVC_ROV")) %>%
    dplyr::mutate(year = as.numeric(substr(campaignid, 1, 4))) %>%
    dplyr::left_join(.,complete_sites) %>%
    dplyr::left_join(.,complete_needed_campaigns) %>%
    dplyr::mutate(complete = dplyr::if_else(is.na(complete_needed), "Consistently sampled", complete)) %>%
    tidyr::replace_na(list(complete = "Intermittently sampled")) %>%

    dplyr::mutate(dbca_zone = as.character(dbca_zone)) %>%

    dplyr::mutate(dbca_zone = stringr::str_replace_all(dbca_zone, c("Sanctaury" = "Sanctuary",
                                                           "SP Benthic" = "SP Benthic Protection",
                                                           "SP Benthic Protection Protection" = "SP Benthic Protection",
                                                           "Marine Management Area" = "General Use",
                                                           "Marine" = "",
                                                           "Recreational" = "Recreation"))) %>%

    dplyr::select(marine_park, method, campaignid, sample, latitude_dd, longitude_dd, date_time,
                  location, status, site,
                  successful_count, successful_length,
                  depth_m, observer_count,
                  year,
                  # month, day,
                  gazetted, re_zoned, complete, dbca_zone, dbca_sanctuary) %>% # Trying to remove columns to save space/time to load the app
    dplyr::filter(!campaignid %in% c("2021-05_JurienBay.MP.Monitoring_UVC")) # removed due to double up with 2021 ROVs

  names(metadata) %>% sort() # All the names of the dataframe
  unique(metadata$marine_park) %>% sort()
  unique(metadata$method) %>% sort()
  unique(metadata$campaignid) %>% sort()
  unique(metadata$dbca_zone) # Make sure these all make sense
  unique(metadata$complete)
  unique(metadata$year) %>% sort()
  unique(metadata$dbca_sanctuary)
  unique(metadata$status)

  test <- metadata %>%
    dplyr::filter(is.na(year)) # finds any missing years

  test_complete <- metadata %>%
    dplyr::filter(complete %in% "Consistently sampled")

  lats <- metadata %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(mean_lat = mean(latitude_dd)) %>% # biggest is the most north
    dplyr::arrange(desc(mean_lat)) # Could make this again on the server side

  metadata$marine_park <- forcats::fct_relevel(metadata$marine_park, c(unique(lats$marine_park)))

  ## ► Summarise to find sampling effort, this is used for the leaflet maps ----
  sampling_effort <- metadata %>%
    dplyr::group_by(marine_park, method, sample, status, site, location, latitude_dd, longitude_dd, depth_m, complete) %>%
    dplyr::summarise(number_of_times_sampled = dplyr::n()) %>%
    dplyr::ungroup()

  ## ► Generic data (still using "sample") ----
  ## ► Count ----

  count <- data.frame()

  for(i in 1:nrow(folders)){

    folder <- folders[i,]
    path <- paste(data_dir, unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

    message(path)

    read_method <- unique(folder$read_method)
    marine_park <- unique(folder$marine_park)
    method <- unique(folder$method)

    if(read_method %in% "point"){

      count_temp <- CheckEM::read_counts(dir = path, method = "BRUVs") %>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    } else {

      count_temp <- CheckEM::read_counts(dir = path, method = "DOVs")%>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    }

    count <- dplyr::bind_rows(count, count_temp)

  }

  count <- count %>%
    dplyr::mutate(count = as.numeric(count)) %>%
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs+UVC" = "UVC_ROV")) %>%
    # Attempt to partially tidy the data ---
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus))%>% dplyr::filter(!campaignid %in% c("2021-05_JurienBay.MP.Monitoring_UVC"))

  unique(count$campaignid)

  names(count)

  ### ► Length ----

  length <- data.frame()

  for(i in 1:nrow(folders)){

    folder <- folders[i,]
    path <- paste(data_dir, unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

    message(path)

    read_method <- unique(folder$read_method)
    marine_park <- unique(folder$marine_park)
    method <- unique(folder$method)

    if(read_method %in% "point"){

      length_temp <- CheckEM::read_gen_length(dir = path, method = "BRUVs") %>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    } else {

      length_temp <- CheckEM::read_gen_length(dir = path, method = "DOVs")%>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    }

    length <- dplyr::bind_rows(length, length_temp)

  }

  length <- length %>%
    dplyr::mutate(number = as.numeric(count)) %>%
    dplyr::mutate(length = as.numeric(length_mm)) %>%
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs+UVC" = "UVC_ROV")) %>%
    dplyr::filter(!is.na(length)) %>%
    # Attempt to partially tidy the data ---
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus))%>% dplyr::filter(!campaignid %in% c("2021-05_JurienBay.MP.Monitoring_UVC"))


  names(length)

  ### ► EventMeasure data ----
  em_campaigns <- list.files(path = paste(data_dir,"/", sep = ""), recursive = T, pattern = "_Lengths.txt|_Lengths.TXT", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(., data_dir = data_dir)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Lengths.txt" = "",
                                                                        "_Lengths.TXT" = ""))) %>%
    dplyr::distinct(campaignid) %>%
    dplyr::filter(!campaignid %in% c("2021-05_JurienBay.MP.Monitoring_UVC")) %>%
    dplyr::pull("campaignid") #%>%
    #dplyr::glimpse()

  # Read in points ----
  points <- data.frame()

  for(i in 1:nrow(folders)){

    folder <- folders[i,]
    path <- paste(data_dir, unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

    message(path)

    read_method <- unique(folder$read_method)
    marine_park <- unique(folder$marine_park)
    method <- unique(folder$method)

    if(read_method %in% "point"){

      points_temp <- CheckEM::read_points(dir = path, method = "BRUVs") %>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    } else {

      points_temp <- CheckEM::read_points(dir = path, method = "DOVs")%>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    }

    points <- dplyr::bind_rows(points, points_temp)

  }



  points <- points %>%
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs+UVC" = "UVC_ROV")) %>%

    # Attempt to partially tidy the data ---
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
    dplyr::mutate(sample = stringr::str_replace_all(.$sample, "SIMP_20200323_PP_DOV_3.", "SIMP_20200323_PP_DOV_3"))%>% dplyr::filter(!campaignid %in% c("2021-05_JurienBay.MP.Monitoring_UVC")) # to fix mistake

  unique(points$campaignid) %>% sort()

  length_threed_points <- data.frame()

  for(i in 1:nrow(folders)){

    folder <- folders[i,]
    path <- paste(data_dir, unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

    message(path)

    read_method <- unique(folder$read_method)
    marine_park <- unique(folder$marine_park)
    method <- unique(folder$method)

    if(read_method %in% "point"){

      length_threed_points_temp <- CheckEM::read_em_length(dir = path, method = "BRUVs") %>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    } else {

      length_threed_points_temp <- CheckEM::read_em_length(dir = path, method = "DOVs")%>%
        dplyr::mutate(marine_park = marine_park) %>%
        dplyr::mutate(method = method)

    }

    length_threed_points <- dplyr::bind_rows(length_threed_points, length_threed_points_temp)

  }

  test <- length_threed_points %>%
    dplyr::group_by(campaignid) %>%
    dplyr::summarise(sum = sum(number))

  length_threed_points <- length_threed_points %>%
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs+UVC" = "UVC_ROV")) %>%
    dplyr::mutate(sample = stringr::str_replace_all(.$sample, "SIMP_20200323_PP_DOV_3.", "SIMP_20200323_PP_DOV_3"))%>% dplyr::filter(!campaignid %in% c("2021-05_JurienBay.MP.Monitoring_UVC")) # to fix mistake

  test <- length_threed_points %>%
    dplyr::group_by(campaignid) %>%
    dplyr::summarise(sum = sum(number))

  ## _______________________________________________________ ----
  ##                   QUICK DATA CHECKS                     ----
  ## _______________________________________________________ ----

  # count missing metadata
  test <- dplyr::anti_join(count, metadata) %>%
    dplyr::distinct(campaignid, sample) # 0 samples

  # length missing metadata
  test <- dplyr::anti_join(length, metadata) %>%
    dplyr::distinct(campaignid, sample) # 0 samples

  # samples not in count
  test <- dplyr::anti_join(metadata, count)%>%
    dplyr::filter(!campaignid %in% c(em_campaigns)) # 144 samples without fish? does that make sense

  numbers <- test %>%
    dplyr::group_by(campaignid) %>%
    dplyr::summarise(n = dplyr::n())

  test <- dplyr::anti_join(metadata, points, by = c("marine_park", "method", "campaignid", "sample")) %>%
    dplyr::filter(campaignid %in% c(em_campaigns))

  ## _______________________________________________________ ----
  ##                     COMPLETE LENGTH DATA                ----
  ## _______________________________________________________ ----

  # Have created length first to calc DOV abundance.

  # Replicate rows where n is >1 for length dataframes
  length <- length[rep(row.names(length), length$number), ]
  length_threed_points <- length_threed_points[rep(row.names(length_threed_points), length_threed_points$number), ]

  test <- length_threed_points %>%
    dplyr::group_by(campaignid) %>%
    dplyr::summarise(sum = sum(number))

  length <- length %>%
    dplyr::mutate(number = 1)

  length_threed_points <- length_threed_points %>%
    dplyr::mutate(number = 1)

  complete_length <- length %>%
    dplyr::bind_rows(length_threed_points) %>%
    dplyr::mutate(number = 1) %>%

    dplyr::left_join(., synonyms) %>%
    dplyr::mutate(genus = ifelse(!is.na(genus_correct), genus_correct, genus)) %>%
    dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
    dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
    dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%

    dplyr::full_join(metadata) %>%
    tidyr::complete(tidyr::nesting(marine_park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(number = 0)) %>%
    dplyr::select(marine_park, campaignid, method, sample, family, genus, species, number, length) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
    dplyr::left_join(common_names) %>%
    dplyr::mutate(scientific_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
    dplyr::mutate(id = paste(campaignid, sample)) %>%

    # Attempt to partially tidy the data ---
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus))

  length(unique(complete_length$id))
  length(unique(complete_length$scientific_name))

  1126 * 9902

  complete_length$marine_park <- forcats::fct_relevel(complete_length$marine_park, c(unique(lats$marine_park)))

  unique(complete_length$marine_park)
  names(complete_length)

  ## _______________________________________________________ ----
  ##                  COMPLETE ABUNDANCE DATA                ----
  ## _______________________________________________________ ----

  # stereo-DOV abundance from 3D points and lengths
  dov_abundance <- length_threed_points %>%
    # dplyr::filter(!method %in% c("stereo-BRUVs")) %>%

    dplyr::left_join(., synonyms) %>%
    dplyr::mutate(genus = ifelse(!is.na(genus_correct), genus_correct, genus)) %>%
    dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
    dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
    dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%

    # Attempt to partially tidy the data ---
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%

    dplyr::mutate(number = as.numeric(number)) %>%

    dplyr::group_by(marine_park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(., metadata) %>%
    dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
    dplyr::filter(!method %in% c("stereo-BRUVs")) %>%
    dplyr::filter(campaignid %in% c(em_campaigns)) %>%
    dplyr::as_data_frame()#%>%
    #dplyr::glimpse()

  #unique(dov_abundance$campaignid)
  # 2017-04_Shoalwater.MP.Monitoring_stereoDOVs

  test <- dov_abundance %>%
    dplyr::group_by(campaignid) %>%
    dplyr::summarise(total = sum(maxn))

  sum(dov_abundance$maxn)

  # Create a complete total abundance dataset (For generic DOVs)
  count_summary <- count %>%
    dplyr::left_join(., synonyms) %>%
    dplyr::mutate(genus = ifelse(!is.na(genus_correct), genus_correct, genus)) %>%
    dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
    dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
    dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
    dplyr::full_join(metadata) %>% # 2726 rows (527 samples)
    dplyr::group_by(marine_park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count)) %>%
    dplyr::ungroup() %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
    dplyr::filter(!method %in% c("stereo-BRUVs"))%>%
    dplyr::filter(!campaignid %in% c(em_campaigns))

  unique(count_summary$campaignid) %>% sort()

  test1 <- count_summary %>%
    dplyr::group_by(marine_park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(n = dplyr::n())

  ### MaxN (For BRUVs) ----
  count_maxn <- count %>%
    dplyr::left_join(., synonyms) %>%
    dplyr::mutate(genus = ifelse(!is.na(genus_correct), genus_correct, genus)) %>%
    dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
    dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
    dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
    dplyr::filter(method %in% c("stereo-BRUVs")) %>%
    dplyr::group_by(marine_park, method, campaignid, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count)) %>%
    dplyr::mutate(scientific_name = paste(genus, species, sep = " "))

  maxn <- points %>%
    dplyr::left_join(., synonyms) %>%
    dplyr::mutate(genus = ifelse(!is.na(genus_correct), genus_correct, genus)) %>%
    dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
    dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
    dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
    dplyr::group_by(marine_park, method, campaignid, sample, filename, period, periodtime, frame, family, genus, species) %>%
    dplyr::summarise(maxn = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(marine_park, method, campaignid, sample, family, genus, species) %>%
    dplyr::slice(which.max(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(count_maxn) %>%
    dplyr::mutate(maxn = as.numeric(maxn)) %>%
    dplyr::full_join(metadata) %>%
    dplyr::select(marine_park, method, campaignid, sample, family, genus, species, maxn) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
    dplyr::filter(method %in% c("stereo-BRUVs"))

  unique(maxn$campaignid)

  abundance <- dplyr::bind_rows(maxn, count_summary, dov_abundance) %>% # 135,674 rows
    dplyr::left_join(common_names) %>%
    dplyr::left_join(foa_codes) %>%
    dplyr::mutate(scientific_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
    dplyr::full_join(metadata) %>%
    dplyr::select(marine_park, campaignid, sample, method, scientific_name, family, genus, species, maxn) %>%
    tidyr::complete(tidyr::nesting(marine_park, campaignid, sample, method), tidyr::nesting(family, genus, species, scientific_name)) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::mutate(id = paste(campaignid, sample, method, sep = "_")) %>% # These are just for checking the number of rows
    dplyr::mutate(species_key = paste0(family, genus, species)) %>% # These are just for checking the number of rows
    dplyr::full_join(metadata) %>%
    dplyr::filter(!species_key %in% "NANANA")

  length(unique(abundance$id)) # 9902
  length(unique(abundance$species_key)) # 1196 species
  # 1196 * 9902 # = 11,842,792 (correct number of rows)

  test <- abundance %>%
    dplyr::group_by(id, species_key) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n>1)

  # CHECKS on abundance data ----
  # unique(abundance$marine_park)
  abundance$marine_park <- forcats::fct_relevel(abundance$marine_park, c(unique(lats$marine_park)))

  unique(abundance$marine_park)
  unique(abundance$method)

  test <- abundance %>%
    dplyr::filter(method %in% "stereo-ROVs+UVC")

  complete_length_summary <- complete_length %>%
    dplyr::group_by(marine_park, method, campaignid, sample) %>%
    dplyr::summarise(lengths = sum(number))

  missing_metadata <- dplyr::anti_join(complete_length_summary, metadata) # One with a random dot on the end

  missing_fish <- dplyr::anti_join(metadata, complete_length_summary) # None

  abundance_summary <- abundance %>%
    dplyr::group_by(marine_park, method, campaignid, sample) %>%
    dplyr::summarise(abundance = sum(maxn))

  missing_metadata <- dplyr::anti_join(abundance_summary, metadata) # One with a random dot on the end

  missing_fish <- dplyr::anti_join(metadata, abundance_summary) # None

  ## _______________________________________________________ ----
  ##                      ABUNDANCE METRICS                  ----
  ## _______________________________________________________ ----

  names(abundance)

  species_to_keep <- fished_species %>%
    tidyr::separate(report_as, into = c("genus", "species")) %>%
    dplyr::select(marine_park, method, genus, species)

  fished_abundance <- dplyr::semi_join(abundance, fished_species) %>%
    dplyr::left_join(fished_species) %>%
    dplyr::select(-c(family, scientific_name)) %>%
    tidyr::separate(report_as, into = c("genus", "species")) %>%
    dplyr::left_join(common_names) %>%
    dplyr::mutate(scientific_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
    # dplyr::filter(maxn > 0) # NOTE REMEMBER TO TURN OFFF!!!!!
    dplyr::group_by(marine_park, campaignid, method, sample, family, genus, species, scientific_name, id, species_key) %>%
    dplyr::summarise(total_abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(metadata) %>%
    dplyr::select(marine_park, campaignid, sample, total_abundance, method, family, genus, species, scientific_name) %>%
    tidyr::complete(tidyr::nesting(marine_park, campaignid, sample, method), tidyr::nesting(family, genus, species, scientific_name)) %>%
    tidyr::replace_na(list(total_abundance = 0)) %>%
    dplyr::full_join(metadata) %>%
    dplyr::filter(!is.na(scientific_name)) %>%
    dplyr::mutate(id = paste(campaignid, sample, method, sep = "_")) %>% # These are just for checking the number of rows
    dplyr::mutate(species_key = paste0(family, genus, species)) %>% # These are just for checking the number of rows
    # Need to only keep the species that are targeted in that marine park
    dplyr::semi_join(species_to_keep)

  names(fished_abundance) %>% sort()

  length(unique(fished_abundance$id)) # 7677
  unique(fished_abundance$marine_park)
  unique(metadata$marine_park)
  #
  #   test <- fished_abundance %>%
  #     dplyr::group_by(marine_park, scientific_name) %>%
  #     dplyr::summarise(n = dplyr::n())

  # The correct number of rows is not easy to calculate - due to number of species being different for each marine park
  fished_summed <- fished_abundance %>%
    dplyr::group_by(marine_park, campaignid, method, sample) %>%
    dplyr::summarise(total_abundance = sum(total_abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::select(marine_park, campaignid, sample, total_abundance, method) %>%
    dplyr::full_join(metadata)

  # If there are any NAs the marine park is missing from the life history sheet
  # test <- fished_summed %>% dplyr::filter(is.na(total_abundance))

  trophic_abundance <- dplyr::left_join(abundance, trophic_groups) %>%
    tidyr::replace_na(list(trophic_group = "Unknown")) %>%
    dplyr::select(marine_park, campaignid, method, sample, trophic_group, maxn) %>%
    dplyr::group_by(marine_park, campaignid, method, sample, trophic_group) %>%
    dplyr::summarise(total_abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(metadata) %>%
    tidyr::complete(tidyr::nesting(marine_park, campaignid, method, sample), tidyr::nesting(trophic_group)) %>%
    tidyr::replace_na(list(total_abundance = 0)) %>%
    dplyr::full_join(metadata) %>%
    dplyr::mutate(id = paste(campaignid, sample, method, sep = "_"))

  # length(unique(trophic_abundance$trophic_group)) # 9
  # length(unique(trophic_abundance$id))
  # 9 * 8870

  total_abundance <- abundance %>%
    dplyr::group_by(marine_park, campaignid, method, sample) %>%
    dplyr::summarise(total_abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::select(marine_park, campaignid, sample, total_abundance, method)

  # Check for double ups
  test <- total_abundance %>%
    dplyr::group_by(marine_park, campaignid, method, sample) %>%
    dplyr::summarise(test = dplyr::n()) %>%
    dplyr::filter(test > 1)

  species_richness <- abundance %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::select(marine_park, method, campaignid, sample, scientific_name, maxn) %>%
    dplyr::group_by(marine_park, method, campaignid, sample) %>%
    dplyr::summarise(species_richness = length(unique(scientific_name))) %>%
    dplyr::select(marine_park, campaignid, sample, species_richness, method) %>%
    dplyr::full_join(metadata) %>%
    tidyr::replace_na(list(species_richness = 0))

  ## _______________________________________________________ ----
  ##                         ALL METRICS                     ----
  ## _______________________________________________________ ----

  all_data <- metadata %>%
    dplyr::left_join(total_abundance) %>%
    dplyr::left_join(species_richness) %>%
    tidyr::pivot_longer(., c(total_abundance, species_richness), names_to = "metric") %>%
    dplyr::mutate(metric = stringr::str_replace_all(.$metric, c(
      "total_abundance" = "Total abundance",
      "species_richness" = "Species richness"
    )))

  fished_complete_length <- dplyr::semi_join(complete_length, fished_species)

  state_mp <- sf::st_read(here::here("inst/data/spatial/WA_MPA_2018.shp"))

  wampa  <- sf::st_read(here::here("inst/data/spatial/WA_MPA_2020_SP.shp"))                          # all wa mpas
  # simplify state parks names
  wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
  wampa$waname <- gsub(" [1-4]", "", wampa$waname)
  # ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <-
  #   c("Special Purpose Zone\n(Habitat Protection)")

  # TODO add Rottnest

  wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
  wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"

  wampa$waname <- dplyr::recode(wampa$waname,
                                "General Use" = "General Use",
                                "Special Purpose Zone (Shore Based Activities)" =
                                  "Special Purpose Zone\n(Shore Based Activities)",
                                "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = "Special Purpose Zone",
                                "MMA" = "Marine Management Area",
                                "MP" = "Marine Park",
                                "Special Purpose Zone" = "Special Purpose",
                                "Sanctuary Zone" = "Sanctuary (no-take)",
                                "General Use Zone" = "General Use",
                                "Recreation Area" = "Recreation",
                                "Sanctuary Area" = "Sanctuary (no-take)",
                                "Recreation Zone" = "Recreation",
                                "Conservation Area" = "Conservation (no-take)",
                                "General Use Area" = "General Use",

  )

  wampa <- wampa %>% dplyr::filter(!waname %in% c("Unassigned", "Marine Management Area", "Marine Nature Reserve", "Fish Habitat Protection Area", "Marine Park")) # TODO will need to include marine park soon
  unique(test$NAME)

  unique(wampa$waname)


  # # filter out unassigned and unclassified
  # state_mp <- state_mp[!state_mp$ZONE_TYPE %in% c("Unassigned (IUCN IA)", "Unassigned (IUCN II)", "Unassigned (IUCN III)", "Unassigned (IUCN IV)", "Unassigned (IUCN VI)", "MMA (Unclassified) (IUCN VI)", "MP (Unclassified) (IUCN VI)"), ]
  # state_mp$zone <- stringr::str_replace_all(state_mp$ZONE_TYPE, c("[^[:alnum:]]" = " "))
  # state_mp$zone <- stringr::str_replace_all(state_mp$zone, c(
  #   "Conservation Area  IUCN IA " = "Conservation (no-take)",
  #   "General Use  IUCN II " = "General Use",
  #   "General Use Area  IUCN VI " = "General Use",
  #   "General Use Zone  IUCN II " = "General Use",
  #   "Recreation Area  IUCN II " = "Recreation",
  #   "Recreation Zone  IUCN II " = "Recreation",
  #   "Sanctuary Area  IUCN VI " = "Sanctuary (no-take)",
  #   "Sanctuary Zone  IUCN IA " = "Sanctuary (no-take)",
  #   "Special Purpose Zone  Aquaculture   IUCN VI " = "Special Purpose",
  #   "Special Purpose Zone  Benthic Protection   IUCN IV " = "Special Purpose",
  #   "Special Purpose Zone  Dugong Protection   IUCN IV " = "Special Purpose",
  #   "Special Purpose Zone  Habitat Protection   IUCN IV " = "Special Purpose",
  #   "Special Purpose Zone  Pearling   IUCN VI " = "Special Purpose",
  #   "Special Purpose Zone  Puerulus   IUCN IA " = "Special Purpose",
  #   "Special Purpose Zone  Scientific Reference   IUCN II " = "Special Purpose",
  #   "Special Purpose Zone  Scientific Reference   IUCN VI " = "Special Purpose",
  #   "Special Purpose Zone  Seagrass Protection   IUCN IV " = "Special Purpose",
  #   "Special Purpose Zone  Shore Based Activities   IUCN II " = "Special Purpose",
  #   "Special Purpose Zone  Wildlife Conservation   IUCN VI " = "Special Purpose",
  #   "Special Purpose Zone  Wildlife Viewing and Protection   IUCN IV " = "Special Purpose",
  #   "Special Purpose Zone 1  Shore based Activities   IUCN II " = "Special Purpose",
  #   "Special Purpose Zone 2  Shore based Activities   IUCN II " = "Special Purpose",
  #   "Special Purpose Zone 3  Shore based Activities   IUCN II " = "Special Purpose",
  #   "Special Purpose Zone 3  Shore based Activities   IUCN VI " = "Special Purpose",
  #   "Special Purpose Zone 4  Shore based Activities   IUCN II " = "Special Purpose"
  # ))

  state_mp <- wampa
  state_mp$zone <- as.factor(state_mp$waname)
  state_mp$zone <- forcats::fct_relevel(
    state_mp$zone,
    "Conservation (no-take)",
    "Sanctuary (no-take)",
    "Recreation",
    "General Use",
    "Special Purpose"
  )

  state_pal <- leaflet::colorFactor(c(
    "#bfaf02", # conservation
    "#7bbc63", # sanctuary = National Park
    "#fdb930", # recreation
    "#b9e6fb", # general use
    "#ccc1d6" # special purpose
  ), state_mp$zone)


  # spatial.data <- state_mp@data

  # -----------------------------------------------------------------------------#
  # Write data to .rds
  # -----------------------------------------------------------------------------#

  # Abundance without zeros for leaflet
  abundance_leaflet <- abundance %>%
    dplyr::group_by(marine_park, method, latitude_dd, longitude_dd, year, status, site, sample, scientific_name, complete) %>%
    dplyr::summarise(maxn = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(maxn > 0)

  abundance_leaflet <- data.table::data.table(abundance_leaflet)

  metadata_leaflet <- metadata %>%
    dplyr::select(marine_park, method, latitude_dd, longitude_dd, year, status, site, sample, complete)

  metadata_leaflet <- data.table::data.table(metadata_leaflet)

  # Reformat data into summaries before sending to shinyapp
  abundance_sum <- abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific_name, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(maxn), se = mpaviewer::se(maxn)) %>%
    dplyr::ungroup()

  abundance_sum <- data.table::data.table(abundance_sum)
  # reduced by 6 million rows

  abundance_sum_sanctuary <- abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific_name, gazetted, re_zoned, dbca_sanctuary, complete) %>% #complete,  #removed complete after meeting with Jordan 6th November
    dplyr::summarise(mean = mean(maxn), se = mpaviewer::se(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(dbca_sanctuary))

  abundance_sum_sanctuary <- data.table::data.table(abundance_sum_sanctuary)

  top_ten <- abundance %>%
    dplyr::filter(!genus == family) %>%
    dplyr::group_by(marine_park, method, scientific_name) %>%
    dplyr::summarise(maxn = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(marine_park, method) %>%
    dplyr::arrange(desc(maxn)) %>%
    dplyr::top_n(10)

  top_ten <- data.table::data.table(top_ten)

  trophic_sum <- trophic_abundance %>%
    dplyr::group_by(marine_park, method, year, status, trophic_group, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(total_abundance), se = mpaviewer::se(total_abundance)) %>%
    dplyr::ungroup()

  trophic_sum <- data.table::data.table(trophic_sum)

  # reduced by 50 000 rows

  ta_sr <- all_data %>%
    dplyr::group_by(marine_park, method, year, status, metric, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()

  ta_sr <- data.table::data.table(ta_sr)

  ta_sr_sanctuary <- all_data %>%
    dplyr::group_by(marine_park, method, year, status, metric, dbca_sanctuary, gazetted, re_zoned) %>% # removed complete after meeting with Jordan 6th Nov 2023
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(dbca_sanctuary))

  ta_sr_sanctuary <- data.table::data.table(ta_sr_sanctuary)

  ta_sr_zone <- all_data %>%
    dplyr::group_by(marine_park, method, year, status, metric, dbca_zone, gazetted, re_zoned) %>% # removed complete after meeting with Jordan 6th Nov 2023
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()

  ta_sr_zone <- data.table::data.table(ta_sr_zone)

  ta_sr_site <- all_data %>%
    dplyr::group_by(marine_park, method, year, status, metric, site, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()

  ta_sr_site <- data.table::data.table(ta_sr_site)

  # Lists for dropdowns
  ordered_top_fished_species <- fished_abundance %>%
    dplyr::group_by(marine_park, method, scientific_name) %>%
    dplyr::summarise(total = sum(total_abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(total))

  ordered_top_fished_species <- data.table::data.table(ordered_top_fished_species)

  ordered_top_species <- abundance %>%
    dplyr::group_by(marine_park, method, scientific_name) %>%
    dplyr::summarise(total = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(total))

  ordered_top_species <- data.table::data.table(ordered_top_species)



  fished_species_sum <- fished_abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific_name, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(total_abundance), se = mpaviewer::se(total_abundance)) %>%
    dplyr::ungroup()

  fished_species_sum <- data.table::data.table(fished_species_sum)

  fished_species_sum_sanctuary <- fished_abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific_name, gazetted, re_zoned, complete, dbca_sanctuary) %>%
    dplyr::summarise(mean = mean(total_abundance), se = mpaviewer::se(total_abundance)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(dbca_sanctuary))

  fished_species_sum_sanctuary <- data.table::data.table(fished_species_sum_sanctuary)


  fished_sum <- fished_summed %>%
    dplyr::group_by(marine_park, method, year, status, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(total_abundance), se = mpaviewer::se(total_abundance)) %>%
    dplyr::ungroup()

  fished_sum <- data.table::data.table(fished_sum)

  fished_sum_sanctuary <- fished_summed %>%
    dplyr::group_by(marine_park, method, year, status, gazetted, re_zoned, complete, dbca_sanctuary) %>%
    dplyr::summarise(mean = mean(total_abundance), se = mpaviewer::se(total_abundance)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(dbca_sanctuary))

  fished_sum_sanctuary <- data.table::data.table(fished_sum_sanctuary)

  # TODO split these into ones that have been reformatted and ones that don't need it
  # Using data table to set keys for faster filtering ----
  metadata <- data.table::data.table(metadata)
  sampling_effort <- data.table::data.table(sampling_effort)
  lats <- data.table::data.table(lats)
  # park_popups <- data.table::data.table(park_popups)
  foa_codes <- data.table::data.table(foa_codes)
  interpretation_trends <- data.table::data.table(interpretation_trends)
  fished_complete_length <- data.table::data.table(fished_complete_length) # cant summrise because I need lengths

  abundance <- data.table::data.table(abundance) # Reformatted but still need this on for he others
  trophic_abundance <- data.table::data.table(trophic_abundance) # Reformatted
  # TODO summary for individual species
  all_data <- data.table::data.table(all_data) # Reformatted

  fished_abundance <- data.table::data.table(fished_abundance) # Reformatted
  fished_summed <- data.table::data.table(fished_summed) # Reformatted

  # TODO Come back to these
  coral_cover_transect <- data.table::data.table(coral_cover_transect)
  coral_cover_metadata <- data.table::data.table(coral_cover_metadata)
  rec_3b <- data.table::data.table(rec_3b)
  rec_3c2 <- data.table::data.table(rec_3c2)

  # common_names <- data.table::data.table(common_names) # Not needed


  methods <- metadata %>%
    dplyr::distinct(marine_park, method) %>%
    dplyr::mutate(marine_park = as.character(marine_park))

  test <- abundance %>%
    dplyr::group_by(campaignid) %>%
    dplyr::summarise(total = sum(maxn))


  total_number_fish <- sum(abundance$maxn)
  total_number_fish

  total_species_fish <- length(unique(abundance$scientific_name))

  total_number_fish_park <- abundance %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(total = sum(maxn))

  total_species_fish_park <- abundance %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(richness = dplyr::n_distinct(scientific_name))

  mins_watched <- metadata %>%
    dplyr::group_by(marine_park, method) %>%
    dplyr::summarise(total = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mins.per.sample = dplyr::case_when(
      method %in% "stereo-BRUVs" ~ 60,
      method %in% "stereo-DOVs" ~ 20,
      method %in% "stereo-ROVs" ~ 20)) %>%
    dplyr::mutate(mins_watched = total * mins.per.sample) %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(mins_watched = sum(mins_watched))

  biggest_fish <- complete_length %>%
    dplyr::filter(length > 0 ) %>%
    dplyr::group_by(marine_park) %>%
    dplyr::slice(which.max(length)) %>%
    dplyr::select(marine_park, method, campaignid, family, genus, species, length)

  data.table::setkey(methods)
  data.table::setkey(abundance_sum)
  data.table::setkey(abundance_sum_sanctuary)
  data.table::setkey(trophic_sum)
  data.table::setkey(ta_sr)
  data.table::setkey(ta_sr_sanctuary)
  data.table::setkey(ta_sr_zone)
  data.table::setkey(ta_sr_site)
  data.table::setkey(fished_species_sum)
  data.table::setkey(fished_sum_sanctuary)
  data.table::setkey(fished_sum)
  data.table::setkey(top_ten)
  data.table::setkey(ordered_top_fished_species)
  data.table::setkey(ordered_top_species)
  data.table::setkey(abundance_leaflet)
  data.table::setkey(metadata_leaflet)


  data.table::setkey(lats)
  data.table::setkey(abundance)
  data.table::setkey(trophic_abundance)
  data.table::setkey(all_data)
  data.table::setkey(fished_complete_length)
  data.table::setkey(fished_abundance)
  # data.table::setkey(fished_summed)
  data.table::setkey(metadata)
  data.table::setkey(sampling_effort)
  # data.table::setkey(park_popups)
  data.table::setkey(coral_cover_transect)
  data.table::setkey(coral_cover_metadata)
  data.table::setkey(rec_3b)
  data.table::setkey(rec_3c2)
  # data.table::setkey(common_names) # Not needed
  data.table::setkey(foa_codes)
  data.table::setkey(interpretation_trends)

  # Version 3: one object
  mpa_data <- structure(
    list(
      downloaded_on = Sys.time(),
      ta_sr = ta_sr,  # summarised
      ta_sr_sanctuary = ta_sr_sanctuary,  # summarised
      ta_sr_zone = ta_sr_zone,  # summarised
      ta_sr_site = ta_sr_site,  # summarised
      top_ten = top_ten,  # summarised
      ordered_top_fished_species = ordered_top_fished_species,
      ordered_top_species = ordered_top_species,
      abundance_sum = abundance_sum, # summarised
      abundance_sum_sanctuary = abundance_sum_sanctuary, # summarised
      abundance_leaflet = abundance_leaflet,
      metadata_leaflet = metadata_leaflet,
      lats = lats,
      methods = methods,
      # abundance = abundance,  # Turned off to speed up app
      # trophic_abundance = trophic_abundance,
      trophic_sum = trophic_sum,  # summarised
      all_data = all_data,
      fished_complete_length = fished_complete_length, # Turned off to speed up app
      fished_abundance = fished_abundance,
      fished_sum = fished_sum, # summarised
      fished_sum_sanctuary = fished_sum_sanctuary, # summarised
      fished_species_sum = fished_species_sum, # summarised
      fished_species_sum_sanctuary = fished_species_sum_sanctuary, # summarised
      metadata = metadata,
      sampling_effort = sampling_effort,
      state_mp = state_mp,
      state_pal = state_pal,
      # park_popups = park_popups,
      coral_cover_transect = coral_cover_transect,
      coral_cover_metadata = coral_cover_metadata,
      rec_3b = rec_3b,
      rec_3c2 = rec_3c2,
      # common_names = common_names, # Not needed
      foa_codes = foa_codes,
      interpretation_trends = interpretation_trends,
      total_number_fish = total_number_fish,
      total_species_fish = total_species_fish,
      total_number_fish_park = total_number_fish_park,
      total_species_fish_park = total_species_fish_park,
      mins_watched = mins_watched
    ),
    class = "mpa_data"
  )

  # if (save == TRUE) {
  # saveRDS(mpa_data, dest, compress = FALSE) #"xz"
  save(mpa_data, file = here::here("inst/data/mpa_data.Rdata"))
  # saveRDS(x, "inst/data/mpa_data.rds", compress = FALSE) #"xz"
  # }

  mpa_data
}

#' @title S3 print method for 'mpa_data'.
#' @description Prints a short representation of mpa_data returned by
#' `generate_data`.
#' @param x An object of class `mpa_data` as returned by `generate_data`.
#' @param ... Extra parameters for `print`
#' @export
#' @family included
print.mpa_data <- function(mpa_data, ...) {
  print(
    glue::glue(
      "<MPA Data> accessed on {mpa_data$downloaded_on}\n",
      "  Abundance:   {nrow(mpa_data$abundance)}\n",
      # "  total_abundance:   {nrow(mpa_data$total_abundance)}\n",
      "  trophic_abundance:   {nrow(mpa_data$trophic_abundance)}\n",
      "  all_data:    {nrow(mpa_data$all_data)}\n",
      "  fished_complete_length:    {nrow(mpa_data$fished_complete_length)}\n",
      "  fished_abundance:    {nrow(mpa_data$fished_abundance)}\n",
      "  metadata:    {nrow(mpa_data$metadata)}\n",
      "  sampling_effort:    {nrow(mpa_data$sampling_effort)}\n",
      "  state_mp:    {nrow(mpa_data$state_mp)}\n",
      "  state_pal:    {nrow(mpa_data$state_pal)}\n"#,
      # "  park_popups:    {nrow(mpa_data$park_popups)}\n"
    )
  )
  invisible(mpa_data)
}

