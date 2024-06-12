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
generate_data <- function(save = TRUE, dest = here::here("inst/data/mpa_data.rds")) {
  message("This function takes a couple minutes to run")

  data_dir <- here::here("inst/data")

  # Benthic data
  # New coral data in dashboard format
  coral_cover <- list.files(path = data_dir, recursive = T, pattern = "_coral.csv", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_csv(.)) %>%
    dplyr::filter(level2class %in% c("Hard coral", "Octocorals - Hard")) %>%
    dplyr::mutate(year = as.numeric(year),
                  percent_cover = as.numeric(percent_cover),
                  plot_year = as.numeric(year),
                  latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:")) # get rid of old files

  coral_cover_metadata <- coral_cover %>%
    dplyr::select(zone, sector, site, site_code, latitude, longitude, replicate, survey, year, date, plot_year, analysis, software, marine_park, method) %>%
    dplyr::distinct()

  coral_cover_transect <- plyr::ddply(coral_cover, plyr::.(marine_park, method, survey, plot_year, sector, site), plyr::summarize, percent_cover = sum(percent_cover))

  rec_3b <- list.files(path = data_dir, recursive = T, pattern = "REC3b.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(year = as.numeric(year),
                  mean = as.numeric(mean))

  rec_3c2 <- list.files(path = data_dir, recursive = T, pattern = "REC3c2.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(year = as.numeric(year),
                  mean = as.numeric(mean),
                  sd = as.numeric(sd),
                  se = as.numeric(se))

  ### ► Life history sheet ----
  ## Will need to replace with DBCA's own version eventually but this will work for time being
  # TODO use the caab common names here instead
  common_names <- here::here("inst/data/australia.life.history.csv") |>
    read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
    CheckEM::clean_names() %>%
    # GlobalArchive::ga.clean.names() %>%
    dplyr::select(scientific, family, genus, species, australian_common_name)

  dbca_googlesheet_url <- "https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?usp=sharing"

  life_history <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "life_history") %>%
    CheckEM::clean_names() %>%
    dplyr::rename(code = region_code)

  2

  life_history <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "functional_traits") %>%
    CheckEM::clean_names() %>%
    dplyr::select(!complex_functional_group) %>%
    dplyr::rename(trophic_group = simple_functional_group) %>%
    dplyr::glimpse()

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

  # duplicates <- complete_sites %>%
  #   dplyr::group_by(marine_park, method)

  complete_needed_campaigns <- complete_sites %>%
    dplyr::distinct(marine_park, method) %>%
    dplyr::mutate(complete_needed = "Consistently sampled")

  codes <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "park_codes") %>%
    dplyr::rename(marine_park = full.name) %>%
    dplyr::select(marine_park, code)

  # fished_species <- life_history %>%
  #   dplyr::filter(target.code %in% c("Highly Retained")) %>%
  #   dplyr::select(code, genus, species) %>%
  #   dplyr::distinct() %>%
  #   dplyr::full_join(codes) %>%
  #   dplyr::filter(!is.na(marine_park)) %>% # To get rid of ones that don't have data yet
  #   dplyr::filter(!is.na(genus))

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

  # test <- trophic_groups %>%
  #   # filter(is.na(marine_park)) %>%
  #   dplyr::distinct(code, marine_park)

  test <- trophic_groups %>%
    dplyr::group_by(family, genus, species) %>%
    dplyr::summarise(n = dplyr::n())

  # There are a few duplicate trophic groups that will cause errors
  # TODO Use the region matching for trophic and fish!!!

  unique(trophic_groups$trophic_group) %>%sort()

  park_popups <- here::here("inst/data/parks.popups.csv") |> # BG TO DO -  CHANGE THIS
    read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
    CheckEM::clean_names()

  zoning <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "park_gazettal") %>%
    CheckEM::clean_names()

  foa_codes <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "fishes_of_australia") %>%
    CheckEM::clean_names() %>%
    dplyr::select(-c(number)) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::left_join(common_names) %>%
    dplyr::mutate(scientific = paste0(scientific, " (", australian_common_name, ")"))

  interpretation_trends <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "interpretation_trends") %>%
    CheckEM::clean_names()

  # _______________________________________________________ ----
  #                        READ IN DATA                     ----
  # _______________________________________________________ ----

  ## ► Metadata (same for every method and data type) ----

  folders <- list.files(path = paste0(data_dir,"/raw"), recursive = T, pattern = "_Metadata.csv", full.names = T) %>%
    as.data.frame() %>%
    dplyr::mutate(folder_structure = stringr::str_replace_all(., paste(data_dir, "/raw/", sep = ""), "")) %>%
    tidyr::separate(folder_structure, into = c("marine_park","indicator", "method", "campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    dplyr::mutate(read_method = forcats::fct_recode(method,
                                               "point" = "BRUVs",
                                               "point" = "BRUVS",
                                               "transect" = "DOVs",
                                               "transect" = "ROVs",
                                               "transect" = "UVC_ROV")) %>%
    dplyr::distinct(marine_park, indicator, read_method, method)

  metadata <- data.frame()

  for(i in 1:nrow(folders)){

    folder <- folders[i,]
    path <- paste(data_dir,"raw", unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

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
                                               "stereo-ROVs/UVC" = "UVC_ROV")) %>%
    dplyr::mutate(year = as.numeric(substr(date, 1, 4))) %>%
    dplyr::left_join(.,complete_sites) %>%
    dplyr::left_join(.,complete_needed_campaigns) %>%
    dplyr::mutate(complete = dplyr::if_else(is.na(complete_needed), "Consistently sampled", complete)) %>%
    tidyr::replace_na(list(complete = "Intermittently sampled")) %>%
    dplyr::select(marine_park, method, campaignid, sample, latitude_dd, longitude_dd, date_time,
                  location, status, site,
                  successful_count, successful_length,
                  depth_m, observer,
                  # year, month, day,
                  gazetted, re_zoned, complete, dbca_zone, dbca_sanctuary) # Trying to remove columns to save space/time to load the app

  names(metadata) %>% sort()
  unique(metadata$complete)

  unique(metadata$marine_park)

  test_complete <- metadata %>%
    dplyr::filter(complete %in% "Consistently sampled")

  testing <- metadata %>%
    dplyr::filter(marine_park %in% "Rottnest Island Marine Reserve")

  unique(testing$status)
  unique(metadata$marine_park) %>% sort()
  unique(metadata$method) %>% sort()
  unique(metadata$campaignid) %>% sort()

  unique(metadata$dbca_zone)
  unique(metadata$dbca_sanctuary)
  unique(metadata$status)

  campaign_list <- metadata %>% dplyr::distinct(marine_park, method, campaignid, sample) # Want to create a list of every sample
  # DOes it have maxn and length associated with it??

  lats <- metadata %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(mean_lat = mean(latitude_dd)) %>% # biggest is the most north
    dplyr::arrange(desc(mean_lat)) # Could make this again on the server side

  metadata$marine_park <- forcats::fct_relevel(metadata$marine_park, c(unique(lats$marine_park)))

  # Testing
  bruv_test <- metadata %>%
    dplyr::filter(method %in% "stereo-BRUVs") %>%
    dplyr::filter(marine_park %in% "Ningaloo Marine Park")

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
    path <- paste(data_dir,"raw", unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

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
                                               "stereo-ROVs/UVC" = "UVC_ROV")) %>%
    # Attempt to partially tidy the data ---
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus))

  unique(count$campaignid)

  names(count)

  ### ► Length ----

  length <- data.frame()

  for(i in 1:nrow(folders)){

    folder <- folders[i,]
    path <- paste(data_dir,"raw", unique(folder$marine_park), unique(folder$indicator), unique(folder$method), sep = "/")

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
                                               "stereo-ROVs/UVC" = "UVC_ROV")) %>%
    dplyr::filter(!is.na(length)) %>%
    # Attempt to partially tidy the data ---
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus))


  names(length)

  ### ► EventMeasure data ----
  em.campaigns <- list.files(path = data_dir, recursive = T, pattern = "_Lengths.txt|_Lengths.TXT", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Lengths.txt" = "",
                                                                        "_Lengths.TXT" = ""))) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:", "G:")) %>% # get rid of old files
    dplyr::distinct(campaignid) %>%
    dplyr::pull("campaignid")

  points <- list.files(path = data_dir, recursive = T, pattern = "_Points.txt|_Points.TXT", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Points.txt" = "",
                                                                        "_Points.TXT" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:", "G:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs/UVC" = "UVC_ROV")) %>%
    dplyr::mutate(sample = dplyr::if_else(method%in% c("stereo-DOVs", "stereo-ROVs", "stereo-ROVs/UVC"), paste(opcode, period, sep = "_"), opcode)) %>%
    # Attempt to partially tidy the data ----
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
    dplyr::mutate(sample = stringr::str_replace_all(.$sample, "SIMP_20200323_PP_DOV_3.", "SIMP_20200323_PP_DOV_3")) # to fix mistake

  unique(points$campaignid) %>% sort()

  threed.points <- list.files(path = data_dir, recursive = T, pattern = "_3DPoints.txt|_3DPoints.TXT", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_3DPoints.txt" = "", "_3DPoints.TXT" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:", "G:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs/UVC" = "UVC_ROV")) %>%
    dplyr::mutate(sample = dplyr::if_else(method%in% c("stereo-DOVs", "stereo-ROVs", "stereo-ROVs/UVC"), paste(opcode, period, sep = "_"), opcode)) %>%
    dplyr::mutate(sample = stringr::str_replace_all(.$sample, "SIMP_20200323_PP_DOV_3.", "SIMP_20200323_PP_DOV_3")) # to fix mistake

  lengths <- list.files(path = data_dir, recursive = T, pattern = "_Lengths.txt|_Lengths.TXT", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Lengths.txt" = "",
                                                                        "_Lengths.TXT" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::mutate(length = as.numeric(length)) %>%
    dplyr::filter(!marine_park %in% c("archive", "C:", "G:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs",
                                               "stereo-ROVs" = "ROVs",
                                               "stereo-ROVs/UVC" = "UVC_ROV")) %>%
    dplyr::mutate(sample = dplyr::if_else(method%in% c("stereo-DOVs", "stereo-ROVs", "stereo-ROVs/UVC"), paste(opcode, period, sep = "_"), opcode)) %>%
    dplyr::mutate(sample = stringr::str_replace_all(.$sample, "SIMP_20200323_PP_DOV_3.", "SIMP_20200323_PP_DOV_3")) # to fix mistake

  ## _______________________________________________________ ----
  ##                   QUICK DATA CHECKS                     ----
  ## _______________________________________________________ ----

  # count missing metadata
  test <- dplyr::anti_join(count, metadata) %>%
    dplyr::distinct(campaignid, sample) # 117 samples - I think 2017-04 is meant to be 2017-03?? I manually renamed but flag with Jordan

  # length missing metadata
  test <- dplyr::anti_join(length, metadata) %>%
    dplyr::distinct(campaignid, sample) # 108 samples - same again I manually renamed but flag with Jordan

  # samples not in count
  test <- dplyr::anti_join(metadata, count)%>%
    dplyr::filter(!campaignid %in% c(em.campaigns)) # 1408 samples without fish? does that make sense


  test <- dplyr::anti_join(metadata, points, by = c("marine_park", "method", "campaignid", "sample")) %>%
    dplyr::filter(campaignid %in% c(em.campaigns))

  ## _______________________________________________________ ----
  ##                     COMPLETE LENGTH DATA                ----
  ## _______________________________________________________ ----

  # Have created length first to calc DOV abundance.

  # Replicate rows where n is >1 for length dataframes
  length <- length[rep(row.names(length), length$number), ]
  lengths <- lengths[rep(row.names(lengths), lengths$number), ]

  complete.length <- length %>%
    dplyr::bind_rows(lengths) %>%
    dplyr::mutate(number = 1) %>%
    dplyr::bind_rows(threed.points) %>%
    dplyr::full_join(metadata) %>%
    tidyr::complete(tidyr::nesting(marine_park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(number = 0)) %>%
    dplyr::select(marine_park, campaignid, method, sample, family, genus, species, number, length) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::left_join(common_names) %>%
    dplyr::mutate(scientific = paste0(scientific, " (", australian_common_name, ")")) %>%
    dplyr::mutate(id = paste(campaignid, sample)) %>%

    # Attempt to partially tidy the data ----
  dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus))

  length(unique(complete.length$id))
  length(unique(complete.length$scientific))

  1152 * 9123

  complete.length$marine_park <- forcats::fct_relevel(complete.length$marine_park, c(unique(lats$marine_park)))

  unique(complete.length$marine_park)
  names(complete.length)

  t <- complete.length %>%
    dplyr::filter(sample %in% "NCMP_22032022_EBS1-5_BRUV")

  ## _______________________________________________________ ----
  ##                  COMPLETE ABUNDANCE DATA                ----
  ## _______________________________________________________ ----

  # stereo-DOV abundance from 3D points and lengths
  dov.abundance <- dplyr::bind_rows(lengths, threed.points) %>%

    # Attempt to partially tidy the data ----
    dplyr::filter(!family %in% c("Unknown", NA)) %>%
    dplyr::mutate(species = dplyr::if_else(is.na(species), "spp", species)) %>%
    dplyr::mutate(genus = dplyr::if_else(is.na(genus), family, genus)) %>%
    dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%

    dplyr::group_by(marine_park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::filter(!method %in% c("stereo-BRUVs")) %>%
    dplyr::filter(campaignid %in% c(em.campaigns))

  unique(dov.abundance$campaignid)

  # Create a complete total abundance dataset (For generic DOVs)
  count.summary <- count %>%
    dplyr::full_join(metadata) %>% # 2726 rows (527 samples)
    dplyr::group_by(marine_park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count)) %>%
    dplyr::ungroup() %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::filter(!method %in% c("stereo-BRUVs"))%>%
    dplyr::filter(!campaignid %in% c(em.campaigns))

  unique(count.summary$campaignid) %>% sort()

  ### MaxN (For BRUVs) ----
  count.maxn <- count %>%
    dplyr::filter(method %in% c("stereo-BRUVs")) %>%
    dplyr::group_by(marine_park, method, campaignid, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count)) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " "))

  maxn <- points %>%
    dplyr::group_by(marine_park, method, campaignid, sample, filename, period, periodtime, frame, family, genus, species) %>%
    dplyr::summarise(maxn = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(marine_park, method, campaignid, sample, family, genus, species) %>%
    dplyr::slice(which.max(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(count.maxn) %>%
    dplyr::mutate(maxn = as.numeric(maxn)) %>%
    dplyr::full_join(metadata) %>%
    dplyr::select(marine_park, method, campaignid, sample, family, genus, species, maxn) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::filter(method %in% c("stereo-BRUVs"))

  unique(maxn$campaignid)

  abundance <- dplyr::bind_rows(maxn, count.summary, dov.abundance) %>% # 74657 rows
    dplyr::left_join(common_names) %>%
    dplyr::left_join(foa_codes) %>%
    dplyr::mutate(scientific = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
    dplyr::full_join(metadata) %>%
    dplyr::select(marine_park, campaignid, sample, method, scientific, family, genus, species, maxn) %>%
    tidyr::complete(tidyr::nesting(marine_park, campaignid, sample, method), tidyr::nesting(family, genus, species, scientific)) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::mutate(id = paste(campaignid, sample, method, sep = "_")) %>% # These are just for checking the number of rows
    dplyr::mutate(species.key = paste0(family, genus, species)) %>% # These are just for checking the number of rows
    dplyr::full_join(metadata)

  length(unique(abundance$id)) # 9123

  # test <- dplyr::anti_join(abundance, metadata) %>%
  #   dplyr::distinct(marine_park, campaignid, sample, method)
  #
  length(unique(abundance$species.key)) # 1237 species
  #
  # 1237 * 9123 # = 11285151 (correct number of rows)
  #
  # test <- abundance %>%
  #   dplyr::group_by(id, species.key) %>%
  #   dplyr::summarise(n = dplyr::n())

  # CHECKS on abundance data ----
  # unique(abundance$marine_park)
  abundance$marine_park <- forcats::fct_relevel(abundance$marine_park, c(unique(lats$marine_park)))

  complete.length.summary <- complete.length %>%
    dplyr::group_by(marine_park, method, campaignid, sample) %>%
    dplyr::summarise(lengths = sum(number))

  missing.metadata <- dplyr::anti_join(complete.length.summary, metadata) # One with a random dot on the end

  missing.fish <- dplyr::anti_join(metadata, complete.length.summary) # None

  abundance.summary <- abundance %>%
    dplyr::group_by(marine_park, method, campaignid, sample) %>%
    dplyr::summarise(abundance = sum(maxn))

  missing.metadata <- dplyr::anti_join(abundance.summary, metadata) # One with a random dot on the end

  missing.fish <- dplyr::anti_join(metadata, abundance.summary) # None

  ## _______________________________________________________ ----
  ##                      ABUNDANCE METRICS                  ----
  ## _______________________________________________________ ----

  names(abundance)

  species.to.keep <- fished_species %>%
    tidyr::separate(report.as, into = c("genus", "species")) %>%
    dplyr::select(marine_park, method, genus, species)

  fished.abundance <- dplyr::semi_join(abundance, fished_species) %>%
    dplyr::left_join(fished_species) %>%
    dplyr::select(-c(family, scientific)) %>%
    tidyr::separate(report.as, into = c("genus", "species")) %>%
    dplyr::left_join(common_names) %>%
    dplyr::mutate(scientific = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
    # dplyr::filter(maxn > 0) # NOTE REMEMBER TO TURN OFFF!!!!!
    dplyr::group_by(marine_park, campaignid, method, sample, family, genus, species, scientific, id, species.key) %>%
    dplyr::summarise(total.abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(metadata) %>%
    dplyr::select(marine_park, campaignid, sample, total.abundance, method, family, genus, species, scientific) %>%
    tidyr::complete(tidyr::nesting(marine_park, campaignid, sample, method), tidyr::nesting(family, genus, species, scientific)) %>%
    tidyr::replace_na(list(total.abundance = 0)) %>%
    dplyr::full_join(metadata) %>%
    dplyr::filter(!is.na(scientific)) %>%
    dplyr::mutate(id = paste(campaignid, sample, method, sep = "_")) %>% # These are just for checking the number of rows
    dplyr::mutate(species.key = paste0(family, genus, species)) %>% # These are just for checking the number of rows
    # Need to only keep the species that are targeted in that marine park
    dplyr::semi_join(species.to.keep)

    names(fished.abundance) %>% sort()

  length(unique(fished.abundance$id)) # 6066
  unique(fished.abundance$marine_park)
  unique(metadata$marine_park)
#
#   test <- fished.abundance %>%
#     dplyr::group_by(marine_park, scientific) %>%
#     dplyr::summarise(n = dplyr::n())

  # The correct number of rows is not easy to calculate - due to number of species being different for each marine park
  fished.summed <- fished.abundance %>%
    dplyr::group_by(marine_park, campaignid, method, sample) %>%
    dplyr::summarise(total.abundance = sum(total.abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::select(marine_park, campaignid, sample, total.abundance, method) %>%
    dplyr::full_join(metadata)

  # If there are any NAs the marine park is missing from the life history sheet
  # test <- fished.summed %>% dplyr::filter(is.na(total.abundance))

  trophic.abundance <- dplyr::left_join(abundance, trophic_groups) %>%
    tidyr::replace_na(list(trophic_group = "Unknown")) %>%
    dplyr::select(marine_park, campaignid, method, sample, trophic_group, maxn) %>%
    dplyr::group_by(marine_park, campaignid, method, sample, trophic_group) %>%
    dplyr::summarise(total.abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(metadata) %>%
    tidyr::complete(tidyr::nesting(marine_park, campaignid, method, sample), tidyr::nesting(trophic_group)) %>%
    tidyr::replace_na(list(total.abundance = 0)) %>%
    dplyr::full_join(metadata) %>%
    dplyr::mutate(id = paste(campaignid, sample, method, sep = "_"))

  # length(unique(trophic.abundance$trophic_group)) # 9
  # length(unique(trophic.abundance$id))
  # 9 * 8870

  total.abundance <- abundance %>%
    dplyr::group_by(marine_park, campaignid, method, sample) %>%
    dplyr::summarise(total.abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::select(marine_park, campaignid, sample, total.abundance, method)

  # Check for double ups
  test <- total.abundance %>%
    dplyr::group_by(marine_park, campaignid, method, sample) %>%
    dplyr::summarise(test = dplyr::n()) %>%
    dplyr::filter(test > 1)

  species.richness <- abundance %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::select(marine_park, method, campaignid, sample, scientific, maxn) %>%
    dplyr::group_by(marine_park, method, campaignid, sample) %>%
    dplyr::summarise(species.richness = length(unique(scientific))) %>%
    dplyr::select(marine_park, campaignid, sample, species.richness, method) %>%
    dplyr::full_join(metadata) %>%
    tidyr::replace_na(list(species.richness = 0))

  ## _______________________________________________________ ----
  ##                         ALL METRICS                     ----
  ## _______________________________________________________ ----

  all.data <- metadata %>%
    dplyr::left_join(total.abundance) %>%
    dplyr::left_join(species.richness) %>%
    tidyr::pivot_longer(., c(total.abundance, species.richness), names_to = "metric") %>%
    dplyr::mutate(metric = stringr::str_replace_all(.$metric, c(
      "total.abundance" = "Total abundance",
      "species.richness" = "Species richness"
    )))

  fished.complete.length <- dplyr::semi_join(complete.length, fished_species)

  state.mp <- sf::st_read(here::here("inst/data/spatial/WA_MPA_2018.shp"))

  wampa  <- sf::st_read(here::here("inst/data/spatial/WA_MPA_2020_SP.shp"))                          # all wa mpas
  # simplify state parks names
  wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
  wampa$waname <- gsub(" [1-4]", "", wampa$waname)
  # ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <-
  #   c("Special Purpose Zone\n(Habitat Protection)")

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
  # state.mp <- state.mp[!state.mp$ZONE_TYPE %in% c("Unassigned (IUCN IA)", "Unassigned (IUCN II)", "Unassigned (IUCN III)", "Unassigned (IUCN IV)", "Unassigned (IUCN VI)", "MMA (Unclassified) (IUCN VI)", "MP (Unclassified) (IUCN VI)"), ]
  # state.mp$zone <- stringr::str_replace_all(state.mp$ZONE_TYPE, c("[^[:alnum:]]" = " "))
  # state.mp$zone <- stringr::str_replace_all(state.mp$zone, c(
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

  state.mp <- wampa
  state.mp$zone <- as.factor(state.mp$waname)
  state.mp$zone <- forcats::fct_relevel(
    state.mp$zone,
    "Conservation (no-take)",
    "Sanctuary (no-take)",
    "Recreation",
    "General Use",
    "Special Purpose"
  )

  state.pal <- leaflet::colorFactor(c(
    "#bfaf02", # conservation
    "#7bbc63", # sanctuary = National Park
    "#fdb930", # recreation
    "#b9e6fb", # general use
    "#ccc1d6" # special purpose
  ), state.mp$zone)


  # spatial.data <- state.mp@data

  # -----------------------------------------------------------------------------#
  # Write data to .rds
  # -----------------------------------------------------------------------------#

  # Abundance without zeros for leaflet
  abundance.leaflet <- abundance %>%
    dplyr::group_by(marine_park, method, latitude, longitude, year, status, site, sample, scientific, complete) %>%
    dplyr::summarise(maxn = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(maxn > 0)

  abundance.leaflet <- data.table::data.table(abundance.leaflet)

  metadata.leaflet <- metadata %>%
    dplyr::select(marine_park, method, latitude, longitude, year, status, site, sample, complete)

  metadata.leaflet <- data.table::data.table(metadata.leaflet)

  # Reformat data into summaries before sending to shinyapp
  abundance.sum <- abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(maxn), se = mpaviewer::se(maxn)) %>%
    dplyr::ungroup()

  abundance.sum <- data.table::data.table(abundance.sum)
  # reduced by 6 million rows

  abundance.sum.sanctuary <- abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific, gazetted, re_zoned, dbca_sanctuary, complete) %>% #complete,  #removed complete after meeting with Jordan 6th November
    dplyr::summarise(mean = mean(maxn), se = mpaviewer::se(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(dbca_sanctuary))

  abundance.sum.sanctuary <- data.table::data.table(abundance.sum.sanctuary)

  top.ten <- abundance %>%
    dplyr::filter(!genus == family) %>%
    dplyr::group_by(marine_park, method, scientific) %>%
    dplyr::summarise(maxn = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(marine_park, method) %>%
    dplyr::arrange(desc(maxn)) %>%
    dplyr::top_n(10)

  top.ten <- data.table::data.table(top.ten)

  trophic.sum <- trophic.abundance %>%
    dplyr::group_by(marine_park, method, year, status, trophic_group, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(total.abundance), se = mpaviewer::se(total.abundance)) %>%
    dplyr::ungroup()

  trophic.sum <- data.table::data.table(trophic.sum)

  # reduced by 50 000 rows

  ta.sr <- all.data %>%
    dplyr::group_by(marine_park, method, year, status, metric, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()

  ta.sr <- data.table::data.table(ta.sr)

  ta.sr.sanctuary <- all.data %>%
    dplyr::group_by(marine_park, method, year, status, metric, dbca_sanctuary, gazetted, re_zoned) %>% # removed complete after meeting with Jordan 6th Nov 2023
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(dbca_sanctuary))

  ta.sr.sanctuary <- data.table::data.table(ta.sr.sanctuary)

  ta.sr.zone <- all.data %>%
    dplyr::group_by(marine_park, method, year, status, metric, dbca_zone, gazetted, re_zoned) %>% # removed complete after meeting with Jordan 6th Nov 2023
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()

  ta.sr.zone <- data.table::data.table(ta.sr.zone)

  ta.sr.site <- all.data %>%
    dplyr::group_by(marine_park, method, year, status, metric, site, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(value), se = mpaviewer::se(value)) %>%
    dplyr::ungroup()

  ta.sr.site <- data.table::data.table(ta.sr.site)

  # Lists for dropdowns
  ordered.top.fished_species <- fished.abundance %>%
    dplyr::group_by(marine_park, method, scientific) %>%
    dplyr::summarise(total = sum(total.abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(total))

  ordered.top.fished_species <- data.table::data.table(ordered.top.fished_species)

  ordered.top.species <- abundance %>%
    dplyr::group_by(marine_park, method, scientific) %>%
    dplyr::summarise(total = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(total))

  ordered.top.species <- data.table::data.table(ordered.top.species)



  fished_species.sum <- fished.abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(total.abundance), se = mpaviewer::se(total.abundance)) %>%
    dplyr::ungroup()

  fished_species.sum <- data.table::data.table(fished_species.sum)

  fished_species.sum.sanctuary <- fished.abundance %>%
    dplyr::group_by(marine_park, method, year, status, scientific, gazetted, re_zoned, complete, dbca_sanctuary) %>%
    dplyr::summarise(mean = mean(total.abundance), se = mpaviewer::se(total.abundance)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(dbca_sanctuary))

  fished_species.sum.sanctuary <- data.table::data.table(fished_species.sum.sanctuary)


  fished.sum <- fished.summed %>%
    dplyr::group_by(marine_park, method, year, status, gazetted, re_zoned, complete) %>%
    dplyr::summarise(mean = mean(total.abundance), se = mpaviewer::se(total.abundance)) %>%
    dplyr::ungroup()

  fished.sum <- data.table::data.table(fished.sum)

  fished.sum.sanctuary <- fished.summed %>%
    dplyr::group_by(marine_park, method, year, status, gazetted, re_zoned, complete, dbca_sanctuary) %>%
    dplyr::summarise(mean = mean(total.abundance), se = mpaviewer::se(total.abundance)) %>%
    dplyr::ungroup()%>%
    dplyr::filter(!is.na(dbca_sanctuary))

  fished.sum.sanctuary <- data.table::data.table(fished.sum.sanctuary)

# TODO split these into ones that have been reformatted and ones that don't need it
  # Using data table to set keys for faster filtering ----
  metadata <- data.table::data.table(metadata)
  sampling_effort <- data.table::data.table(sampling_effort)
  lats <- data.table::data.table(lats)
  park_popups <- data.table::data.table(park_popups)
  foa_codes <- data.table::data.table(foa_codes)
  interpretation_trends <- data.table::data.table(interpretation_trends)
  fished.complete.length <- data.table::data.table(fished.complete.length) # cant summrise because I need lengths

  abundance <- data.table::data.table(abundance) # Reformatted but still need this on for he others
  trophic.abundance <- data.table::data.table(trophic.abundance) # Reformatted
  # TODO summary for individual species
  all.data <- data.table::data.table(all.data) # Reformatted

  fished.abundance <- data.table::data.table(fished.abundance) # Reformatted
  fished.summed <- data.table::data.table(fished.summed) # Reformatted

 # TODO Come back to these
  coral_cover_transect <- data.table::data.table(coral_cover_transect)
  coral_cover_metadata <- data.table::data.table(coral_cover_metadata)
  rec_3b <- data.table::data.table(rec_3b)
  rec_3c2 <- data.table::data.table(rec_3c2)

  # common_names <- data.table::data.table(common_names) # Not needed


  methods <- metadata %>%
    dplyr::distinct(marine_park, method) %>%
    dplyr::mutate(marine_park = as.character(marine_park))


  total.number.fish <- sum(abundance$maxn)

  total.species.fish <- length(unique(abundance$scientific))

  total.number.fish.park <- abundance %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(total = sum(maxn))

  total.species.fish.park <- abundance %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(richness = dplyr::n_distinct(scientific))

  mins.watched <- metadata %>%
    dplyr::group_by(marine_park, method) %>%
    dplyr::summarise(total = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mins.per.sample = dplyr::case_when(
                  method %in% "stereo-BRUVs" ~ 60,
                  method %in% "stereo-DOVs" ~ 20,
                  method %in% "stereo-ROVs" ~ 20)) %>%
    dplyr::mutate(mins.watched = total * mins.per.sample) %>%
    dplyr::group_by(marine_park) %>%
    dplyr::summarise(mins_watched = sum(mins.watched))

  biggest.fish <- complete.length %>%
    dplyr::filter(length > 0 ) %>%
    dplyr::group_by(marine_park) %>%
    dplyr::slice(which.max(length)) %>%
    dplyr::select(marine_park, method, campaignid, family, genus, species, length)

  data.table::setkey(methods)
  data.table::setkey(abundance.sum)
  data.table::setkey(abundance.sum.sanctuary)
  data.table::setkey(trophic.sum)
  data.table::setkey(ta.sr)
  data.table::setkey(ta.sr.sanctuary)
  data.table::setkey(ta.sr.zone)
  data.table::setkey(ta.sr.site)
  data.table::setkey(fished_species.sum)
  data.table::setkey(fished.sum.sanctuary)
  data.table::setkey(fished.sum)
  data.table::setkey(top.ten)
  data.table::setkey(ordered.top.fished_species)
  data.table::setkey(ordered.top.species)
  data.table::setkey(abundance.leaflet)
  data.table::setkey(metadata.leaflet)


  data.table::setkey(lats)
  data.table::setkey(abundance)
  data.table::setkey(trophic.abundance)
  data.table::setkey(all.data)
  data.table::setkey(fished.complete.length)
  data.table::setkey(fished.abundance)
  # data.table::setkey(fished.summed)
  data.table::setkey(metadata)
  data.table::setkey(sampling_effort)
  data.table::setkey(park_popups)
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
      ta.sr = ta.sr,  # summarised
      ta.sr.sanctuary = ta.sr.sanctuary,  # summarised
      ta.sr.zone = ta.sr.zone,  # summarised
      ta.sr.site = ta.sr.site,  # summarised
      top.ten = top.ten,  # summarised
      ordered.top.fished_species = ordered.top.fished_species,
      ordered.top.species = ordered.top.species,
      abundance.sum = abundance.sum, # summarised
      abundance.sum.sanctuary = abundance.sum.sanctuary, # summarised
      abundance.leaflet = abundance.leaflet,
      metadata.leaflet = metadata.leaflet,
      lats = lats,
      methods = methods,
      # abundance = abundance,  # Turned off to speed up app
      # trophic.abundance = trophic.abundance,
      trophic.sum = trophic.sum,  # summarised
      all.data = all.data,
      fished.complete.length = fished.complete.length, # Turned off to speed up app
      fished.abundance = fished.abundance,
      fished.sum = fished.sum, # summarised
      fished.sum.sanctuary = fished.sum.sanctuary, # summarised
      fished_species.sum = fished_species.sum, # summarised
      fished_species.sum.sanctuary = fished_species.sum.sanctuary, # summarised
      metadata = metadata,
      sampling_effort = sampling_effort,
      state.mp = state.mp,
      state.pal = state.pal,
      park_popups = park_popups,
      coral_cover_transect = coral_cover_transect,
      coral_cover_metadata = coral_cover_metadata,
      rec_3b = rec_3b,
      rec_3c2 = rec_3c2,
      # common_names = common_names, # Not needed
      foa_codes = foa_codes,
      interpretation_trends = interpretation_trends,
      total.number.fish = total.number.fish,
      total.species.fish = total.species.fish,
      total.number.fish.park = total.number.fish.park,
      total.species.fish.park = total.species.fish.park,
      mins.watched = mins.watched
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
      # "  total.abundance:   {nrow(mpa_data$total.abundance)}\n",
      "  trophic.abundance:   {nrow(mpa_data$trophic.abundance)}\n",
      "  all.data:    {nrow(mpa_data$all.data)}\n",
      "  fished.complete.length:    {nrow(mpa_data$fished.complete.length)}\n",
      "  fished.abundance:    {nrow(mpa_data$fished.abundance)}\n",
      "  metadata:    {nrow(mpa_data$metadata)}\n",
      "  sampling_effort:    {nrow(mpa_data$sampling_effort)}\n",
      "  state.mp:    {nrow(mpa_data$state.mp)}\n",
      "  state.pal:    {nrow(mpa_data$state.pal)}\n",
      "  park_popups:    {nrow(mpa_data$park_popups)}\n"
    )
  )
  invisible(mpa_data)
}

