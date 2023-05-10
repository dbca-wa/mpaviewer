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
  data.dir <- here::here("inst/data")

  #TODO fix empty shark bay year data



  # Benthic data
  # Currently not very pretty. Just saving csv files of the dataframes that Claire uses to create her plots.

  # coral.cover_site.means <- list.files(path = data.dir, recursive = T, pattern = "_site_means.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
  #   purrr::map_df(~ read_dbca_files_csv(.)) %>%
  #   dplyr::mutate(year = as.numeric(year),
  #          mean = as.numeric(mean),
  #          sd = as.numeric(sd),
  #          se = as.numeric(se))
  #
  # coral.cover_mean_sector <- list.files(path = data.dir, recursive = T, pattern = "_mean_sector.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
  #   purrr::map_df(~ read_dbca_files_csv(.)) %>%
  #   dplyr::mutate(year = as.numeric(year),
  #                 mean = as.numeric(mean),
  #                 sd = as.numeric(sd),
  #                 se = as.numeric(se))
  #
  # coral.cover_mean_site <- list.files(path = data.dir, recursive = T, pattern = "_mean_site.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
  #   purrr::map_df(~ read_dbca_files_csv(.)) %>%
  #   dplyr::mutate(year = as.numeric(year),
  #                 mean = as.numeric(mean),
  #                 sd = as.numeric(sd),
  #                 se = as.numeric(se))

  # TODO upload _coralcover.csv to GoogleDrive

  # New coral data in dashboard format
  coral_cover <- list.files(path = data.dir, recursive = T, pattern = "_coralcover.csv", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_csv(.)) %>%
    dplyr::filter(!marine.park %in% c("archive")) %>% # get rid of old files
    dplyr::filter(level2class %in% c("Hard coral", "Octocorals - Hard")) %>%
    dplyr::mutate(year = as.numeric(year),
                  percent_cover = as.numeric(percent_cover),
                  plot_year = as.numeric(year),
                  latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude))


  coral_cover_metadata <- coral_cover %>%
    dplyr::select(zone, sector, site, site_code, latitude, longitude, replicate, survey, year, date, plot_year, analysis, software, marine.park, method) %>%
    dplyr::distinct()

  coral_cover_transect <- plyr::ddply(coral_cover, plyr::.(marine.park, method, survey, plot_year, sector, site), plyr::summarize, percent_cover = sum(percent_cover))

  # test <- anti_join(coral_cover_metadata, coral_cover_transect)
  #
  # test <- coral_cover_metadata %>%
  #   group_by(sector, site, survey, plot_year) %>%
  #   summarise(n=n())

  # test <- plyr::ddply(coral_cover_transect, plyr::.(marine.park, method, plot_year), .inform=TRUE, plyr::summarise,
  #             n    = length(unique(site)),
  #             mean = mean(percent_cover),
  #             sd   = sd(percent_cover),
  #             se   = sd(percent_cover) / sqrt(length(unique(site))))



  rec_3b <- list.files(path = data.dir, recursive = T, pattern = "REC3b.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>%
    dplyr::filter(!marine.park %in% c("archive")) %>% # get rid of old files
    dplyr::mutate(year = as.numeric(year),
                  mean = as.numeric(mean))

  rec_3c2 <- list.files(path = data.dir, recursive = T, pattern = "REC3c2.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>%
    dplyr::filter(!marine.park %in% c("archive")) %>% # get rid of old files
    dplyr::mutate(year = as.numeric(year),
                  mean = as.numeric(mean),
                  sd = as.numeric(sd),
                  se = as.numeric(se))

  ### ► Life history sheet ----
  ## Will need to replace with DBCA's own version eventually but this will work for time being
  common.names <- here::here("inst/data/australia.life.history.csv") |>
    read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
    GlobalArchive::ga.clean.names() %>%
    dplyr::select(scientific, family, genus, species, australian.common.name)

  # life.history <- here::here("inst/data/DBCA.fish.feeding.guilds.target.status.csv") %>%
  #   read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
  #   GlobalArchive::ga.clean.names()


  dbca.googlesheet.url <- "https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?usp=sharing"

  Sys.time()
  print("this takes a long time")

  life.history <- googlesheets4::read_sheet(dbca.googlesheet.url, sheet = "life_history") %>%
    GlobalArchive::ga.clean.names()
  Sys.time()

  complete.sites <- googlesheets4::read_sheet(dbca.googlesheet.url, sheet = "temporal_years_sites") %>%
    dplyr::mutate(year = strsplit(as.character(include_years), split = ", "))%>%
    tidyr::unnest(year) %>%
    dplyr::mutate(site = strsplit(as.character(include_sites), split = ", "))%>%
    tidyr::unnest(site) %>%
    dplyr::distinct() %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::select(marine.park, method, year, site) %>%
    dplyr::mutate(complete = "Yes")

  complete.needed.campaigns <- complete.sites %>%
    dplyr::distinct(marine.park, method) %>%
    dplyr::mutate(complete.needed = "Yes")

  # unique(life.history$fishing.type)
  # unique(life.history$rls.trophic.group)
  unique(life.history$target.code)

  fished.species <- life.history %>%
    # dplyr::filter(fishing.type %in% c("B/R", "B/C/R", "R", "C/R", "B/C", "C")) %>% # could minimise these
    dplyr::filter(target.code %in% c("HT", "T")) %>%
    dplyr::select(genus, species) %>%
    dplyr::distinct() #TODO keep the marine park column in this

  unique(life.history$trophic.guild)

  trophic.groups <- life.history %>%
    # dplyr::mutate(trophic.group = GlobalArchive::ga.capitalise(rls.trophic.group)) %>%
    # dplyr::select(scientific, family, genus, species, trophic.group) %>%
    # dplyr::mutate(trophic.group = stringr::str_replace_all(.$trophic.group, c("NANA" = "Unknown"))) %>%
    dplyr::mutate(trophic.group = stringr::str_replace_all(.$trophic.guild, c("NANA" = "Unknown",
                                                                              "NA" = "Unknown",
                                                                              "planktivore" = "Planktivore",
                                                                              "Algal Feeder" = "Algal feeder"))) %>%
    tidyr::replace_na(list(trophic.group = "Unknown"))


  unique(trophic.groups$trophic.group) %>%sort()

  park.popups <- here::here("inst/data/parks.popups.csv") |> # BG TO DO -  CHANGE THIS
    read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
    GlobalArchive::ga.clean.names()

  zoning <- googlesheets4::read_sheet(dbca.googlesheet.url, sheet = "park_gazettal") %>%
    GlobalArchive::ga.clean.names()

  foa.codes <- googlesheets4::read_sheet(dbca.googlesheet.url, sheet = "fishes_of_australia") %>%
    GlobalArchive::ga.clean.names() %>%
    dplyr::select(-c(number)) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::left_join(common.names) %>%
    dplyr::mutate(scientific = paste0(scientific, " (", australian.common.name, ")"))

  interpretation.trends <- googlesheets4::read_sheet(dbca.googlesheet.url, sheet = "interpretation_trends") %>%
    GlobalArchive::ga.clean.names()

  ## _______________________________________________________ ----
  ##                        READ IN DATA                     ----
  ## _______________________________________________________ ----

  ### ► Metadata (same for every method and data type) ----

  metadata <- list.files(path = data.dir, recursive = T, pattern = "_Metadata.csv", full.names = T) %>% # list all files ending in "_Metadata.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Metadata.csv" = ""))) %>%
    dplyr::mutate(year = substr(date, 1, 4)) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::mutate(month = substr(date, 5, 6)) %>%
    dplyr::mutate(day = substr(date, 7, 8)) %>%
    dplyr::mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
    dplyr::left_join(zoning) %>%
    dplyr::mutate(status = stringr::str_replace_all(.$status, c("Sanctuary" = "No-take",
                                                                "No-Take" = "No-take",
                                                                "Protected" = "No-take"))) %>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs")) %>%
    dplyr::left_join(.,complete.sites) %>%
    dplyr::left_join(.,complete.needed.campaigns) %>%
    dplyr::mutate(complete = dplyr::if_else(is.na(complete.needed), "Yes", complete)) %>%
    dplyr::mutate(sample = dplyr::if_else(is.na(sample) & method%in% "stereo-DOVs", paste(opcode, period, sep = "_"), sample)) %>%
    dplyr::mutate(sample = dplyr::if_else(is.na(sample), opcode, sample)) %>%
    dplyr::select(marine.park, method, campaignid, sample, latitude, longitude, date, time, location, status, site, successful.count, successful.length, depth, observer, year, month, day, gazetted, re.zoned, complete, dbca_zone, dbca_sanctuary) # Trying to remove columns to save space/time to load the app


  names(metadata) %>% sort()

  test.complete <- metadata %>%
    dplyr::filter(complete %in% "Yes")

  unique(metadata$marine.park) %>% sort()
  unique(metadata$method) %>% sort()
  unique(metadata$campaignid) %>% sort()

  unique(metadata$dbca_zone)
  unique(metadata$dbca_sanctuary)

  names(metadata)

  campaign.list <- metadata %>% dplyr::distinct(marine.park, method, campaignid, sample) # Want to create a list of every sample
  # DOes it have maxn and length associated with it??

  #Check sites that were not sampled consistently across all years

  # Me trying to work out complete sites

  years <- metadata %>%
    dplyr::group_by(marine.park, method) %>%
    dplyr::summarise(no.years.sampled = length(unique(year)))

  times.sites.sampled <- metadata %>%
    dplyr::group_by(marine.park, method, site) %>%
    dplyr::summarise(no.times.site.sampled = length(unique(year))) %>%
    dplyr::left_join(years) %>%
    dplyr::mutate(complete.site = dplyr::if_else(no.times.site.sampled == no.years.sampled, "Yes", "No"))

  # test <- metadata %>% dplyr::filter(marine.park %in% "Ngari Capes Marine Park", method %in% "stereo-BRUVs") %>%
  #   dplyr::distinct(marine.park, site, year)
  #
  # test2 <- test %>% dplyr::group_by(marine.park, site) %>% dplyr::summarise(n = length(unique(year)))
  #
  # unique(test$year)
  #
  # table = table(test$site, test$year)
  # table
  # row_sub = apply(table, 1, function(row) all(row !=0 ))
  # finalsites = table[row_sub,]
  # finalsites
  # #Have also removed L2 due to abnormally large schools of nebs in 2014
  # #Note will show all sites in separate plot
  # sites.limited = long.ab %>% filter(Site %in% rownames(finalsites))%>%
  #   filter(!Site %in% "L2")

  names(metadata)

  lats <- metadata %>%
    dplyr::group_by(marine.park) %>%
    dplyr::summarise(mean.lat = mean(latitude)) %>% # biggest is the most north
    dplyr::arrange(desc(mean.lat)) # Could make this again on the server side

  metadata$marine.park <- forcats::fct_relevel(metadata$marine.park, c(unique(lats$marine.park)))

  # Testing
  bruv.test <- metadata %>%
    dplyr::filter(method %in% "stereo-BRUVs") %>%
    dplyr::filter(marine.park %in% "Ningaloo Marine Park")

  ### ► Summarise to find sampling effort, this is used for the leaflet maps ----
  sampling.effort <- metadata %>%
    dplyr::group_by(marine.park, method, sample, status, site, location, latitude, longitude, depth) %>%
    dplyr::summarise(number.of.times.sampled = dplyr::n()) %>%
    dplyr::ungroup() # Could also make this on the server side

  ### ► Generic data (still using "sample") ----
  ### ► Count ----
  count.csv <- list.files(path = data.dir, recursive = T, pattern = "_Count.csv", full.names = T) %>% # list all files ending in "_Count.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Count.csv" = ""))) %>%
    dplyr::mutate(count = as.numeric(count)) %>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs"))

  count.txt <- list.files(path = data.dir, recursive = T, pattern = "_Count.txt", full.names = T)  %>%
    purrr::map_df(~read_dbca_files_txt(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Count.txt" = ""))) %>%
    dplyr::mutate(count = as.numeric(count))%>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs"))

  count <- dplyr::bind_rows(count.csv, count.txt)

  unique(count$campaignid)

  names(count)

  ### ► Length ----
  length.csv <- list.files(path = data.dir, recursive = T, pattern = "_Length.csv", full.names = T) %>% # list all files ending in "_Length.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Length.csv" = ""))) %>%
    dplyr::mutate(count = as.numeric(count)) %>%
    dplyr::mutate(length = as.numeric(length))%>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs"))

  length.txt <- list.files(path = data.dir, recursive = T, pattern = "_Length.txt", full.names = T) %>% # list all files ending in "_Length.csv"
    purrr::map_df(~read_dbca_files_txt(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Length.txt" = ""))) %>%
    dplyr::mutate(count = as.numeric(count)) %>%
    dplyr::mutate(length = as.numeric(length))%>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs"))

  length <- dplyr::bind_rows(length.csv, length.txt) %>%
    dplyr::filter(!is.na(length))

  names(length)

  ### ► EventMeasure data ----
  em.campaigns <- list.files(path = data.dir, recursive = T, pattern = "_Lengths.txt", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Lengths.txt" = ""))) %>%
    dplyr::distinct(campaignid) %>%
    dplyr::pull("campaignid")

  points <- list.files(path = data.dir, recursive = T, pattern = "_Points.txt", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Points.txt" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs")) %>%
    dplyr::mutate(sample = dplyr::if_else(method %in% "stereo-DOVs", paste(opcode, period, sep = "_"), opcode))

  unique(points$campaignid)

  threed.points <- list.files(path = data.dir, recursive = T, pattern = "_3DPoints.txt", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_3DPoints.txt" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs")) %>%
    dplyr::mutate(sample = dplyr::if_else(method %in% "stereo-DOVs", paste(opcode, period, sep = "_"), opcode))

  lengths <- list.files(path = data.dir, recursive = T, pattern = "_Lengths.txt", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Lengths.txt" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::mutate(length = as.numeric(length)) %>%
    dplyr::filter(!marine.park %in% c("archive", "C:")) %>% # get rid of old files
    dplyr::mutate(method = forcats::fct_recode(method,
                                               "stereo-BRUVs" = "BRUVs",
                                               "stereo-BRUVs" = "BRUVS",
                                               "stereo-DOVs" = "DOVs")) %>%
    dplyr::mutate(sample = dplyr::if_else(method %in% "stereo-DOVs", paste(opcode, period, sep = "_"), opcode))

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
  test <- dplyr::anti_join(metadata, count) # 1408 samples without fish? does that make sense


  ## _______________________________________________________ ----
  ##                     COMPLETE LENGTH DATA                ----
  ## _______________________________________________________ ----

  # Have created length first to calc DOV abundance.

  # Replicate rows where n is >1 for length dataframes
  length <- length[rep(row.names(length), length$count), ]
  lengths <- lengths[rep(row.names(lengths), lengths$number), ]

  # names(threed.points)

  generic <- length %>%
    dplyr::mutate(number = 1) %>%
    dplyr::select(!c(count))

  complete.length <- generic %>%
    dplyr::bind_rows(lengths) %>%
    dplyr::mutate(number = 1) %>%
    dplyr::bind_rows(threed.points) %>%
    dplyr::full_join(metadata) %>%
    # tidyr::complete(tidyr::nesting(marine.park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(number = 0)) %>%
    dplyr::select(marine.park, campaignid, method, sample, family, genus, species, number, length) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::left_join(common.names) %>%
    dplyr::mutate(scientific = paste0(scientific, " (", australian.common.name, ")"))

  complete.length$marine.park <- forcats::fct_relevel(complete.length$marine.park, c(unique(lats$marine.park)))

  unique(complete.length$marine.park)
  names(complete.length)
  ## _______________________________________________________ ----
  ##                  COMPLETE ABUNDANCE DATA                ----
  ## _______________________________________________________ ----

  # stereo-DOV abundance from 3D points and lengths

  dov.abundance <- dplyr::bind_rows(lengths, threed.points) %>%
    dplyr::group_by(marine.park, campaignid, method, sample, family, genus, species) %>%
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
    dplyr::group_by(marine.park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count)) %>%
    dplyr::ungroup() %>%
    # tidyr::complete(tidyr::nesting(marine.park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::filter(!method %in% c("stereo-BRUVs"))%>%
    dplyr::filter(!campaignid %in% c(em.campaigns))

  unique(count.summary$campaignid) %>%sort()


  ### MaxN (For BRUVs) ----
  count.maxn <- count %>%
    dplyr::filter(method %in% c("stereo-BRUVs")) %>%
    dplyr::group_by(marine.park, method, campaignid, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count)) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " "))

  maxn <- points %>%
    dplyr::group_by(marine.park, method, campaignid, sample, filename, period, periodtime, frame, family, genus, species) %>%
    dplyr::summarise(maxn = sum(number)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(marine.park, method, campaignid, sample, family, genus, species) %>%
    dplyr::slice(which.max(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(count.maxn) %>%
    dplyr::mutate(maxn = as.numeric(maxn)) %>%
    dplyr::full_join(metadata) %>%
    dplyr::select(marine.park, method, campaignid, sample, family, genus, species, maxn) %>%
    # tidyr::complete(tidyr::nesting(marine.park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::filter(method %in% c("stereo-BRUVs"))

  unique(maxn$campaignid)

  abundance <- dplyr::bind_rows(maxn, count.summary, dov.abundance) %>%
    dplyr::left_join(common.names) %>%
    dplyr::left_join(foa.codes) %>%
    dplyr::mutate(scientific = paste0(scientific, " (", australian.common.name, ")"))

  unique(abundance$marine.park)


  abundance$marine.park <- forcats::fct_relevel(abundance$marine.park, c(unique(lats$marine.park)))


  complete.length.summary <- complete.length %>%
    dplyr::group_by(marine.park, method, campaignid, sample) %>%
    dplyr::summarise(lengths = sum(number))

  missing.metadata <- dplyr::anti_join(complete.length.summary, metadata) # One with a random dot on the end

  missing.fish <- dplyr::anti_join(metadata, complete.length.summary) # None




  abundance.summary <- abundance %>%
    dplyr::group_by(marine.park, method, campaignid, sample) %>%
    dplyr::summarise(abundance = sum(maxn))

  missing.metadata <- dplyr::anti_join(abundance.summary, metadata) # One with a random dot on the end

  missing.fish <- dplyr::anti_join(metadata, abundance.summary) # None




  abundance.test <- abundance %>%
    dplyr::filter(marine.park %in% "Ningaloo Marine Park") %>%
    dplyr::filter(method %in% "stereo-BRUVs")

  ## _______________________________________________________ ----
  ##                      ABUNDANCE METRICS                  ----
  ## _______________________________________________________ ----

  fished.abundance <- dplyr::semi_join(abundance, fished.species) %>%
    dplyr::group_by(marine.park, campaignid, method, sample, scientific, family, genus, species, url) %>%
    dplyr::summarise(total.abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::select(marine.park, campaignid, sample, total.abundance, method, scientific, url) %>%
    dplyr::left_join(metadata)

  trophic.abundance <- dplyr::left_join(abundance, trophic.groups) %>%
    tidyr::replace_na(list(trophic.group = "Unknown")) %>%
    dplyr::group_by(marine.park, campaignid, method, sample, trophic.group) %>%
    dplyr::summarise(total.abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::select(marine.park, campaignid, sample, trophic.group, total.abundance) %>%
    dplyr::left_join(metadata)

  total.abundance <- abundance %>%
    dplyr::group_by(marine.park, campaignid, method, sample) %>%
    dplyr::summarise(total.abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::select(marine.park, campaignid, sample, total.abundance, method)

  species.richness <- abundance %>%
    # dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::select(marine.park, method, campaignid, sample, scientific, maxn) %>%
    dplyr::group_by(marine.park, method, campaignid, sample) %>%
    dplyr::summarise(species.richness = length(unique(scientific))) %>%
    dplyr::select(marine.park, campaignid, sample, species.richness, method) %>%
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

  fished.complete.length <- dplyr::semi_join(complete.length, fished.species)

  state.mp <- rgdal::readOGR(here::here("inst/data/spatial/WA_MPA_2018.shp"))


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
  # Version 1: save entire workspace, works if all of the above ran and worked
  # save.image(here::here("inst/data/workspace.RData"))

  # Use in userver.R as:
  # load(here::here("inst/data/workspace.RData"))
  #
  # Version 2: save to .rds
  # saveRDS(abundance, here::here("inst/data/abundance.rds"), compress="xz")
  # abundance <- readRDS(here::here("inst/data/abundance.rds"))
  # saveRDS(all.data, here::here("inst/data/all.data.rds"), compress="xz")
  # all.data readRDS(here::here("inst/data/all.data.rds"))
  #
  # TODO replicate for other objects required by server.R

  # # Using data table to set keys for faster filtering ----
  # lats <- data.table::data.table(lats)
  # abundance <- data.table::data.table(abundance)
  # trophic.abundance <- data.table::data.table(trophic.abundance)
  # all.data <- data.table::data.table(all.data)
  # fished.complete.length <- data.table::data.table(fished.complete.length)
  # fished.abundance <- data.table::data.table(fished.abundance)
  # metadata <- data.table::data.table(metadata)
  # sampling.effort <- data.table::data.table(sampling.effort)
  # # state.mp <- data.table::data.table(state.mp)
  # # state.pal <- data.table::data.table(state.pal)
  # park.popups <- data.table::data.table(park.popups)
  # coral_cover_transect <- data.table::data.table(coral_cover_transect)
  # coral_cover_metadata <- data.table::data.table(coral_cover_metadata)
  # rec_3b <- data.table::data.table(rec_3b)
  # rec_3c2 <- data.table::data.table(rec_3c2)
  # common.names <- data.table::data.table(common.names)
  # foa.codes <- data.table::data.table(foa.codes)
  # interpretation.trends <- data.table::data.table(interpretation.trends)

  # data.table::setkey(lats)
  # data.table::setkey(abundance)
  # data.table::setkey(trophic.abundance)
  # data.table::setkey(all.data)
  # data.table::setkey(fished.complete.length)
  # data.table::setkey(fished.abundance)
  # data.table::setkey(metadata)
  # data.table::setkey(sampling.effort)
  # # data.table::setkey(state.mp)
  # # data.table::setkey(state.pal)
  # data.table::setkey(park.popups)
  # data.table::setkey(coral_cover_transect)
  # data.table::setkey(coral_cover_metadata)
  # data.table::setkey(rec_3b)
  # data.table::setkey(rec_3c2)
  # data.table::setkey(common.names)
  # data.table::setkey(foa.codes)
  # data.table::setkey(interpretation.trends)

  # Version 3: one object
  mpa_data <- structure(
    list(
      downloaded_on = Sys.time(),
      lats = lats,
      abundance = abundance,
      # total.abundance = total.abundance, # I don't think this is being used
      trophic.abundance = trophic.abundance,
      all.data = all.data,
      fished.complete.length = fished.complete.length,
      fished.abundance = fished.abundance,
      metadata = metadata,
      sampling.effort = sampling.effort,
      state.mp = state.mp,
      state.pal = state.pal,
      park.popups = park.popups,
      coral_cover_transect = coral_cover_transect,
      coral_cover_metadata = coral_cover_metadata,
      rec_3b = rec_3b,
      rec_3c2 = rec_3c2,
      common.names = common.names,
      foa.codes = foa.codes,
      interpretation.trends = interpretation.trends
      ),
    class = "mpa_data"
  )

  if (save == TRUE) {
    saveRDS(mpa_data, dest, compress = FALSE) #"xz"
    # save(mpa_data, file = here::here("inst/data/mpa_data.Rdata"))
    # saveRDS(x, "inst/data/mpa_data.rds", compress = FALSE) #"xz"
  }

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
      "  sampling.effort:    {nrow(mpa_data$sampling.effort)}\n",
      "  state.mp:    {nrow(mpa_data$state.mp)}\n",
      "  state.pal:    {nrow(mpa_data$state.pal)}\n",
      "  park.popups:    {nrow(mpa_data$park.popups)}\n"
    )
  )
  invisible(mpa_data)
}

