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

  ### ► Life history sheet ----
  ## Will need to replace with DBCA's own version eventually but this will work for time being
  life.history <- here::here("inst/data/australia.life.history.csv") |>
    read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
    GlobalArchive::ga.clean.names()

  # unique(life.history$fishing.type)
  # unique(life.history$rls.trophic.group)

  fished.species <- life.history %>%
    dplyr::filter(fishing.type %in% c("B/R", "B/C/R", "R", "C/R", "B/C", "C")) %>% # could minimise these
    dplyr::select(genus, species) # turn on for testing Rottnest data

  trophic.groups <- life.history %>%
    dplyr::mutate(trophic.group = GlobalArchive::ga.capitalise(rls.trophic.group)) %>%
    dplyr::select(scientific, family, genus, species, trophic.group) %>%
    dplyr::mutate(trophic.group = stringr::str_replace_all(.$trophic.group, c("NANA" = "Unknown")))

  # unique(trophic.groups$trophic.group)

  park.popups <- here::here("inst/data/parks.popups.csv") |>
    read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
    GlobalArchive::ga.clean.names()

  zoning <- here::here("inst/data/zoning.csv") |>
    read.csv(na.strings = c("NA", "NaN", " ", "", NA)) |>
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
    dplyr::left_join(zoning)
  # dplyr::mutate(year = as.Date(year, "%Y")) %>%
  # dplyr::mutate(year = format(year, "%Y"))

  lats <- metadata %>%
    dplyr::group_by(marine.park) %>%
    dplyr::summarise(mean.lat = mean(latitude)) %>% # biggest is the most north
    dplyr::arrange(desc(mean.lat))

  metadata$marine.park <- forcats::fct_relevel(metadata$marine.park, c(unique(lats$marine.park)))

  ### ► Summarise to find sampling effort, this is used for the leaflet maps ----
  sampling.effort <- metadata %>%
    dplyr::group_by(marine.park, method, sample, status, site, location, latitude, longitude, depth) %>%
    dplyr::summarise(number.of.times.sampled = dplyr::n()) %>%
    dplyr::ungroup()

  ### ► Generic data (still using "sample") ----
  ### ► Count ----
  count.csv <- list.files(path = data.dir, recursive = T, pattern = "_Count.csv", full.names = T) %>% # list all files ending in "_Count.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Count.csv" = ""))) %>%
    dplyr::mutate(count = as.numeric(count))

  count.txt <- list.files(path = data.dir, recursive = T, pattern = "_Count.txt", full.names = T)  %>%
    purrr::map_df(~read.dbca.files_txt(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Count.txt" = ""))) %>%
    dplyr::mutate(count = as.numeric(count))

  count <- dplyr::bind_rows(count.csv, count.txt)

  ### ► Length ----
  length <- list.files(path = data.dir, recursive = T, pattern = "_Length.csv", full.names = T) %>% # list all files ending in "_Length.csv"
    purrr::map_df(~ read_dbca_files_csv(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Length.csv" = ""))) %>%
    dplyr::mutate(count = as.numeric(count)) %>%
    dplyr::mutate(length = as.numeric(length))

  length.txt <- list.files(path = data.dir, recursive = T, pattern = "_Length.txt", full.names = T) %>% # list all files ending in "_Length.csv"
    purrr::map_df(~read.dbca.files_txt(.)) %>% # combine into dataframe
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Length.txt" = ""))) %>%
    dplyr::mutate(count = as.numeric(count)) %>%
    dplyr::mutate(length = as.numeric(length))

  length <- dplyr::bind_rows(length.csv, length.txt) %>%
    dplyr::filter(!is.na(length))

  ### ► EventMeasure data ----
  points <- list.files(path = data.dir, recursive = T, pattern = "_Points.txt", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Points.txt" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::rename(sample = opcode)

  threed.points <- list.files(path = data.dir, recursive = T, pattern = "_3DPoints.txt", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_3DPoints.txt" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::rename(sample = opcode)

  lengths <- list.files(path = data.dir, recursive = T, pattern = "_Lengths.txt", full.names = T) %>%
    purrr::map_df(~ read_dbca_files_txt(.)) %>%
    dplyr::mutate(campaignid = stringr::str_replace_all(.$campaignid, c("_Lengths.txt" = ""))) %>%
    dplyr::mutate(number = as.numeric(number)) %>%
    dplyr::mutate(length = as.numeric(length)) %>%
    dplyr::rename(sample = opcode)

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
  test <- dplyr::anti_join(metadata, count) # 138 samples without fish? does that make sense

  ## _______________________________________________________ ----
  ##                  COMPLETE ABUNDANCE DATA                ----
  ## _______________________________________________________ ----

  # Create a complete total abundance dataset (For DOVs)
  count.summary <- count %>%
    dplyr::full_join(metadata) %>% # 2726 rows (527 samples)
    dplyr::group_by(marine.park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(marine.park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::filter(!method %in% c("stereo-BRUVs"))


  ### MaxN (For BRUVs) ----
  count.maxn <- count %>%
    dplyr::filter(method %in% c("stereo-BRUVs")) %>%
    dplyr::group_by(marine.park, method, campaignid, sample, family, genus, species) %>%
    dplyr::summarise(maxn = sum(count))

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
    tidyr::complete(tidyr::nesting(marine.park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(maxn = 0)) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " "))

  abundance <- dplyr::bind_rows(maxn, count.summary, count.maxn)
  abundance$marine.park <- forcats::fct_relevel(abundance$marine.park, c(unique(lats$marine.park)))

  ## _______________________________________________________ ----
  ##                      ABUNDANCE METRICS                  ----
  ## _______________________________________________________ ----

  fished.abundance <- dplyr::semi_join(abundance, fished.species) %>%
    dplyr::group_by(marine.park, campaignid, method, sample, family, genus, species) %>%
    dplyr::summarise(total.abundance = sum(maxn)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
    dplyr::select(marine.park, campaignid, sample, total.abundance, method, scientific) %>%
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
    dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
    dplyr::filter(maxn > 0) %>%
    dplyr::select(marine.park, method, campaignid, sample, scientific, maxn) %>%
    dplyr::group_by(marine.park, method, campaignid, sample) %>%
    dplyr::summarise(species.richness = length(unique(scientific))) %>%
    dplyr::select(marine.park, campaignid, sample, species.richness, method) %>%
    dplyr::full_join(metadata) %>%
    tidyr::replace_na(list(species.richness = 0))

  ## _______________________________________________________ ----
  ##                     COMPLETE LENGTH DATA                ----
  ## _______________________________________________________ ----

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
    tidyr::complete(tidyr::nesting(marine.park, method, campaignid, sample), tidyr::nesting(family, genus, species)) %>%
    tidyr::replace_na(list(number = 0)) %>%
    dplyr::select(marine.park, campaignid, method, sample, family, genus, species, number, length) %>%
    dplyr::left_join(metadata) %>%
    dplyr::mutate(scientific = paste(genus, species, sep = " "))

  complete.length$marine.park <- forcats::fct_relevel(complete.length$marine.park, c(unique(lats$marine.park)))


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

  # filter out unassigned and unclassified
  state.mp <- state.mp[!state.mp$ZONE_TYPE %in% c("Unassigned (IUCN IA)", "Unassigned (IUCN II)", "Unassigned (IUCN III)", "Unassigned (IUCN IV)", "Unassigned (IUCN VI)", "MMA (Unclassified) (IUCN VI)", "MP (Unclassified) (IUCN VI)"), ]
  state.mp$zone <- stringr::str_replace_all(state.mp$ZONE_TYPE, c("[^[:alnum:]]" = " "))
  state.mp$zone <- stringr::str_replace_all(state.mp$zone, c(
    "Conservation Area  IUCN IA " = "Conservation (no-take)",
    "General Use  IUCN II " = "General Use",
    "General Use Area  IUCN VI " = "General Use",
    "General Use Zone  IUCN II " = "General Use",
    "Recreation Area  IUCN II " = "Recreation",
    "Recreation Zone  IUCN II " = "Recreation",
    "Sanctuary Area  IUCN VI " = "Sanctuary (no-take)",
    "Sanctuary Zone  IUCN IA " = "Sanctuary (no-take)",
    "Special Purpose Zone  Aquaculture   IUCN VI " = "Special Purpose",
    "Special Purpose Zone  Benthic Protection   IUCN IV " = "Special Purpose",
    "Special Purpose Zone  Dugong Protection   IUCN IV " = "Special Purpose",
    "Special Purpose Zone  Habitat Protection   IUCN IV " = "Special Purpose",
    "Special Purpose Zone  Pearling   IUCN VI " = "Special Purpose",
    "Special Purpose Zone  Puerulus   IUCN IA " = "Special Purpose",
    "Special Purpose Zone  Scientific Reference   IUCN II " = "Special Purpose",
    "Special Purpose Zone  Scientific Reference   IUCN VI " = "Special Purpose",
    "Special Purpose Zone  Seagrass Protection   IUCN IV " = "Special Purpose",
    "Special Purpose Zone  Shore Based Activities   IUCN II " = "Special Purpose",
    "Special Purpose Zone  Wildlife Conservation   IUCN VI " = "Special Purpose",
    "Special Purpose Zone  Wildlife Viewing and Protection   IUCN IV " = "Special Purpose",
    "Special Purpose Zone 1  Shore based Activities   IUCN II " = "Special Purpose",
    "Special Purpose Zone 2  Shore based Activities   IUCN II " = "Special Purpose",
    "Special Purpose Zone 3  Shore based Activities   IUCN II " = "Special Purpose",
    "Special Purpose Zone 3  Shore based Activities   IUCN VI " = "Special Purpose",
    "Special Purpose Zone 4  Shore based Activities   IUCN II " = "Special Purpose"
  ))

  state.mp$zone <- as.factor(state.mp$zone)
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


  spatial.data <- state.mp@data

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


  # Version 3: one object
  x <- structure(
    list(
      downloaded_on = Sys.time(),
      abundance = abundance,
      total.abundance = total.abundance,
      trophic.abundance = trophic.abundance,
      all.data = all.data,
      fished.complete.length = fished.complete.length,
      fished.abundance = fished.abundance,
      metadata = metadata,
      sampling.effort = sampling.effort,
      state.mp = state.mp,
      state.pal = state.pal,
      park.popups = park.popups
    ),
    class = "mpa_data"
  )

  if (save == TRUE) {
    saveRDS(x, dest, compress = "xz")
  }

  x
}

#' @title S3 print method for 'mpa_data'.
#' @description Prints a short representation of mpa_data returned by
#' `generate_data`.
#' @param x An object of class `mpa_data` as returned by `generate_data`.
#' @param ... Extra parameters for `print`
#' @export
#' @family included
print.mpa_data <- function(x, ...) {
  print(
    glue::glue(
      "<MPA Data> accessed on {x$downloaded_on}\n",
      "  Abundance:   {nrow(x$abundance)}\n",
      "  total.abundance:   {nrow(x$total.abundance)}\n",
      "  trophic.abundance:   {nrow(x$trophic.abundance)}\n",
      "  all.data:    {nrow(x$all.data)}\n",
      "  fished.complete.length:    {nrow(x$fished.complete.length)}\n",
      "  fished.abundance:    {nrow(x$fished.abundance)}\n",
      "  metadata:    {nrow(x$metadata)}\n",
      "  sampling.effort:    {nrow(x$sampling.effort)}\n",
      "  state.mp:    {nrow(x$state.mp)}\n",
      "  state.pal:    {nrow(x$state.pal)}\n",
      "  park.popups:    {nrow(x$park.popups)}\n"
    )
  )
  invisible(x)
}
