#utils_data is a helper function to load data once per R process and share it by every session
#restart R/the app after regenerating the .Rdata, the cache is not invalidated
#create an empty environment when package loads that persists for the whole R process
.mpa_cache <-new.env()

#' Load the dashboard data
#'
#' Reads 'inst/data/mpa_data.Rdata' on first call and returns the cached 'mpa_data' lists afterwards
#' @noRd
load_mpa_data<- function() {
  if (is.null(.mpa_cache$mpa_data)) { #check if cache is stored yet
    path <- app_sys("data/mpa_data.Rdata") #local file in dev, downloaded azure blob in prod
    if(!nzchar(path)) path<- "inst/data/mpa_data.Rdata" #fall back to repo-relative path
    e<-new.env() #make temp envionment to load into
    load(path, envir=e)

    #MPA zone polygons have ~2m vertices (~47mb of JSON per map). Simplify once to ~25m (10x smaller, same look at park zoom). Projected CRS as tolerance is in meters
    e$mpa_data$state_mp <- e$mpa_data$state_mp |>
      sf::st_transform(3577) |>
      sf::st_simplify(dTolerance = 25, preserveTopology = TRUE) |>
      sf::st_transform(4326)

    .mpa_cache$mpa_data <-e$mpa_data #store rdata object into cache
  }
  .mpa_cache$mpa_data #return freshly loaded/cached data
}

#' filter a data.frame/data.table a marine park (and optionally a method)
#' @noRd
# Base subsetting as it works for both data.frame and data.table inputs
filter_park<- function(tbl, park, method = NULL){
  keep<- tbl$marine_park %in% park
  if (!is.null(method)) keep <- keep & tbl$method %in% method
  tbl[keep,]
}

#' Zone polygons for one park
#'
#' The statewide zones are ~5mb of JSON per map and a single park only needs its own zones (0.1-2.6mb JSON).
#' A zone is kept when its `name` matches the park (ignoring "Marine Park"/"Marine Reserve' and punctuation), or when the park's sampling points sit in/next to it (<= `near_m`)
#' which also brings in neighbours like Murion Islands (Ningaloo). Every zone of a matched name is kept whole, so unsampled zones are never droped.
#' @param zones The `state_mp` sf object.
#' @param park Selected marine park name, e.g. "Ningaloo Marine Park".
#' @param pts Data with `latitude_dd` / `longitude_dd` for that park's samples
#' @param near_m How close (metres) a sample must be to count as being in a zone's area
#' @noRd

zones_for_park <- function(zones, park, pts, near_m=10000){
  norm <- function(x) gsub("[^a-z]", "", tolower(sub(" Marine (Park|Reserve)$", "", x, ignore.case=TRUE)))
  nz <- norm(zones$name); np <- norm(as.character(park))
  keep <- zones$name[nz==np | (nzchar(nz) & (startsWith(nz, np)|startsWith(np, nz)))]
  lon<- as.numeric(pts$longitude_dd); lat<- as.numeric(pts$latitude_dd); ok<- is.finite(lon) & is.finite(lat)
  pts<-unique(data.frame(longitude_dd = lon[ok], latitude_dd = lat[ok]))
  if (nrow(pts)){
    zm <- sf::st_transform(zones, 3577) #metres, so near_m means metres
    pm <- sf::st_transform(sf::st_as_sf(pts, coords = c("longitude_dd", "latitude_dd"), crs=4326), 3577)
    idx <- sf::st_nearest_feature(pm, zm)
    near <- as.numeric(sf::st_distance(pm, zm[idx,], by_element = TRUE)) <= near_m
    keep<- union(keep, zones$name[idx[near]])
  }
  if (!length(keep)) return(zones) #nothing match: show every zone rather than none
  zones[zones$name %in% keep,]
}

