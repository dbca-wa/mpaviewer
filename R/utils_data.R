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
