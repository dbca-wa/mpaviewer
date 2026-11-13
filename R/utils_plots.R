#Helpers for the pre-rendered PNG plots in inst/app/www/plots (created by generate_plots())
#Previously every plot required an 8-line code block, now each plot requires one row in a table

#This is a slug that replaces the same _str_replace_all that was previously copied into every old code block
#' 'Ningaloo Marine Park -> "ningaloo (matches the PNG /banner file names)
#' @noRd
park_slug<- function(park) {
  stringr::str_replace_all(tolower(park), c("marine park" = "", "island marine reserve" = "", " "=""))
}

#Falls back to the repo path then the package is not installed
plots_dir<- function(){
  d<-app_sys("app/www/plots")
  if (nzchar(d)) d else "inst/app/www/plots"
}

#Builds the img tag for one PNG
#' An <img> for a pre-rendered plot, or a short message if the file is missing
#'
#' @param file Path relative to 'www/plots', e.g. "Fish_ningaloo_stereo-DOVs_cti.png".
#' @param title Optional heading shown above the plot
#' @noRd
plot_img <- function(file, title= NULL) {
  content<- if (file.exists(file.path(plots_dir(), file))) { #  #file.exists() check; a missing png shows a message instead of broken image
    img(src = paste0("www/plots/", file), align = "left", width = "100%")
  } else {
    p(em("Plot not available for this selection."))
  }
  if (is.null(title)) content else tagList(h4(title), content)
}

#Loops over the rows of the plot table and returns one output$<id> for each row
#' Register one renderUI output per row of a plot spec
#'
#' One row = one plot. Columns:
#' *`id` output id (must match the `ns("id")` in the UI)
#' *`suffix` file name ending e.g. "total_abundance" -> Fish_<park>_<method>_total_abundance.png
#' *`title` optional heading (NA = none)
#' *`methods` optional comma-separated methods the plot is shown for (NA = all)
#'
#' @param prefix File name prefix e.g. "Fish" or "Coral"
#' @param park,method Reactives returning the selected park/method
#' (`method = NULL` for assets without methods)
#' @noRd
register_png_plots <- function(output, spec, prefix, park, method = NULL) {
  # local () is essential: without it every output would capture the last row of the loop
  for (i in seq_len(nrow(spec))) {
    local({
      s<-spec[i, ]
      output[[s$id]] <- renderUI({
        req(park()) #wait until dropdown has a value
        if (!is.null(method)) req(method())
        if (!is.na(s$methods) && !method() %in% strsplit(s$methods, ",")[[1]]) return (NULL) #hide plots when restricted to certain methods (e.g. site plots)
        file<-paste(c(prefix, park_slug(park()), if (!is.null(method)) method (), s$suffix), collapse="_")
        plot_img(paste0(file, ".png"), if (is.na(s$title)) NULL else s$title)
      })
   })
  }
}

popups_dir <- function() {
  d <- app_sys("app/www/popups")
  if (nzchar(d)) d else "inst/app/www/popups"
}

#Insert an HTML file as-is without includeHTML()'s 'complete HTML document warning (not essential just annoying)
#A missing file shows a short message instead of ending the session
include_page <-function(path){
  if (!file.exists(path)) return(p(em("No information available for this selection.")))
  HTML(paste(readLines(path, warn=FALSE), collapse = "\n"))
}
