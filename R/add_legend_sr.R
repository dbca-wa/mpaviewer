#' Add a legend to a Leaflet map
#'
#' This function wraps `leaflet::addLegend` with some defaults.
#'
#' @param map A Leaflet map object
#' @param colors A string of colours, e.g. `c("black", "yellow", "yellow")`.
#' @param labels A list of labels, e.g. `c(0:10)`.
#' @param sizes A list of sizes, e.g. `c(5, 20, 40)`.
#' @param opacity The legend's opacity as decimal number between 0 and 1,
#'   default: `1`.
#' @param group A string containing the intended layer name,
#'   e.g. `"Species richness"`.
#'
#' @return The output of `leaflet::addLegend`.
#' @export
add_legend_sr <- function(map, colors, labels, sizes, opacity = 1, group) {
  colorAdditions <- glue::glue(
    "{colors}; border-radius: 50%; width:{sizes}px; height:{sizes}px"
  )
  labelAdditions <- glue::glue(
    "<div style='display: inline-block; height: {sizes}px; ",
    "margin-top: 4px;line-height: {sizes}px;'>{labels}</div>"
  )

  return(
    leaflet::addLegend(map,
                   colors = colorAdditions,
                   labels = labelAdditions,
                   opacity = opacity,
                   title = "Species richness",
                   group = group,
                   position = "bottomright"
  ))
}
