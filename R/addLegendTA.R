#' Title
#'
#' @param map
#' @param colors
#' @param labels
#' @param sizes
#' @param opacity
#' @param group
#'
#' @return
#' @export
#'
#' @examples
addLegendTA <- function(map, colors, labels, sizes, opacity = 1, group) {
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0(
    "<div style='display: inline-block;height: ",
    sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
    labels, "</div>"
  )

  return(leaflet::addLegend(map,
    colors = colorAdditions,
    labels = labelAdditions,
    opacity = opacity,
    title = "Total abundance",
    position = "bottomright",
    group = group
  ))
}
