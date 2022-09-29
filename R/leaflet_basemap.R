#' Create a Leaflet basemap for Western Australia
#'
#' @param data The map data, default: NULL
#' @param l_width The leaflet map width, default: NULL
#' @param l_height The leaflet map height, default: NULL
#'
#' @return A Leaflet map focused on WA
#' @export
#' @family helpers
#'
#' @examples
#' leaflet_basemap()
leaflet_basemap <- function(data = NULL, l_width = NULL, l_height = NULL) {
  leaflet::leaflet(data = data, width = l_width, height = l_height) %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemap") %>%
    leaflet::addProviderTiles(
      "OpenStreetMap.Mapnik",
      group = "Basemap",
      options = leaflet::providerTileOptions(opacity = 0.35)
    ) %>%
    # leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE, position = "bottomright") %>%
    leaflet::addScaleBar(
      position = "bottomleft",
      options = leaflet::scaleBarOptions(
        imperial = FALSE,
        maxWidth = 200
      )
    ) %>%
    leaflet::addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
    leaflet::setView(130, -20, 5)
}

# use_test("leaflet_basemap")  # nolint
