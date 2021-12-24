#' assetmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_assetmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    navbarPage(
      "MPA Viewer",
      tabPanel("Map", leaflet::leafletOutput(ns("map")))
    )

  )
}

#' assetmap Server Functions
#'
#' @noRd
mod_assetmap_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("OpenStreetMap.Mapnik") %>%
        leaflet::addProviderTiles("Esri.WorldImagery",
                         options=leaflet::providerTileOptions(opacity=0.8)) %>%
        leaflet::setView(lng = 120, lat = -25, zoom = 4) %>%
        leaflet::addMiniMap(toggleDisplay=T, zoomLevelOffset=-8) %>%
        leaflet::addScaleBar() %>%
        leaflet::clearShapes()
    })

  })
}

## To be copied in the UI
# mod_assetmap_ui("assetmap_ui_1")

## To be copied in the server
# mod_assetmap_server("assetmap_ui_1")
