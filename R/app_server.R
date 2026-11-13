#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet dplyr grid ggplot2 leafgl forcats cachem leaflegend ggh4x scales
#' @noRd
#'

#Shell server: load the (cached) data, start one server per asset module, hander the header button.
#To add an asset: add mod_<asset>_server("<asset>", mpa_data) below (id must match its UI call)

app_server <- function(input, output, session) {

  mpa_data <- load_mpa_data()

  mod_fish_server("fish", mpa_data)
  mod_coral_server("coral", mpa_data)

  observeEvent(input$access, {
    updateTabItems(session, "tabs", "info")
  })
}



