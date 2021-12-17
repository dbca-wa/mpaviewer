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
 
  )
}
    
#' assetmap Server Functions
#'
#' @noRd 
mod_assetmap_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_assetmap_ui("assetmap_ui_1")
    
## To be copied in the server
# mod_assetmap_server("assetmap_ui_1")
