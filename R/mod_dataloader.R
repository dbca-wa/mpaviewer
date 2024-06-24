#' dataloader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dataloader_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' dataloader Server Functions
#'
#' @noRd 
mod_dataloader_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_dataloader_ui("dataloader_1")
    
## To be copied in the server
# mod_dataloader_server("dataloader_1")
