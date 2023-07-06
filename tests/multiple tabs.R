
library(shinydashboard)
## Only run this example in interactive R sessions
if (interactive()) {
  library(shiny)

  body <- dashboardBody(
    fluidRow(
      tabBox(
        title = "First tabBox",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Tab1", "First tab content"),
        tabPanel("Tab2", "Tab content 2"),
        tabPanel("Tab3", "First tab content"),
        tabPanel("Tab4", "Tab content 2"),
        tabPanel("Tab5", "First tab content"),
        tabPanel("Tab6", "Tab content 2"),
        tabPanel("Tab7", "First tab content"),
        tabPanel("Tab8", "Tab content 2"),
        tabPanel("Tab9", "First tab content"),
        tabPanel("Tab10", "Tab content 2"),
        tabPanel("Tab11", "First tab content"),
        tabPanel("Tab12", "Tab content 2"),
        tabPanel("Tab13", "First tab content"),
        tabPanel("Tab14", "Tab content 2"),
        tabPanel("Tab15", "First tab content"),
        tabPanel("Tab22", "Tab content 2")
      ),
      tabBox(
        side = "right", height = "250px",
        selected = "Tab3",
        tabPanel("Tab1", "Tab content 1"),
        tabPanel("Tab2", "Tab content 2"),
        tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
      )
    ),
    fluidRow(
      tabBox(
        # Title can include an icon
        title = tagList(shiny::icon("gear"), "tabBox status"),
        tabPanel("Tab1",
                 "Currently selected tab from first box:",
                 verbatimTextOutput("tabset1Selected")
        ),
        tabPanel("Tab2", "Tab content 2")
      )
    )
  )

  shinyApp(
    ui = dashboardPage(dashboardHeader(title = "tabBoxes"), dashboardSidebar(), body),
    server = function(input, output) {
      # The currently selected tab from the first box
      output$tabset1Selected <- renderText({
        input$tabset1
      })
    }
  )
}
