#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tags
#' @importFrom shinycssloaders withSpinner
#' @import shinyWidgets shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboardPlus::dashboardPage(
      shinydashboardPlus::dashboardHeader(

        tags$li(class = "dropdown"
        ),

        title = tags$a(
          href = "https://www.dbca.wa.gov.au/",
          tags$img(
            src = "www/DBCA_logo1_reversed_BCS.png",
            width = "465px"
          )
        ),
        titleWidth = "500px",
        tags$li(actionButton("access", "Accessibility", class = "btn-btn-lg")
                # a(href = "https://www.dbca.wa.gov.au/", target = "_blank", "Accessibility")
                , class = "dropdown"
        )
      ),
      shinydashboardPlus::dashboardSidebar(

        width = "125px",
        shinydashboard::sidebarMenu(id = "tabs",
                                    h1(" "), # to move the fish down a lil bit
                                    h1(" "), # to move the fish down a lil bit
                                    menuItem(tags$div(tags$i(icon("fish")), tags$span("Fish")),
                                             tabName = "fishtab"), #, icon = icon("fish")

                                    menuItem(tags$div(tags$img(src = "www/coral.png", width="20px"),
                                                      # tags$i(icon("pagelines")),
                                                      tags$span("Coral")),
                                             tabName = "coraltab"),

                                    menuItem(tags$div(tags$i(icon("info")), tags$span("Info")),
                                             tabName = "info")#, icon = icon("pagelines")
                                    # div(tags$img(src = "www/coral.png", width="15px")))
        )
      ),
      shinydashboard::dashboardBody(

        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass" margin-top: 10px;> <b>DBCA Marine Monitoring Dashboard </b> </span>\');
      })
     ')),


          # TAB CONTENT ----
        tabItems(
          # One line per asset module; the id must match the mod_*_server() call in app_server.R,
          # and the module's tabItem tabName must match a menuItem in the sidebar above.
          mod_fish_ui("fish"),

          mod_coral_ui("coral"),

          tabItem(
            tabName = "info",
            fluidRow(
              tabBox(
                title = "Info",
                width = 12,
                # width = "55%", # was 95%
                id = "tabset1", height = "78vh",
                # tabPanel("Accessibility",
                #          style = "overflow: visible",
                #          # column(width = 5,
                #          htmltools::includeMarkdown("inst/app/www/accessibility.md")
                # ),
                tabPanel("Methodology",
                         style = "overflow: auto",
                         fluidRow(
                           box(width = 12,
                               column(width = 7, style = "padding:15px",
                                      includeHTML("inst/app/www/images/text/stereo-BRUVs.html")),
                               column(width = 4,
                                      img (src = "www/images/method_bruv.png", align="right")))),
                         fluidRow(
                           box(width = 12,
                               column(width = 7, style = "padding:15px",
                                      includeHTML("inst/app/www/images/text/stereo-DOVs.html")),
                               column(width = 4,
                                      img (src = "www/images/method_dov.png", align="right")))),
                         fluidRow(
                           box(width = 12,
                               column(width = 7, style = "padding:15px",
                                      includeHTML("inst/app/www/images/text/stereo-ROV.html")),
                               column(width = 4,
                                      img (src = "www/images/method_rov.png", align="right"))))),
                tabPanel("Contact",
                         style = "overflow: visible"#,
                         # column(width = 5,
                         #htmltools::includeMarkdown("inst/app/www/accessibility.Rmd")
                )

              ))
          )
        )
      )
      # ,
      # footer = shinydashboardPlus::dashboardFooter(
      #   left = "By Divad Nojnarg",
      #   right = "Zurich, 2019"
      # )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mpaviewer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
