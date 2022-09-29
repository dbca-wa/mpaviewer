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
    # mod_assetmap_ui("assetmap_ui_1")

    shinydashboardPlus::dashboardPage(
      shinydashboardPlus::dashboardHeader(

        tags$li(class = "dropdown",
                tags$style(".main-header {max-height: 75px}"),
                tags$style(".main-header .logo {height: 75px;}"),
                tags$style(".sidebar-toggle {height: 75px; padding-top: 1px !important;}"),
                tags$style(".navbar {min-height:75px !important}")
        ),

        title = tags$a(
          href = "https://www.dbca.wa.gov.au/",
          tags$img(
            src = "www/DBCA_logo1_reversed_BCS.png",
            width = "465px"#,
            # height = "90px"#,
            #style = "margin-top:-15px; padding-top:-50px; margin-bottom:-15px; padding-bottom:-50px;"#, width = "65"
          )
        ),
        titleWidth = "500px",

        # tags$li(a(href = "https://www.dbca.wa.gov.au/", target = "_blank", "DBCA Marine Monitoring Dashboard"), class = "myClass"),

        tags$li(actionButton("access", "Accessibility", class = "btn-btn-lg")
          # a(href = "https://www.dbca.wa.gov.au/", target = "_blank", "Accessibility")
          , class = "dropdown"
          )#,

        # shinydashboardPlus::dropdownBlock(
        #   id = "dropdown",
        #   title = "DBCA Marine Monitoring Dashboard",
        #   # icon = "sliders-h",
        #
        #   tagList(a("Accessibility", href="https://www.google.com/")),
        #   tagList(a("About", href="https://www.google.com/")),
        #   tagList(a("Contact", href="https://www.google.com/"))

          # tags$li(a(href = "https://www.dbca.wa.gov.au/", target = "_blank", "Accessibility"), class = "dropdown")
        # )#,


        # tags$li(a(
        #   href = "https://www.dbca.wa.gov.au/", target = "_blank",
        #   img(
        #     src = "www/DBCA_logo1_reversed_BCS.png",
        #     title = "DBCA",
        #     height = "67px",
        #     style = "margin-top:-15px; padding-top:-50px; margin-bottom:-15px; padding-bottom:-50px;"
        #   )
        # ), class = "dropdown")
        # tags$li(a(
        #   href = "https://www.dbca.wa.gov.au/", target = "_blank",
        #   img(
        #     src = "www/DBCA_BCS_white.png",
        #     title = "DBCA",
        #     height = "67px",
        #     style = "margin-top:-15px; padding-top:-50px; margin-bottom:-15px; padding-bottom:-50px;"
        #   )
        # ), class = "dropdown")
        # tags$li(a(href = 'https://marineecology.io/', target="_blank",
        #           img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/MEG-white.png?raw=true',
        #               title = "Marine Ecology Group",
        #               height = "40px")
        # ), class = "dropdown"
        # )
      ),
      shinydashboardPlus::dashboardSidebar(
        # div(class = "sticky_footer", p("test footer")),

        tags$style(".left-side, .main-sidebar {padding-top: 55px}"),

        width = "150px",
        shinydashboard::sidebarMenu(id = "tabs",
          h1(" "), # to move the fish down a lil bit
          h1(" "), # to move the fish down a lil bit
          menuItem(tags$div(tags$i(icon("fish")), tags$span("Fish")),
                   tabName = "fishtab"), #, icon = icon("fish")

          menuItem(tags$div(tags$img(src = "www/coral.png", width="30px"),
            # tags$i(icon("pagelines")),
            tags$span("Benthic")),
                   tabName = "benthictab"),

          menuItem(tags$div(tags$i(icon("info")), tags$span("Info")),
                   tabName = "info")#, icon = icon("pagelines")
          # div(tags$img(src = "www/coral.png", width="15px")))
        )
      ),
      shinydashboard::dashboardBody(
        tags$head(
          tags$style(HTML(".main-sidebar { font-size: 20px; }
                       .skin-blue .main-header .navbar .sidebar-toggle {display: none;}
                      .main-header .navbar {height: 75px;; padding-top: 0px;}
                      .navbar-statis-top {height: 75px;}
                      .main-header .logo {height: 75px; padding-top: 0%;}
                      .content-wrapper {background-colour: #602727;}"))
        ),
        tags$style("html, body {overflow: hidden !important;"),

        #  Links with dotted underline
        # a {
        #   font-weight: 500;
        #   border-bottom: dotted 2px #7f7f7f;
        #   text-decoration: none;
        #   -moz-transition: all .3s ease;
        #   -o-transition: all .3s ease;
        #   -webkit-transition: all .3s ease;
        #   transition: all .3s ease;
        # }

        tags$head(tags$style(HTML(

          # font-size: 20px;
          ".myClass {
        line-height: 50px;
        text-align: left;
        font-family: 'Heebo',Arial,Helvetica,sans-serif;
        padding: -0 10px;
        overflow: hidden;
        color: white;
        font-weight: 600;
        font-size: 1.5rem;
        margin-bottom: 1.25rem;
          }

       .skin-blue .main-header .navbar {
    background-color: #2d2f32;
      }

      .skin-blue .main-header .logo {
    background-color: #2d2f32;
    color: #fff;
    border-bottom: 0 solid transparent;
      }

.skin-blue .sidebar-menu>li.active>a {
    border-left-color: #d14210;
}

.skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #3b3e42;
}




    "
        ))),
        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass" margin-top: 10px;> <b>DBCA Marine Monitoring Dashboard </b> </span>\');
      })
     ')),
        tabItems(
          # First tab content
          tabItem(
            tabName = "fishtab",
            fluidRow(
              tabBox(
                title = "Choose a spatial scale to plot",
                width = 8,
                # width = "55%", # was 95%
                id = "tabset1", height = "78vh",
                tabPanel("State-wide",
                  style = "overflow: visible",
                  # column(width = 5,
                  htmlOutput("fish.state.park.dropdown"),
                  htmlOutput("fish.state.method.dropdown", multiple = FALSE),
                  selectInput(
                    width = "100%",
                    "fishstatemetric",
                    "Choose a group of metrics to plot:",
                    choices = c("Whole assemblage", "Individual species", "Target species", "Life history traits"),
                    multiple = FALSE,
                    selectize = TRUE
                  ),
                  conditionalPanel(
                    'input.fishstatemetric == "Whole assemblage"',
                    withSpinner(plotOutput("fish.state.total.plot", height = 500)), # ui.
                    withSpinner(plotOutput("fish.state.rich.plot", height = 500)) # ui.
                    # plotOutput("fish.state.stack.plot", height = 500)
                  ),
                  conditionalPanel(
                    'input.fishstatemetric == "Target species"',
                    htmlOutput("fish.state.fished.species.dropdown"),
                    withSpinner(plotOutput("fish.state.fished.species.abundance.plot", height = 500)),
                    withSpinner(plotOutput("fish.state.fished.species.kde.plot", height = 500))
                  ),
                  conditionalPanel(
                    'input.fishstatemetric == "Individual species"',
                    htmlOutput("fish.state.all.species.dropdown"),
                    withSpinner(plotOutput("fish.state.all.species.abundance.plot", height = 500))
                  ),
                  conditionalPanel(
                    'input.fishstatemetric == "Life history traits"',
                    htmlOutput("fish.state.trophic.dropdown"),
                    withSpinner(plotOutput("fish.state.trophic.plot", height = 500))
                  )
                ),
                tabPanel(
                  "Marine park",
                  column(
                    width = 6,
                    htmlOutput("fish.park.dropdown"),
                    htmlOutput("fish.park.method.dropdown"),
                    htmlOutput("fish.park.site.dropdown")
                  ),
                  column(
                    width = 6,
                    column(
                      width = 11,
                      uiOutput("ui.fish.park.image")
                    ),
                    column(
                      width = 1,
                      actionBttn(
                        inputId = "alert.marinepark",
                        label = NULL,
                        style = "material-circle",
                        color = "primary",
                        icon = icon("info")
                      )
                    )
                  ),

                  # ),
                  # column(width = 9,
                  # h4("Sampling locations"),
                  # leafletOutput(width = "100%", "fish.park.sampling.leaflet", height = 400),
                  # ),

                  selectInput(
                    width = "100%",
                    "fishparkmetric",
                    "Choose a group of metrics to plot:",
                    choices = c("Whole assemblage", "Individual species", "Target species", "Life history traits"),
                    multiple = FALSE,
                    selectize = TRUE
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Whole assemblage"',
                    withSpinner(plotOutput("fish.park.total.plot", height = 250)),
                    withSpinner(plotOutput("fish.park.rich.plot", height = 250)),
                    withSpinner(plotOutput("fish.park.stack.plot", height = 500))
                    # leafletOutput("fish.park.total.leaflet", height = 400)
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Target species"',
                    htmlOutput("fish.park.fished.species.dropdown"),
                    withSpinner(plotOutput("fish.park.fished.species.abundance.plot", height = 500)),
                    withSpinner(plotOutput("fish.park.fished.species.kde.plot", height = 500))
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Individual species"',
                    htmlOutput("fish.park.all.species.dropdown"),
                    withSpinner(plotOutput("fish.park.all.species.abundance.plot", height = 500))
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Life history traits"',
                    htmlOutput("fish.park.trophic.dropdown"),
                    withSpinner(plotOutput("fish.park.trophic.plot", height = 750))
                  )
                )
                # tabPanel(
                #   "Location",
                #   selectInput(
                #     width = "100%",
                #     "fishlocationpark",
                #     "Choose a marine park:",
                #     choices = c("Ningaloo", "Jurien", "Rottnest"),
                #     multiple = FALSE,
                #     selectize = TRUE
                #   ),
                #   selectInput(
                #     width = "100%",
                #     "fishlocationlocation",
                #     "Choose a location:",
                #     choices = c("Location 1", "Location 2", "Location 3"),
                #     multiple = FALSE,
                #     selectize = TRUE
                #   ),
                #   selectInput(
                #     width = "100%",
                #     "fishlocationmethod",
                #     "Choose a method:",
                #     choices = c("DOVs", "BRUVs", "ROV"),
                #     multiple = FALSE,
                #     selectize = TRUE
                #   ),
                #   selectInput(
                #     width = "100%",
                #     "fishlocationmetric",
                #     "Choose a metric:",
                #     choices = c("Total abundance", "Species richness", "Targeted abundance"),
                #     multiple = FALSE,
                #     selectize = TRUE
                #   )
                # )
              ),
              fluidRow(
                div(column(
                  width = 4,
                  box(
                    width = 12, title = "Sampling locations",
                    conditionalPanel(
                      'input.tabset1 == "State-wide"',
                      withSpinner(leafletOutput(width = "100%", "fish.state.sampling.leaflet", height = "78vh")),
                      style="z-index:1002;"
                    ),
                    conditionalPanel(
                      'input.tabset1 == "Marine park"',
                      withSpinner(leafletOutput(width = "100%", "fish.park.sampling.leaflet", height = "78vh"))
                    )

                    # h4("Sampling locations"),
                    # leafletOutput(width = "100%", "fish.state.sampling.leaflet", height = "78vh")
                  ), # end of box
                  style = "position:fixed; right: 0;"
                ))
              )
            )
          ),

          # Second tab content - enable when content is ready
          tabItem(
            tabName = "benthictab",
            h2("to add to")
          ),

          tabItem(
            tabName = "info",
          #   fluidRow(width = 12,
          #            box(#style = "overflow: visible;",
          #       width = 12, #height = "900", # 60vh shows all text but not all box
          #       collapsible = FALSE, solidHeader = FALSE,
          #       htmltools::includeMarkdown("inst/app/www/accessibility.Rmd"))
          # )

          fluidRow(
            tabBox(
              title = "Info",
              width = 12,
              # width = "55%", # was 95%
              id = "tabset1", height = "78vh",
              tabPanel("Accessibility",
                       style = "overflow: visible",
                       # column(width = 5,
                       htmltools::includeMarkdown("inst/app/www/accessibility.Rmd")
              ),
              tabPanel("Contact",
                       style = "overflow: visible",
                       # column(width = 5,
                       htmltools::includeMarkdown("inst/app/www/accessibility.Rmd")
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
