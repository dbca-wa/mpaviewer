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

        tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),

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

        tags$style(
          type = "text/css",
          "
      .loader {
        min-height: 80px !important;
      }
    "
        ),

    tags$style(".left-side, .main-sidebar {padding-top: 55px}

.wrap
{
    width: 500px;
    height: 750px;
    padding: 0;
    overflow: hidden;
}

.frame
{
    width: 1000px;
    height: 1300px;
    border: 0;

    -ms-transform: scale(0.5);
    -moz-transform: scale(0.5);
    -o-transform: scale(0.5);
    -webkit-transform: scale(0.5);
    transform: scale(0.5);

    -ms-transform-origin: 0 0;
    -moz-transform-origin: 0 0;
    -o-transform-origin: 0 0;
    -webkit-transform-origin: 0 0;
    transform-origin: 0 0;
}

                   "),

        tags$head(
          tags$style(HTML(".main-sidebar { font-size: 18px; }
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

        tags$style(".small-box.bg-yellow { background-color: #0CA2B0 !important; color: #ffffff !important; }"),
        tags$style(".inner { color: #ffffff !important; }"),

        tags$head(tags$style(HTML(
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
} "
        ))),
        tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass" margin-top: 10px;> <b>DBCA Marine Monitoring Dashboard </b> </span>\');
      })
     ')),

    # FISH TABS CONTENT ----
        tabItems(
          tabItem(
            tabName = "fishtab",
            fluidRow(


              # valueBoxOutput("box.total.fish"),

              tabBox(
                title = "Choose a spatial scale to plot",
                width = 8,
                # width = "55%", # was 95%
                id = "tabset1", height = "78vh",


                ## STATE ----
                tabPanel("State-wide summary",
                         style = "overflow: auto",
                         # column(width = 5,
                         # htmlOutput("fish.state.park.dropdown"),
                         # h4("State summary:"),
                         valueBoxOutput("box.total.number.fish", width = 6),
                         valueBoxOutput("box.total.species.fish", width = 6),
                         h4("Marine Parks Surveyed:"),
                         uiOutput("statewide_plots"),
                ),

                ### End of State

                ## MARINE PARK ----

                tabPanel(
                  "Marine park",
                  style = "overflow: auto",
                  # box(width = 12, title = "Filter data", solidHeader = TRUE, #status = "warning",
                  column(
                    width = 6,
                    htmlOutput("fish.park.dropdown"),
                    uiOutput("fish.park.method.dropdown"),
                    htmlOutput("acknowledgement")
                  ),
                  column(
                    width = 6,
                    column(
                      width = 11,
                      uiOutput("ui.fish.park.image")
                    ),
                    column(
                      width = 1,
                      actionButton("alert.marinepark",
                                   label = " ",
                                   icon = icon("info"),
                                   icon.library = "font awesome",
                                   style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px"
                      )


                    )
                  ),

                  # box(width = 12, title = "Plot data", #solidHeader = TRUE, #status = "warning",
                  selectInput(
                    width = "100%",
                    "fishparkmetric",
                    "Choose a group of metrics to plot:",
                    choices = c("Whole assemblage", "Individual species", "Target species", "Feeding guilds"),
                    multiple = FALSE,
                    selectize = TRUE
                  )#)
                  ,
                  conditionalPanel(
                    'input.fishparkmetric == "Whole assemblage"',

                    h3("Reef fish thermal index (RFTI):", actionButton("park.cti", label = " ",
                                                        icon = icon("info"),
                                                        icon.library = "font awesome",
                                                        style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

                    withSpinner(uiOutput("fish.park.cti")),

                    h3("Total abundance:", actionButton("park.ta", label = " ",
                                                        icon = icon("info"),
                                                        icon.library = "font awesome",
                                                        style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

                    withSpinner(uiOutput("fish.park.total.plot")),

                    uiOutput("fish.park.total.trend"),

                    h4("Most abundant species:"),
                    withSpinner(uiOutput("fish.park.stack.plot")),

                    h4("Total abundance by sanctuary:"),
                    withSpinner(uiOutput("fish.park.total.sanctuary.plot")),

                    h4("Total abundance by zone:"),
                    # withSpinner(uiOutput("ui.fish.park.total.zone.plot")),
                    withSpinner(uiOutput("fish.park.total.zone.plot")),

                    ## Only show site plots if stereo-DOVs
                    withSpinner(uiOutput("fish.park.total.site.plot")),


                    h3("Species richness:", actionButton("park.sr", label = " ",
                                                          icon = icon("info"),
                                                          icon.library = "font awesome",
                                                          style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
                    withSpinner(uiOutput("fish.park.rich.plot", height = 350)),

                    uiOutput("fish.park.rich.trend"),

                    h4("Species richness by sanctuary:"),
                    withSpinner(uiOutput("fish.park.rich.sanctuary.plot")),

                    h4("Species richness by zone:"),
                    withSpinner(uiOutput("fish.park.rich.zone.plot")),
                    # withSpinner(plotOutput("fish.park.rich.zone.plot", height = 250)),

                    ## Only show site plots if stereo-DOVs
                    withSpinner(uiOutput("fish.park.rich.site.plot"))


                    #h4("Spatial"),
                    #withSpinner(leafletOutput(width = "100%", "fish.park.metric.leaflet", height = 300))
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Target species"',

                    h4("Total abundance of highly retained demersal species:"),
                    withSpinner(uiOutput("fish.park.fished.sum.plot")),

                    h4("Total abundance of all targeted species:"),
                    withSpinner(uiOutput("fish.park.fished.all.sum.plot")),

                    h4("Total abundance of highly retained demersal species by sanctuary:"),
                    withSpinner(uiOutput("fish.park.fished.sum.sanctuary.plot")),

                    h4("Total abundance of all targeted species by sanctuary:"),
                    withSpinner(uiOutput("fish.park.fished.all.sum.sanctuary.plot")),

                    htmlOutput("fish.park.fished.species.dropdown"),
                    withSpinner(uiOutput("fish.park.fished.species.abundance.plot")),
                    withSpinner(uiOutput("fish.park.fished.species.abundance.sanctuary.plot")),

                    htmlOutput("fish.park.fished.species.dropdown"),
                    withSpinner(uiOutput("fish.park.fished.species.abundance.plot")),
                    withSpinner(uiOutput("fish.park.fished.species.abundance.sanctuary.plot")),


                    # withSpinner(plotOutput("fish.park.fished.species.kde.plot", height = 500)),
                    # h4("KDE:"),
                    # withSpinner(uiOutput("fish.park.fished.species.kde.plot")),
                    #
                    # # withSpinner(uiOutput("ui.fish.park.fished.species.kde.sanctuary.plot")),
                    # htmlOutput("fish.park.fished.species.iframe")
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Individual species"',
                    htmlOutput("UIfish.park.all.species.dropdown"),

                    # h4("Total abundance of chosen species:"),
                    withSpinner(uiOutput("fish.park.all.species.abundance.plot")),

                    # h4("Total abundance of chosen species by sanctuary:"),
                    withSpinner(uiOutput("fish.park.all.species.abundance.sanctuary.plot")),

                    fluidRow(
                      box(
                    h4("Spatial"),
                    withSpinner(leafletOutput(width = "100%", "fish.park.all.species.leaflet", height = 600))),
                    box(
                    div(class = "wrap",
                    htmlOutput("fish.park.all.species.iframe"))
                    ))
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Feeding guilds"',

                    # h4("Piscivores:"),
                    uiOutput("fish.park.trophic.pisc.plot"),

                    # h4("Herbivores:"),
                    withSpinner(uiOutput("fish.park.trophic.herb.plot")),

                    # h4("Corallivores:"),
                    withSpinner(uiOutput("fish.park.trophic.coral.plot")),

                    # h4("Omnivores:"),
                    withSpinner(uiOutput("fish.park.trophic.omni.plot")),

                    # h4("Intertivores:"),
                    withSpinner(uiOutput("fish.park.trophic.invert.plot")),

                    # h4("Planktivores:"),
                    withSpinner(uiOutput("fish.park.trophic.plank.plot"))
                  )
                )

              ),

              ### End of marine park plots

              ### Leaflet box ----

              fluidRow(
                div(column(
                  width = 4,
                  box(
                    width = 12, title = "Sampling locations",
                    conditionalPanel(
                      'input.tabset1 !== "Marine park"',
                      withSpinner(leafletOutput(width = "100%", "fish.state.sampling.leaflet", height = "78vh")),
                      style="z-index:1002;"
                    ),
                    conditionalPanel(
                      'input.tabset1 == "Marine park"',
                      withSpinner(leafletOutput(width = "100%", "fish.park.sampling.leaflet", height = "78vh"))
                    )

                    # h4("Sampling locations"),
                    # leafletOutput(width = "100%", "fish.state.sampling.leaflet", height = "78vh")
                  ),

                  style = "position:fixed; right: 0;"
                ))
              )
            )
          ),

          # CORAL TABS CONTENT ----

          tabItem(
            tabName = "coraltab",
            fluidRow(


              # valueBoxOutput("box.total.fish"),

              tabBox(
                title = "Choose a spatial scale to plot",
                width = 8,
                # width = "55%", # was 95%
                id = "tabset2", height = "78vh",


                ## STATE ----
                tabPanel("State-wide summary",
                         style = "overflow: auto",
                         valueBoxOutput("box.total.number.coral", width = 6),
                         valueBoxOutput("box.total.species.coral", width = 6),
                         h4("Marine Parks Surveyed:"),
                          column(width = 11,
                             img(src = "www/plots/Coral_All_Parks_coral_cover.png", align="centre", width = "100%"))
                ),


          ## MARINE PARK ----
          tabPanel(
            "Marine park",
            style = "overflow: auto",
            # box(width = 12, title = "Filter data", solidHeader = TRUE, #status = "warning",
            column(
              width = 6,
              htmlOutput("benthic.park.coralcover.dropdown"),
              selectInput(
                           width = "100%",
                           "benthicparkmethoddropdown",
                           "Choose a metric to plot:",
                           choices = c("Coral cover", "Coral recruitment"),
                           multiple = FALSE,
                           selectize = TRUE
                         ),
            ),
            column(
              width = 6,
              column(
                width = 11,
                uiOutput("ui.benthic.park.image")
              ),
              column(
                width = 1,
                actionButton("alert.coral.marinepark",
                             label = " ",
                             icon = icon("info"),
                             icon.library = "font awesome",
                             style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px"
                )


              )
            ),

            conditionalPanel(
              'input.benthicparkmethoddropdown == "Coral cover"',

              h3("Coral cover:", actionButton("park.coralcover", label = " ",
                                                  icon = icon("info"),
                                                  icon.library = "font awesome",
                                                  style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

              withSpinner(uiOutput("benthic.park.coralcover.plot")),

              h4("Most abundant families:"),
              withSpinner(uiOutput("benthic.park.top5.coralcover.plot")),

              h4("Coral cover by reef zone:"),
              withSpinner(uiOutput("benthic.park.reefzone.coralcover.plot")),

              h4("Most abundant families by site:"),

              htmlOutput("benthic.park.site.coralcover.dropdown"),
              # withSpinner(uiOutput("ui.fish.park.total.zone.plot")),
              withSpinner(uiOutput("benthic.site.coralcover.plot")),

              h4("Coral cover by site:"),

              withSpinner(uiOutput("benthic.site.coralcover.plot.facet"))
            )

          )

              ),

              # end tab box
              fluidRow(
                div(column(
                  width = 4,
                  box(
                    width = 12, title = "Sampling locations",
                    conditionalPanel(
                      'input.tabset2 == "State-wide summary"',
                      withSpinner(leafletOutput(width = "100%", "benthic.state.sampling.leaflet", height = "78vh")),
                      style = "z-index:1002;"
                    ),
                    conditionalPanel(
                      'input.tabset2 == "Marine park"',
                      withSpinner(leafletOutput(width = "100%", "benthic.park.sampling.leaflet", height = "78vh"))
                    )
                    ), # end of box
                  style = "position:fixed; right: 0;"
                ))
              )
            )
          ),

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
