#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tags
#' @importFrom shinycssloaders withSpinner
#' @import shinyWidgets shinydashboard
#' @noRd
app_ui <- function(request) {


  # ui <- fluidPage(
  #   selectInput(
  #     "input1",
  #     "Dropdwon 1",
  #     c("ID1","ID2"),
  #     selected = "ID1",
  #     multiple = FALSE
  #   ),
  #
  #   htmlOutput("fish.state.method.dropdown", multiple = FALSE)
  # )



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
        tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),

        width = "125px",
        shinydashboard::sidebarMenu(id = "tabs",
          h1(" "), # to move the fish down a lil bit
          h1(" "), # to move the fish down a lil bit
          menuItem(tags$div(tags$i(icon("fish")), tags$span("Fish")),
                   tabName = "fishtab"), #, icon = icon("fish")

          menuItem(tags$div(tags$img(src = "www/coral.png", width="20px"),
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


                ## MARINE PARK ----
                tabPanel(
                  "Marine park",
                  style = "overflow: auto",
                  # box(width = 12, title = "Filter data", solidHeader = TRUE, #status = "warning",
                  column(
                    width = 6,
                    htmlOutput("fish.park.dropdown"),
                    uiOutput("fish.park.method.dropdown")
                    # selectizeInput("fish.park.method.dropdown", "Choose a method:", multiple = FALSE, choices = character(0))

                    # selectizeInput("fish.park.method.dropdown", "Choose a method:",
                    #                selected = "stereo-BRUVs", multiple = FALSE,
                    #                choices = c("stereo-BRUVs", "stereo-DOVs", "stereo-ROVs"))
                    #,
                    # htmlOutput("fish.park.site.dropdown")
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
                    choices = c("Whole assemblage", "Individual species", "Target species", "Life history traits"),
                    multiple = FALSE,
                    selectize = TRUE
                  )#)
                  ,
                  conditionalPanel(
                    'input.fishparkmetric == "Whole assemblage"',


                    h3("Species richness:", actionButton("park.sr", label = " ",
                                                          icon = icon("info"),
                                                          icon.library = "font awesome",
                                                          style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
                    withSpinner(plotOutput("fish.park.rich.plot", height = 250)),

                    uiOutput("fish.park.rich.trend"),

                    h4("Species richness by sanctuary:"),
                    withSpinner(uiOutput("ui.fish.park.rich.sanctuary.plot")),

                    h4("Species richness by zone:"),
                    # withSpinner(uiOutput("ui.fish.park.rich.zone.plot")),
                    withSpinner(plotOutput("fish.park.rich.zone.plot", height = 250)),

                    ## Only show site plots if stereo-DOVs
                    withSpinner(uiOutput("ui.fish.park.rich.site.plot")),

                    h3("Total abundance:", actionButton("park.ta", label = " ",
                                                        icon = icon("info"),
                                                        icon.library = "font awesome",
                                                        style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

                    withSpinner(plotOutput("fish.park.total.plot", height = 250)),

                    uiOutput("fish.park.total.trend"),

                    h4("Most abundant species:"), # TODO check
                    withSpinner(plotOutput("fish.park.stack.plot", height = 500)),

                    h4("Total abundance by sanctuary:"),
                    withSpinner(uiOutput("ui.fish.park.total.sanctuary.plot")),

                    h4("Total abundance by zone:"),
                    # withSpinner(uiOutput("ui.fish.park.total.zone.plot")),
                    withSpinner(plotOutput("fish.park.total.zone.plot", height = 250)),

                    ## Only show site plots if stereo-DOVs
                    withSpinner(uiOutput("ui.fish.park.total.site.plot")),


                    h4("Spatial"),
                    withSpinner(leafletOutput(width = "100%", "fish.park.metric.leaflet", height = 300))
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Target species"',

                    h4("Total abundance of targeted species:"),
                    withSpinner(plotOutput("fish.park.fished.sum.plot", height = 250)),

                    h4("Total abundance of targeted species by sanctuary:"),
                    withSpinner(uiOutput("ui.fish.park.fished.sum.sanctuary.plot")),

                    htmlOutput("fish.park.fished.species.dropdown"),
                    withSpinner(plotOutput("fish.park.fished.species.abundance.plot", height = 250)),
                    withSpinner(uiOutput("ui.fish.park.fished.species.abundance.sanctuary.plot")),

                    # withSpinner(plotOutput("fish.park.fished.species.kde.plot", height = 500)),
                    withSpinner(uiOutput("ui.fish.park.fished.species.kde.plot")),

                    # withSpinner(uiOutput("ui.fish.park.fished.species.kde.sanctuary.plot")),
                    htmlOutput("fish.park.fished.species.iframe")
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Individual species"',
                    htmlOutput("UIfish.park.all.species.dropdown"),

                    h4("Total abundance of chosen species:"),
                    withSpinner(plotOutput("fish.park.all.species.abundance.plot", height = 250)),

                    h4("Total abundance of chosen species by sanctuary:"),
                    withSpinner(uiOutput("ui.fish.park.all.species.abundance.sanctuary.plot")),

                    h4("Spatial"),
                    withSpinner(leafletOutput(width = "100%", "fish.park.all.species.leaflet", height = 500)),

                    br(),

                    htmlOutput("fish.park.all.species.iframe")
                  ),
                  conditionalPanel(
                    'input.fishparkmetric == "Life history traits"',
                    htmlOutput("fish.park.trophic.dropdown"),
                    withSpinner(plotOutput("fish.park.trophic.plot", height = 750))
                  )
                )
                #,


                # ## STATE ----
                # tabPanel("State-wide",
                #          style = "overflow: auto",
                #          # column(width = 5,
                #          htmlOutput("fish.state.park.dropdown"),
                #          htmlOutput("fish.state.method.dropdown", multiple = FALSE),
                #          selectInput(
                #            width = "100%",
                #            "fishstatemetric",
                #            "Choose a group of metrics to plot:",
                #            choices = c("Whole assemblage", "Individual species", "Target species", "Life history traits"),
                #            multiple = FALSE,
                #            selectize = TRUE
                #          ),
                #          conditionalPanel(
                #            'input.fishstatemetric == "Whole assemblage"',
                #
                #            h4("Species richness  ", actionButton("state.sr", label = " ", icon = icon("info"), icon.library = "font awesome", style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
                #            withSpinner(uiOutput("ui.fish.state.rich.plot")), # ui.
                #
                #            h4("Total abundance  ", actionButton("state.ta", label = " ", icon = icon("info"), icon.library = "font awesome", style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
                #            withSpinner(uiOutput("ui.fish.state.total.plot")), # ui.
                #
                #            h4("Spatial"),
                #            withSpinner(leafletOutput(width = "100%", "fish.state.metric.leaflet", height = 500))
                #
                #            # plotOutput("fish.state.stack.plot", height = 500)
                #          ),
                #          conditionalPanel(
                #            'input.fishstatemetric == "Target species"',
                #            htmlOutput("fish.state.fished.species.dropdown"),
                #            withSpinner(uiOutput("ui.fish.state.fished.species.abundance.plot")),
                #            withSpinner(plotOutput("fish.state.fished.species.kde.plot", height = 500)), #TODO make this a better height
                #            htmlOutput("fish.state.fished.species.iframe")
                #          ),
                #          conditionalPanel(
                #            'input.fishstatemetric == "Individual species"',
                #            htmlOutput("fish.state.all.species.dropdown"),
                #            withSpinner(plotOutput("fish.state.all.species.abundance.plot", height = 600)),
                #
                #            h4("Spatial"),
                #            withSpinner(leafletOutput(width = "100%", "fish.state.all.species.leaflet", height = 500)),
                #
                #            br(),
                #
                #            htmlOutput("fish.state.all.species.iframe")
                #          ),
                #          conditionalPanel(
                #            'input.fishstatemetric == "Life history traits"',
                #            htmlOutput("fish.state.trophic.dropdown"),
                #            withSpinner(uiOutput("ui.fish.state.trophic.plot"))
                #          )
                # )
              ),

              # box(solidHeader = TRUE, "test box"),

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
            fluidRow(
              tabBox(
                title = "Choose a spatial scale to plot",
                width = 8,
                # width = "55%", # was 95%
                id = "tabset2", height = "78vh",
                tabPanel("State-wide",
                         style = "overflow: visible",
                         # column(width = 5,

                         selectInput(
                           width = "100%",
                           "benthicstatemethoddropdown",
                           "Choose a metric to plot:",
                           choices = c("Coral cover", "Coral recruitment"),
                           multiple = FALSE,
                           selectize = TRUE
                         )
                         ,

                         conditionalPanel('input.benthicstatemethoddropdown == "Coral cover"',
                           htmlOutput("benthic.state.park.coralcover.dropdown"),
                           withSpinner(plotOutput("benthic.state.coralcover.plot", height = 500))
                           ),

                         conditionalPanel(
                           'input.benthicstatemethoddropdown == "Coral recruitment"',

                           htmlOutput("benthic.state.park.coralrecruitment.dropdown"),

                           selectInput(
                             width = "100%",
                             "benthicstatecoralrecruitmentmetric",
                             "Plot by:",
                             choices = c("All park", "Taxa"),
                             multiple = FALSE,
                             selectize = TRUE
                           )
                         ),

                           conditionalPanel(
                             'input.benthicstatemethoddropdown == "Coral recruitment" && input.benthicstatecoralrecruitmentmetric == "All park"',
                             withSpinner(plotOutput("benthic.state.coralrecruitment.all.plot", height = 500))
                           ),

                           conditionalPanel(
                             'input.benthicstatemethoddropdown == "Coral recruitment" && input.benthicstatecoralrecruitmentmetric == "Taxa"',
                             htmlOutput("benthic.state.coralrecruitment.taxa.dropdown"),
                             withSpinner(plotOutput("benthic.state.coralrecruitment.taxa.plot", height = 500))
                           )
                ), # end tab panel


                tabPanel("Marine park",
                         style = "overflow: visible",

                         column(
                           width = 6,

                         selectInput(
                           width = "100%",
                           "benthicparkmethoddropdown",
                           "Choose a metric to plot:",
                           choices = c("Coral cover", "Coral recruitment"),
                           multiple = FALSE,
                           selectize = TRUE
                         )
                         ,

                         conditionalPanel('input.benthicparkmethoddropdown == "Coral cover"', #width = 6,
                                          htmlOutput("benthic.park.coralcover.dropdown"),
                                          htmlOutput("benthic.park.site.coralcover.dropdown"),

                                             )
                                          )

                         ,

                         column(
                           width = 6,
                           column(
                             width = 11,
                             uiOutput("ui.benthic.park.image")
                           ),
                           column(
                             width = 1,
                             # actionBttn(
                             #   inputId = "alert.benthic.marinepark",
                             #   label = NULL,
                             #   style = "material-circle",
                             #   color = "primary",
                             #   icon = icon("info")
                             # )

                             actionButton("alert.benthic.marinepark",
                                          label = " ",
                                          icon = icon("info"),
                                          icon.library = "font awesome",
                                          style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px"
                                          )



                           )
                         ),



                    # column(width = 12,
                           conditionalPanel('input.benthicparkmethoddropdown == "Coral cover"',
                                     htmlOutput("benthic.park.sites.coralcover.dropdown"),
                                     h4("Whole park:"),
                                     withSpinner(plotOutput("benthic.park.coralcover.plot", height = 500)),

                                     h4("By sector:"),
                                     withSpinner(plotOutput("benthic.sector.coralcover.plot", height = 500)),

                                     h4("By site:"),
                                     withSpinner(plotOutput("benthic.site.coralcover.plot", height = 1000))
                    # )
                    ),


                    htmlOutput("benthic.park.dropdown"),
                    htmlOutput("benthic.park.method.dropdown"),
                    htmlOutput("benthic.park.site.dropdown")


                  )

              )
, # end tab box
              fluidRow(
                div(column(
                  width = 4,
                  box(
                    width = 12, title = "Sampling locations",
                    conditionalPanel(
                      'input.tabset2 == "State-wide"',
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
              tabPanel("Accessibility",
                       style = "overflow: visible",
                       # column(width = 5,
                       htmltools::includeMarkdown("inst/app/www/accessibility.md")
              ),
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
