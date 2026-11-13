# UI for the fish page (modularised app)
#' fish UI function
#' @param id Module id (namespace).
#' @noRd
#' @importFrom shiny NS tagList

#UI for the fish tab. GOTCHA: every id must be wrapped in ns("..."), and conditionalPanel
# conditions are JS strings, so they use paste0("input['", ns("id"), "']== ...") not input.id
##### FISH UI #####
mod_fish_ui <- function(id) {
  ns <- NS(id)
    tabItem(
      tabName = "fishtab",
      fluidRow(


        # valueBoxOutput("box.total.fish"),

        tabBox(
          title = "Choose a spatial scale to plot",
          width = 8,
          # width = "55%", # was 95%
          id = ns("tabset1"), height = "78vh",


          ## STATE ----
          tabPanel("State-wide summary",
                   style = "overflow: auto",
                   # column(width = 5,
                   # htmlOutput("fish.state.park.dropdown"),
                   # h4("State summary:"),
                   valueBoxOutput(ns("box.total.number.fish"), width = 6),
                   valueBoxOutput(ns("box.total.species.fish"), width = 6),
                   h4("Marine Parks Surveyed:"),
                   uiOutput(ns("statewide_plots")),
          ),

          ### End of State

          ## MARINE PARK ----

          tabPanel(
            "Marine park",
            style = "overflow: auto",
            # box(width = 12, title = "Filter data", solidHeader = TRUE, #status = "warning",
            column(
              width = 6,
              htmlOutput(ns("fish.park.dropdown")),
              uiOutput(ns("fish.park.method.dropdown")),
              htmlOutput(ns("acknowledgement"))
            ),
            column(
              width = 6,
              column(
                width = 11,
                uiOutput(ns("ui.fish.park.image"))
              ),
              column(
                width = 1,
                actionButton(ns("alert.marinepark"),
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
              ns("fishparkmetric"),
              "Choose a group of metrics to plot:",
              choices = c("Whole assemblage", "Individual species", "Target species", "Feeding guilds", "Sharks and Rays"),
              multiple = FALSE,
              selectize = TRUE
            )#)
            ,
            conditionalPanel(
              paste0("input['", ns("fishparkmetric"), "'] == \"Whole assemblage\""),

              h3("Reef fish thermal index (RFTI):", actionButton(ns("park.cti"), label = " ",
                                                                 icon = icon("info"),
                                                                 icon.library = "font awesome",
                                                                 style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

              withSpinner(uiOutput(ns("fish.park.cti"))),

              h3("Total abundance:", actionButton(ns("park.ta"), label = " ",
                                                  icon = icon("info"),
                                                  icon.library = "font awesome",
                                                  style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

              withSpinner(uiOutput(ns("fish.park.total.plot"))),

              uiOutput(ns("fish.park.total.trend")),

              h4("Most abundant species:"),
              withSpinner(uiOutput(ns("fish.park.stack.plot"))),

              h4("Total abundance by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.total.sanctuary.plot"))),

              h4("Total abundance by zone:"),
              # withSpinner(uiOutput("ui.fish.park.total.zone.plot")),
              withSpinner(uiOutput(ns("fish.park.total.zone.plot"))),

              ## Only show site plots if stereo-DOVs
              withSpinner(uiOutput(ns("fish.park.total.site.plot"))),


              h3("Species richness:", actionButton(ns("park.sr"), label = " ",
                                                   icon = icon("info"),
                                                   icon.library = "font awesome",
                                                   style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
              withSpinner(uiOutput(ns("fish.park.rich.plot"), height = 350)),

              uiOutput(ns("fish.park.rich.trend")),

              h4("Species richness by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.rich.sanctuary.plot"))),

              h4("Species richness by zone:"),
              withSpinner(uiOutput(ns("fish.park.rich.zone.plot"))),
              # withSpinner(plotOutput("fish.park.rich.zone.plot", height = 250)),

              ## Only show site plots if stereo-DOVs
              withSpinner(uiOutput(ns("fish.park.rich.site.plot")))


              #h4("Spatial"),
              #withSpinner(leafletOutput(width = "100%", "fish.park.metric.leaflet", height = 300))
            ),
            conditionalPanel(
              paste0("input['",ns("fishparkmetric"),"'] == \"Target species\""),

              h4("Total abundance of highly retained demersal species:"),
              withSpinner(uiOutput(ns("fish.park.fished.sum.plot"))),

              h4("Total abundance of all targeted species:"),
              withSpinner(uiOutput(ns("fish.park.fished.all.sum.plot"))),

              h4("Total abundance of highly retained demersal species by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.fished.sum.sanctuary.plot"))),

              h4("Total abundance of all targeted species by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.fished.all.sum.sanctuary.plot"))),

              htmlOutput(ns("fish.park.fished.species.dropdown")),
              withSpinner(uiOutput(ns("fish.park.fished.species.abundance.plot"))),
              withSpinner(uiOutput(ns("fish.park.fished.species.abundance.sanctuary.plot"))),

              # withSpinner(plotOutput("fish.park.fished.species.kde.plot", height = 500)),
              # h4("KDE:"),
              # withSpinner(uiOutput("fish.park.fished.species.kde.plot")),
              #
              # # withSpinner(uiOutput("ui.fish.park.fished.species.kde.sanctuary.plot")),
              # htmlOutput("fish.park.fished.species.iframe")
            ),
            conditionalPanel(
              paste0("input['",ns("fishparkmetric"),"'] == \"Individual species\""),
              htmlOutput(ns("UIfish.park.all.species.dropdown")),

              # h4("Total abundance of chosen species:"),
              withSpinner(uiOutput(ns("fish.park.all.species.abundance.plot"))),

              # h4("Total abundance of chosen species by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.all.species.abundance.sanctuary.plot"))),

              fluidRow(
                box(
                  h4("Spatial"),
                  withSpinner(leafletOutput(width = "100%", ns("fish.park.all.species.leaflet"), height = 600))),
                box(
                  div(class = "wrap",
                      htmlOutput(ns("fish.park.all.species.iframe")))
                ))
            ),
            conditionalPanel(
              paste0("input['",ns("fishparkmetric"),"'] == \"Feeding guilds\""),

              # h4("Piscivores:"),
              uiOutput(ns("fish.park.trophic.pisc.plot")),

              # h4("Herbivores:"),
              withSpinner(uiOutput(ns("fish.park.trophic.herb.plot"))),

              # h4("Corallivores:"),
              withSpinner(uiOutput(ns("fish.park.trophic.coral.plot"))),

              # h4("Omnivores:"),
              withSpinner(uiOutput(ns("fish.park.trophic.omni.plot"))),

              # h4("Intertivores:"),
              withSpinner(uiOutput(ns("fish.park.trophic.invert.plot"))),

              # h4("Planktivores:"),
              withSpinner(uiOutput(ns("fish.park.trophic.plank.plot")))
            ),
            conditionalPanel(
              paste0("input['",ns("fishparkmetric"),"'] == \"Sharks and Rays\""),

              h3("Shark and Ray abundance:", actionButton(ns("park.ta"), label = " ",
                                                          icon = icon("info"),
                                                          icon.library = "font awesome",
                                                          style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

              withSpinner(uiOutput(ns("shark.park.total.plot"))),

              h4("Shark and Ray abundance by sanctuary:"),
              withSpinner(uiOutput(ns("shark.park.total.sanctuary.plot"))),

              h4("Shark and Ray abundance by zone:"),
              # withSpinner(uiOutput("ui.fish.park.total.zone.plot")),
              withSpinner(uiOutput(ns("shark.park.total.zone.plot")))

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
                paste0("input['",ns("tabset1"),"'] !== \"Marine park\""),
                withSpinner(leafletOutput(width = "100%", ns("fish.state.sampling.leaflet"), height = "78vh")),
                style="z-index:1002;"
              ),
              conditionalPanel(
                paste0("input['",ns("tabset1"),"'] == \"Marine park\""),
                withSpinner(leafletOutput(width = "100%", ns("fish.park.sampling.leaflet"), height = "78vh"))
              )

              # h4("Sampling locations"),
              # leafletOutput(width = "100%", "fish.state.sampling.leaflet", height = "78vh")
            ),

            style = "position:fixed; right: 0;"
          ))
        )
      )
    )
}

##### FISH PLOT TABLE #####
#Table of the pre-rendered PNG plots on the fish tab (see register_png_plots())).
#GOTCHA: `id` must also appear as ns("id") in mod_fish_ui, and `suffix` must match the file names written by generate_plots()
#`methods` limits a plot to those methods (NA=all)
#`suffix` completes the file name Fish_<park>_<method>_<suffix>.png

fish_png_spec<- tibble::tribble(
  ~id,                                     ~suffix,                          ~title,                      ~methods,
"fish.park.cti",                           "cti",                             NA,                          NA,
"fish.park.total.plot",                    "total_abundance",                 NA,                          NA,
"fish.park.total.site.plot",               "total_abundance_site",            "Total abundance by site:",  "stereo-DOVs,stereo-ROVs",
"fish.park.total.sanctuary.plot",          "total_abundance_sanctuary",       NA,                          NA,
"fish.park.total.zone.plot",               "total_abundance_zone",            NA,                          NA,
"fish.park.rich.plot",                     "species_richness",                NA,                          NA,
"fish.park.rich.site.plot",                "species_richness_site",           "Species richness by site:", "stereo-DOVs,stereo-ROVs",
"fish.park.rich.sanctuary.plot",           "species_richness_sanctuary",      NA,                          NA,
"fish.park.rich.zone.plot",                "species_richness_zone",           NA,                          NA,
"fish.park.stack.plot",                    "top10",                           NA,                          NA,
"fish.park.trophic.pisc.plot",             "trophic_Piscivore",               NA,                          NA,
"fish.park.trophic.herb.plot",             "trophic_Herbivore",               NA,                          NA,
"fish.park.trophic.coral.plot",            "trophic_Corallivore",             NA,                          NA,
"fish.park.trophic.omni.plot",             "trophic_Omnivore",                NA,                          NA,
"fish.park.trophic.invert.plot",           "trophic_Invertivore",             NA,                          NA,
"fish.park.trophic.plank.plot",            "trophic_Planktivore",             NA,                          NA,
"shark.park.total.plot",                   "shark_total_abundance",           NA,                          NA,
"shark.park.total.sanctuary.plot",         "shark_total_abundance_sanctuary", NA,                          NA,
"shark.park.total.zone.plot",              "shark_total_abundance_zone",      NA,                          NA,
"fish.park.fished.sum.plot",               "sum_targets",                     NA,                          NA,
"fish.park.fished.sum.sanctuary.plot",     "sum_targets_sanctuary",           NA,                          NA,
"fish.park.fished.all.sum.plot",           "sum_all_targets",                 NA,                          NA,
"fish.park.fished.all.sum.sanctuary.plot", "sum_all_targets_sanctuary",       NA,                          NA
)


##### FISH SERVER #####
#' fish Server function
#'
#' @param id Module id (namespace); must match the id given to mod_fish_ui().
#' @param mpa_data List of ddashboard data loaded in app_server
#' @noRd

#Server for the fish tab. `mpa_data` is the shared list from load_mpa_data(); treat it as read-only
#Inside moduleServer, input$x are automatically namespaced to this module
mod_fish_server <- function(id, mpa_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #short reactive aliases for the three main selections (NULL until the dropdowns render)
    park<-reactive(input$fish.park.dropdown)
    method<-reactive(input$fish.park.method.dropdown)
    species.all<-reactive(input$fish.park.all.species.dropdown)

    #One row per plot: add a row to fish_png_spec (and a matching ns("id") in mod_fish_ui) for a new plot
    register_png_plots(output, fish_png_spec, "Fish", park, method)

    #State-wide summary ----
    #Yellow summary box (CSS in style.css turns bg-yellow teal)
    value_box<- function(value, subtitle, width=NULL){
      valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(value, big.mark = ","), '</font></p>')),
               subtitle, icon = icon("fish"), color="yellow", width = width)
    }


    output$box.total.number.fish <- renderValueBox(value_box(mpa_data$total_number_fish,"Fish counted"))
    output$box.total.species.fish <- renderValueBox(value_box(mpa_data$total_species_fish,"Fish species identified"))


    output$statewide_plots <- renderUI({
      total_fish <- mpa_data$total_number_fish_park
      total_species <- mpa_data$total_species_fish_park
      mins_watched <- mpa_data$mins_watched

      # if (nrow(total_fish) > 0) {

      number_of_parks <- nrow(total_fish)

      marine_park_summaries <-
        lapply(1:number_of_parks, function(i) {

          total_ind <- total_fish %>% slice(i)  #%>% glimpse()
          total_spe <- total_species %>% slice(i)  #%>% glimpse()
          mins_watched <- mins_watched %>% slice(i)


          ind <- unique(total_ind$total)
          spe <- unique(total_spe$richness)
          mins <- unique(mins_watched$mins_watched)

          marine_park <- unique(total_ind$marine_park)

          park <- stringr::str_replace_all(tolower(marine_park), c("marine park" = "", "island marine reserve" = "", " " = ""))

          tagList(
            HTML(paste0('<center>')),
            h4(marine_park),
            HTML(paste0('</left>')),
            valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">',
                                 prettyNum(ind,  big.mark = ","),
                                 '</font></p>')),
                     "Fish counted",
                     icon = icon("fish"),
                     color = "yellow", width = 4),

            valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">',
                                 prettyNum(spe,  big.mark = ","),
                                 '</font></p>')),
                     "Fish species indentified",
                     icon = icon("fish"),
                     color = "yellow", width = 4),
            valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">',
                                 prettyNum(round(mins/60),  big.mark = ","),
                                 '</font></p>')),
                     "Total hours of video watched ",
                     icon = icon("fish"),
                     color = "yellow", width = 4),


            HTML(paste0('<center><img src = "www/images/fish_', park, '_banner.jpg" width = "97%"></center>')),
            # img(src = paste0("www/images/fish_", park, "_banner.jpg"), align = "center", width = "95%"),#, height = 200 , align = "left"
            br(),
            br(),
            br()
          )

        })

      do.call(tagList, marine_park_summaries)

    })

    # FISH ----

    #### MARINE PARK DROPDOWNS ----
    ####### ->  Create a marine park dropdown ----
    output$fish.park.dropdown <- renderUI({

      lats <- mpa_data$lats %>%
        dplyr::arrange(mean_lat) # removed desc for marine park workshop

      pickerInput(
        inputId = ns("fish.park.dropdown"),
        label = "Choose a marine park:",
        choices = c(unique(lats$marine_park)),
        multiple = FALSE,
        # selected = unique(lats$marine_park)[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    })

    # ####### ->  Create method dropdown ----
    observeEvent(input$fish.park.dropdown, {
      output$fish.park.method.dropdown <- renderUI({
        req(input$fish.park.dropdown)

        dat <- mpa_data$methods[marine_park %in% c(input$fish.park.dropdown)]

        selectizeInput(ns("fish.park.method.dropdown"), "Choose a method:", choices = c(unique(sort(dat$method))), selected = c(unique(sort(dat$method)))[1])


      }) #%>% bindCache(input$fish.park.dropdown)
    })

    ####### ->  Create a fished species dropdown ----
    output$fish.park.fished.species.dropdown <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$ordered_top_fished_species[marine_park %in% c(input$fish.park.dropdown) &
                                                   method %in% c(input$fish.park.method.dropdown)]
      choices <- dat %>%
        dplyr::arrange(desc(total)) %>%
        dplyr::filter(total > 0) %>%
        dplyr::distinct(scientific_name) %>%
        dplyr::pull("scientific_name")

      pickerInput(
        inputId = ns("fish.park.fished.species.dropdown"),
        width = "100%",
        label = "Choose target species to plot:",
        choices = choices,
        multiple = FALSE,
        selected = choices[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

    ####### ->  Create an all species dropdown ----
    output$UIfish.park.all.species.dropdown <- renderUI({

      dat <- mpa_data$ordered_top_species[marine_park %in% c(input$fish.park.dropdown) &
                                            method %in% c(input$fish.park.method.dropdown)]

      choices <- dat %>%
        dplyr::arrange(desc(total)) %>%
        dplyr::distinct(scientific_name) %>%
        dplyr::pull("scientific_name")

      shinyWidgets::pickerInput(
        inputId = ns("fish.park.all.species.dropdown"),
        label = "Choose species to plot:",
        width = "100%",
        choices = choices,
        multiple = FALSE,
        selected = choices[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

    ####### ->  Create a trophic group dropdown ----
    # output$fish.park.trophic.dropdown <- renderUI({
    #
    #   dat <- mpa_data$trophic.sum[marine_park %in% c(input$fish.park.dropdown) &
    #                                 method %in% c(input$fish.park.method.dropdown)]
    #
    #   choices <- dat %>%
    #     dplyr::distinct(trophic.group) %>%
    #     dplyr::pull("trophic.group")
    #
    #   pickerInput(
    #     inputId = "fish.park.trophic.dropdown",
    #     label = "Choose trophic groups to plot:",
    #     choices = sort(choices),
    #     multiple = TRUE,
    #     selected = sort(choices)[1:3],
    #     options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    #   )
    # }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

    #----------------------------------------------------------------------------#
    ####### DATA FILTERED BY DROPDOWNS ----

    ####### ->  Total abundance and Species Richness for leaflets ----
    fish_park_alldata <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$all.data[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)


    fish_park_ta <- reactive({
      req(fish_park_alldata())

      fish_park_alldata()[metric %in% c("Total abundance")]

    })

    # TODO finish splitting this
    fish_park_sr <- reactive({
      req(fish_park_alldata())

      fish_park_alldata()[metric %in% c("Species richness")]

    })

    # ####### ->  Overall SR + TA dataframes ----
    # fish_park_overall <- reactive({
    #   req(input$fish.park.dropdown, input$fish.park.method.dropdown)
    #
    #   dat <- mpa_data$ta_sr[marine_park %in% c(input$fish.park.dropdown)]
    #   dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    #
    #   dat
    # })
    #
    # ####### ->  Overall Species richness ----
    # fish_park_sr_overall <- reactive({
    #   req(fish_park_overall())
    #
    #   dat <- fish_park_overall()[metric %in% c("Species richness")]
    #
    #   dat
    # })
    #
    # ####### ->  Overall Total abundance ----
    # fish_park_ta_overall <- reactive({
    #   req(fish_park_overall())
    #
    #   dat <- fish_park_overall()[metric %in% c("Total abundance")]
    #
    #   dat
    # })

    ####### ->  Sanctuary SR + TA plots ----
    fish_park_sanctuary <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$ta_sr_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat
    })

    # ####### ->  Sanctuary Species richness ----
    # fish_park_sr_sanctuary <- reactive({
    #   req(fish_park_sanctuary())
    #
    #   dat <- fish_park_sanctuary()[metric %in% c("Species richness")]
    #
    #   dat
    # })

    ####### ->  Sanctuary Total abundance ----
    fish_park_ta_sanctuary <- reactive({
      req(fish_park_sanctuary())

      dat <- fish_park_sanctuary()[metric %in% c("Total abundance")]

      dat
    })

    ####### ->  Site SR + TA plots ----
    fish_park_site <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$ta_sr_site[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat
    })

    ####### ->  Site Species richness ----
    fish_park_sr_site <- reactive({
      req(fish_park_site())

      dat <- fish_park_site()[metric %in% c("Species richness")]

      dat
    })

    ####### ->  Site Total abundance ----
    fish_park_ta_site <- reactive({
      req(fish_park_site())

      dat <- fish_park_site()[metric %in% c("Total abundance")]

      dat
    })

    ####### ->  Zone SR + TA plots ----
    fish_park_zone <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$ta_sr_zone[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat
    })

    ####### ->  Zone Species richness ----
    fish_park_sr_zone <- reactive({
      req(fish_park_zone())

      dat <- fish_park_zone()[metric %in% c("Species richness")]

      dat
    })

    ####### ->  Zone Total abundance ----
    fish_park_ta_zone <- reactive({
      req(fish_park_zone())

      dat <- fish_park_zone()[metric %in% c("Total abundance")]

      dat
    })


    ####### ->  Sampling effort for leaflet ----
    fish_park_samplingeffort <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$sampling_effort[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Sample:", sample, "</b>", "<br/>",
          "<b>Status:</b>", status, "<br/>",
          "<b>Depth:</b>", depth_m, "m", "<br/>",
          "<b>Site:</b>", site, "<br/>",
          "<b>Location:</b>", location, "<br/>",
          "<b>Number of times sampled:</b>", number_of_times_sampled, "<br/>"
        ))
    })

    ####### ->  Abundance by species NOT summarised (for leaflets)----
    fish_park_abundance_species_leaflet <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

      dat <- mpa_data$abundance_leaflet[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]
      dat <- dat[scientific_name %in% c(input$fish.park.all.species.dropdown)]

      dat

    })

    ####### ->  Metadata for leaflet Abundance by species -----
    fish_park_metadata_leaflet <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

      dat <- mpa_data$metadata_leaflet[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })


    ####### ->  Abundance by species summarised ----
    fish_park_abundance_species <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

      dat <- mpa_data$abundance.sum[marine_park %in% c(input$fish.park.dropdown)] # changed from abundance to summarised
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]
      dat <- dat[scientific_name %in% c(input$fish.park.all.species.dropdown)]

      dat

    })

    ####### ->  Abundance by species by Sanctuary----
    fish_park_abundance_species_sanctuary <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

      dat <- mpa_data$abundance_sum_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]
      dat <- dat[scientific_name %in% c(input$fish.park.all.species.dropdown)]

      dat

    })

    ####### ->  Top ten species ----
    fish_park_top_ten <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$top_ten[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })

    ####### ->  Individual Fished species abundance ----
    fish_park_fishedabundance <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$fished_species_sum[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })

    ####### ->  Individual Fished species abundance by sanctuary ----
    fish_park_fishedabundance_sanctuary <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$fished_species_sum_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })

    ####### ->  All fished species summed together ----
    fish_park_fishedsum <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$fished_sum[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })

    ####### ->  All fished species summed together by Sanctuary----
    fish_park_fishedsum_sanctuary <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$fished_sum_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })

    ####### ->  Summarised trophic abundance ----
    # fish_park_trophicabundance <- reactive({
    #   req(input$fish.park.dropdown, input$fish.park.method.dropdown)
    #
    #   dat <- mpa_data$trophic.sum[marine_park %in% c(input$fish.park.dropdown)]
    #   dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    #
    #   dat
    #
    # })

    ####### ->  Complete length ----
    fish_park_fishedcompletelength <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$fished_complete_length[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })

    ####### ->  Trends ----
    fish_park_trends <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$interpretation_trends[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      dat

    })

    # STATE DROPDOWNS ----

    # ####### ->  Create method dropdown ----
    # output$fish.state.method.dropdown <- renderUI({
    #   # choices <- mpa_data$metadata %>%
    #   #   # dplyr::filter(marine_park %in% c(input$fish.state.park.dropdown)) %>%
    #   #   dplyr::distinct(method) %>%
    #   #   dplyr::pull("method")
    #   #
    #   # create_dropdown("fish.state.method.dropdown", choices, "Choose a method:", FALSE)
    #   #
    #   #
    #   dat <- mpa_data$methods
    #
    #   selectizeInput("fish.state.method.dropdown", "Choose a method:", choices = c(unique(sort(dat$method))), selected = c(unique(sort(dat$method)))[1])
    #
    #
    # })

    ####### ->  Sampling effort leaflet ----
    output$fish.state.sampling.leaflet <- renderLeaflet({
      # req(input$fish.state.method.dropdown)

      dat <- fish_samplingeffort()
      pal <- data.frame(marker_col = c("#F1652C", "yellow", "#2ECFE2","#2ECFE2", "#739AF8"), method = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs","stereo-ROVs+UVC", "UVC"))
      dat <- dplyr::left_join(dat, pal, by = "method")

      projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      #
      #     bruvs <- fish_samplingeffort() %>%
      #       dplyr::filter(method %in% "stereo-BRUVs")
      #
      #     bruvs <- sf::st_as_sf(x = bruvs,
      #                           coords = c("longitude_dd", "latitude_dd"),
      #                           crs = projcrs)
      #
      #     dovs <- fish_samplingeffort() %>%
      #       dplyr::filter(method %in% "stereo-DOVs")
      #
      #     dovs <- sf::st_as_sf(x = dovs,
      #                          coords = c("longitude_dd", "latitude_dd"),
      #                          crs = projcrs)
      #
      #     rovs <- fish_samplingeffort() %>%
      #       dplyr::filter(method %in% "stereo-ROVs")
      #
      #     rovs <- sf::st_as_sf(x = rovs,
      #                          coords = c("longitude_dd", "latitude_dd"),
      #                          crs = projcrs)
      #
      #     uvc <- fish_samplingeffort() %>%
      #       dplyr::filter(method %in% "UVC")
      #
      #     uvc <- sf::st_as_sf(x = uvc,
      #                         coords = c("longitude_dd", "latitude_dd"),
      #                         crs = projcrs)

      map <- leaflet_basemap(fish_samplingeffort()) %>%
        fitBounds(
          ~ min(longitude_dd),
          ~ min(latitude_dd),
          ~ max(longitude_dd),
          ~ max(latitude_dd)
        ) %>%

        addPolygons(
          data =  mpa_data$state_mp,
          color = ~ mpa_data$state_pal(zone),
          popup =  mpa_data$state_mp$COMMENTS,
          group = "Marine Parks",
          fillOpacity = 0.9,
          stroke = FALSE
        ) %>%

        addLegend(
          pal = mpa_data$state_pal,
          values = mpa_data$state_mp$zone,
          opacity = 1,
          title = "Zones",
          position = "bottomright",
          group = "Marine Parks"
        ) %>%

        addLabelOnlyMarkers(
          data = mpa_data$park_labels,
          ~longitude_dd, ~latitude_dd,
          label = ~marine_park,
          labelOptions = labelOptions(noHide = TRUE,
                                      textOnly = TRUE,
                                      textsize="15px",
                                      offset = c(3,-8),
                                      style=list(color ="white"))
        ) %>%

        addCircleMarkers(data = dat,
                         lng = ~longitude_dd,
                         lat = ~latitude_dd,
                         popup = ~content,
                         label = ~as.character(sample),
                         color = "black", weight = 0.5,
                         fillColor = ~marker_col,
                         opacity = 1, radius = 7,
                         group = ~method) %>%

        # # UVC
        # addCircleMarkers(data = uvc,
        #             popup = ~content,
        #             color = "black",
        #             fillColor = "#6383db",
        #             opacity = 1, radius = 7,
        #             group = "UVC") %>%
        #
        # # ROVs
        # addCircleMarkers(data = rovs,
        #             popup = ~content,
        #             color = "black",
        #             fillColor = "#0CA2B0",
        #             opacity = 1, radius = 7, stroke = FALSE,
        #             group = "stereo-ROVs") %>%
        #
        # # BRUVS
        # addCircleMarkers(data = bruvs,
        #             popup = ~content,
        #             color = "black",
        #             fillColor = "#FEC52E",
        #             opacity = 1, radius = 7, stroke = FALSE,
        #             group = "stereo-BRUVs") %>%
        #
        # # DOVS
        # addCircleMarkers(data = dovs,
        #             popup = ~content,
        #             color = "black",
        #             fillColor = "#d14210",
        #             opacity = 1, radius = 7, stroke = FALSE,
        #             group = "stereo-DOVs") %>%


        addLayersControl(
          overlayGroups = c(
            "Marine Parks",
            "stereo-BRUVs",
            "stereo-DOVs",
            "stereo-ROVs",
            "UVC"

          ),
          options = layersControlOptions(collapsed = FALSE)
        )%>%
        addLegend("bottomright",
                  colors = c("#F1652C", "#FFEC58", "#2ECFE2","#2ECFE2", "#739AF8"),
                  labels = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs", "stereo-ROVs+UVC", "UVC"),
                  title = "Sampling locations",
                  opacity = 1)
    })


    fish_samplingeffort <- reactive({
      req(mpa_data) # , input$fish.state.method.dropdown

      mpa_data$sampling_effort %>%
        # dplyr::filter(marine_park %in% c(input$fish.state.park.dropdown)) %>%
        # dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Method:", method, "</b>", "<br/>",
          "<b>Sample:", sample, "</b>", "<br/>",
          "<b>Status:</b>", status, "<br/>",
          "<b>Depth:</b>", depth_m, "m", "<br/>",
          "<b>Site:</b>", site, "<br/>",
          "<b>Location:</b>", location, "<br/>",
          "<b>Number of times sampled:</b>", number_of_times_sampled, "<br/>"
        ))
    })



    #----------------------------------------------------------------------------#
    #### STATE PLOTS ----




    #----------------------------------------------------------------------------#
    #### MARINE PARK PLOTS ----
    ####### ->  Sampling effort leaflet ----
    output$fish.park.sampling.leaflet <- renderLeaflet({

      dat <- fish_park_samplingeffort()

      # Create the basemap with marine parks and legend
      pal <- data.frame(marker_col = c("#F1652C", "yellow", "#2ECFE2","#2ECFE2", "#739AF8"), method = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs","stereo-ROVs+UVC", "UVC"))
      dat <- dplyr::left_join(dat, pal, by = "method")
      groups <- dat %>%
        dplyr::distinct(year)

      map <- leaflet_basemap(dat) %>%
        fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>%
        leaflet:: addPolygons(
          data =  mpa_data$state_mp,
          color = ~ mpa_data$state_pal(zone),
          popup =  mpa_data$state_mp$COMMENTS,
          group = "Marine Parks"
        ) %>%
        leaflet::addCircleMarkers(data = dat,
                                  lng = ~longitude_dd,
                                  lat = ~latitude_dd,
                                  popup = ~content,
                                  label = ~as.character(sample),
                                  color = "black", weight = 1,
                                  fillColor = ~marker_col,
                                  opacity = 1, radius = 8,
                                  group = ~year) %>% #"Sampling locations"
        leaflet::addLayersControl(data = dat,
                                  overlayGroups = ~year,
                                  options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend(
          pal = mpa_data$state_pal, values = mpa_data$state_mp$zone, opacity = 1,
          title = "Zones",
          position = "bottomleft", group = "Marine Parks"
        )

      map
    })

    ####### ->  Acknowledgements  ----

    observeEvent(input$fish.park.dropdown, {
      output$acknowledgement <- renderText({
        #req(input$fish.park.dropdown)
        HTML(ack <- if_else(mpa_data$acknowledgements$park == input$fish.park.dropdown, mpa_data$acknowledgements$acknowledgement, " "))
        #HTML(ack <- paste("hello", "Line two", sep = "<br/>"))
      })
    })

    # TODO move to the generate data function
    pal <- c("Sanctuary" = "#C0D435",
             "SP Scientific Reference" = "#CAC3D5",
             "Recreation" = "#F8EE75",
             "General Use" = "#BBE3EF",
             "SP Benthic Protection" = "#CAC3D5",
             "Outside Park" = "#B3B3B3",
             "Conservation Area" = "#C0B134",
             "Marine Management Area" = "#b7cfe1",
             "SP Seagrass Protection" = "#AB9ECC",
             "Marine Nature Reserve" = "#bfd054",
             "SP Habitat Protection" = "#CAC3D5",
             "Shore Based Activities" = "#231D1D",
             "SP Wildlife Conservation" = "#7C7CB8")

  ##############################################################################

#KDE plot code here was deleted 13/11/26 as not used (available in git history if reinstated later)

    ####### ->  Individual Fished species abundance ----
    # Only includes consistently sampled sites
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.fished.species.abundance.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.fished.species.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- stringr::str_replace_all(input$fish.park.fished.species.dropdown, c(" Targeted" = "", " Highly retained" = ""), "")

        tagList(h4(paste0("Total abundance of ", species, ":")),
                img(src = paste0("www/plots/species/", "Fish_", park, "_", method, "_", species, "_targeted.png"), align = "left", width = "100%"))


      })
    })

    ####### ->  Individual Fished species abundance by Sanctuary ----
    # Only includes consistently sampled sites
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.fished.species.abundance.sanctuary.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.fished.species.dropdown)

        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- stringr::str_replace_all(input$fish.park.fished.species.dropdown, c(" Targeted" = "", " Highly retained" = ""), "")

        tagList(h4(paste0("Total abundance of ", species, " by sanctuary:")),
                img(src = paste0("www/plots/species/", "Fish_", park, "_", method, "_", species, "_sanctuary.png"), align = "left", width = "100%"))
      })
    })

    ####### ->  Individual Species abundance ----
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.all.species.abundance.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- input$fish.park.all.species.dropdown

        tagList(h4(paste0("Total abundance of ", species, ":")),
                img(src = paste0("www/plots/species/", "Fish_", park, "_", method, "_", species, ".png"), align = "left", width = "100%"))
      })
    })

    ####### ->  Individual Species abundance by Sanctuary ----
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.all.species.abundance.sanctuary.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- input$fish.park.all.species.dropdown

        tagList(h4(paste0("Total abundance of ", species, " by sanctuary:")),
                img(src = paste0("www/plots/species/", "Fish_", park, "_", method, "_", species, "_sanctuary.png"), align = "left", width = "100%"))
      })
    })

    ####### ->  Leaflet - All species abundance ----
    output$fish.park.all.species.leaflet <- renderLeaflet({

      req(fish_park_abundance_species_leaflet())

      dat <- fish_park_abundance_species_leaflet()

      dat <- dplyr::full_join(dat, fish_park_metadata_leaflet()) %>%
        tidyr::replace_na(list(maxn = 0))

      # TODO I don't think this is the complete data?!?!?!?

      overzero.ta <- dplyr::filter(dat, maxn > 0) #%>% dplyr::glimpse()
      equalzero.ta <- dplyr::filter(dat, maxn == 0) #%>% dplyr::glimpse()

      max.ta <- base::max(overzero.ta$maxn)

      map <- leaflet_basemap(dat) %>%
        fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>%
        addGlPolygons(
          data =  mpa_data$state_mp,
          color = ~ mpa_data$state_pal(zone),
          popup =  mpa_data$state_mp$COMMENTS,
          group = "Marine Parks"
        ) %>%
        addLegend(
          pal = mpa_data$state_pal, values = mpa_data$state_mp$zone, opacity = 1,
          title = "Zones",
          position = "bottomleft", group = "Marine Parks"
        ) %>%
        addLayersControl(
          overlayGroups = c("Marine Parks",
                            "Abundance"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        add_legend_ta(
          colors = c("black", "blue", "blue"),
          labels = c(0, round(max.ta / 2), max.ta),
          sizes = c(5, 20, 40), group = "Abundance"
        )

      if (nrow(overzero.ta)) {
        map <- map %>%
          addCircleMarkers(
            data = overzero.ta, lat = ~latitude_dd, lng = ~longitude_dd,
            radius = ~ scales::rescale(maxn, c(5, 25)), #((((maxn / max(maxn))) * 20)),
            fillOpacity = 0.5, stroke = FALSE,
            label = ~ as.character(maxn), group = "Abundance", color = "blue"
          )
      }

      if (nrow(equalzero.ta)) {
        map <- map %>%
          addCircleMarkers(
            data = equalzero.ta, lat = ~latitude_dd, lng = ~longitude_dd,
            radius = 3, fillOpacity = 0.5, color = "black", stroke = FALSE,
            label = ~ as.character(maxn), group = "Abundance"
          )
      }

      map
    })


    # EXTRA CONTENT ----

    # Marine Park image ----
    # NOTE this depends on the image filename being identical to the data folders
    # in the source data (data/Ningaloo or data/Ningaloo Marine Park)
    observeEvent(input$fish.park.dropdown, {
      output$ui.fish.park.image <- renderUI({
        req(input$fish.park.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))

        img(src = paste0("www/images/fish_", park, ".jpg"), align = "right", width = "100%") # removed www from URL to get rid of golem
      })
    })

    # Species iFrames -----
    ####### ->  Marine Park All species ----
    output$fish.park.all.species.iframe <- renderUI({

      dat <- mpa_data$foa_codes[scientific_name %in% c(input$fish.park.all.species.dropdown)] %>%
        dplyr::distinct(url) %>%
        dplyr::pull("url")

      frame <- tags$iframe(src = paste0(dat), width = "100%", height = 600, class = "frame")
      frame
    })

    ####### ->  Marine Park Fished species ----
    output$fish.park.fished.species.iframe <- renderUI({

      dat <- mpa_data$foa_codes[scientific_name %in% c(input$fish.park.fished.species.dropdown)] %>%
        dplyr::distinct(url) %>%
        dplyr::pull("url")

      frame <- tags$iframe(src = paste0(dat), height = 800, width = "100%")
      frame

    })

    observeEvent(input$park.cti,
                 showModal(modalDialog(
                   title = "What is reef fish temperature index (RFTI)?",
                   include_page(file.path(popups_dir(),"fish.cti.html"))))
    )

    observeEvent(input$park.ta,
                 showModal(modalDialog(
                   title = "How do we measure total abundance?",
                   include_page(file.path(popups_dir(),"total.abundance.html"))))
    )

    observeEvent(input$park.shark.ta,
                 showModal(modalDialog(
                   title = "How do we measure total abundance?",
                   include_page(file.path(popups_dir(),"total.abundance.html"))))
    )

    observeEvent(input$park.sr,
                 # CHANGED THIS DEPLOYIN
                 # print(dir())
                 showModal(modalDialog(
                   title = "How do we measure species richness?",
                   include_page(file.path(popups_dir(),"species.richness.html"))))
    )

    # MARINE PARK ----
    # FOR FISH
    observeEvent(
      input$alert.marinepark,

      showModal(modalDialog(
        title = input$fish.park.dropdown,
        include_page(file.path(popups_dir(),paste0("fish_",
                           stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", " " = "")),
                           ".html")))
      )
      ))

    # Interpretation of trends ----
    ####### ->  Total abundance ----
    output$fish.park.total.trend <- renderUI({

      dat <- fish_park_trends()[metric %in% c("total_abundance")]

      if(nrow(dat > 0)){

        box(width = 12, #height = 100,
            title = "Expand to see trend in total abundance",
            collapsed = TRUE,
            collapsible = TRUE,
            as.character(unique(dat$interpretation)),
            unique(dat$modified.by),
            unique(dat$date.modified)
        )
      }
    })

    ####### ->  Species richness ----
    output$fish.park.rich.trend <- renderUI({

      dat <- fish_park_trends()[metric %in% c("species.richness")]

      if(nrow(dat > 0)){

        box(width = 12, #height = 100,
            title = "Expand to see trend in species richness",
            collapsed = TRUE,
            collapsible = TRUE,
            as.character(unique(dat$interpretation)),
            unique(dat$modified.by),
            unique(dat$date.modified)
        )
      }

    })



  })
}
