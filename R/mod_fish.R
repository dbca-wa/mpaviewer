#Fish page
#Comprised of 3 main components: 1. user interface, 2. plot table, 3. server function

#---- UI -------------------------------------------------------------------------------------------------------------
#' fish UI function
#' @param id Module id (namespace).
#' @noRd
#' @importFrom shiny NS tagList

#UI for the coral tab. GOTCHA: every id must be wrapped in ns("...")
#similarly for conditionalPanel() conditions are JS strings, so they use paste0("input['", ns("id"), "']== ...") not input.id
mod_fish_ui <- function(id) {
  ns <- NS(id)  #makes every id unique in this module (fish-<id>)
    tabItem(
      tabName = "fishtab",
      fluidRow(

        #left side: the box that holds the plots with two tabs (state-wide, one marine park)
        #Its id (tabset1) is read by the map panels at the bottom to decide which ID to show
        tabBox(title = "Choose a spatial scale to plot",
               width = 8,
               id = ns("tabset1"), height = "78vh",

        ## STATE-WIDE TAB ----
        #two summary boxes then a block per park (summary boxes and a banner) all built in the server
          tabPanel("State-wide summary",
                   style = "overflow: auto",
                   valueBoxOutput(ns("box.total.number.fish"), width = 6),
                   valueBoxOutput(ns("box.total.species.fish"), width = 6),
                   h4("Marine Parks Surveyed:"),
                   uiOutput(ns("statewide_plots")),),

          ## MARINE PARK TAB ----
          tabPanel("Marine park",
            style = "overflow: auto",

            #Row 1, left: park and method dropdowns, built in server, and data acknowledgement text
            column(width = 6,
              htmlOutput(ns("fish.park.dropdown.ui")),
              uiOutput(ns("fish.park.method.dropdown.ui")),
              htmlOutput(ns("acknowledgement"))),

            #Row 1, right: banner photo of the chosen park and its info button (opens a pop-up)
            column(width = 6,
              column(width = 11,
                uiOutput(ns("ui.fish.park.image"))),
              column(width = 1,
                actionButton(ns("alert.marinepark"),
                             label = " ",
                             icon = icon("info"),
                             icon.library = "font awesome",
                             style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px"))),

            #Picks which group of plots to show: each choice has its own conditionalPanel below
            selectInput(
              width = "100%",
              ns("fishparkmetric"),
              "Choose a group of metrics to plot:",
              choices = c("Whole assemblage", "Individual species", "Target species", "Feeding guilds", "Sharks and Rays"),
              multiple = FALSE,
              selectize = TRUE),

            #WHOLE ASSEMBLAGE: CTI, total abundance, and species richness plots
            #Most outputs are pre-rendered PNGs (ids match fish_png_spec); withSpinner shows a spinner while they load
            conditionalPanel(paste0("input['", ns("fishparkmetric"), "'] == \"Whole assemblage\""),

              ## CTI ##
              h3("Reef fish thermal index (RFTI):", actionButton(ns("park.cti"), label = " ",
                                                                 icon = icon("info"),
                                                                 icon.library = "font awesome",
                                                                 style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
              withSpinner(uiOutput(ns("fish.park.cti"))),

              ## Total abundance/most abundant species ##
              h3("Total abundance:", actionButton(ns("park.ta"), label = " ",
                                                  icon = icon("info"),
                                                  icon.library = "font awesome",
                                                  style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
              withSpinner(uiOutput(ns("fish.park.total.plot"))),

              #Collapsible text on the trend, built in the server (only appears when there is one)
              uiOutput(ns("fish.park.total.trend")),

              h4("Most abundant species:"),
              withSpinner(uiOutput(ns("fish.park.stack.plot"))),

              h4("Total abundance by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.total.sanctuary.plot"))),

              h4("Total abundance by zone:"),
              withSpinner(uiOutput(ns("fish.park.total.zone.plot"))),

              ## Site plot: Only shown for stereo-DOVs and stereo-ROVs
              withSpinner(uiOutput(ns("fish.park.total.site.plot"))),

              ## Species richness ##
              h3("Species richness:", actionButton(ns("park.sr"), label = " ",
                                                   icon = icon("info"),
                                                   icon.library = "font awesome",
                                                   style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
              withSpinner(uiOutput(ns("fish.park.rich.plot"), height = 350)),

              #Collapsible text on the trend, built in the server (only appears when there is one)
              uiOutput(ns("fish.park.rich.trend")),

              h4("Species richness by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.rich.sanctuary.plot"))),

              h4("Species richness by zone:"),
              withSpinner(uiOutput(ns("fish.park.rich.zone.plot"))),

              ## Site plot: Only shown for stereo-DOVs and stereo-ROVs
              withSpinner(uiOutput(ns("fish.park.rich.site.plot")))),

            # TARGET SPECIES: summed plots, then one species picked from a drop down
            conditionalPanel(paste0("input['",ns("fishparkmetric"),"'] == \"Target species\""),

              ## Summed abundance ##
              h4("Total abundance of highly retained demersal species:"),
              withSpinner(uiOutput(ns("fish.park.fished.sum.plot"))),

              h4("Total abundance of all targeted species:"),
              withSpinner(uiOutput(ns("fish.park.fished.all.sum.plot"))),

              h4("Total abundance of highly retained demersal species by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.fished.sum.sanctuary.plot"))),

              h4("Total abundance of all targeted species by sanctuary:"),
              withSpinner(uiOutput(ns("fish.park.fished.all.sum.sanctuary.plot"))),

              ##Individual species: species dropdown (built in the server) then that species two plots ##
              htmlOutput(ns("fish.park.fished.species.dropdown.ui")),
              withSpinner(uiOutput(ns("fish.park.fished.species.abundance.plot"))),
              withSpinner(uiOutput(ns("fish.park.fished.species.abundance.sanctuary.plot"))),),

            # INDIVIDUAL SPECIES: pick any species, see its plots, a map of where it was seen, and its web page
            conditionalPanel(paste0("input['",ns("fishparkmetric"),"'] == \"Individual species\""),
              htmlOutput(ns("UIfish.park.all.species.dropdown")),
              withSpinner(uiOutput(ns("fish.park.all.species.abundance.plot"))),
              withSpinner(uiOutput(ns("fish.park.all.species.abundance.sanctuary.plot"))),

              #map
              fluidRow(box(h4("Spatial"),
                  withSpinner(leafletOutput(width = "100%", ns("fish.park.all.species.leaflet"), height = 600))),
                box(div(class = "wrap",htmlOutput(ns("fish.park.all.species.iframe")))))),

            #FEEDING GUILDS: one plot per trophic group
            conditionalPanel(paste0("input['",ns("fishparkmetric"),"'] == \"Feeding guilds\""),

              uiOutput(ns("fish.park.trophic.pisc.plot")),#Piscivores
              withSpinner(uiOutput(ns("fish.park.trophic.herb.plot"))), #Herbivores
              withSpinner(uiOutput(ns("fish.park.trophic.coral.plot"))), #Corallivores
              withSpinner(uiOutput(ns("fish.park.trophic.omni.plot"))), #Omnivores
              withSpinner(uiOutput(ns("fish.park.trophic.invert.plot"))), #Invertivores
              withSpinner(uiOutput(ns("fish.park.trophic.plank.plot")))            ), #Planktivores

            #SHARKS AND RAYS: Abundance plots,
            conditionalPanel(paste0("input['",ns("fishparkmetric"),"'] == \"Sharks and Rays\""),
              h3("Shark and Ray abundance:", actionButton(ns("park.shark.ta"), label = " ",
                                                          icon = icon("info"),
                                                          icon.library = "font awesome",
                                                          style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),
              withSpinner(uiOutput(ns("shark.park.total.plot"))),

              h4("Shark and Ray abundance by sanctuary:"),
              withSpinner(uiOutput(ns("shark.park.total.sanctuary.plot"))),

              h4("Shark and Ray abundance by zone:"),
              withSpinner(uiOutput(ns("shark.park.total.zone.plot")))))
        ), #End of marine park plots

        ### Leaflet box ----
        #Right side: one map, pinned to the right edge of the screen. Which of the two maps shows depends on the plot tab selected (tabset1)
        fluidRow(div(column(width = 4,
            box(width = 12, title = "Sampling locations",
              conditionalPanel(paste0("input['",ns("tabset1"),"'] !== \"Marine park\""),
                withSpinner(leafletOutput(width = "100%", ns("fish.state.sampling.leaflet"), height = "78vh")),
                style="z-index:1002;"),

              conditionalPanel(paste0("input['",ns("tabset1"),"'] == \"Marine park\""),
                withSpinner(leafletOutput(width = "100%", ns("fish.park.sampling.leaflet"), height = "78vh")))),
            style = "position:fixed; right: 0;"))))
      ) #end tabitem
} #end UI function

#---- PLOT TABLE -------------------------------------------------------------------------------------------------------------
#Table of the pre-rendered PNG plots on the fish tab (see register_png_plots())).
# One row = one plot. To add one: add a row here and a matching ns("id") in mod_fish_ui().
#GOTCHA: `id` must also appear as ns("id") in mod_fish_ui, and `suffix` must match the file names written by generate_plots()
#`title` is a heading shown above the plot (NA=none)
#`methods` limits a plot to those methods (NA=all)
#`suffix` completes the file name Fish_<park>_<method>_<suffix>.png
# The species plots are NOT here: their file names contain the species so they have their own blocks below

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


#---- SERVER -------------------------------------------------------------------------------------------------------------
#' fish Server function
#'
#' @param id Module id (namespace); must match the id given to mod_fish_ui().
#' @param mpa_data List of ddashboard data loaded in app_server
#' @noRd

#Server for the fish tab. `mpa_data` is the shared list from load_mpa_data(); treat it as read-only
#Inside moduleServer, input$x are automatically namespaced to this module
mod_fish_server <- function(id, mpa_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns #needed for inputs built inside renderUI

    #short reactive aliases for the three main selections (NULL until the dropdowns render)
    park<-reactive(input$fish.park.dropdown)
    method<-reactive(input$fish.park.method.dropdown)

    #pre-rendered png plots: one call creates every plot listed in fish_png_spec (see table above)
    register_png_plots(output, fish_png_spec, "Fish", park, method)

    #State-wide summary ----
    #Helper for the yellow summary boxes (CSS in style.css turns bg-yellow teal)
    value_box<- function(value, subtitle, width=NULL){
      valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(value, big.mark = ","), '</font></p>')),
               subtitle, icon = icon("fish"), color="yellow", width = width)}

    #Total fish counted box
    output$box.total.number.fish <- renderValueBox(value_box(mpa_data$total_number_fish,"Fish counted"))

    #Total fish species identified box
    output$box.total.species.fish <- renderValueBox(value_box(mpa_data$total_species_fish,"Fish species identified"))

    #One block per marine park: its name, three summary boxes and a banner photo
    #three tables below hold one row per park in the same order
    output$statewide_plots <- renderUI({
      total_fish <- mpa_data$total_number_fish_park
      total_species <- mpa_data$total_species_fish_park
      mins_watched <- mpa_data$mins_watched

      number_of_parks <- nrow(total_fish)

      marine_park_summaries <-
        lapply(1:number_of_parks, function(i) {
          total_ind <- total_fish %>% slice(i)
          total_spe <- total_species %>% slice(i)
          mins_watched <- mins_watched %>% slice(i)

          ind <- unique(total_ind$total)
          spe <- unique(total_spe$richness)
          mins <- unique(mins_watched$mins_watched)

          marine_park <- unique(total_ind$marine_park)

          #filename version of the park (used for the banner image)
          park <- stringr::str_replace_all(tolower(marine_park), c("marine park" = "", "island marine reserve" = "", " " = ""))

          #The banner file is www/images/fish_<park>_banner.jpg; a park without one shows a broken image
          tagList(HTML(paste0('<center>')),
            h4(marine_park),
            HTML(paste0('</left>')),

            #Total fish counted box
            valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">',
                                 prettyNum(ind,  big.mark = ","),
                                 '</font></p>')),
                     "Fish counted",
                     icon = icon("fish"),
                     color = "yellow", width = 4),

            #Total fish species identified box
            valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">',
                                 prettyNum(spe,  big.mark = ","),
                                 '</font></p>')),
                     "Fish species indentified",
                     icon = icon("fish"),
                     color = "yellow", width = 4),

            #Total hours of video watched box
            valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">',
                                 prettyNum(round(mins/60),  big.mark = ","),
                                 '</font></p>')),
                     "Total hours of video watched ",
                     icon = icon("fish"),
                     color = "yellow", width = 4),

            #Park banner image
            HTML(paste0('<center><img src = "www/images/fish_', park, '_banner.jpg" width = "97%"></center>')),
            br(),
            br(),
            br())
         }) #end of marine_park_summaries
      do.call(tagList, marine_park_summaries)
    }) #end of state-wide plots

    # Dropdowns (marine park tab) ----

    #Park dropdown: ordered by latitude. pickerInput gives a searchable list
    output$fish.park.dropdown.ui <- renderUI({
        lats <- mpa_data$lats %>%
        dplyr::arrange(mean_lat)

      pickerInput(
        inputId = ns("fish.park.dropdown"),
        label = "Choose a marine park:",
        choices = c(unique(lats$marine_park)),
        multiple = FALSE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE))})

    #Method dropdown: only the methods used in the chosen park
    observeEvent(input$fish.park.dropdown, {
      output$fish.park.method.dropdown.ui <- renderUI({
        req(input$fish.park.dropdown)

        dat <- mpa_data$methods[marine_park %in% c(input$fish.park.dropdown)]
        selectizeInput(ns("fish.park.method.dropdown"), "Choose a method:", choices = c(unique(sort(dat$method))),
                       selected = c(unique(sort(dat$method)))[1])})})

    #Target-species dropdown: species with a total above 0 in this park and method, most abundant first
    output$fish.park.fished.species.dropdown.ui <- renderUI({
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
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE))})

    #All species dropdown: all species that have a plot file for this park and method
    #generate_plots() only draws a species plot when its total is > 10 so most of the species in the data have no plot
    species_with_plot<- reactive({
      req(park(), method())
      files<-list.files(file.path(plots_dir(), "species"), pattern = "\\.png$")
      prefix<-paste0("Fish_", park_slug(park()), "_", method(), "_")
      f<-files[startsWith(files, prefix)]
      f<-f[!grepl("_(sanctuary|targeted|retained|KDE)\\.png$",f)] #base plot only not the variants
      sub("\\.png$", "", substring(f, nchar(prefix)+1))})

    output$UIfish.park.all.species.dropdown <- renderUI({
      dat <- mpa_data$ordered_top_species[marine_park %in% c(input$fish.park.dropdown) &
                                            method %in% c(input$fish.park.method.dropdown)]

      choices <- dat %>%
        dplyr::arrange(desc(total)) %>%
        dplyr::distinct(scientific_name) %>%
        dplyr::pull("scientific_name")

      choices<-choices[choices %in% species_with_plot()] #drop species with no plot

      shinyWidgets::pickerInput(
        inputId = ns("fish.park.all.species.dropdown"),
        label = "Choose species to plot:",
        width = "100%",
        choices = choices,
        multiple = FALSE,
        selected = choices[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE))})


    # Data for the maps and trend boxes ----
    #Sampling sites for the selected path and method, wth the popup HTML pre-built
    # Rows without useable lat/lon are dropped to avoid NAs causing 'bounds are not valid' issue.

    #Sampling effort
    fish_park_samplingeffort <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$sampling_effort[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]

      req(nrow(dat)>0) #park/method can briefly disagree after a park change, this skips it quietly until the method dropdown catches up

      dat %>%
        dplyr::filter(is.finite(latitude_dd), is.finite(longitude_dd)) %>% #protect map from broken lat/longs
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Sample:", sample, "</b>", "<br/>",
          "<b>Status:</b>", status, "<br/>",
          "<b>Depth:</b>", depth_m, "m", "<br/>",
          "<b>Site:</b>", site, "<br/>",
          "<b>Location:</b>", location, "<br/>",
          "<b>Number of times sampled:</b>", number_of_times_sampled, "<br/>"))})

    #Where the chosen species was counted (one row per sample with a count)
    fish_park_abundance_species_leaflet <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

      dat <- mpa_data$abundance_leaflet[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]
      dat <- dat[scientific_name %in% c(input$fish.park.all.species.dropdown)]
      dat })

    #Every sample in this park and method (so samples where species was not seen can be shown as zero)
      fish_park_metadata_leaflet <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

      dat <- mpa_data$metadata_leaflet[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]
      dat })


    #Trends: written interpretation of the trends for this park and method (feeds the two collapsible boxes below)
    fish_park_trends <- reactive({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      dat <- mpa_data$interpretation_trends[marine_park %in% c(input$fish.park.dropdown)]
      dat <- dat[method %in% c(input$fish.park.method.dropdown)]
      dat})

    #All sampling effort in the state, for every method, with a popup HTML (used by the state-wide map)
    fish_samplingeffort <- reactive({
      req(mpa_data)

      dat <- mpa_data$sampling_effort%>%
        dplyr::filter(is.finite(latitude_dd), is.finite(longitude_dd)) %>% #protect map from broken lat/longs
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Method:", method, "</b>", "<br/>",
          "<b>Sample:", sample, "</b>", "<br/>",
          "<b>Status:</b>", status, "<br/>",
          "<b>Depth:</b>", depth_m, "m", "<br/>",
          "<b>Site:</b>", site, "<br/>",
          "<b>Location:</b>", location, "<br/>",
          "<b>Number of times sampled:</b>", number_of_times_sampled, "<br/>"))})

    # Maps ----
    #State-wide map: every sampling location coloured by method, over all marine park zones, with park names
    #All zones are kept (this map shows the whole state), whereas the park maps below only draw their own zones
    output$fish.state.sampling.leaflet <- renderLeaflet({
      dat <- fish_samplingeffort()
      #specify marker colour per method. GOTCHA: a method missing from this table gets no colour so add new methods here
      pal <- data.frame(marker_col = c("#F1652C", "yellow", "#2ECFE2","#2ECFE2", "#739AF8"), method = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs","stereo-ROVs+UVC", "UVC"))
      dat <- dplyr::left_join(dat, pal, by = "method")
      projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

      map <- leaflet_basemap(fish_samplingeffort()) %>%
        fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>% #zoom to fit all sites
        addPolygons(data =  mpa_data$state_mp, #zone polygons coloured by zone type
          color = ~ mpa_data$state_pal(zone),
          popup =  mpa_data$state_mp$COMMENTS,
          group = "Marine Parks",
          fillOpacity = 0.9,
          stroke = FALSE) %>%
        addLegend(pal = mpa_data$state_pal,
          values = mpa_data$state_mp$zone,
          opacity = 1,
          title = "Zones",
          position = "bottomright",
          group = "Marine Parks") %>%
        addLabelOnlyMarkers(#Park names written on the map. Parks with no coordinates in park_labels are skipped
          data = dplyr::filter(mpa_data$park_labels, is.finite(latitude_dd), is.finite(longitude_dd)), #skip rows without coordinates (Dampier & walpole)
          ~longitude_dd, ~latitude_dd,
          label = ~marine_park,
          labelOptions = labelOptions(noHide = TRUE,
                                      textOnly = TRUE,
                                      textsize="15px",
                                      offset = c(3,-8),
                                      style=list(color ="white"))) %>%
        addCircleMarkers(data = dat, #one circle per sample, grouped by method so the layer swtich can turn each method off
                         lng = ~longitude_dd,
                         lat = ~latitude_dd,
                         popup = ~content,
                         label = ~as.character(sample),
                         color = "black", weight = 0.5,
                         fillColor = ~marker_col,
                         opacity = 1, radius = 7,
                         group = ~method) %>%
        addLayersControl(overlayGroups = c(
            "Marine Parks",
            "stereo-BRUVs",
            "stereo-DOVs",
            "stereo-ROVs",
            "UVC"),
          options = layersControlOptions(collapsed = FALSE))%>%
        addLegend("bottomright",
                  colors = c("#F1652C", "#FFEC58", "#2ECFE2","#2ECFE2", "#739AF8"),
                  labels = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs", "stereo-ROVs+UVC", "UVC"),
                  title = "Sampling locations",
                  opacity = 1)})

    #Marine park map: the selected park's sites coloured by method, one layer switch per year, over just that parks zones
    output$fish.park.sampling.leaflet <- renderLeaflet({
      dat <- fish_park_samplingeffort()

      # marker colour per method (same table as the state-wide map)
      pal <- data.frame(marker_col = c("#F1652C", "yellow", "#2ECFE2","#2ECFE2", "#739AF8"), method = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs","stereo-ROVs+UVC", "UVC"))
      dat <- dplyr::left_join(dat, pal, by = "method")

      zones<-zones_for_park(mpa_data$state_mp, park(), dat) #only this parks zones to speed up rendering

      map <- leaflet_basemap(dat) %>%
        fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>%
        leaflet:: addPolygons(
          data =  zones,
          color = ~ mpa_data$state_pal(zone),
          popup =  zones$COMMENTS,
          group = "Marine Parks") %>%
        leaflet::addCircleMarkers(data = dat, #circles grouped by year: layer control lets you show one year at a time
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
                                  options = layersControlOptions(collapsed = FALSE)) %>%
        leaflet::addLegend(pal = mpa_data$state_pal, values = zones$zone, opacity = 1,
          title = "Zones",
          position = "bottomleft", group = "Marine Parks")
      map })

    #Individual species map: circles sized by the highest count (maxn) of the chosen species per sample.
    #Samples where it was not seen are small black dots
    #Note that currently it is possible to select a species that wasn't seen at all in a given park + method (i.e. map full of small black dots).
    #We can change this so those species are hidden from the drop down if we want
    output$fish.park.all.species.leaflet <- renderLeaflet({
      req(fish_park_abundance_species_leaflet())
      dat <- fish_park_abundance_species_leaflet()

      dat <- dplyr::full_join(dat, fish_park_metadata_leaflet()) %>%
        dplyr::filter(is.finite(latitude_dd), is.finite(longitude_dd)) %>% #protect map from broken lat/longs
        tidyr::replace_na(list(maxn = 0))

      # Species seen nowhere in this park in this park + method are not in abundance_leaflet (zeros are removed in generate_data(),
      #so they come out of the join below as all-zero samples which is correct)
      overzero.ta <- dplyr::filter(dat, maxn > 0)
      equalzero.ta <- dplyr::filter(dat, maxn == 0)

      #Largest count, for the size of the legend.Guarded so all zero or empty species can't push -Inf to legend labels
      max.ta <- if (nrow(overzero.ta)) base::max(overzero.ta$maxn) else 0

      zones<- zones_for_park(mpa_data$state_mp, park(), dat) #only this parks zones

      map <- leaflet_basemap(dat) %>%
        fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>% #zoom to fit this parks sites
        addGlPolygons(data =  zones, #draw zone polygons
          color = ~ mpa_data$state_pal(zone),
          popup =  zones$COMMENTS,
          group = "Marine Parks") %>%
        addLegend(pal = mpa_data$state_pal, values = zones$zone, opacity = 1,
          title = "Zones",
          position = "bottomleft", group = "Marine Parks") %>%
        addLayersControl(overlayGroups = c("Marine Parks",
                            "Abundance"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        add_legend_ta(
          colors = c("black", "blue", "blue"),
          labels = c(0, round(max.ta / 2), max.ta),
          sizes = c(5, 20, 40), group = "Abundance")

      # Blue circles where the species was seen, sized by count
      if (nrow(overzero.ta)) {
        map <- map %>%
          addCircleMarkers(data = overzero.ta, lat = ~latitude_dd, lng = ~longitude_dd,
            radius = ~ scales::rescale(maxn, c(5, 25)),
            fillOpacity = 0.5, stroke = FALSE,
            label = ~ as.character(maxn), group = "Abundance", color = "blue")}

      #Small black ddts where species was not seen
      if (nrow(equalzero.ta)) {
        map <- map %>%
          addCircleMarkers(data = equalzero.ta, lat = ~latitude_dd, lng = ~longitude_dd,
            radius = 3, fillOpacity = 0.5, color = "black", stroke = FALSE,
            label = ~ as.character(maxn), group = "Abundance")}

      map })

    # Extra text and images (marine park tab)  ----
    # Data acknowledgement for the chosen park: the text for the matching row or a blank for the others
    observeEvent(input$fish.park.dropdown, {
      output$acknowledgement <- renderText({
        HTML(ack <- if_else(mpa_data$acknowledgements$park == input$fish.park.dropdown, mpa_data$acknowledgements$acknowledgement, " "))})})


    # Banner photo for the chosen park. GOTCHA file is www/images/fish_<park>.jpg where park = park name in lower case without "marine park"/"island marine reserve" or spaces
    observeEvent(input$fish.park.dropdown, {
      output$ui.fish.park.image <- renderUI({
        req(input$fish.park.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        img(src = paste0("www/images/fish_", park, ".jpg"), align = "right", width = "100%")})})


    # Species plots  ----
    #these are not in fish_png_spec because the filename contains the species

    #Target species (one chosen from the dropdown). GOTCHA: "Targeted"/"Highly retained" are stripped
    # Only includes consistently sampled sites
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.fished.species.abundance.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.fished.species.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- stringr::str_replace_all(input$fish.park.fished.species.dropdown, c(" Targeted" = "", " Highly retained" = ""), "")
        plot_img(paste0("species/Fish_", park, "_", method, "_", species, "_targeted.png"), paste0("Total abundance of ", species, ":"))})})

    # Target species, by sanctuary
    # Only includes consistently sampled sites
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.fished.species.abundance.sanctuary.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.fished.species.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- stringr::str_replace_all(input$fish.park.fished.species.dropdown, c(" Targeted" = "", " Highly retained" = ""), "")
        plot_img(paste0("species/Fish_", park, "_", method, "_", species, "_sanctuary.png"), paste0("Total abundance of ", species, " by sanctuary:"))})})

    #Any species chosen from the all-species dropdown
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.all.species.abundance.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- input$fish.park.all.species.dropdown
        plot_img(paste0("species/Fish_", park, "_", method, "_", species, ".png"), paste0("Total abundance of ", species, ":"))})})

    #The same species, by sanctuary
    observeEvent(input$fish.park.method.dropdown, {
      output$fish.park.all.species.abundance.sanctuary.plot <- renderUI({
        req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown
        species <- input$fish.park.all.species.dropdown
        plot_img(paste0("species/Fish_", park, "_", method, "_", species, "_sanctuary.png"), paste0("Total abundance of ", species, " by sanctuary:"))})})

    # Species webpage (iFrames) -----
    #Embeds the species' page from the Fishes of Australia site, using the url in foa_codes
    output$fish.park.all.species.iframe <- renderUI({
      dat <- mpa_data$foa_codes[scientific_name %in% c(input$fish.park.all.species.dropdown)] %>%
        dplyr::distinct(url) %>%
        dplyr::pull("url")
      frame <- tags$iframe(src = paste0(dat), width = "100%", height = 600, class = "frame")
      frame})

    # Info pop-ups -----
    #Each button opens a modal with an HTML file from inst/app/www/popups
    #CTI
    observeEvent(input$park.cti,
                 showModal(modalDialog(
                   title = "What is reef fish temperature index (RFTI)?",
                   include_page(file.path(popups_dir(),"fish.cti.html")))))

    #Total abundance
    observeEvent(input$park.ta,
                 showModal(modalDialog(
                   title = "How do we measure total abundance?",
                   include_page(file.path(popups_dir(),"total.abundance.html")))))
    #Total abundance (sharks) - same popup as above but it needs its own button id
    observeEvent(input$park.shark.ta,
                 showModal(modalDialog(
                   title = "How do we measure total abundance?",
                   include_page(file.path(popups_dir(),"total.abundance.html")))))

    #Species richness
    observeEvent(input$park.sr,
                 showModal(modalDialog(
                   title = "How do we measure species richness?",
                   include_page(file.path(popups_dir(),"species.richness.html")))))

    # Marine park info button
    #GOTCHA: this file is fish_<park>.html, only "marine park" and spaces are removed from the name (not "island marine reserve") so don't swap this code for park_slug as they do different things
    observeEvent(input$alert.marinepark,
      showModal(modalDialog(
        title = input$fish.park.dropdown,
        include_page(file.path(popups_dir(),paste0("fish_",
                           stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", " " = "")),
                           ".html"))))))

    # Trend boxes ----
    # Collapsible box with the written interpretation of the trend in total abundance
    # Shows nothing when there is no trend text for this park and method
        output$fish.park.total.trend <- renderUI({
         dat <- fish_park_trends()[metric %in% c("total_abundance")]

        if(nrow(dat) > 0){
        box(width = 12,
            title = "Expand to see trend in total abundance",
            collapsed = TRUE,
            collapsible = TRUE,
            as.character(unique(dat$interpretation)),
            unique(dat$modified.by),
            unique(dat$date.modified))}})

    #Same for species richness
    output$fish.park.rich.trend <- renderUI({
      dat <- fish_park_trends()[metric %in% c("species.richness")]

      if(nrow(dat) > 0){
        box(width = 12,
            title = "Expand to see trend in species richness",
            collapsed = TRUE,
            collapsible = TRUE,
            as.character(unique(dat$interpretation)),
            unique(dat$modified.by),
            unique(dat$date.modified))}})
  }) #end of module server
} #end of fish server
