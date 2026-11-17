#Coral page
#Comprised of 3 main components: 1. user interface, 2. plot table, 3. server function
#---- UI -------------------------------------------------------------------------------------------------------------
# UI for the coral page (modularised app)
#' coral UI function
#' @param id Module id (namespace).
#' @noRd
#' @importFrom shiny NS tagList

#UI for the coral tab. GOTCHA: every id must be wrapped in ns("...")
#similarly for conditionalPanel() conditions are JS strings, so they use paste0("input['", ns("id"), "']== ...") not input.id
mod_coral_ui <- function(id) {
  ns <- NS(id) #makes every id unique in this module (coral-<id>)

  tabItem(
    tabName = "coraltab",
    fluidRow(

      #left side: the box that holds the plots with two tabs (state-wide, one marine park)
      #Its id (tabset 2) is read by the map panels at the bottom to decide which ID to show
      tabBox(title = "Choose a spatial scale to plot",
             width = 8,
             id = ns("tabset2"), height = "78vh",

        ## STATE-WIDE TAB ----
        #two summary boxes (filled by the server) and one static plot made by generate_plots()
        tabPanel("State-wide summary",
                 style = "overflow: auto",
                 valueBoxOutput(ns("box.total.number.coral"), width = 6),
                 valueBoxOutput(ns("box.total.species.coral"), width = 6),
                 h4("Marine Parks Surveyed:"),
                 column(width = 11, img(src = "www/plots/Coral_All_Parks_coral_cover.png", align="centre", width = "100%"))), #fixed file with no park in its name so not called in coral_png_spec

        ## MARINE PARK TAB----
        tabPanel("Marine park",
          style = "overflow: auto",

          #Row 1, left: park dropdown (built in server from the data and the metric picker)
          column(width = 6,
              htmlOutput(ns("benthic.park.coralcover.dropdown.ui")),
              selectInput(
              width = "100%",
              ns("benthicparkmethoddropdown"),
              "Choose a metric to plot:",
              choices = c("Coral cover", "Coral recruitment"), #coral recruitment is currently an option but no panel is built for it yet
              multiple = FALSE,
              selectize = TRUE),),

          #Row 1, right: banner photo of the chosen park and its info button (opens a popup)
          column(width = 6,
              column(width = 11,
              uiOutput(ns("ui.benthic.park.image"))),

            column(width = 1,
              actionButton(ns("alert.coral.marinepark"),
                           label = " ",
                           icon = icon("info"),
                           icon.library = "font awesome",
                           style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px"))),

          #Plots, shown only when the metric picker says 'coral cover'
          conditionalPanel(
            paste0("input['", ns("benthicparkmethoddropdown"), "'] == \"Coral cover\""),

            h3("Coral cover:", actionButton(ns("park.coralcover"), label = " ",
                                            icon = icon("info"),
                                            icon.library = "font awesome",
                                            style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

            #pre-rendered PNGs (ids match coral_png_spec); withSpinner shows a spinner while they load
            withSpinner(uiOutput(ns("benthic.park.coralcover.plot"))),

            h4("Most abundant families:"),
            withSpinner(uiOutput(ns("benthic.park.top5.coralcover.plot"))),

            h4("Coral cover by reef zone:"),
            withSpinner(uiOutput(ns("benthic.park.reefzone.coralcover.plot"))),

            #One plot per site: the site dropdown is built in the server, the plot file name contains the site
            h4("Most abundant families by site:"),

            htmlOutput(ns("benthic.park.site.coralcover.dropdown.ui")),
            withSpinner(uiOutput(ns("benthic.site.coralcover.plot"))),

            h4("Coral cover by site:"),
            withSpinner(uiOutput(ns("benthic.site.coralcover.plot.facet")))
          )
        )
      ),

      # Right side: one map, pinned to the right edge of the screen - which of the two maps show depends on the plot tab selected (tabset2)
      fluidRow(
        div(column(width = 4,
           box(width = 12, title = "Sampling locations",

              conditionalPanel(paste0("input['", ns("tabset2"), "'] == \"State-wide summary\""),
              withSpinner(leafletOutput(width = "100%", ns("benthic.state.sampling.leaflet"), height = "78vh")),
              style = "z-index:1002;"),

              conditionalPanel(paste0("input['", ns("tabset2"), "'] == \"Marine park\""),
              withSpinner(leafletOutput(width = "100%", ns("benthic.park.sampling.leaflet"), height = "78vh")))), # end of box
              style = "position:fixed; right: 0;")))
    )
  )
}


#---- PLOT TABLE -------------------------------------------------------------------------------------------------------------
#' @param id Module id (namespace); must match the id given to mod_coral_ui().
#' @param mpa_data List of dashboard data loaded in app_server().
#' @noRd

# Pre-rendered PNG plots on the coral tab: file is Coral_<park>_<suffix>.png (no method in the name).
# One row = one plot. To add one: add a row here and a matching ns("id") in mod_coral_ui().
# GOTCHA: `id` must match an ns("id") in mod_coral_ui(); title/methods are unused here (NA).
coral_png_spec <- tibble::tribble(
  ~id,                                    ~suffix,                ~title, ~methods,
  "benthic.park.coralcover.plot",         "coral_cover",           NA,     NA,
  "benthic.park.top5.coralcover.plot",    "species_coral_cover",   NA,     NA,
  "benthic.park.reefzone.coralcover.plot", "reefzone_coral_cover", NA,     NA,
  "benthic.site.coralcover.plot.facet",   "site_coral_cover",      NA,     NA
)


#---- SERVER -------------------------------------------------------------------------------------------------------------
#Server for the coral tab. `mpa_data` is the shared list from load_mpa_data(); treat it as read-only
#Inside moduleServer, input$x are automatically namespaced to this module
mod_coral_server <- function(id, mpa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns #needed for inputs built inside renderUI()

    # Short aliases for the selected park and site (NULL until the dropdowns exist to req() before use)
    park <- reactive(input$benthic.park.coralcover.dropdown)
    site <- reactive(input$benthic.park.site.coralcover.dropdown)

    # State-wide summary boxes ----
    #yellow boxes (CSS in style.css turns bg-yellow teal)
    #Total images analysed box
    output$box.total.number.coral <- renderValueBox({
      valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_number_coral, big.mark = ","), '</font></p>')),
               "Images Analysed",
               icon = tags$i(class = "fa-solid fa-camera", style = "font-size: 50px"),
               color = "yellow")})

    #Total genera identified box
    output$box.total.species.coral <- renderValueBox({
      valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_species_coral, big.mark = ","), '</font></p>')),
               "Total Genera Identified",
               icon = tags$i(img(src = "www/coral.png", width = "50px")),
               color = "yellow")})

    # Dropdowns ----
    #Park dropdown: parks that have coral data. create_dropdown() keeps the current choice if choices change
    output$benthic.park.coralcover.dropdown.ui <- renderUI({
      create_dropdown(input, ns, "benthic.park.coralcover.dropdown",
                      unique(mpa_data$coral_cover_metadata$marine_park), "Choose a marine park:", FALSE)})

    #Site dropdown: Only sites with more than 2 years of data in the chosen park are offered
    output$benthic.park.site.coralcover.dropdown.ui <- renderUI({
      sites <- mpa_data$coral_cover_metadata %>%
        dplyr::group_by(marine_park, site) %>%
        dplyr::summarise(n = length(plot_year), .groups = "drop") %>%
        dplyr::filter(n > 2, marine_park %in% park()) %>%
        dplyr::distinct(site) %>%
        dplyr::pull("site")
    create_dropdown(input, ns, "benthic.park.site.coralcover.dropdown", sites, "Choose a site:", FALSE)})

    #Banner photo for the chosen park (file is in www/images/coral_<park_slug>.jpg)
    output$ui.benthic.park.image <- renderUI({
      req(park())
      img(src = paste0("www/images/coral_", park_slug(park()), ".jpg"), align = "right", width = "100%")})

    # Pre-rendered PNG plots ----
    #One call creates all the plots listed in coral_png_spec (see table above)
    register_png_plots(output, coral_png_spec, "Coral", park)

    # Per-site plot: separate because its file name contains the site, not just the park
    output$benthic.site.coralcover.plot <- renderUI({
      req(park(), site())
      plot_img(paste0("Coral_", park_slug(park()), "_site_", site(), "_species_coral_cover.png"))})

    # Maps ----
    # Sampling sites for the selected park, with the popup html pre-built
    # Rows without useable lat/lon are dropped to avoid NAs causing 'bounds are not valid' issue.
    park_sampling_effort <- reactive({
      req(park())
      dplyr::mutate(
        mpa_data$coral_sampling_effort[mpa_data$coral_sampling_effort$marine_park %in% park(), ],
        content = paste(
          sep = " ",
          "<b>Sector:</b>", sector, "<br/>",
          "<b>Site:", site, "</b>", "<br/>",
          "<b>Status:</b>", status, "<br/>",
          "<b>Depth:</b>", depth_m, "m", "<br/>",
          "<b>Reef Zone:</b>", reef_zone, "<br/>"))%>%
        dplyr::filter(is.finite(latitude_dd), is.finite(longitude_dd))})#protect map from broken lat/longs)

    #State-wide map: every coral site, coloured by park, over all park zones
    output$benthic.state.sampling.leaflet <- renderLeaflet({
      dat <-  mpa_data$coral_sampling_effort %>%
        dplyr::distinct(marine_park, site, latitude_dd, longitude_dd) %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Marine Park:</b>", marine_park, "<br/>",
          "<b>Site:", site, "</b>", "<br/>")) %>%
        dplyr::filter(is.finite(latitude_dd), is.finite(longitude_dd))#protect map from broken lat/longs

      #Marker colour per park (GOTCHA: a park missing from this table never gets a colour so add new parks here)
      pal <- data.frame(marker_col = c("#F1652C", "yellow", "#2ECFE2","#739AF8"), marine_park = c("Shark Bay Marine Park", "Ningaloo Marine Park", "Montebello Islands Marine Park","Rowley Shoals Marine Park"))
      dat <- dplyr::left_join(dat, pal, by="marine_park")

      map <- leaflet_basemap(dat) %>%
        fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>% #zoom to fit all sites
        leaflet::addCircleMarkers(data = dat,
                                  lng = ~longitude_dd,
                                  lat = ~latitude_dd,
                                  popup = ~content,
                                  label = ~as.character(site),
                                  color = "black", weight = 2,
                                  fillColor = ~marker_col,
                                  opacity = 1, radius = 8) %>%
          addGlPolygons( #Zone polygons (all of them as map covers whole state)
            data =  mpa_data$state_mp,
            color = ~ mpa_data$state_pal(zone),
            popup =  mpa_data$state_mp$COMMENTS,
            group = "Marine Parks") %>%
        addLegend(
          pal = mpa_data$state_pal,
          values = mpa_data$state_mp$zone,
          opacity = 1,
          title = "Zones",
          position = "bottomright",
          group = "Marine Parks") %>%
        addLayersControl(
          overlayGroups = c(
            "Sampling locations", #markers have no group so right now the sampling locations checkbox does nothing on this map??
            "Marine Parks"),
          options = layersControlOptions(collapsed = FALSE))
      map
    })

    #Marine park map: the selected park's sites over just that park's zones
    output$benthic.park.sampling.leaflet <- renderLeaflet({

      zones<-zones_for_park(mpa_data$state_mp, park(), park_sampling_effort()) #filter down to this parks zones to keep size of JSON call down

      map <- leaflet_basemap(park_sampling_effort()) %>%
        fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>% #zoom to fit all sites
        leaflet::addCircleMarkers(lng = ~longitude_dd,
                                  lat = ~latitude_dd,
                                  color = "black",
                                  weight = 2,
                                  fillColor = "pink",
                                  radius = 6,
                                  opacity = 1,
                                  popup = ~content,
                                  label = ~as.character(site),
                                  group = "Sampling locations" ) %>%
        addGlPolygons(
          data =  zones,
          color = ~ mpa_data$state_pal(zone),
          popup =  zones$COMMENTS,
          group = "Marine Parks") %>%
        addLegend(
          pal = mpa_data$state_pal,
          values = zones$zone,
          opacity = 1,
          title = "Zones",
          position = "bottomright",
          group = "Marine Parks") %>%
        addLayersControl(
          overlayGroups = c(
            "Sampling locations",
            "Marine Parks"),
          options = layersControlOptions(collapsed = FALSE))
      map}) %>%
      bindCache(park()) #redrawing same park is instant; the cache key is the selected park

    # Info popups ----
    #Park info button
    observeEvent(input$alert.coral.marinepark, {
      showModal(modalDialog(title = park(), include_page(file.path(popups_dir(), paste0("fish_", park_slug(park()), ".html")))))})

    #How do we measure coral cover button
    observeEvent(input$park.coralcover, {
      showModal(modalDialog(title = "How do we measure coral cover?", include_page(file.path(popups_dir(), "coral.cover.html"))))})
  })
}
