# UI for the coral page (modularised app)
#' coral UI function
#' @param id Module id (namespace).
#' @noRd
#' @importFrom shiny NS tagList

#UI for the coral tab. GOTCHA: every id must be wrapped in ns("..."), and conditionalPanel
# conditions are JS strings, so they use paste0("input['", ns("id"), "']== ...") not input.id
##### CORAL UI #####
mod_coral_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "coraltab",
    fluidRow(


      # valueBoxOutput("box.total.fish"),

      tabBox(
        title = "Choose a spatial scale to plot",
        width = 8,
        # width = "55%", # was 95%
        id = ns("tabset2"), height = "78vh",


        ## STATE ----
        tabPanel("State-wide summary",
                 style = "overflow: auto",
                 valueBoxOutput(ns("box.total.number.coral"), width = 6),
                 valueBoxOutput(ns("box.total.species.coral"), width = 6),
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
            htmlOutput(ns("benthic.park.coralcover.dropdown")),
            selectInput(
              width = "100%",
              ns("benthicparkmethoddropdown"),
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
              uiOutput(ns("ui.benthic.park.image"))
            ),
            column(
              width = 1,
              actionButton(ns("alert.coral.marinepark"),
                           label = " ",
                           icon = icon("info"),
                           icon.library = "font awesome",
                           style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px"
              )


            )
          ),

          conditionalPanel(
            paste0("input['", ns("benthicparkmethoddropdown"), "'] == \"Coral cover\""),

            h3("Coral cover:", actionButton(ns("park.coralcover"), label = " ",
                                            icon = icon("info"),
                                            icon.library = "font awesome",
                                            style = "color: #fff; background-color: #d14210; border-color: #d14210; border-radius: 10px;  border-width: 2px")),

            withSpinner(uiOutput(ns("benthic.park.coralcover.plot"))),

            h4("Most abundant families:"),
            withSpinner(uiOutput(ns("benthic.park.top5.coralcover.plot"))),

            h4("Coral cover by reef zone:"),
            withSpinner(uiOutput(ns("benthic.park.reefzone.coralcover.plot"))),

            h4("Most abundant families by site:"),

            htmlOutput(ns("benthic.park.site.coralcover.dropdown")),
            # withSpinner(uiOutput("ui.fish.park.total.zone.plot")),
            withSpinner(uiOutput(ns("benthic.site.coralcover.plot"))),

            h4("Coral cover by site:"),

            withSpinner(uiOutput(ns("benthic.site.coralcover.plot.facet")))
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
              paste0("input['", ns("tabset2"), "'] == \"State-wide summary\""),
              withSpinner(leafletOutput(width = "100%", ns("benthic.state.sampling.leaflet"), height = "78vh")),
              style = "z-index:1002;"
            ),
            conditionalPanel(
              paste0("input['", ns("tabset2"), "'] == \"Marine park\""),
              withSpinner(leafletOutput(width = "100%", ns("benthic.park.sampling.leaflet"), height = "78vh"))
            )
          ), # end of box
          style = "position:fixed; right: 0;"
        ))
      )
    )
  )
}



##### CORAL SERVER #####
#'
#' @param id Module id (namespace); must match the id given to mod_coral_ui().
#' @param mpa_data List of dashboard data loaded in app_server().
#' @noRd

# Pre-rendered PNG plots on the coral tab: file is Coral_<park>_<suffix>.png (no method in the name).
# GOTCHA: `id` must match an ns("id") in mod_coral_ui(); title/methods are unused here (NA).
coral_png_spec <- tibble::tribble(
  ~id,                                    ~suffix,               ~title, ~methods,
  "benthic.park.coralcover.plot",         "coral_cover",         NA,     NA,
  "benthic.park.top5.coralcover.plot",    "species_coral_cover", NA,     NA,
  "benthic.park.reefzone.coralcover.plot", "reefzone_coral_cover", NA,    NA,
  "benthic.site.coralcover.plot.facet",   "site_coral_cover",    NA,     NA
)

#Server for the coral tab. `mpa_data` is the shared list from load_mpa_data(); treat it as read-only
#Inside moduleServer, input$x are automatically namespaced to this module
mod_coral_server <- function(id, mpa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Selected park / site (NULL until the dropdowns render, so always req() before use)
    park <- reactive(input$benthic.park.coralcover.dropdown)
    site <- reactive(input$benthic.park.site.coralcover.dropdown)

    # State-wide summary ----
    output$box.total.number.coral <- renderValueBox({
      valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_number_coral, big.mark = ","), '</font></p>')),
               "Images Analysed",
               icon = tags$i(class = "fa-solid fa-camera", style = "font-size: 50px"),
               color = "yellow")
    })

    output$box.total.species.coral <- renderValueBox({
      valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_species_coral, big.mark = ","), '</font></p>')),
               "Total Genera Identified",
               icon = tags$i(img(src = "www/coral.png", width = "50px")),
               color = "yellow")
    })

    # Dropdowns ----
    output$benthic.park.coralcover.dropdown <- renderUI({
      create_dropdown(input, ns, "benthic.park.coralcover.dropdown",
                      unique(mpa_data$coral_cover_metadata$marine_park), "Choose a marine park:", FALSE)
    })

    # Only sites with more than 2 years of data in the chosen park are offered
    output$benthic.park.site.coralcover.dropdown <- renderUI({
      sites <- mpa_data$coral_cover_metadata %>%
        dplyr::group_by(marine_park, site) %>%
        dplyr::summarise(n = length(plot_year), .groups = "drop") %>%
        dplyr::filter(n > 2, marine_park %in% park()) %>%
        dplyr::distinct(site) %>%
        dplyr::pull("site")

      create_dropdown(input, ns, "benthic.park.site.coralcover.dropdown", sites, "Choose a site:", FALSE)
    })

    output$ui.benthic.park.image <- renderUI({
      req(park())
      img(src = paste0("www/images/coral_", park_slug(park()), ".jpg"), align = "right", width = "100%")
    })

    # Pre-rendered PNG plots ----
    # Add a row to coral_png_spec (and a matching ns("id") in mod_coral_ui) for a new plot.
    register_png_plots(output, coral_png_spec, "Coral", park)

    # Needs the site in the file name, so it is not in coral_png_spec
    output$benthic.site.coralcover.plot <- renderUI({
      req(park(), site())
      plot_img(paste0("Coral_", park_slug(park()), "_site_", site(), "_species_coral_cover.png"))
    })

    # Maps ----
    # Coral sampling sites for the selected park, with popup HTML. The state map below uses all sites.
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
          "<b>Reef Zone:</b>", reef_zone, "<br/>"))
    })

    output$benthic.state.sampling.leaflet <- renderLeaflet({
      dat <-  mpa_data$coral_sampling_effort %>%
        dplyr::distinct(marine_park, site, latitude_dd, longitude_dd) %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Marine Park:</b>", marine_park, "<br/>",
          "<b>Site:", site, "</b>", "<br/>")) %>%
        dplyr::filter(!is.na(latitude_dd))
      pal <- data.frame(marker_col = c("#F1652C", "yellow", "#2ECFE2","#739AF8"), marine_park = c("Shark Bay Marine Park", "Ningaloo Marine Park", "Montebello Islands Marine Park","Rowley Shoals Marine Park"))
      dat <- dplyr::left_join(dat, pal, by="marine_park")
      map <- leaflet_basemap(dat) %>%
        fitBounds(
          ~ min(longitude_dd),
          ~ min(latitude_dd),
          ~ max(longitude_dd),
          ~ max(latitude_dd)
        ) %>%
        leaflet::addCircleMarkers(data = dat,
                                  lng = ~longitude_dd,
                                  lat = ~latitude_dd,
                                  popup = ~content,
                                  label = ~as.character(site),
                                  color = "black", weight = 2,
                                  fillColor = ~marker_col,
                                  opacity = 1, radius = 8) %>%
        addGlPolygons(
          data =  mpa_data$state_mp,
          color = ~ mpa_data$state_pal(zone),
          popup =  mpa_data$state_mp$COMMENTS,
          group = "Marine Parks"
        ) %>%
        addLegend(
          pal = mpa_data$state_pal,
          values = mpa_data$state_mp$zone,
          opacity = 1,
          title = "Zones",
          position = "bottomright",
          group = "Marine Parks"
        ) %>%
        addLayersControl(
          overlayGroups = c(
            "Sampling locations",
            "Marine Parks"),
          options = layersControlOptions(collapsed = FALSE)
        )
      map
    })

    output$benthic.park.sampling.leaflet <- renderLeaflet({
      map <- leaflet_basemap(park_sampling_effort()) %>%
        fitBounds(
          ~ min(longitude_dd),
          ~ min(latitude_dd),
          ~ max(longitude_dd),
          ~ max(latitude_dd)
        ) %>%
        leaflet::addCircleMarkers(lng = ~longitude_dd,
                                  lat = ~latitude_dd,
                                  color = "black",
                                  weight = 2,
                                  fillColor = "pink",
                                  radius = 6,
                                  opacity = 1,
                                  popup = ~content,
                                  label = ~as.character(site),
                                  group = "Sampling locations"
        ) %>%
        addGlPolygons(
          data =  mpa_data$state_mp,
          color = ~ mpa_data$state_pal(zone),
          popup =  mpa_data$state_mp$COMMENTS,
          group = "Marine Parks"
        ) %>%
        addLegend(
          pal = mpa_data$state_pal,
          values = mpa_data$state_mp$zone,
          opacity = 1,
          title = "Zones",
          position = "bottomright",
          group = "Marine Parks"
        ) %>%
        addLayersControl(
          overlayGroups = c(
            "Sampling locations",
            "Marine Parks"),
          options = layersControlOptions(collapsed = FALSE)
        )
      map
    }) %>%
      bindCache(park())

    # Info popups ----
    observeEvent(input$alert.coral.marinepark, {
      showModal(modalDialog(title = park(), includeHTML(file.path(popups_dir(), paste0("fish_", park_slug(park()), ".html")))))
    })

    observeEvent(input$park.coralcover, {
      showModal(modalDialog(title = "How do we measure coral cover?", includeHTML(file.path(popups_dir(), "coral.cover.html"))))
    })
  })
}
