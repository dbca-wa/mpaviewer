#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet dplyr grid ggplot2 leafgl rgdal forcats cachem leaflegend ggh4x scales
#' @noRd
#'

app_server <- function(input, output, session) {

  load("inst/data/mpa_data.Rdata")

  output$box.total.number.fish <- renderValueBox({

    valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total.number.fish,  big.mark = ","), '</font></p>')),
             "Fish counted",
             icon = icon("fish"),
             color = "yellow") # have changed CSS colours
  })

  output$box.total.species.fish <- renderValueBox({
    valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total.species.fish,  big.mark = ","), '</font></p>')),

             "Fish species indentified",
             icon = icon("fish"),
             color = "yellow") # have changed CSS colours
  })

  output$statewide_plots <- renderUI({
    print("total fish")
    total_fish <- mpa_data$total.number.fish.park #%>% dplyr::glimpse()
    total_species <- mpa_data$total.species.fish.park # %>% glimpse()
    mins_watched <- mpa_data$mins.watched

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

          marine_park <- unique(total_ind$marine.park)

          park <- stringr::str_replace_all(tolower(marine_park), c("marine park" = "", "island marine reserve" = "", " " = ""))

          # box( status = "primary",
          #      solidHeader = TRUE,
          #      width = 12,

          tagList(
            HTML(paste0('<center>')),
            h4(marine_park),
            HTML(paste0('</center>')),

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


            HTML(paste0('<center><img src = "www/images/fish_', park, '_banner.jpg" width = "97%> </center> "')),
               # img(src = paste0("www/images/fish_", park, "_banner.jpg"), align = "center", width = "95%"),#, height = 200 , align = "left"
            br(),
            br(),
            br()
          )

        })

      do.call(tagList, marine_park_summaries)

      # } else {
      #   NULL
      # }


  })


  ## FUNCTIONS -----
  # TODO refactor as R/create_dropdown.R
  create_dropdown <- function(input_name, choices, label, multiple) {
    if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
      selected <- input[[input_name]]
    } else {
      selected <- choices[1]
    }

    shiny::selectInput(
      inputId = input_name,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple, width = "100%"
    )
  }

  observeEvent(input$access, {
    updateTabItems(session, "tabs", "info")
  })

# FISH ----

  #### MARINE PARK DROPDOWNS ----
  ####### ►  Create a marine park dropdown ----
  output$fish.park.dropdown <- renderUI({

    lats <- mpa_data$lats %>%
      dplyr::arrange(desc(mean.lat)) # removed desc for marine park workshop

    pickerInput(
      inputId = "fish.park.dropdown",
      label = "Choose a marine park:",
      choices = c(unique(lats$marine.park)),
      multiple = FALSE,
      # selected = unique(lats$marine.park)[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })

  # ####### ►  Create method dropdown ----
  # observeEvent(input$fish.park.dropdown, {
  #
  #
  # })
  observeEvent(input$fish.park.dropdown, {
  output$fish.park.method.dropdown <- renderUI({
    req(input$fish.park.dropdown)
  #
  #   print("park chosen")
  #   print(input$fish.park.dropdown)
  #
  #   dat <- mpa_data$metadata[marine.park %in% c(input$fish.park.dropdown)]
  #
  #   print(unique(dat$method))
  #
  #   choices <- dat %>%
  #     dplyr::distinct(method) %>%
  #     dplyr::pull("method")
  #
  #   print("choices")
  #   print(choices)
  #
  #
  #   create_dropdown("fish.park.method.dropdown", choices, "Choose a method:", FALSE)

  dat <- mpa_data$methods[marine.park %in% c(input$fish.park.dropdown)]

  glimpse(unique(dat$method))

  selectizeInput("fish.park.method.dropdown", "Choose a method:", choices = c(unique(sort(dat$method))), selected = c(unique(sort(dat$method)))[1])


  }) #%>% bindCache(input$fish.park.dropdown)
  })

  ####### ►  Create a fished species dropdown ----
  output$fish.park.fished.species.dropdown <- renderUI({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$ordered.top.fished.species[marine.park %in% c(input$fish.park.dropdown) &
                                         method %in% c(input$fish.park.method.dropdown)]
    choices <- dat %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::distinct(scientific) %>%
      dplyr::pull("scientific")

    pickerInput(
      inputId = "fish.park.fished.species.dropdown",
      label = "Choose target species to plot:",
      choices = choices,
      multiple = FALSE,
      selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  ####### ►  Create an all species dropdown ----
  output$UIfish.park.all.species.dropdown <- renderUI({

      dat <- mpa_data$ordered.top.species[marine.park %in% c(input$fish.park.dropdown) &
                                  method %in% c(input$fish.park.method.dropdown)]

      choices <- dat %>%
        dplyr::arrange(desc(total)) %>%
        dplyr::distinct(scientific) %>%
        dplyr::pull("scientific")

      shinyWidgets::pickerInput(
        inputId = "fish.park.all.species.dropdown",
        label = "Choose species to plot:",
        choices = choices,
        multiple = FALSE,
        selected = choices[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  ####### ►  Create a trophic group dropdown ----
  output$fish.park.trophic.dropdown <- renderUI({

    dat <- mpa_data$trophic.sum[marine.park %in% c(input$fish.park.dropdown) &
                                  method %in% c(input$fish.park.method.dropdown)]

    choices <- dat %>%
      dplyr::distinct(trophic.group) %>%
      dplyr::pull("trophic.group")

    pickerInput(
      inputId = "fish.park.trophic.dropdown",
      label = "Choose trophic groups to plot:",
      choices = sort(choices),
      multiple = TRUE,
      selected = sort(choices)[1:3],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  #----------------------------------------------------------------------------#
  ####### DATA FILTERED BY DROPDOWNS ----

  ####### ►  Total abundance and Species Richness for leaflets ----
  fish_park_alldata <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$all.data[marine.park %in% c(input$fish.park.dropdown)]
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

  # ####### ►  Overall SR + TA dataframes ----
  # fish_park_overall <- reactive({
  #   req(input$fish.park.dropdown, input$fish.park.method.dropdown)
  #
  #   dat <- mpa_data$ta.sr[marine.park %in% c(input$fish.park.dropdown)]
  #   dat <- dat[method %in% c(input$fish.park.method.dropdown)]
  #
  #   dat
  # })
  #
  # ####### ►  Overall Species richness ----
  # fish_park_sr_overall <- reactive({
  #   req(fish_park_overall())
  #
  #   dat <- fish_park_overall()[metric %in% c("Species richness")]
  #
  #   dat
  # })
  #
  # ####### ►  Overall Total abundance ----
  # fish_park_ta_overall <- reactive({
  #   req(fish_park_overall())
  #
  #   dat <- fish_park_overall()[metric %in% c("Total abundance")]
  #
  #   dat
  # })

  ####### ►  Sanctuary SR + TA plots ----
  fish_park_sanctuary <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$ta.sr.sanctuary[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat
  })

  ####### ►  Sanctuary Species richness ----
  fish_park_sr_sanctuary <- reactive({
    req(fish_park_sanctuary())

    dat <- fish_park_sanctuary()[metric %in% c("Species richness")]

    dat
  })

  ####### ►  Sanctuary Total abundance ----
  fish_park_ta_sanctuary <- reactive({
    req(fish_park_sanctuary())

    dat <- fish_park_sanctuary()[metric %in% c("Total abundance")]

    dat
  })

  ####### ►  Site SR + TA plots ----
  fish_park_site <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$ta.sr.site[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat
  })

  ####### ►  Site Species richness ----
  fish_park_sr_site <- reactive({
    req(fish_park_site())

    dat <- fish_park_site()[metric %in% c("Species richness")]

    dat
  })

  ####### ►  Site Total abundance ----
  fish_park_ta_site <- reactive({
    req(fish_park_site())

    dat <- fish_park_site()[metric %in% c("Total abundance")]

    dat
  })

  ####### ►  Zone SR + TA plots ----
  fish_park_zone <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$ta.sr.zone[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat
  })

  ####### ►  Zone Species richness ----
  fish_park_sr_zone <- reactive({
    req(fish_park_zone())

    dat <- fish_park_zone()[metric %in% c("Species richness")]

    dat
  })

  ####### ►  Zone Total abundance ----
  fish_park_ta_zone <- reactive({
    req(fish_park_zone())

    dat <- fish_park_zone()[metric %in% c("Total abundance")]

    dat
  })


  ####### ►  Sampling effort for leaflet ----
  fish_park_samplingeffort <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$sampling.effort[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat %>%
      dplyr::mutate(content = paste(
        sep = " ",
        "<b>Sample:", sample, "</b>", "<br/>",
        "<b>Status:</b>", status, "<br/>",
        "<b>Depth:</b>", depth, "m", "<br/>",
        "<b>Site:</b>", site, "<br/>",
        "<b>Location:</b>", location, "<br/>",
        "<b>Number of times sampled:</b>", number.of.times.sampled, "<br/>"
      ))
  })

  ####### ►  Abundance by species NOT summarised (for leaflets)----
  fish_park_abundance_species_leaflet <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$abundance.leaflet[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    dat <- dat[scientific %in% c(input$fish.park.all.species.dropdown)]

    dat

  })

  ####### ►  Metadata for leaflet Abundance by species -----
  fish_park_metadata_leaflet <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$metadata.leaflet[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })


  ####### ►  Abundance by species summarised ----
  fish_park_abundance_species <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$abundance.sum[marine.park %in% c(input$fish.park.dropdown)] # changed from abundance to summarised
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    dat <- dat[scientific %in% c(input$fish.park.all.species.dropdown)]

    dat

  })

  ####### ►  Abundance by species by Sanctuary----
  fish_park_abundance_species_sanctuary <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$abundance.sum.sanctuary[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    dat <- dat[scientific %in% c(input$fish.park.all.species.dropdown)]

    dat

  })

  ####### ►  Top ten species ----
  fish_park_top_ten <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$top.ten[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Individual Fished species abundance ----
  fish_park_fishedabundance <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.species.sum[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Individual Fished species abundance by sanctuary ----
  fish_park_fishedabundance_sanctuary <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.species.sum.sanctuary[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  All fished species summed together ----
  fish_park_fishedsum <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.sum[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  All fished species summed together by Sanctuary----
  fish_park_fishedsum_sanctuary <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.sum.sanctuary[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Summarised trophic abundance ----
  fish_park_trophicabundance <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$trophic.sum[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Complete length ----
  fish_park_fishedcompletelength <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.complete.length[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Trends ----
  fish_park_trends <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$interpretation.trends[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  # STATE DROPDOWNS ----

  # ####### ►  Create method dropdown ----
  # output$fish.state.method.dropdown <- renderUI({
  #   # choices <- mpa_data$metadata %>%
  #   #   # dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
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


  fish_samplingeffort <- reactive({
    req(mpa_data) # , input$fish.state.method.dropdown

    mpa_data$sampling.effort %>%
      # dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      # dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::mutate(content = paste(
        sep = " ",
        "<b>Method:", method, "</b>", "<br/>",
        "<b>Sample:", sample, "</b>", "<br/>",
        "<b>Status:</b>", status, "<br/>",
        "<b>Depth:</b>", depth, "m", "<br/>",
        "<b>Site:</b>", site, "<br/>",
        "<b>Location:</b>", location, "<br/>",
        "<b>Number of times sampled:</b>", number.of.times.sampled, "<br/>"
      ))
  })

  #----------------------------------------------------------------------------#
  #### STATE PLOTS ----

  ####### ►  Sampling effort leaflet ----
  output$fish.state.sampling.leaflet <- renderLeaflet({
    # req(input$fish.state.method.dropdown)

    projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    bruvs <- fish_samplingeffort() %>%
      dplyr::filter(method %in% "stereo-BRUVs")

    bruvs <- sf::st_as_sf(x = bruvs,
                       coords = c("longitude", "latitude"),
                       crs = projcrs)

    dovs <- fish_samplingeffort() %>%
      dplyr::filter(method %in% "stereo-DOVs")

    dovs <- sf::st_as_sf(x = dovs,
                       coords = c("longitude", "latitude"),
                       crs = projcrs)

    rovs <- fish_samplingeffort() %>%
      dplyr::filter(method %in% "stereo-ROVs")

    rovs <- sf::st_as_sf(x = rovs,
                       coords = c("longitude", "latitude"),
                       crs = projcrs)

    uvc <- fish_samplingeffort() %>%
      dplyr::filter(method %in% "UVC")

    uvc <- sf::st_as_sf(x = uvc,
                       coords = c("longitude", "latitude"),
                       crs = projcrs)

    map <- leaflet_basemap(fish_samplingeffort()) %>%
      fitBounds(
        ~ min(longitude),
        ~ min(latitude),
        ~ max(longitude),
        ~ max(latitude)
      ) %>%

      addGlPolygons(
        data =  mpa_data$state.mp,
        color = ~ mpa_data$state.pal(zone),
        popup =  mpa_data$state.mp$COMMENTS,
        group = "Marine Parks"
      ) %>%

      addLegend(
        pal = mpa_data$state.pal,
        values = mpa_data$state.mp$zone,
        opacity = 1,
        title = "Zones",
        position = "bottomright",
        group = "Marine Parks"
      ) %>%

      # UVC
      addGlPoints(data = uvc,
                  popup = ~content,
                  color = "black",
                  fillColor = "#6383db",
                  opacity = 1, radius = 20,
                  group = "UVC") %>%

      # ROVs
      addGlPoints(data = rovs,
                  popup = ~content,
                  color = "black",
                  fillColor = "#0CA2B0",
                  opacity = 1, radius = 20,
                  group = "stereo-ROVs") %>%

      # BRUVS
      addGlPoints(data = bruvs,
                  popup = ~content,
                  color = "black",
                  fillColor = "#FEC52E",
                  opacity = 1, radius = 20,
                  group = "stereo-BRUVs") %>%

      # DOVS
      addGlPoints(data = dovs,
                  popup = ~content,
                  color = "black",
                  fillColor = "#d14210",
                  opacity = 1, radius = 20,
                  group = "stereo-DOVs") %>%


      addLayersControl(
        overlayGroups = c(
          "stereo-BRUVs",
          "stereo-DOVs",
          "stereo-ROVs",
          "UVC",
          "Marine Parks"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addLegend("bottomright",
                colors = c("#F1652C", "#FFEC58", "#2ECFE2", "#739AF8"),
                labels = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs", "UVC"),
                title = "Sampling locations",
                opacity = 1)
  })


  #----------------------------------------------------------------------------#
  #### MARINE PARK PLOTS ----
  ####### ►  Sampling effort leaflet ----
  output$fish.park.sampling.leaflet <- renderLeaflet({

    dat <- fish_park_samplingeffort()

    # Create the basemap with marine parks and legend
    map <- leaflet_basemap(dat) %>%
      fitBounds(~ min(longitude), ~ min(latitude), ~ max(longitude), ~ max(latitude)) %>%
    addGlPolygons(
      data =  mpa_data$state.mp,
      color = ~ mpa_data$state.pal(zone),
      popup =  mpa_data$state.mp$COMMENTS,
      group = "Marine Parks"
    ) %>%
      addLegend(
        pal = mpa_data$state.pal, values = mpa_data$state.mp$zone, opacity = 1,
        title = "Zones",
        position = "bottomleft", group = "Marine Parks"
      ) %>%
      addLayersControl(
        overlayGroups = c("Sampling locations", "Marine Parks"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      leaflet::addAwesomeMarkers(data = dat,
                                 lng = ~longitude,
                                 lat = ~latitude,
                                 icon = leaflet::awesomeIcons(
                                   icon = 'surf',
                                   iconColor = 'white',
                                   library = 'fa',
                                   markerColor = 'green' #Possible values are "red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray", "lightgray", "black"
                                 ),
                                 popup = ~content,
                                 label = ~as.character(sample),
                                 group = "Sampling locations"
      )

    map
  })
  #
  # ####### ►  Leaflet - Total abundance and species richness ----
  # output$fish.park.metric.leaflet <- renderLeaflet({
  #
  #   ta <- fish_park_ta()
  #   sr <- fish_park_sr()
  #
  #   dat <- fish_park_samplingeffort()
  #
  #   overzero.ta <- filter(ta, value > 0)
  #   equalzero.ta <- filter(ta, value == 0)
  #   max.ta <- max(overzero.ta$value)
  #
  #   overzero.sr <- filter(sr, value > 0)
  #   equalzero.sr <- filter(sr, value == 0)
  #   max.sr <- max(overzero.sr$value)
  #
  #   map <- leaflet_basemap(dat) %>%
  #     fitBounds(~ min(longitude), ~ min(latitude), ~ max(longitude), ~ max(latitude)) %>%
  #     addGlPolygons(
  #       data =  mpa_data$state.mp,
  #       color = ~ mpa_data$state.pal(zone),
  #       popup =  mpa_data$state.mp$COMMENTS,
  #       group = "Marine Parks"
  #     ) %>%
  #     addLegend(
  #       pal = mpa_data$state.pal, values = mpa_data$state.mp$zone, opacity = 1,
  #       title = "Zones",
  #       position = "bottomleft", group = "Marine Parks"
  #     ) %>%
  #     addLayersControl(
  #       overlayGroups = c("Marine Parks",
  #                         "Total abundance",
  #                         "Species richness"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     ) %>%
  #     add_legend_ta(
  #       colors = c("black", "yellow", "yellow"),
  #       labels = c(0, round(max.ta / 2), max.ta),
  #       sizes = c(5, 20, 40), group = "Total abundance"
  #     ) %>%
  #     add_legend_sr(
  #       colors = c("black", "green", "green"),
  #       labels = c(0, round(max.sr / 2), max.sr),
  #       sizes = c(5, 20, 40), group = "Species richness"
  #     )
  #
  #   if (nrow(overzero.ta)) {
  #     map <- map %>%
  #       addCircleMarkers(
  #         data = overzero.ta, lat = ~latitude, lng = ~longitude,
  #         radius = ~ (((value / max(value)) * 20)), fillOpacity = 0.5, stroke = FALSE,
  #         label = ~ as.character(value), group = "Total abundance", color = "yellow"
  #       )
  #   }
  #
  #   if (nrow(equalzero.ta)) {
  #     map <- map %>%
  #       addCircleMarkers(
  #         data = equalzero.ta, lat = ~latitude, lng = ~longitude,
  #         radius = 2, fillOpacity = 0.5, color = "black", stroke = FALSE,
  #         label = ~ as.character(value), group = "Total abundance"
  #       )
  #   }
  #
  #   if (nrow(overzero.sr)) {
  #     map <- map %>%
  #       addCircleMarkers(
  #         data = overzero.sr, lat = ~latitude, lng = ~longitude,
  #         radius = ~ ((value / max(value)) * 20), fillOpacity = 0.5, stroke = FALSE,
  #         label = ~ as.character(value), group = "Species richness", color = "green"
  #       )
  #   }
  #
  #   if (nrow(equalzero.sr)) {
  #     map <- map %>%
  #       addCircleMarkers(
  #         data = equalzero.sr, lat = ~latitude, lng = ~longitude,
  #         radius = 2, fillOpacity = 0.5, color = "black", stroke = FALSE,
  #         label = ~ as.character(value), group = "Species richness"
  #       )
  #   }
  #
  #   map %>%
  #     hideGroup("Species richness")
  # })

  ####### ►  Total abundance ----
  # TODO see if this works
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.total.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_total_abundance.png"), align = "left", width = "100%", height = 250)
    })
  })

  # # observeEvent(input$fish.park.method.dropdown, {
  # output$fish.park.total.plot <- renderPlot({
  #   # Changed to summarized data
  #   # Only includes consistently sampled
  #
  #   ta <- fish_park_ta_overall()
  #   dat <- ta[complete %in% c("Consistently sampled")]
  #
  #   p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
  #     geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
  #     geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +xlab("Year") +
  #     ylab("Average total abundance per sample \n(+/- SE)") +
  #     stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
  #     scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
  #                        expand = expand_scale(mult = c(0, 0.05))) +
  #     scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
  #     ggplot_mpatheme()
  #
  #   gazetted <- unique(dat$gazetted)
  #   re.zoned <- unique(dat$re.zoned)
  #   min.year <- min(dat$year)
  #
  #   # Add gazettal and rezoned dates if they occured after sampling
  #   if(!gazetted %in% c("NA", NA, NULL)){
  #
  #     if(min.year < gazetted) {
  #     p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
  #       geom_label(
  #         x = gazetted,
  #         y = +Inf,
  #         label = "\n\n gazetted",
  #         size = 5,
  #         fill = "white",
  #         check_overlap = TRUE,
  #         label.size = NA
  #       )}
  #   }
  #
  #   if(!re.zoned %in% c("NA", NA, NULL)){
  #     if(min.year < re.zoned) {
  #     p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
  #       geom_label(
  #         x = re.zoned,
  #         y = +Inf,
  #         label = "\n\n rezoned",
  #         size = 5,
  #         fill = "white",
  #         check_overlap = TRUE,
  #         label.size = NA
  #       )}
  #   }
  #   p
  # }) #%>% bindCache(fish_park_ta())
  # # })

  ####### ►  Total abundance by site ----

  output$fish.park.total.site.plot <- renderPlot({
    # Changed to summarized data

    dat <- fish_park_ta_site()

    if(input$fish.park.method.dropdown %in% c("stereo-DOVs", "stereo-ROVs")){

      ggplot(dat, aes(x = year, y = mean, fill = status, group = complete)) +
        geom_point(aes(shape = complete), size = 6, col = "black", position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +xlab("Year") +
        ylab("Average total abundance per sample \n(+/- SE)") +
        stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                           expand = expand_scale(mult = c(0, 0.05))) +
        scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        scale_shape_manual(values = c("Consistently sampled" = 21, "Intermittently sampled" = 22)) +
        ggh4x::facet_wrap2(vars(site), axes = "all", ncol = 3) +
        ggplot_mpatheme()
    }
  }) #%>% bindCache(fish_park_ta())

  output$ui.fish.park.total.site.plot <- renderUI({
    dat <- fish_park_ta_site()

    if(input$fish.park.method.dropdown %in% c("stereo-DOVs", "stereo-ROVs")){

      if (length(unique(dat$site)) %in% c(1,2,3) ){
        p.height <- 250
      } else {
        p.height <- 175 * ceiling(length(unique(dat$site))/3)
      }

      tagList(h4("Total abundance by site:"),
              plotOutput("fish.park.total.site.plot", height = p.height))
    }
  }) #%>% bindCache(fish_park_ta())

  ####### ►  Total abundance by Sanctuary ----
  output$fish.park.total.sanctuary.plot <- renderPlot({
    # Changed to summarized data
    # Only includes consistently sampled

    dat <- fish_park_ta_sanctuary()
    # dat <- dat[complete %in% c("Consistently sampled")] # Turned off after meeting with Jordan 6th Nov 2023

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      ggh4x::facet_wrap2(vars(dbca_sanctuary), axes = "all", ncol = 3) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){

      if(min.year < gazetted) {

        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }

    if(!re.zoned %in% c("NA", NA, NULL)){

      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }
    p
  }) # %>% bindCache(fish_park_ta())

  output$ui.fish.park.total.sanctuary.plot <- renderUI({
    dat <- fish_park_ta_sanctuary()

    if (length(unique(dat$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 175 * ceiling(length(unique(dat$dbca_sanctuary))/3)
    }

    plotOutput("fish.park.total.sanctuary.plot", height = p.height)
  }) %>%
    bindCache(fish_park_ta())

# TODO move to the generate data function
  pal <- c("Sanctuary" = "#C0D435",
           "Recreation" = "#F8EE75",
           "General Use" = "#BBE3EF",
           "SP Benthic Protection" = "#CAC3D5",
           "Outside Park" = "pink",
           "Conservation Area" = "#C0B134",
           "Marine Management Area" = "#b7cfe1",
           "SP Seagrass Protection" = "#AB9ECC",
           "Marine Nature Reserve" = "#bfd054",
           "SP Habitat Protection" = "#CAC3D5",
           "Shore Based Activities" = "#231D1D",
           "SP Wildlife Conservation" = "#7C7CB8")


  ####### ►  Total abundance by Zone ----
  output$fish.park.total.zone.plot <- renderPlot({
    # Changed to summarized data

    dat <- fish_park_ta_zone()
    # dat <- dat[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

    p <- ggplot(dat, aes(x = year, y = mean, fill = dbca_zone)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c(pal)) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){
      if(min.year < gazetted) {
        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }

    if(!re.zoned %in% c("NA", NA, NULL)){
      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }
    p
  }) # %>% bindCache(fish_park_ta())

  # output$ui.fish.park.total.zone.plot <- renderUI({
  #   dat <- fish_park_ta()
  #
  #   if (length(unique(dat$dbca_zone)) %in% c(1,2,3) ){
  #     p.height <- 250
  #   } else {
  #     p.height <- 250 * ceiling(length(unique(dat$dbca_zone))/3)
  #   }
  #
  #   plotOutput("fish.park.total.zone.plot", height = p.height)
  # }) # %>% bindCache(fish_park_ta())

  ####### ►  Species richness ----

  # TODO see if this works
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.rich.plot <- renderUI({
      # req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      print("plotting species richness")
      print(park)
      print(method)

      img(src = paste0("www/plots/", park, "_", method, "_species_richness.png"), align = "left", width = "100%", height = 250)
      # img(src = paste0("www/images/fish_", park, ".jpg"), align = "right", width = "100%") # removed www from URL to get rid of golem
    })
  })

  # observeEvent(input$fish.park.dropdown, {
  # observeEvent(input$fish.park.method.dropdown, {
  # # Changed to summarized data
  # output$fish.park.rich.plot <- renderPlot({
  #
  #   dat <- fish_park_sr_overall()
  #   dat <- dat[complete %in% c("Consistently sampled")]
  #
  #   p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
  #     geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
  #     geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.5)) +
  #     xlab("Year") +
  #     ylab("Average number of species per sample \n(+/- SE)") +
  #     stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
  #     scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
  #                        expand = expand_scale(mult = c(0, 0.05))) +
  #     scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
  #     ggplot_mpatheme()
  #
  #   gazetted <- unique(dat$gazetted)
  #   re.zoned <- unique(dat$re.zoned)
  #   min.year <- min(dat$year)
  #
  #   # Add gazettal and rezoned dates if they occured after sampling
  #   if(!gazetted %in% c("NA", NA, NULL)){
  #
  #     if(min.year < gazetted) {
  #
  #       p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
  #         geom_label(
  #           x = gazetted,
  #           y = +Inf,
  #           label = "\n\n gazetted",
  #           size = 5,
  #           fill = "white",
  #           check_overlap = TRUE,
  #           label.size = NA
  #         )}
  #
  #   }
  #
  #   if(!re.zoned %in% c("NA", NA, NULL)){
  #
  #     if(min.year < re.zoned) {
  #       p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
  #         geom_label(
  #           x = re.zoned,
  #           y = +Inf,
  #           label = "\n\n rezoned",
  #           size = 5,
  #           fill = "white",
  #           check_overlap = TRUE,
  #           label.size = NA
  #         )}
  #
  #   }
  #   p
  # }) # %>% bindCache(fish_park_sr())
  # })
  # })

  ####### ►  Species richness by site ----
  output$fish.park.rich.site.plot <- renderPlot({
    # Changed to summarized data

    dat <- fish_park_sr_site()

    if(input$fish.park.method.dropdown %in% c("stereo-DOVs", "stereo-ROVs")){

      ggplot(dat, aes(x = year, y = mean, fill = status)) +
        geom_point(aes(shape = complete), size = 6, col = "black", position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
        xlab("Year") +
        ylab("Average number of species per sample \n(+/- SE)") +
        stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                           expand = expand_scale(mult = c(0, 0.05))) +
        scale_shape_manual(values = c("Consistently sampled" = 21, "Intermittently sampled" = 22)) +
        scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        ggh4x::facet_wrap2(vars(site), axes = "all", ncol = 3) +
        ggplot_mpatheme()
    }
  }) # %>% bindCache(fish_park_sr())

  output$ui.fish.park.rich.site.plot <- renderUI({
    dat <- fish_park_sr_site()

    if(input$fish.park.method.dropdown %in% c("stereo-DOVs", "stereo-ROVs")){

      if (length(unique(dat$site)) %in% c(1,2,3) ){
        p.height <- 250
      } else {
        p.height <- 175 * ceiling(length(unique(dat$site))/3)
      }
      tagList(h4("Species richness by site:"),
              plotOutput("fish.park.rich.site.plot", height = p.height))
    }
  }) #%>% bindCache(fish_park_sr())

  ####### ►  Species richness by Sanctuary ----
  output$fish.park.rich.sanctuary.plot <- renderPlot({
    # Changed to summarized
    # Only includes consistently sampled

    dat <- fish_park_sr_sanctuary()
    # dat <- dat[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.5)) +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      ggh4x::facet_wrap2(vars(dbca_sanctuary), axes = "all", ncol = 3) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occurred after sampling
    if(!gazetted %in% c("NA", NA, NULL)){

      if(min.year < gazetted) {

        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }

    if(!re.zoned %in% c("NA", NA, NULL)){
      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }
    p

  }) #%>% bindCache(fish_park_sr())

  output$ui.fish.park.rich.sanctuary.plot <- renderUI({
    dat <- fish_park_sr_sanctuary()

    if (length(unique(dat$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 175 * ceiling(length(unique(dat$dbca_sanctuary))/3)
    }

    plotOutput("fish.park.rich.sanctuary.plot", height = p.height)
  }) #%>% bindCache(fish_park_sr())

  ####### ►  Species richness by Zone ----
  output$fish.park.rich.zone.plot <- renderPlot({
    # Changed to summarized
    # Only includes consistently sampled

    dat <- fish_park_sr_zone()
    # dat <- dat[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

    p <- ggplot(dat, aes(x = year, y = mean, fill = dbca_zone)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c(pal)) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occurred after sampling
    if(!gazetted %in% c("NA", NA, NULL)){

      if(min.year < gazetted) {
        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }

    if(!re.zoned %in% c("NA", NA, NULL)){
      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}
    }
    p
  })

  # output$ui.fish.park.rich.zone.plot <- renderUI({
  #   dat <- fish_park_sr_zone()
  #
  #   if (length(unique(dat$dbca_zone)) %in% c(1,2,3) ){
  #     p.height <- 250
  #   } else {
  #     p.height <- 250 * ceiling(length(unique(dat$dbca_zone))/3)
  #   }
  #
  #   plotOutput("fish.park.rich.zone.plot", height = p.height)
  # }) #%>% bindCache(fish_park_sr())

  ####### ►  Stacked Abundance Plot ----
  output$fish.park.stack.plot <- renderPlot({

    maxn.sum <- fish_park_top_ten() %>%
      arrange(desc(maxn))

    ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      coord_flip() +
      xlab("Species") +
      ylab("Overall abundance") +
      ggplot_mpatheme() +
      theme(axis.text.y = element_text(face = "italic")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  })

  ####### ►  Trophic group ----
  output$fish.park.trophic.plot <- renderPlot({
    # Changed to summarized
    # Only includes consistently sampled

    dat <- fish_park_trophicabundance()[trophic.group %in% c(input$fish.park.trophic.dropdown)]
    dat <- dat[complete %in% c("Consistently sampled")]

    #TODO figure out why trophic has two gazettal (NA and 2018) for Ngari Capes
    gazetted <- min(dat$gazetted)
    re.zoned <- min(dat$re.zoned)
    min.year <- min(dat$year)

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.5)) +
      xlab("Year") +
      ylab("Average abundance per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggplot_mpatheme()

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){

      if(min.year < gazetted) {

        p <- p + geom_vline(data = gazetted, aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }

    if(!re.zoned %in% c("NA", NA, NULL)){

      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    p + ggh4x::facet_wrap2(vars(trophic.group), axes = "all", ncol = 1, scales = "free_y")

    # dat <- fish_park_trophicabundance()[trophic.group %in% c(input$fish.park.trophic.dropdown)]
    #
    # p <- ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
    #   stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
    #   stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
    #   xlab("Year") +
    #   ylab("Average abundance per sample \n(+/- SE)") +
    #   scale_y_continuous(expand = c(0, 0.1)) +
    #   scale_x_continuous(
    #     breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
    #     expand = expand_scale(mult = c(0, 0.05))
    #   ) +
    #   scale_fill_manual(values = c("#b9e6fb",
    #                                "#7bbc63")) +
    #   stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
    #   # facet_wrap(trophic.group ~ ., scales = "free", ncol = 1) +
    #   ggh4x::facet_wrap2(vars(trophic.group), axes = "all", ncol = 1) +
    #   ggplot_mpatheme()
    #
    # #TODO figure out why trophic has two gazettal (NA and 2018) for Ngari Capes
    # gazetted <- min(dat$gazetted)
    # re.zoned <- min(dat$re.zoned)
    # min.year <- min(dat$year)
    #
    # # Add gazettal and rezoned dates if they occured after sampling
    # if(!gazetted %in% c("NA", NA, NULL)){
    #
    #   if(min.year < gazetted) {
    #
    #     p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
    #       geom_label(
    #         x = gazetted,
    #         y = +Inf,
    #         label = "\n\n gazetted",
    #         size = 5,
    #         fill = "white",
    #         check_overlap = TRUE,
    #         label.size = NA
    #       )}
    #
    # }
    #
    # if(!re.zoned %in% c("NA", NA, NULL)){
    #
    #   if(min.year < re.zoned) {
    #     p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
    #       geom_label(
    #         x = re.zoned,
    #         y = +Inf,
    #         label = "\n\n rezoned",
    #         size = 5,
    #         fill = "white",
    #         check_overlap = TRUE,
    #         label.size = NA
    #       )}
    #
    # }
    # p


  }) #%>% bindCache(fish_park_trophicabundance())

  ####### ►  KDE plot ----
  # 20 individuals min to plot a line (KDE). Depends on scale. If zones in one year then you need 20 in the zone for that year.
  output$fish.park.fished.species.kde.plot <- renderPlot({
    req(fish_park_fishedcompletelength())

    more.than.20 <- fish_park_fishedcompletelength() %>%
      dplyr::group_by(marine.park, method, campaignid, status, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::filter(number > 20) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(marine.park, method, campaignid, status, scientific)

    dat <- fish_park_fishedcompletelength()[scientific %in% c(input$fish.park.fished.species.dropdown)]
    dat <- dat[length > 0]
    dat <- dat[!is.na(length)]
    dat <- dat[complete %in% c("Consistently sampled")]

    dat <- dat %>%
      dplyr::semi_join(more.than.20)

    validate(
      need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
    )

    ggplot(dat, aes(x = length, fill = status)) +
      geom_density(aes(y = ..density.. * 1000), alpha = 0.5, size = 0.7) +
      theme(legend.position = ("bottom")) +
      theme(
        strip.text.y = element_text(size = 12, angle = 270),
        strip.background = element_blank(),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "italic", hjust = 0.5),
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      ylab("Weighted KDE (*1000)") +
      xlab("Total Length (mm)") +
      ggplot_mpatheme() +
      facet_grid(rows = vars(year))
  })

  output$ui.fish.park.fished.species.kde.plot <- renderUI({
    more.than.20 <- fish_park_fishedcompletelength() %>%
      dplyr::group_by(marine.park, method, campaignid, status, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::filter(number > 20) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(marine.park, method, campaignid, status, scientific)

    dat <- fish_park_fishedcompletelength()[scientific %in% c(input$fish.park.fished.species.dropdown)]
    dat <- dat[length > 0]
    dat <- dat[!is.na(length)]
    dat <- dat[complete %in% c("Consistently sampled")]

    dat <- dat %>%
      dplyr::semi_join(more.than.20)

    validate(
      need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
    )

      p.height <- 175 * length(unique(dat$year))

    plotOutput("fish.park.fished.species.kde.plot", height = p.height)
  })

  # ####### ►  KDE plot by sanctuary ----
  # # 20 individuals min to plot a line (KDE). Depends on scale. If zones in one year then you need 20 in the zone for that year.
  # output$fish.park.fished.species.kde.sanctuary.plot <- renderPlot({
  #   req(fish_park_fishedcompletelength())
  #
  #   more.than.20 <- fish_park_fishedcompletelength() %>%
  #     dplyr::group_by(marine.park, method, campaignid, status, scientific, dbca_sanctuary) %>%
  #     dplyr::summarise(number = sum(number)) %>%
  #     dplyr::filter(number > 20) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(marine.park, method, campaignid, status, scientific, dbca_sanctuary)
  #
  #   dat <- fish_park_fishedcompletelength()[scientific %in% c(input$fish.park.fished.species.dropdown)]
  #   dat <- dat[length > 0]
  #   dat <- dat[!is.na(length)]
  #   dat <- dat[complete %in% c("Consistently sampled")]
  #
  #   dat <- dat %>%
  #     dplyr::semi_join(more.than.20)
  #
  #   validate(
  #     need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
  #   )
  #
  #   ggplot(dat, aes(x = length, fill = status)) +
  #     geom_density(aes(y = ..density.. * 1000), alpha = 0.5, size = 0.7) +
  #     theme(legend.position = ("bottom")) +
  #     theme(
  #       strip.text.y = element_text(size = 12, angle = 270),
  #       strip.background = element_blank(),
  #       axis.title = element_text(face = "bold"),
  #       plot.title = element_text(face = "italic", hjust = 0.5),
  #       strip.text.x = element_text(size = 14),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank()
  #     ) +
  #     scale_y_continuous(expand = c(0, 0.1)) +
  #     scale_fill_manual(values = c("#b9e6fb",
  #                                  "#7bbc63")) +
  #     ylab("Weighted KDE (*1000)") +
  #     xlab("Total Length (mm)") +
  #     ggplot_mpatheme() +
  #     facet_grid(rows = vars(year, dbca_sanctuary))
  # })
  #
  # output$ui.fish.park.fished.species.kde.sanctuary.plot <- renderUI({
  #   more.than.20 <- fish_park_fishedcompletelength() %>%
  #     dplyr::group_by(marine.park, method, campaignid, status, scientific, dbca_sanctuary) %>%
  #     dplyr::summarise(number = sum(number)) %>%
  #     dplyr::filter(number > 20) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(marine.park, method, campaignid, status, scientific, dbca_sanctuary)
  #
  #   dat <- fish_park_fishedcompletelength()[scientific %in% c(input$fish.park.fished.species.dropdown)]
  #   dat <- dat[length > 0]
  #   dat <- dat[!is.na(length)]
  #   dat <- dat[complete %in% c("Consistently sampled")]
  #
  #   dat <- dat %>%
  #     dplyr::semi_join(more.than.20)
  #
  #   validate(
  #     need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
  #   )
  #
  #   p.height <- 175 * length(unique(dat$year))
  #
  #   plotOutput("fish.park.fished.species.kde.sanctuary.plot", height = p.height)
  # })


  ####### ►  Summed Fished Species ----
  output$fish.park.fished.sum.plot <- renderPlot({
    # Summarised
    # Only includes consistently sampled sites

    dat <- fish_park_fishedsum()
    dat <- dat[complete %in% c("Consistently sampled")]

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      #annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){

      if(min.year < gazetted) {

        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }

    if(!re.zoned %in% c("NA", NA, NULL)){

      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    p

  })

  ####### ►  Summed Fished Species by Sanctuary ----
  output$fish.park.fished.sum.sanctuary.plot <- renderPlot({
    req(fish_park_fishedsum_sanctuary())
    # Summarised
    # Only includes consistently sampled sites

    dat <- fish_park_fishedsum_sanctuary()
    dat <- dat[complete %in% c("Consistently sampled")]

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      ggh4x::facet_wrap2(vars(dbca_sanctuary), axes = "all", ncol = 3) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){

      if(min.year < gazetted) {

        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }

    if(!re.zoned %in% c("NA", NA, NULL)){

      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    p

  })

  output$ui.fish.park.fished.sum.sanctuary.plot <- renderUI({
    dat <- fish_park_fishedsum_sanctuary()

    if (length(unique(dat$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 175 * ceiling(length(unique(dat$dbca_sanctuary))/3)
    }

    plotOutput("fish.park.fished.sum.sanctuary.plot", height = p.height)
  })

  ####### ►  Individual Fished species abundance ----
  # Summarised
  # Only includes consistently sampled sites

  output$fish.park.fished.species.abundance.plot <- renderPlot({

    dat <- fish_park_fishedabundance()[scientific %in% c(input$fish.park.fished.species.dropdown)]
    dat <- dat[complete %in% c("Consistently sampled")]

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(scientific), axes = "all", ncol = 1) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){
      if(min.year < gazetted) {
        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    if(!re.zoned %in% c("NA", NA, NULL)){
      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    p
  })


  ####### ►  Individual Fished species abundance by Sanctuary ----
  # Summarised
  # Only includes consistently sampled sites

  output$fish.park.fished.species.abundance.sanctuary.plot <- renderPlot({

    dat <- fish_park_fishedabundance_sanctuary()[scientific %in% c(input$fish.park.fished.species.dropdown)]
    dat <- dat[complete %in% c("Consistently sampled")]

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(dbca_sanctuary), axes = "all", ncol = 3) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){
      if(min.year < gazetted) {
        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    if(!re.zoned %in% c("NA", NA, NULL)){
      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    p
  })

  output$ui.fish.park.fished.species.abundance.sanctuary.plot <- renderUI({
    dat <- fish_park_fishedabundance_sanctuary()

    if (length(unique(dat$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 175 * ceiling(length(unique(dat$dbca_sanctuary))/3)
    }

    plotOutput("fish.park.fished.species.abundance.sanctuary.plot", height = p.height)
  })


  ####### ►  Individual Species abundance ----
  output$fish.park.all.species.abundance.plot <- renderPlot({
    # Summarised
    # Only includes consistently sampled sites

    req(fish_park_abundance_species())
    dat <- fish_park_abundance_species()
    dat <- dat[complete %in% c("Consistently sampled")]

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      xlab("Year") +
      ylab("Average abundance of species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(scientific), axes = "all", ncol = 1) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){

      if(min.year < gazetted) {

        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }

    if(!re.zoned %in% c("NA", NA, NULL)){

      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    p


  })


  ####### ►  Individual Species abundance by Sanctuary ----
  # Summarised
  # Only includes consistently sampled sites

  output$fish.park.all.species.abundance.sanctuary.plot <- renderPlot({

    dat <- fish_park_abundance_species_sanctuary()
    dat <- dat[complete %in% c("Consistently sampled")]

    p <- ggplot(dat, aes(x = year, y = mean, fill = status)) +
      geom_point(shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, position = position_dodge(.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                         expand = expand_scale(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(dbca_sanctuary), axes = "all", ncol = 3) +
      ggplot_mpatheme()

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)
    min.year <- min(dat$year)

    # Add gazettal and rezoned dates if they occured after sampling
    if(!gazetted %in% c("NA", NA, NULL)){
      if(min.year < gazetted) {
        p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
          geom_label(
            x = gazetted,
            y = +Inf,
            label = "\n\n gazetted",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    if(!re.zoned %in% c("NA", NA, NULL)){
      if(min.year < re.zoned) {
        p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
          geom_label(
            x = re.zoned,
            y = +Inf,
            label = "\n\n rezoned",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA
          )}

    }
    p
  })

  output$ui.fish.park.all.species.abundance.sanctuary.plot <- renderUI({
    dat <- fish_park_abundance_species_sanctuary()

    if (length(unique(dat$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 175 * ceiling(length(unique(dat$dbca_sanctuary))/3)
    }

    plotOutput("fish.park.all.species.abundance.sanctuary.plot", height = p.height)
  })



  ###### ►  Leaflet - All species abundance ----
  output$fish.park.all.species.leaflet <- renderLeaflet({

    req(fish_park_abundance_species_leaflet())

    dat <- fish_park_abundance_species_leaflet()

    dat <- dplyr::full_join(dat, fish_park_metadata_leaflet()) %>%
      tidyr::replace_na(list(maxn = 0))

    print(unique(dat$scientific))

    # TODO I don't think this is the complete data?!?!?!?

    overzero.ta <- dplyr::filter(dat, maxn > 0) #%>% dplyr::glimpse()
    equalzero.ta <- dplyr::filter(dat, maxn == 0) #%>% dplyr::glimpse()

    print(max(overzero.ta$maxn))

    max.ta <- base::max(overzero.ta$maxn)
    print(max.ta)


    map <- leaflet_basemap(dat) %>%
      fitBounds(~ min(longitude), ~ min(latitude), ~ max(longitude), ~ max(latitude)) %>%
      addGlPolygons(
        data =  mpa_data$state.mp,
        color = ~ mpa_data$state.pal(zone),
        popup =  mpa_data$state.mp$COMMENTS,
        group = "Marine Parks"
      ) %>%
      addLegend(
        pal = mpa_data$state.pal, values = mpa_data$state.mp$zone, opacity = 1,
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
          data = overzero.ta, lat = ~latitude, lng = ~longitude,
          radius = ~ scales::rescale(maxn, c(5, 25)), #((((maxn / max(maxn))) * 20)),
          fillOpacity = 0.5, stroke = FALSE,
          label = ~ as.character(maxn), group = "Abundance", color = "blue"
        )
    }

    if (nrow(equalzero.ta)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero.ta, lat = ~latitude, lng = ~longitude,
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
    print(park)

    img(src = paste0("www/images/fish_", park, ".jpg"), align = "right", width = "100%") # removed www from URL to get rid of golem
  })
  })

  output$ui.benthic.park.image <- renderUI({
    park <- stringr::str_replace_all(tolower(input$benthic.park.coralcover.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
    print(park)

    img(src = paste0("www/images/coral_", park, ".jpg"), align = "right", width = "100%")  # removed www from URL to get rid of golem
  })

  # Species iFrames -----
  ####### ►  State All species ----
  output$fish.state.all.species.iframe <- renderUI({

    dat <- mpa_data$foa.codes[scientific %in% c(input$fish.state.all.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat))
    frame

  })

  ####### ►  State Fished species ----
  output$fish.state.fished.species.iframe <- renderUI({

    dat <- mpa_data$foa.codes[scientific %in% c(input$fish.state.fished.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), height = 600, width = "100%")
    frame

  })

  ####### ►  Marine Park All species ----
  output$fish.park.all.species.iframe <- renderUI({

    dat <- mpa_data$foa.codes[scientific %in% c(input$fish.park.all.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), width = "100%", height = 600, class = "frame")
    frame
  })

  ####### ►  Marine Park Fished species ----
  output$fish.park.fished.species.iframe <- renderUI({

    dat <- mpa_data$foa.codes[scientific %in% c(input$fish.park.fished.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), height = 800, width = "100%")
    frame

  })

  # ####### ►  Info buttons ----
  # observeEvent(input$state.ta,
  #              showModal(modalDialog(
  #                title = "How do we measure total abundance?",
  #                htmltools::includeMarkdown(paste0("inst/app/www/popups/total.abundance.md"))))
  # )
  #
  # observeEvent(input$state.sr,
  #              showModal(modalDialog(
  #                title = "How do we measure species richness?",
  #                htmltools::includeMarkdown(paste0("inst/app/www/popups/species.richness.md"))))
  # )
  #
  #
  observeEvent(input$park.ta,
               showModal(modalDialog(
                 title = "How do we measure total abundance?",
                 includeHTML("inst/app/www/popups/total.abundance.html")))
  )

  observeEvent(input$park.sr,
# CHANGED THIS DEPLOYIN
               # print(dir())
               showModal(modalDialog(
                 title = "How do we measure species richness?",
                 includeHTML("inst/app/www/popups/species.richness.html")))
  )

  # MARINE PARK ----
  # FOR FISH
  observeEvent(
    input$alert.marinepark,

    showModal(modalDialog(
      title = input$fish.park.dropdown,
      includeHTML(paste0("inst/app/www/popups/fish_",
                         stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", " " = "")),
                         ".html"))
      )
    ))

  # Interpretation of trends ----
  ####### ►  Total abundance ----
  output$fish.park.total.trend <- renderUI({

    dat <- fish_park_trends()[metric %in% c("total.abundance")]

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

  ####### ►  Species richness ----
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



    # BENTHIC ----
    # State data ----
    ####### ►  Create a marine park dropdown ----
    output$benthic.state.park.coralcover.dropdown <- renderUI({
      pickerInput(
        inputId = "benthic.state.park.coralcover.dropdown",
        label = "Choose Marine Parks to include:",
        choices = c(unique(mpa_data$coral_cover_metadata$marine.park)), #choices,
        multiple = TRUE,
        selected = c(unique(mpa_data$coral_cover_metadata$marine.park)), # choices,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    })

    output$benthic.state.park.coralrecruitment.dropdown <- renderUI({
      pickerInput(
        inputId = "benthic.state.park.coralrecruitment.dropdown",
        label = "Choose Marine Parks to include:",
        choices = c(unique(mpa_data$rec_3b$marine.park)), #choices,
        multiple = TRUE,
        selected = c(unique(mpa_data$rec_3b$marine.park)), # choices,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    })

    ####### ►  Filter coral cover data to marine park and summarise per marine park per year
    benthic_state_coral_cover <- reactive({
      req(mpa_data, input$benthic.state.park.coralcover.dropdown)

      dat <- mpa_data$coral_cover_transect %>%
        dplyr::filter(marine.park %in% c(input$benthic.state.park.coralcover.dropdown)) %>%
        dplyr::group_by(marine.park, method, plot_year) %>%
        dplyr::summarise(n    = length(unique(site)),
                         mean = mean(percent_cover),
                         sd   = sd(percent_cover),
                         se   = sd(percent_cover) / sqrt(length(unique(site))))



      # plyr::ddply(dat, plyr::.(marine.park, method, plot_year), .inform=TRUE, dplyr::summarise,
      #       n    = length(unique(site)),
      #       mean = mean(percent_cover),
      #       sd   = sd(percent_cover),
      #       se   = sd(percent_cover) / sqrt(length(unique(site))))


    })

    ####### ►  Create state plot for coral cover per year faceted by marine park
    output$benthic.state.coralcover.plot <- renderPlot({
      req(benthic_state_coral_cover())

      p <- ggplot(data = subset(benthic_state_coral_cover(), !plot_year %in% c("1999")), aes(x = plot_year, y = mean)) +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .025) +
        stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, size = 1, col = "black", linetype = "solid") +
        geom_point(size = 2) +
        xlab("") +
        ylab("% Coral Cover (mean ± SE)") +
        scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
          expand = expand_scale(mult = c(0, 0.05))
        ) +

        scale_y_continuous(expand = c(0, 0.1)) +
        # facet_wrap(marine.park ~ ., scales = "free", ncol = 1) +
        ggh4x::facet_wrap2(vars(marine.park), axes = "all", ncol = 1) +
        ggplot_mpatheme()

      p
    })

    # Marine park data ----
    ####### ►  Create a marine park dropdown ----
    output$benthic.park.coralcover.dropdown <- renderUI({

      options <- mpa_data$coral_cover_metadata %>%
        dplyr::distinct(marine.park) %>%
        dplyr::pull("marine.park")

      create_dropdown("benthic.park.coralcover.dropdown", options, "Choose a marine park:", FALSE)

    })

    ####### ►  Create a site dropdown ----
    output$benthic.park.site.coralcover.dropdown <- renderUI({
      options <- mpa_data$coral_cover_metadata %>%
        dplyr::filter(marine.park %in% c(input$benthic.park.coralcover.dropdown)) %>%
        dplyr::distinct(site) %>%
        dplyr::arrange() %>%
        dplyr::pull("site")

      pickerInput(
        inputId = "benthic.park.site.coralcover.dropdown",
        label = "Choose sites to include:",
        choices = sort(options),
        multiple = TRUE,
        selected = options,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE),
        width = "100%"
      )
    })

    ####### ►  Create marine park specific data
    benthic_park_coral_cover <- reactive({
      req(mpa_data, input$benthic.park.coralcover.dropdown)

      dat <- mpa_data$coral_cover_transect %>%
        dplyr::filter(marine.park %in% c(input$benthic.park.coralcover.dropdown)) %>%
        dplyr::filter(site %in% c(input$benthic.park.site.coralcover.dropdown)) %>%
        dplyr::group_by(marine.park, method, plot_year) %>%
        dplyr::summarise(n    = length(unique(site)),
                         mean = mean(percent_cover),
                         sd   = sd(percent_cover),
                         se   = sd(percent_cover) / sqrt(length(unique(site))))

      # plyr::ddply(dat, plyr::.(plot_year), .inform = TRUE, summarise,
      #       n    = length(unique(site)),
      #       mean = mean(percent_cover),
      #       sd   = sd(percent_cover),
      #       se   = sd(percent_cover) / sqrt(length(unique(site))))

    })

    ####### ►  Create plot for coral cover for one marine park
    output$benthic.park.coralcover.plot <- renderPlot({
      req(benthic_park_coral_cover())

      p <- ggplot(data = subset(benthic_park_coral_cover(), !plot_year %in% c("1999")), aes(x = plot_year, y = mean)) +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .025) +
        # stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, size = 1, col = "black", linetype = "solid") +
        geom_point(size = 2) +
        xlab("") +
        ylab("% Coral Cover (mean ± SE)") +
        ggplot_mpatheme() +
        scale_y_continuous(expand = c(0, 0.1))

      p
    })

    ####### ►  Create marine park specific data for sector/site
    benthic_park_coral_cover_sector <- reactive({
      req(mpa_data, input$benthic.park.coralcover.dropdown)

      dat <- mpa_data$coral_cover_transect %>%
        dplyr::filter(marine.park %in% c(input$benthic.park.coralcover.dropdown))%>%
        dplyr::filter(site %in% c(input$benthic.park.site.coralcover.dropdown)) %>%
        dplyr::group_by(marine.park, method, plot_year, sector, site) %>%
        dplyr::summarise(n    = length(unique(site)),
                         mean = mean(percent_cover),
                         sd   = sd(percent_cover),
                         se   = sd(percent_cover) / sqrt(length(unique(site))))

      # plyr::ddply(dat, plyr::.(plot_year, sector, site), .inform = TRUE, summarise,
      #             n    = length(unique(site)),
      #             mean = mean(percent_cover),
      #             sd   = sd(percent_cover),
      #             se   = sd(percent_cover) / sqrt(length(unique(site))))

    })

    ####### ►  Create plot for coral cover for one marine park by sector
    output$benthic.sector.coralcover.plot <- renderPlot({
      req(benthic_park_coral_cover_sector())

      p <- ggplot(data = subset(benthic_park_coral_cover_sector(), !plot_year %in% c("1999")), aes(x = plot_year, y = mean)) +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .025) +
        # stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, size = 1, col = "black", linetype = "solid") +
        geom_point(size = 2) +
        xlab("") +
        ylab("% Coral Cover (mean ± SE)") +

        scale_y_continuous(expand = c(0, 0.1)) +
        # facet_wrap(sector ~ ., scales = "free", ncol = 1) +
        ggh4x::facet_wrap2(vars(sector), axes = "all", ncol = 1) +
        ggplot_mpatheme()

      p
    })

    ####### ►  Create plot for coral cover for one marine park by site
    output$benthic.site.coralcover.plot <- renderPlot({
      req(benthic_park_coral_cover_sector())

      p <- ggplot(data = subset(benthic_park_coral_cover_sector(), !plot_year %in% c("1999")), aes(x = plot_year, y = mean)) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .025) + # error bars
        # stat_smooth(method = "gam", formula = y ~ poly(x, 3), se = F, size = 0.3, col = "black", linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = 2, colour = "grey") +
        geom_point(size=1) +
        xlab("") +
        ylab(" % Coral Cover (mean ? SE)") +
        scale_y_continuous(expand = c(0, 0.1)) +
        # facet_wrap(site ~ ., scales = "free", ncol = 3) +
        ggh4x::facet_wrap2(vars(site), axes = "all", ncol = 1) +
        ggplot_mpatheme()

      p
    })


    benthic_rec_3c2 <- reactive({
      req(mpa_data, input$benthic.state.park.coralrecruitment.dropdown)

      mpa_data$rec_3c2 %>%
        dplyr::filter(marine.park %in% c(input$benthic.state.park.coralrecruitment.dropdown))
    })

    output$benthic.state.coralrecruitment.all.plot <- renderPlot({
      req(benthic_rec_3c2())

      pd <- position_dodge(.25)

      p <- ggplot(benthic_rec_3c2(), aes(x=year, y=mean)) +
        # geom_smooth(method = "gam", formula = y ~  s(x, k=length(unique(benthic_rec_3c2()$year)-2)), se=F, size = 1, col="black") +
        geom_point(position = pd, size = 2)+
        xlab("") +
        ylab("Number of coral recruits per tile") +
        scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
          expand = expand_scale(mult = c(0, 0.05))
        ) + ggplot_mpatheme() +
        scale_y_continuous(expand = c(0, 0.1)) +
        # facet_wrap(marine.park ~ ., scales = "free", ncol = 1) +
        ggh4x::facet_wrap2(vars(marine.park), axes = "all", ncol = 1) +
        ggplot_mpatheme()

      p
    })


    benthic_coralcover_state_samplingeffort <- reactive({
      req(mpa_data, input$benthic.state.park.coralcover.dropdown)

      mpa_data$coral_cover_metadata %>%
        dplyr::distinct(marine.park, zone, sector, site, latitude, longitude) %>%
        dplyr::filter(marine.park %in% c(input$benthic.state.park.coralcover.dropdown)) %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Site:", site, "</b>", "<br/>",
          "<b>Sector:</b>", sector, "<br/>",
          "<b>Zone:</b>", zone, "<br/>"))%>%
        dplyr::filter(!is.na(latitude))
    })

    output$benthic.state.sampling.leaflet <- renderLeaflet({
      #req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

      map <- leaflet_basemap(benthic_coralcover_state_samplingeffort()) %>%
        fitBounds(
          ~ min(longitude),
          ~ min(latitude),
          ~ max(longitude),
          ~ max(latitude)
        ) %>%

        leaflet::addAwesomeMarkers(lng = ~longitude,
                                   lat = ~latitude,
                                   icon = leaflet::awesomeIcons(
                                     icon = 'surf',
                                     iconColor = 'white',
                                     library = 'fa',
                                     markerColor = 'green'
                                   ),
                                   popup = ~content,
                                   label = ~as.character(site),
                                   group = "Sampling locations"
        ) %>%
        # addMarkers(
        #   lng = ~longitude,
        #   lat = ~latitude,
        #   label = ~ as.character(site),
        #   popup = ~content,
        #   group = "Sampling locations"
        # ) %>%
        addGlPolygons(
          data =  mpa_data$state.mp,
          color = ~ mpa_data$state.pal(zone),
          popup =  mpa_data$state.mp$COMMENTS,
          group = "Marine Parks"
        ) %>%
        addLegend(
          pal = mpa_data$state.pal,
          values = mpa_data$state.mp$zone,
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

    benthic_coralcover_park_samplingeffort <- reactive({
      req(mpa_data, input$benthic.park.coralcover.dropdown)

      mpa_data$coral_cover_metadata %>%
        dplyr::distinct(marine.park, zone, sector, site, latitude, longitude) %>%
        dplyr::filter(marine.park %in% c(input$benthic.park.coralcover.dropdown)) %>%
        dplyr::filter(site %in% c(input$benthic.park.site.coralcover.dropdown)) %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Site:", site, "</b>", "<br/>",
          "<b>Sector:</b>", sector, "<br/>",
          "<b>Zone:</b>", zone, "<br/>"))%>%
        dplyr::filter(!is.na(latitude))
    })

    output$benthic.park.sampling.leaflet <- renderLeaflet({
      #req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

      map <- leaflet_basemap(benthic_coralcover_park_samplingeffort()) %>%
        fitBounds(
          ~ min(longitude),
          ~ min(latitude),
          ~ max(longitude),
          ~ max(latitude)
        ) %>%

        leaflet::addAwesomeMarkers(lng = ~longitude,
                                   lat = ~latitude,
                                   icon = leaflet::awesomeIcons(
                                     icon = 'surf',
                                     iconColor = 'white',
                                     library = 'fa',
                                     markerColor = 'green'
                                   ),
                                   popup = ~content,
                                   label = ~as.character(site),
                                   group = "Sampling locations"
        ) %>%
        # addMarkers(
        #   lng = ~longitude,
        #   lat = ~latitude,
        #   label = ~ as.character(site),
        #   popup = ~content,
        #   group = "Sampling locations"
        # ) %>%
        addGlPolygons(
          data =  mpa_data$state.mp,
          color = ~ mpa_data$state.pal(zone),
          popup =  mpa_data$state.mp$COMMENTS,
          group = "Marine Parks"
        ) %>%
        addLegend(
          pal = mpa_data$state.pal,
          values = mpa_data$state.mp$zone,
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

}
