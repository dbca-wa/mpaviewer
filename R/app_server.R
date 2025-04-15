#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet dplyr grid ggplot2 leafgl forcats cachem leaflegend ggh4x scales
#' @noRd
#'

app_server <- function(input, output, session) {

  load("inst/data/mpa_data.Rdata")

  output$box.total.number.fish <- renderValueBox({

    valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_number_fish,  big.mark = ","), '</font></p>')),
             "Fish counted",
             icon = icon("fish"),
             color = "yellow") # have changed CSS colours
  })

  output$box.total.species.fish <- renderValueBox({
    valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_species_fish,  big.mark = ","), '</font></p>')),

             "Fish species indentified",
             icon = icon("fish"),
             color = "yellow") # have changed CSS colours
  })

  output$statewide_plots <- renderUI({
    print("total fish")
    total_fish <- mpa_data$total_number_fish_park #%>% dplyr::glimpse()
    total_species <- mpa_data$total_species_fish_park # %>% glimpse()
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


            HTML(paste0('<center><img src = "www/images/fish_', park, '_banner.jpg" width = "97%> </center> "')),
               # img(src = paste0("www/images/fish_", park, "_banner.jpg"), align = "center", width = "95%"),#, height = 200 , align = "left"
            br(),
            br(),
            br()
          )

        })

      do.call(tagList, marine_park_summaries)

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
      dplyr::arrange(mean_lat) # removed desc for marine park workshop

    pickerInput(
      inputId = "fish.park.dropdown",
      label = "Choose a marine park:",
      choices = c(unique(lats$marine_park)),
      multiple = FALSE,
      # selected = unique(lats$marine_park)[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })

  # ####### ►  Create method dropdown ----
  observeEvent(input$fish.park.dropdown, {
  output$fish.park.method.dropdown <- renderUI({
    req(input$fish.park.dropdown)

  dat <- mpa_data$methods[marine_park %in% c(input$fish.park.dropdown)]

  glimpse(unique(dat$method))

  selectizeInput("fish.park.method.dropdown", "Choose a method:", choices = c(unique(sort(dat$method))), selected = c(unique(sort(dat$method)))[1])


  }) #%>% bindCache(input$fish.park.dropdown)
  })

  ####### ►  Create a fished species dropdown ----
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
      inputId = "fish.park.fished.species.dropdown",
      width = "100%",
      label = "Choose target species to plot:",
      choices = choices,
      multiple = FALSE,
      selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  ####### ►  Create an all species dropdown ----
  output$UIfish.park.all.species.dropdown <- renderUI({

      dat <- mpa_data$ordered_top_species[marine_park %in% c(input$fish.park.dropdown) &
                                  method %in% c(input$fish.park.method.dropdown)]

      choices <- dat %>%
        dplyr::arrange(desc(total)) %>%
        dplyr::distinct(scientific_name) %>%
        dplyr::pull("scientific_name")

      shinyWidgets::pickerInput(
        inputId = "fish.park.all.species.dropdown",
        label = "Choose species to plot:",
        width = "100%",
        choices = choices,
        multiple = FALSE,
        selected = choices[1],
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  ####### ►  Create a trophic group dropdown ----
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

  ####### ►  Total abundance and Species Richness for leaflets ----
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

  # ####### ►  Overall SR + TA dataframes ----
  # fish_park_overall <- reactive({
  #   req(input$fish.park.dropdown, input$fish.park.method.dropdown)
  #
  #   dat <- mpa_data$ta_sr[marine_park %in% c(input$fish.park.dropdown)]
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

    dat <- mpa_data$ta_sr_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat
  })

  # ####### ►  Sanctuary Species richness ----
  # fish_park_sr_sanctuary <- reactive({
  #   req(fish_park_sanctuary())
  #
  #   dat <- fish_park_sanctuary()[metric %in% c("Species richness")]
  #
  #   dat
  # })

  ####### ►  Sanctuary Total abundance ----
  fish_park_ta_sanctuary <- reactive({
    req(fish_park_sanctuary())

    dat <- fish_park_sanctuary()[metric %in% c("Total abundance")]

    dat
  })

  ####### ►  Site SR + TA plots ----
  fish_park_site <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$ta_sr_site[marine_park %in% c(input$fish.park.dropdown)]
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

    dat <- mpa_data$ta_sr_zone[marine_park %in% c(input$fish.park.dropdown)]
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

  ####### ►  Abundance by species NOT summarised (for leaflets)----
  fish_park_abundance_species_leaflet <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$abundance_leaflet[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    dat <- dat[scientific_name %in% c(input$fish.park.all.species.dropdown)]

    dat

  })

  ####### ►  Metadata for leaflet Abundance by species -----
  fish_park_metadata_leaflet <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$metadata_leaflet[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })


  ####### ►  Abundance by species summarised ----
  fish_park_abundance_species <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$abundance.sum[marine_park %in% c(input$fish.park.dropdown)] # changed from abundance to summarised
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    dat <- dat[scientific_name %in% c(input$fish.park.all.species.dropdown)]

    dat

  })

  ####### ►  Abundance by species by Sanctuary----
  fish_park_abundance_species_sanctuary <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$abundance_sum_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    dat <- dat[scientific_name %in% c(input$fish.park.all.species.dropdown)]

    dat

  })

  ####### ►  Top ten species ----
  fish_park_top_ten <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$top_ten[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Individual Fished species abundance ----
  fish_park_fishedabundance <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished_species_sum[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Individual Fished species abundance by sanctuary ----
  fish_park_fishedabundance_sanctuary <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished_species_sum_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  All fished species summed together ----
  fish_park_fishedsum <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished_sum[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  All fished species summed together by Sanctuary----
  fish_park_fishedsum_sanctuary <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished_sum_sanctuary[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Summarised trophic abundance ----
  # fish_park_trophicabundance <- reactive({
  #   req(input$fish.park.dropdown, input$fish.park.method.dropdown)
  #
  #   dat <- mpa_data$trophic.sum[marine_park %in% c(input$fish.park.dropdown)]
  #   dat <- dat[method %in% c(input$fish.park.method.dropdown)]
  #
  #   dat
  #
  # })

  ####### ►  Complete length ----
  fish_park_fishedcompletelength <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished_complete_length[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  ####### ►  Trends ----
  fish_park_trends <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$interpretation_trends[marine_park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  })

  # STATE DROPDOWNS ----

  # ####### ►  Create method dropdown ----
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

  ####### ►  Sampling effort leaflet ----
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
                       opacity = 1, radius = 7) %>%

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
  ####### ►  Sampling effort leaflet ----
  output$fish.park.sampling.leaflet <- renderLeaflet({

    dat <- fish_park_samplingeffort()

    # Create the basemap with marine parks and legend
    pal <- data.frame(marker_col = c("#F1652C", "yellow", "#2ECFE2","#2ECFE2", "#739AF8"), method = c("stereo-DOVs", "stereo-BRUVs", "stereo-ROVs","stereo-ROVs+UVC", "UVC"))
    dat <- dplyr::left_join(dat, pal, by = "method")

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
                                color = "grey", weight = 1,
                                fillColor = ~marker_col,
                                opacity = 1, radius = 8,
                                group = "Sampling locations") %>%
      addLegend(
        pal = mpa_data$state_pal, values = mpa_data$state_mp$zone, opacity = 1,
        title = "Zones",
        position = "bottomleft", group = "Marine Parks"
      ) %>%
      addLayersControl(
        overlayGroups = c("Sampling locations", "Marine Parks"),
        options = layersControlOptions(collapsed = FALSE)
      )

    map
  })


   ####### ►  Leaflet - Total abundance and species richness ----
  output$fish.park.metric.leaflet <- renderLeaflet({

     ta <- fish_park_ta()
     sr <- fish_park_sr()

     dat <- fish_park_samplingeffort()

     overzero.ta <- filter(ta, value > 0)
     equalzero.ta <- filter(ta, value == 0)
     max.ta <- max(overzero.ta$value)

     overzero.sr <- filter(sr, value > 0)
     equalzero.sr <- filter(sr, value == 0)
     max.sr <- max(overzero.sr$value)

     map <- leaflet_basemap(dat) %>%
       fitBounds(~ min(longitude_dd), ~ min(latitude_dd), ~ max(longitude_dd), ~ max(latitude_dd)) %>%
       addPolygons(
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
                           "Total abundance",
                           "Species richness"),
         options = layersControlOptions(collapsed = FALSE)
       ) %>%
       add_legend_ta(
         colors = c("black", "yellow", "yellow"),
         labels = c(0, round(max.ta / 2), max.ta),
         sizes = c(5, 20, 40), group = "Total abundance"
       ) %>%
       add_legend_sr(
         colors = c("black", "green", "green"),
         labels = c(0, round(max.sr / 2), max.sr),
         sizes = c(5, 20, 40), group = "Species richness"
       )

     if (nrow(overzero.ta)) {
       map <- map %>%
         addCircleMarkers(
           data = overzero.ta, lat = ~latitude_dd, lng = ~longitude_dd,
           radius = ~ (((value / max(value)) * 20)), fillOpacity = 0.5, stroke = FALSE,
           label = ~ as.character(value), group = "Total abundance", color = "yellow"
         )
     }

     if (nrow(equalzero.ta)) {
       map <- map %>%
         addCircleMarkers(
           data = equalzero.ta, lat = ~latitude_dd, lng = ~longitude_dd,
           radius = 2, fillOpacity = 0.5, color = "black", stroke = FALSE,
           label = ~ as.character(value), group = "Total abundance"
         )
     }

     if (nrow(overzero.sr)) {
       map <- map %>%
         addCircleMarkers(
           data = overzero.sr, lat = ~latitude_dd, lng = ~longitude_dd,
           radius = ~ ((value / max(value)) * 20), fillOpacity = 0.5, stroke = FALSE,
           label = ~ as.character(value), group = "Species richness", color = "green"
         )
     }

     if (nrow(equalzero.sr)) {
       map <- map %>%
         addCircleMarkers(
           data = equalzero.sr, lat = ~latitude_dd, lng = ~longitude_dd,
           radius = 2, fillOpacity = 0.5, color = "black", stroke = FALSE,
           label = ~ as.character(value), group = "Species richness"
         )
     }

     map %>%
       hideGroup("Species richness")
   })

  ####### ►  Acknowledgements  ----

  observeEvent(input$fish.park.dropdown, {
    output$acknowledgement <- renderText({
      #req(input$fish.park.dropdown)
      HTML(ack <- if_else(mpa_data$acknowledgements$park == input$fish.park.dropdown, mpa_data$acknowledgements$acknowledgement, " "))
      #HTML(ack <- paste("hello", "Line two", sep = "<br/>"))
    })
  })

  ####### ► CTI Metric ----

  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.cti <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_cti.png"), align = "left", width = "100%")
    })
  })



  ####### ►  Total abundance ----

  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.total.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_total_abundance.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Total abundance by site ----
  observeEvent(input$fish.park.dropdown, {

    output$fish.park.total.site.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      if(input$fish.park.method.dropdown %in% c("stereo-DOVs", "stereo-ROVs")){
        # req(input$fish.park.dropdown, input$fish.park.method.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown


        tagList(h4("Total abundance by site:"),
                img(src = paste0("www/plots/", park, "_", method, "_total_abundance_site.png"), align = "left", width = "100%"))

      }
    })

  })

  ####### ►  Total abundance by Sanctuary ----
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.total.sanctuary.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_total_abundance_sanctuary.png"), align = "left", width = "100%")
    })
  })

# TODO move to the generate data function
  pal <- c("Sanctuary" = "#C0D435",
           "SP Scientific Reference" = "#CAC3D5",
           "Recreation" = "#F8EE75",
           "General Use" = "#BBE3EF",
           "SP Benthic Protection" = "#CAC3D5",
           "Outside Park" = "#bebebe",
           "Conservation Area" = "#C0B134",
           "Marine Management Area" = "#b7cfe1",
           "SP Seagrass Protection" = "#AB9ECC",
           "Marine Nature Reserve" = "#bfd054",
           "SP Habitat Protection" = "#CAC3D5",
           "Shore Based Activities" = "#231D1D",
           "SP Wildlife Conservation" = "#7C7CB8")


  ####### ►  Total abundance by Zone ----
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.total.zone.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_total_abundance_zone.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Species richness ----
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.rich.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_species_richness.png"), align = "left", width = "100%")
      # img(src = paste0("www/images/fish_", park, ".jpg"), align = "right", width = "100%") # removed www from URL to get rid of golem
    })
  })

  ####### ►  Species richness by site ----
  observeEvent(input$fish.park.dropdown, {

    output$fish.park.rich.site.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      if(input$fish.park.method.dropdown %in% c("stereo-DOVs", "stereo-ROVs")){
        # req(input$fish.park.dropdown, input$fish.park.method.dropdown)
        park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        method <- input$fish.park.method.dropdown


        tagList(h4("Species richness by site:"),
                img(src = paste0("www/plots/", park, "_", method, "_species_richness_site.png"), align = "left", width = "100%"))

      }
    })

  })

  ####### ►  Species richness by Sanctuary ----
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.rich.sanctuary.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_species_richness_sanctuary.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Species richness by Zone ----
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.rich.zone.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_species_richness_zone.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Stacked Abundance Plot ----
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.stack.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_top10.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Trophic group ----
  observeEvent(input$fish.park.dropdown, {
    output$fish.park.trophic.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_trophic.png"), align = "left", width = "100%")
    })
  })

  ####### ►  KDE plot ----
  # 20 individuals min to plot a line (KDE). Depends on scale. If zones in one year then you need 20 in the zone for that year.
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.fished.species.kde.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.fished.species.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown
      species <- input$fish.park.fished.species.dropdown

      print(dir())

      file <- paste0("inst/app/www/plots/species/", park, "_", method, "_", species, "_KDE.png")

      message(file)

      if (file.exists(isolate(file))) {
        print("The file exists!")

        tagList(h4(paste0("Kernel Density Estimate: ", species)),
                img(src = paste0("www/plots/species/", park, "_", method, "_", species, "_KDE.png"), align = "left", width = "100%"))

      } else {

        print("The file does not exist.")

        tagList(h5("Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections"))

      }


      # req(fish_park_fishedcompletelength())
      #
      # more.than.20 <- fish_park_fishedcompletelength() %>%
      #   dplyr::group_by(marine_park, method, campaignid, status, scientific_name_name) %>%
      #   dplyr::summarise(number = sum(number)) %>%
      #   dplyr::filter(number > 20) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::distinct(marine_park, method, campaignid, status, scientific_name_name)
      #
      # dat <- fish_park_fishedcompletelength()[scientific_name %in% c(input$fish.park.fished.species.dropdown)]
      # dat <- dat[length > 0]
      # dat <- dat[!is.na(length)]
      # dat <- dat[complete %in% c("Consistently sampled")]
      #
      # dat <- dat %>%
      #   dplyr::semi_join(more.than.20)
      #
      # # TODO could work this out before the server
      # validate(
      #   need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
      # )




    })
  })



  # output$fish.park.fished.species.kde.plot <- renderPlot({
  #   req(fish_park_fishedcompletelength())
  #
  #   more.than.20 <- fish_park_fishedcompletelength() %>%
  #     dplyr::group_by(marine_park, method, campaignid, status, scientific_name) %>%
  #     dplyr::summarise(number = sum(number)) %>%
  #     dplyr::filter(number > 20) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(marine_park, method, campaignid, status, scientific_name)
  #
  #   dat <- fish_park_fishedcompletelength()[scientific_name %in% c(input$fish.park.fished.species.dropdown)]
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
  #     scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
  #     ylab("Weighted KDE (*1000)") +
  #     xlab("Total Length (mm)") +
  #     ggplot_mpatheme() +
  #     facet_grid(rows = vars(year))
  # })

  # output$ui.fish.park.fished.species.kde.plot <- renderUI({
  #   more.than.20 <- fish_park_fishedcompletelength() %>%
  #     dplyr::group_by(marine_park, method, campaignid, status, scientific_name) %>%
  #     dplyr::summarise(number = sum(number)) %>%
  #     dplyr::filter(number > 20) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(marine_park, method, campaignid, status, scientific_name)
  #
  #   dat <- fish_park_fishedcompletelength()[scientific_name %in% c(input$fish.park.fished.species.dropdown)]
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
  #     p.height <- 175 * length(unique(dat$year))
  #
  #   plotOutput("fish.park.fished.species.kde.plot", height = p.height)
  # })

  ####### ►  KDE plot by sanctuary ----
  # # 20 individuals min to plot a line (KDE). Depends on scale. If zones in one year then you need 20 in the zone for that year.
  # output$fish.park.fished.species.kde.sanctuary.plot <- renderPlot({
  #   req(fish_park_fishedcompletelength())
  #
  #   more.than.20 <- fish_park_fishedcompletelength() %>%
  #     dplyr::group_by(marine_park, method, campaignid, status, scientific_name, dbca_sanctuary) %>%
  #     dplyr::summarise(number = sum(number)) %>%
  #     dplyr::filter(number > 20) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(marine_park, method, campaignid, status, scientific_name, dbca_sanctuary)
  #
  #   dat <- fish_park_fishedcompletelength()[scientific_name %in% c(input$fish.park.fished.species.dropdown)]
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
  #     dplyr::group_by(marine_park, method, campaignid, status, scientific_name, dbca_sanctuary) %>%
  #     dplyr::summarise(number = sum(number)) %>%
  #     dplyr::filter(number > 20) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::distinct(marine_park, method, campaignid, status, scientific_name, dbca_sanctuary)
  #
  #   dat <- fish_park_fishedcompletelength()[scientific_name %in% c(input$fish.park.fished.species.dropdown)]
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
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.fished.sum.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_sum_targets.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Summed Fished Species by Sanctuary ----
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.fished.sum.sanctuary.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_sum_targets_sanctuary.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Summed All Fished Species ----
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.fished.all.sum.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_sum_all_targets.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Summed All Fished Species by Sanctuary ----
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.fished.all.sum.sanctuary.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown

      img(src = paste0("www/plots/", park, "_", method, "_sum_all_targets_sanctuary.png"), align = "left", width = "100%")
    })
  })

  ####### ►  Individual Fished species abundance ----
  # Only includes consistently sampled sites
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.fished.species.abundance.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.fished.species.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown
      species <- stringr::str_replace_all(input$fish.park.fished.species.dropdown, c(" Targeted" = "", " Highly retained" = ""), "")

      tagList(h4(paste0("Total abundance of ", species, ":")),
              img(src = paste0("www/plots/species/", park, "_", method, "_", species, ".png"), align = "left", width = "100%"))


    })
  })

  ####### ►  Individual Fished species abundance by Sanctuary ----
  # Only includes consistently sampled sites
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.fished.species.abundance.sanctuary.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.fished.species.dropdown)

      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown
      species <- stringr::str_replace_all(input$fish.park.fished.species.dropdown, c(" Targeted" = "", " Highly retained" = ""), "")

      tagList(h4(paste0("Total abundance of ", species, " by sanctuary:")),
              img(src = paste0("www/plots/species/", park, "_", method, "_", species, "_sanctuary.png"), align = "left", width = "100%"))
    })
  })

  ####### ►  Individual Species abundance ----
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.all.species.abundance.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown
      species <- input$fish.park.all.species.dropdown

      tagList(h4(paste0("Total abundance of ", species, ":")),
              img(src = paste0("www/plots/species/", park, "_", method, "_", species, ".png"), align = "left", width = "100%"))
    })
  })

  ####### ►  Individual Species abundance by Sanctuary ----
  observeEvent(input$fish.park.method.dropdown, {
    output$fish.park.all.species.abundance.sanctuary.plot <- renderUI({
      req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)
      park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
      method <- input$fish.park.method.dropdown
      species <- input$fish.park.all.species.dropdown

      tagList(h4(paste0("Total abundance of ", species, " by sanctuary:")),
              img(src = paste0("www/plots/species/", park, "_", method, "_", species, "_sanctuary.png"), align = "left", width = "100%"))
    })
  })

  ####### ►  Leaflet - All species abundance ----
  output$fish.park.all.species.leaflet <- renderLeaflet({

    req(fish_park_abundance_species_leaflet())

    dat <- fish_park_abundance_species_leaflet()

    dat <- dplyr::full_join(dat, fish_park_metadata_leaflet()) %>%
      tidyr::replace_na(list(maxn = 0))

    print(unique(dat$scientific_name))

    # TODO I don't think this is the complete data?!?!?!?

    overzero.ta <- dplyr::filter(dat, maxn > 0) #%>% dplyr::glimpse()
    equalzero.ta <- dplyr::filter(dat, maxn == 0) #%>% dplyr::glimpse()

    print(max(overzero.ta$maxn))

    max.ta <- base::max(overzero.ta$maxn)
    print(max.ta)


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

    dat <- mpa_data$foa.codes[scientific_name %in% c(input$fish.state.all.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat))
    frame

  })

  ####### ►  State Fished species ----
  output$fish.state.fished.species.iframe <- renderUI({

    dat <- mpa_data$foa.codes[scientific_name %in% c(input$fish.state.fished.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), height = 600, width = "100%")
    frame

  })

  ####### ►  Marine Park All species ----
  output$fish.park.all.species.iframe <- renderUI({

    dat <- mpa_data$foa_codes[scientific_name %in% c(input$fish.park.all.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), width = "100%", height = 600, class = "frame")
    frame
  })

  ####### ►  Marine Park Fished species ----
  output$fish.park.fished.species.iframe <- renderUI({

    dat <- mpa_data$foa_codes[scientific_name %in% c(input$fish.park.fished.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), height = 800, width = "100%")
    frame

  })

  observeEvent(input$park.cti,
               showModal(modalDialog(
                 title = "What is community temperature index (CTI)?",
                 includeHTML("inst/app/www/popups/fish.cti.html")))
  )

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


    # CORAL ----
    # State data ----

  output$box.total.number.coral <- renderValueBox({
    valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_number_coral,  big.mark = ","), '</font></p>')),
             "Images Analysed",
             icon = tags$i(class = "fa-solid fa-camera", style="font-size: 50px"),
             color = "yellow") # have changed CSS colours
  })

  output$box.total.species.coral <- renderValueBox({
    valueBox(HTML(paste0('<p style = "color:#ffffff";><font size="+3">', prettyNum(mpa_data$total_species_coral,  big.mark = ","), '</font></p>')),
             "Total Genera Indentified",
             icon = tags$i(img(src = "www/coral.png", width="50px")),
             color = "yellow") # have changed CSS colours

  })
    ####### ►  Create a marine park dropdown ----
    output$benthic.state.park.coralcover.dropdown <- renderUI({
      pickerInput(
        inputId = "benthic.state.park.coralcover.dropdown",
        label = "Choose Marine Parks to include:",
        choices = c(unique(mpa_data$coral_cover_metadata$marine_park)), #choices,
        multiple = TRUE,
        selected = c(unique(mpa_data$coral_cover_metadata$marine_park)), # choices,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    })

    output$benthic.state.park.coralrecruitment.dropdown <- renderUI({
      pickerInput(
        inputId = "benthic.state.park.coralrecruitment.dropdown",
        label = "Choose Marine Parks to include:",
        choices = c(unique(mpa_data$rec_3b$marine_park)), #choices,
        multiple = TRUE,
        selected = c(unique(mpa_data$rec_3b$marine_park)), # choices,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
      )
    })

    ####### ►  Filter coral cover data to marine park and summarise per marine park per year
    benthic_state_coral_cover <- reactive({
      req(mpa_data, input$benthic.state.park.coralcover.dropdown)

      dat <- mpa_data$coral_cover_transect %>%
        dplyr::filter(marine_park %in% c(input$benthic.state.park.coralcover.dropdown)) %>%
        dplyr::group_by(marine_park, method, plot_year) %>%
        dplyr::summarise(n    = length(unique(site)),
                         mean = mean(percent_cover),
                         sd   = sd(percent_cover),
                         se   = sd(percent_cover) / sqrt(length(unique(site))))



      # plyr::ddply(dat, plyr::.(marine_park, method, plot_year), .inform=TRUE, dplyr::summarise,
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
        # facet_wrap(marine_park ~ ., scales = "free", ncol = 1) +
        ggh4x::facet_wrap2(vars(marine_park), axes = "all", ncol = 1) +
        ggplot_mpatheme()

      p
    })

    # Marine park data ----
    ####### ►  Create a marine park dropdown ----
    output$benthic.park.coralcover.dropdown <- renderUI({

      options <- mpa_data$coral_cover_metadata %>%
        dplyr::distinct(marine_park) %>%
        dplyr::pull("marine_park")

      create_dropdown("benthic.park.coralcover.dropdown", options, "Choose a marine park:", FALSE)

    })

    ####### ►  Create a site dropdown ----
    output$benthic.park.site.coralcover.dropdown <- renderUI({
      options <- mpa_data$coral_cover_metadata %>%
        dplyr::group_by(marine_park, site) %>%
        dplyr::summarise(n = length(plot_year)) %>%
        dplyr::filter(n > 2) %>%
        dplyr::filter(marine_park %in% c(input$benthic.park.coralcover.dropdown)) %>%
        dplyr::distinct(site) %>%
        dplyr::arrange() %>%
        dplyr::pull("site")

      create_dropdown("benthic.park.site.coralcover.dropdown", options, "Choose a site:", FALSE)

      # pickerInput(
      #   inputId = "benthic.park.site.coralcover.dropdown",
      #   label = "Choose sites to include:",
      #   choices = sort(options),
      #   multiple = TRUE,
      #   selected = options,
      #   options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE),
      #   width = "100%"
      # )
    })

    ####### ►  Create marine park specific data
    benthic_park_coral_cover <- reactive({
      req(mpa_data, input$benthic.park.coralcover.dropdown)

      dat <- mpa_data$coral_cover_transect %>%
        dplyr::filter(marine_park %in% c(input$benthic.park.coralcover.dropdown)) %>%
        dplyr::filter(site %in% c(input$benthic.park.site.coralcover.dropdown)) %>%
        dplyr::group_by(marine_park, method, plot_year) %>%
        dplyr::summarise(n    = length(unique(site)),
                         mean = mean(percent_cover),
                         sd   = sd(percent_cover),
                         se   = sd(percent_cover) / sqrt(length(unique(site))))

    })

    ####### ►  Create plot for coral cover for one marine park ----

    observeEvent(input$benthic.park.coralcover.dropdown, {

      output$benthic.park.coralcover.plot <- renderUI({
        req(input$benthic.park.coralcover.dropdown)
        park <- stringr::str_replace_all(tolower(input$benthic.park.coralcover.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        img(src = paste0("www/plots/", "Coral_", park, "_coral_cover.png"), align = "left", width = "100%")
      })
    })

    ####### ► Coral Reefzone plot ----
    observeEvent(input$benthic.park.coralcover.dropdown, {
      output$benthic.park.reefzone.coralcover.plot <- renderUI({
        req(input$benthic.park.coralcover.dropdown)

        park <- stringr::str_replace_all(tolower(input$benthic.park.coralcover.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))

        img(src = paste0("www/plots/", "Coral_", park, "_reefzone_coral_cover.png"), align = "left", width = "100%")
      })
    })
    ####### ► Coral Top 5 plot ----
    observeEvent(input$benthic.park.coralcover.dropdown, {
      output$benthic.park.top5.coralcover.plot <- renderUI({
        req(input$benthic.park.coralcover.dropdown)

        park <- stringr::str_replace_all(tolower(input$benthic.park.coralcover.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))

        img(src = paste0("www/plots/", "Coral_", park, "_species_coral_cover.png"), align = "left", width = "100%")
      })
    })

    ####### ► Coral per site plot ----
    observeEvent(input$benthic.park.site.coralcover.dropdown, {
      output$benthic.site.coralcover.plot <- renderUI({
        req(input$benthic.park.coralcover.dropdown, input$benthic.park.site.coralcover.dropdown)

        park <- stringr::str_replace_all(tolower(input$benthic.park.coralcover.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        site <- input$benthic.park.site.coralcover.dropdown

        img(src = paste0("www/plots/", "Coral_", park, "_site_", site, "_species_coral_cover.png"), align = "left", width = "100%")
      })
    })


    ####### ► Coral per site facet plot ----
    observeEvent(input$benthic.park.coralcover.dropdown, {

      output$benthic.site.coralcover.plot.facet <- renderUI({
        req(input$benthic.park.coralcover.dropdown)
        park <- stringr::str_replace_all(tolower(input$benthic.park.coralcover.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
        img(src = paste0("www/plots/", "Coral_", park, "_site_coral_cover.png"), align = "left", width = "100%")
      })
    })


    # output$benthic.park.coralcover.plot <- renderPlot({
    #   req(benthic_park_coral_cover())
    #
    #   # p <- ggplot(data = subset(benthic_park_coral_cover(), !plot_year %in% c("1999")), aes(x = plot_year, y = mean)) +
    #   #   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .025) +
    #   #   # stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, size = 1, col = "black", linetype = "solid") +
    #   #   geom_point(size = 2) +
    #   #   xlab("") +
    #   #   ylab("% Coral Cover (mean ± SE)") +
    #   #   ggplot_mpatheme() +
    #   #   scale_y_continuous(expand = c(0, 0.1))
    #   #
    #   # p
    # })

    ####### ►  Create marine park specific data for sector/site
    benthic_park_coral_cover_sector <- reactive({
      req(mpa_data, input$benthic.park.coralcover.dropdown)

      dat <- mpa_data$coral_cover_transect %>%
        dplyr::filter(marine_park %in% c(input$benthic.park.coralcover.dropdown))%>%
        dplyr::filter(site %in% c(input$benthic.park.site.coralcover.dropdown)) %>%
        dplyr::group_by(marine_park, method, plot_year, sector, site) %>%
        dplyr::summarise(n    = length(unique(site)),
                         mean = mean(percent_cover),
                         sd   = sd(percent_cover),
                         se   = sd(percent_cover) / sqrt(length(unique(site))))

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
        dplyr::filter(marine_park %in% c(input$benthic.state.park.coralrecruitment.dropdown))
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
        # facet_wrap(marine_park ~ ., scales = "free", ncol = 1) +
        ggh4x::facet_wrap2(vars(marine_park), axes = "all", ncol = 1) +
        ggplot_mpatheme()

      p
    })


    benthic_coralcover_state_samplingeffort <- reactive({
      req(mpa_data)

      mpa_data$coral_sampling_effort %>%
        dplyr::distinct(marine_park, status, site, latitude_dd, longitude_dd) %>%
        #dplyr::filter(marine_park %in% c(input$benthic.state.park.coralcover.dropdown)) %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Marine Park:</b>", marine_park, "<br/>",
          "<b>Site:", site, "</b>", "<br/>",
          "<b>Zone:</b>", status, "<br/>"))%>%
        dplyr::filter(!is.na(latitude_dd))
    })

    output$benthic.state.sampling.leaflet <- renderLeaflet({
      #req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
      dat <-  mpa_data$coral_sampling_effort %>%
        dplyr::distinct(marine_park, site, latitude_dd, longitude_dd) %>%
        #dplyr::filter(marine_park %in% c(input$benthic.state.park.coralcover.dropdown)) %>%
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
                                  # group = "Sampling locations") %>%


        # leaflet::addAwesomeMarkers(lng = ~longitude_dd,
        #                            lat = ~latitude_dd,
        #                            icon = leaflet::awesomeIcons(
        #                              icon = 'surf',
        #                              iconColor = 'white',
        #                              library = 'fa',
        #                              markerColor = ~marker_col
        #                            ),
        #                            popup = ~content,
        #                            label = ~as.character(site)
        #                            #group = "Sampling locations"
        # addMarkers(
        #   lng = ~longitude_dd,
        #   lat = ~latitude_dd,
        #   label = ~ as.character(site),
        #   popup = ~content,
        #   group = "Sampling locations"
        # ) %>%
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

    benthic_coralcover_park_samplingeffort <- reactive({
      req(input$benthic.park.coralcover.dropdown)

      dat <- mpa_data$coral_sampling_effort[marine_park %in% c(input$benthic.park.coralcover.dropdown)]

      dat %>%
        dplyr::mutate(content = paste(
          sep = " ",
          "<b>Sector:</b>", sector, "<br/>",
          "<b>Site:", site, "</b>", "<br/>",
          "<b>Status:</b>", status, "<br/>",
          "<b>Depth:</b>", depth_m, "m", "<br/>",
          "<b>Reef Zone:</b>", reef_zone, "<br/>"))

    })

    output$benthic.park.sampling.leaflet <- renderLeaflet({
      #req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

      map <- leaflet_basemap(benthic_coralcover_park_samplingeffort()) %>%
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
        # addMarkers(
        #   lng = ~longitude_dd,
        #   lat = ~latitude_dd,
        #   label = ~ as.character(site),
        #   popup = ~content,
        #   group = "Sampling locations"
        # ) %>%
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
}

