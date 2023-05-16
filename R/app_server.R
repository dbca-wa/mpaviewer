#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet dplyr grid ggplot2 leafgl rgdal forcats cachem leaflegend ggh4x
#' @noRd
#'

app_server <- function(input, output, session) {

  # session$cache <- cachem::cache_mem(max_size = 500e6)


  # mpa_data <- if (!fs::file_exists(fn_mpa_data)) {
  #   # Download mpa_data.rds from the DBCA CKAN data catalogue
  #   # TODO schedule download_data() to run e.g. every hour
  #   # TODO automate generate_data() after moving all data assets to DBCA's CKAN
  #   message("Data file not found, downloading from DBCA data catalogue...")
  #   download_data()
  # } else {
  #   reactiveFileReader(
  #     1000, # Poll data file for changes every 1 second
  #     NULL, # across sessions
  #     fn_mpa_data, # relative to project or Dockerfile workdir
  #     readRDS # using function readRDS
  #   )
  # }

  shinyOptions(cache = cachem::cache_disk("./bind-cache"))

  # mpa_data <- reactive({readRDS(here::here("inst/data/mpa_data.rds"))})
  load("inst/data/mpa_data1.Rdata")

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
  #### STATE DROPDOWNS ----
  ####### ►  Create a marine park dropdown ----
  output$fish.state.park.dropdown <- renderUI({

    lats <- mpa_data$lats %>%
      dplyr::arrange(desc(mean.lat))

    pickerInput(
      inputId = "fish.state.park.dropdown",
      label = "Choose Marine Parks to include:",
      choices = c(unique(lats$marine.park)), #choices,
      multiple = TRUE,
      selected = c(unique(lats$marine.park)), # choices,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })

  ####### ►  Create method dropdown ----
  output$fish.state.method.dropdown <- renderUI({

    req(input$fish.state.park.dropdown)

    dat <- mpa_data$metadata[marine.park %in% c(input$fish.state.park.dropdown)]

    choices <- dat %>%
      dplyr::distinct(method) %>%
      dplyr::pull("method")

    create_dropdown("fish.state.method.dropdown", choices, "Choose a method:", FALSE)

  })# %>% bindCache(input$fish.state.park.dropdown)

  ####### ►  Create a fished species dropdown ----
  output$fish.state.fished.species.dropdown <- renderUI({

    dat <- mpa_data$fished.complete.length[marine.park %in% c(input$fish.state.park.dropdown) &
                                             method %in% c(input$fish.state.method.dropdown) &
                                             number > 0]
    choices <- dat %>%
      dplyr::group_by(scientific) %>%
      dplyr::summarise(total = sum(number)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::distinct(scientific) %>%
      dplyr::pull("scientific")

    pickerInput(
      inputId = "fish.state.fished.species.dropdown",
      label = "Choose target species to plot:",
      choices = choices,
      multiple = FALSE,
      selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  }) # %>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

  ####### ►  Create all species dropdown ----
  output$fish.state.all.species.dropdown <- renderUI({

    dat <- mpa_data$abundance[marine.park %in% c(input$fish.state.park.dropdown) &
                                method %in% c(input$fish.state.method.dropdown) &
                                maxn > 0]

    choices <- dat %>%
      dplyr::group_by(scientific) %>%
      dplyr::summarise(total = sum(maxn)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::distinct(scientific) %>%
      dplyr::pull("scientific")

    shinyWidgets::pickerInput(
      inputId = "fish.state.all.species.dropdown",
      label = "Choose species to plot:",
      choices = choices,
      multiple = FALSE,
      selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

  ####### ►  Create a trophic group dropdown ----
  output$fish.state.trophic.dropdown <- renderUI({

    dat <- mpa_data$trophic.abundance[marine.park %in% c(input$fish.state.park.dropdown) &
                                method %in% c(input$fish.state.method.dropdown)]

    choices <- dat %>%
      dplyr::distinct(trophic.group) %>%
      dplyr::pull("trophic.group")

    pickerInput(
      inputId = "fish.state.trophic.dropdown",
      label = "Choose trophic groups to plot:",
      choices = sort(choices),
      multiple = TRUE,
      selected = sort(choices)[1:3],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

  #### MARINE PARK DROPDOWNS ----
  ####### ►  Create a marine park dropdown ----
  output$fish.park.dropdown <- renderUI({

    lats <- mpa_data$lats %>%
      dplyr::arrange(desc(mean.lat))

    pickerInput(
      inputId = "fish.park.dropdown",
      label = "Choose a marine park:",
      choices = c(unique(lats$marine.park)),
      multiple = FALSE,
      # selected = unique(lats$marine.park)[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })

  ####### ►  Create method dropdown ----
  output$fish.park.method.dropdown <- renderUI({
    req(input$fish.park.dropdown)

    print("park chosen")
    print(input$fish.park.dropdown)

    dat <- mpa_data$metadata[marine.park %in% c(input$fish.park.dropdown)]

    print(unique(dat$method))

    choices <- dat %>%
      dplyr::distinct(method) %>%
      dplyr::pull("method")

    print("choices")
    print(choices)

    # pickerInput(
    #   inputId = "fish.park.method.dropdown",
    #   label = "Choose a method:",
    #   choices = c(choices),
    #   multiple = FALSE,
    #   selected = choices[1],
    #   options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    # )

    create_dropdown("fish.park.method.dropdown", choices, "Choose a method:", FALSE)
  }) #%>% bindCache(input$fish.park.dropdown)

  ####### ►  Create a fished species dropdown ----
  output$fish.park.fished.species.dropdown <- renderUI({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.complete.length[marine.park %in% c(input$fish.park.dropdown) &
                                             method %in% c(input$fish.park.method.dropdown) &
                                             number > 0]
    choices <- dat %>%
      dplyr::group_by(scientific) %>%
      dplyr::summarise(total = sum(number)) %>%
      dplyr::ungroup() %>%
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

      dat <- mpa_data$abundance[marine.park %in% c(input$fish.park.dropdown) &
                                  method %in% c(input$fish.park.method.dropdown) &
                                  maxn > 0]

      choices <- dat %>%
        dplyr::group_by(scientific) %>%
        dplyr::summarise(total = sum(maxn)) %>%
        dplyr::ungroup() %>%
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

    dat <- mpa_data$trophic.abundance[marine.park %in% c(input$fish.park.dropdown) &
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
  fish_alldata <- reactive({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

    dat <- mpa_data$all.data[marine.park %in% c(input$fish.state.park.dropdown)]
    dat <- dat[method %in% c(input$fish.state.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

  fish_park_alldata <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$all.data[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  fish_ta <- reactive({
    req(fish_alldata())

    fish_alldata()[metric %in% c("Total abundance")]

  })

  fish_sr <- reactive({
    req(fish_alldata())

    fish_alldata()[metric %in% c("Species richness")]

  })

  fish_park_ta <- reactive({
    req(fish_park_alldata())

    fish_park_alldata()[metric %in% c("Total abundance")]

  })

  fish_park_sr <- reactive({
    req(fish_park_alldata())

    fish_park_alldata()[metric %in% c("Species richness")]

  })

  fish_samplingeffort <- reactive({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

    dat <- mpa_data$sampling.effort[marine.park %in% c(input$fish.state.park.dropdown)]
    dat <- dat[method %in% c(input$fish.state.method.dropdown)]

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
  }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

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
  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  fish_abundance <- reactive({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

    dat <- mpa_data$abundance[marine.park %in% c(input$fish.state.park.dropdown)]
    dat <- dat[method %in% c(input$fish.state.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

  fish_fishedabundance <- reactive({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

    dat <- mpa_data$fished.abundance[marine.park %in% c(input$fish.state.park.dropdown)]
    dat <- dat[method %in% c(input$fish.state.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

  fish_park_abundance <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$abundance[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  fish_park_abundance_species <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data$abundance[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]
    dat <- dat[scientific %in% c(input$fish.park.all.species.dropdown)]

    dat

  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown, input$fish.park.all.species.dropdown)


  fish_park_fishedabundance <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.abundance[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  fish_park_trophicabundance <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$trophic.abundance[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)


  fish_park_fishedcompletelength <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$fished.complete.length[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)


  fish_park_trends <- reactive({
    req(input$fish.park.dropdown, input$fish.park.method.dropdown)

    dat <- mpa_data$interpretation.trends[marine.park %in% c(input$fish.park.dropdown)]
    dat <- dat[method %in% c(input$fish.park.method.dropdown)]

    dat

  }) #%>% bindCache(input$fish.park.dropdown, input$fish.park.method.dropdown)

  #----------------------------------------------------------------------------#
  #### STATE PLOTS ----
  ####### ►  Sampling effort leaflet ----
  output$fish.state.sampling.leaflet <- renderLeaflet({

    map <- leaflet_basemap(fish_samplingeffort()) %>%
      leaflet::fitBounds(
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
                                 label = ~as.character(sample),
                                 group = "Sampling locations"
      ) %>%
      # leaflet::addMarkers(
      #   lng = ~longitude,
      #   lat = ~latitude,
      #   label = ~ as.character(sample),
      #   popup = ~content,
      #   group = "Sampling locations"
      # ) %>%
      addGlPolygons(
        data =  mpa_data$state.mp,
        color = ~ mpa_data$state.pal(zone),
        popup =  mpa_data$state.mp$COMMENTS,
        group = "Marine Parks"
      ) %>%
      # TODO fix this
      addLegend(
        pal = mpa_data$state.pal,
        values = mpa_data$state.mp$zone,
        opacity = 1,
        title = "Zones",
        position = "bottomleft",
        group = "Marine Parks"
      ) %>%
      addLayersControl(
        overlayGroups = c(
          "Sampling locations",
          "Marine Parks"),
        options = layersControlOptions(collapsed = FALSE)
      )

    map
  }) #%>% bindCache(fish_samplingeffort())

  ####### ►  Leaflet Total Abundance and Species Richness ----
  output$fish.state.metric.leaflet <- renderLeaflet({

    overzero.ta <- dplyr::filter(fish_ta(), value > 0)
    equalzero.ta <- dplyr::filter(fish_ta(), value == 0)
    max.ta <- max(overzero.ta$value)

    overzero.sr <- dplyr::filter(fish_sr(), value > 0)
    equalzero.sr <- dplyr::filter(fish_sr(), value == 0)
    max.sr <- max(overzero.sr$value)

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
        position = "bottomleft",
        group = "Marine Parks"
      ) %>%
      addLayersControl(
        overlayGroups = c(
          "Marine Parks",
          "Total abundance",
          "Species richness"
        ),
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
          data = overzero.ta,
          lat = ~latitude,
          lng = ~longitude,
          radius = ~ (((value / max(value)) * 20)),
          fillOpacity = 0.5,
          stroke = FALSE,
          label = ~ as.character(value),
          group = "Total abundance",
          color = "yellow"
        )
    }

    if (nrow(equalzero.ta)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero.ta,
          lat = ~latitude,
          lng = ~longitude,
          radius = 2,
          fillOpacity = 0.5,
          color = "black",
          stroke = FALSE,
          label = ~ as.character(value),
          group = "Total abundance"
        )
    }

    if (nrow(overzero.sr)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero.sr,
          lat = ~latitude,
          lng = ~longitude,
          radius = ~ ((value / max(value)) * 20),
          fillOpacity = 0.5,
          stroke = FALSE,
          label = ~ as.character(value),
          group = "Species richness",
          color = "green"
        )
    }

    if (nrow(equalzero.sr)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero.sr,
          lat = ~latitude,
          lng = ~longitude,
          radius = 2,
          fillOpacity = 0.5,
          color = "black",
          stroke = FALSE,
          label = ~ as.character(value),
          group = "Species richness"
        )
    }

    map %>%
      # hideGroup("Total abundance") %>%
      hideGroup("Species richness")
  }) #%>% bindCache(fish_ta())

  ####### ►  Total abundance ----
  output$fish.state.total.plot <- renderPlot({
    req(fish_ta())

    label <- grobTree(textGrob(as.character("Total abundance"),
                               x = 0.01, y = 0.97, hjust = 0,
                               gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    ))

    p <- ggplot(
      fish_ta(),
      aes(x = year, y = value, fill = status)
    ) +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 6,
        col = "black",
        position = position_dodge(width = 0.5)
      ) +
      stat_summary(
        fun.min = se.min,
        fun.max = se.max,
        geom = "errorbar",
        width = 0.1,
        col = "black",
        position = position_dodge(width = 0.5)
      ) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
      ggh4x::facet_wrap2(vars(marine.park), axes = "all", ncol = 1, scales = "free_y") +
      scale_x_continuous(
         breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
         expand = expand_scale(mult = c(0, 0.05)))+
      ggplot_mpatheme()


    p

  }) %>% bindCache(fish_ta())

  output$ui.fish.state.total.plot <- renderUI({

    plotOutput("fish.state.total.plot", height = length(unique(fish_ta()$marine.park))*200)

  }) %>% bindCache(fish_ta())

  ####### ►  Species richness ----
  output$fish.state.rich.plot <- renderPlot({
    req(fish_sr())

    label <- grobTree(textGrob(as.character("Species richness"),
                               x = 0.01, y = 0.97, hjust = 0,
                               gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    ))

    ggplot(fish_sr(), aes(x = year, y = value, fill = status, group = status)) + # , col = status
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(marine.park), axes = "all", ncol = 1, scales = "free_y") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      )+
      ggplot_mpatheme()
  }) %>% bindCache(fish_sr())

 ####### ►  Make species richness plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.rich.plot <- renderUI({

    plotOutput("fish.state.rich.plot", height = length(unique(fish_sr()$marine.park))*200)

  }) %>% bindCache(fish_sr())

  ####### ►  Trophic group ----
  output$fish.state.trophic.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.trophic.dropdown)

    dat <- mpa_data$trophic.abundance[marine.park %in% c(input$fish.state.park.dropdown)]
    dat <- dat[method %in% c(input$fish.state.method.dropdown)]

    metadata <- fish_samplingeffort()

    dat <- dat[trophic.group %in% c(input$fish.state.trophic.dropdown)]
    dat <- dplyr::full_join(dat, metadata) %>%
      tidyr::complete(tidyr::nesting(marine.park, method), trophic.group) %>%
      tidyr::replace_na(list(total.abundance = 0)) %>%
      dplyr::filter(!is.na(trophic.group))

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(marine.park, trophic.group), axes = "all", ncol = length(unique(dat$trophic.group)), scales = "free_y") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      ggplot_mpatheme()

  }) #%>%
    #TODO change this to be only one dataset
    #bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.trophic.dropdown)

  ####### ►  Make trophic plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.trophic.plot <- renderUI({
    plotOutput("fish.state.trophic.plot", height = length(unique(input$fish.state.park.dropdown))*200)
  })


  ####### ►  Fished species KDE ----
  output$fish.state.fished.species.kde.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)

    dat <- mpa_data$fished.complete.length[marine.park %in% c(input$fish.state.park.dropdown)]
    dat <- dat[method %in% c(input$fish.state.method.dropdown)]

    more.than.20 <- dat %>%
      dplyr::group_by(marine.park, method, campaignid, status, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::filter(number > 20) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(marine.park, method, campaignid, status, scientific)

    dat <- dat %>%
      dplyr::semi_join(more.than.20) %>%
      dplyr::filter(!is.na(length)) %>%
      dplyr::filter(scientific %in% c(input$fish.state.fished.species.dropdown))

    split.dat <- split(dat, f = dat$marine.park, drop = TRUE)

    plot_list <- lapply(split.dat, function(dat) {
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
        scale_fill_manual(values = c("#b9e6fb",
                                     "#7bbc63")) +
        ylab("Weighted KDE (*1000)") +
        xlab("Total Length (mm)") +
        ggtitle(dat$marine.park) +
        facet_grid(year ~ scientific)
    })

    validate(
      need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
    )

    cowplot::plot_grid(plotlist = plot_list, ncol = 1)
  }) #%>%
    #TODO change this to be only one dataset
    #bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)

  # #   ####### ►  Make KDE plot interactive so the height changes with the number of inputs ----
  # output$ui.fish.state.fished.species.kde.plot <- renderPlot({
  #   plotOutput("fish.state.fished.species.kde.plot", height = 300 * length(unique(fish_alldata()$marine.park)))
  # })
  #
  ####### ►  Fished species abundance plot -----
  output$fish.state.fished.species.abundance.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)

    dat <- fish_fishedabundance()[scientific %in% c(input$fish.state.fished.species.dropdown)]

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +

      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(marine.park, scientific), axes = "all", ncol = 1, scales = "free_y") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      ggplot_mpatheme()
  }) #%>%
    #TODO change this to be only one dataset
    #bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)

  # ####### ►  Make fished abundance plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.fished.species.abundance.plot <- renderUI({

    dat <- fish_fishedabundance()[scientific %in% c(input$fish.state.fished.species.dropdown)]

    plotOutput("fish.state.fished.species.abundance.plot", height = length(unique(dat$marine.park))*200)
  })

  ####### ►  All species abundance ----
  output$fish.state.all.species.abundance.plot <- renderPlot({

    req(fish_abundance())

    dat <- fish_abundance()[scientific %in% c(input$fish.state.all.species.dropdown)]

    ggplot(dat, aes(x = year, y = maxn, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      ggh4x::facet_wrap2(vars(marine.park, scientific), axes = "all", ncol = 1, scales = "free_y") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      ggplot_mpatheme()


  }) #%>%
    #bindCache(fish_abundance())

  # TODO COME BACK AND MAKE THIS ONE WORK - FOR SOME REASON STOPS DROPDOWN FROM SHOWING UP.
  # NEED TO CHANGE IN UI TOO
    ####### ►  Make all species abundance plot interactive so the height changes with the number of inputs ----
    output$ui.fish.state.all.species.abundance.plot <- renderPlot({

      dat <- fish_abundance()[scientific %in% c(input$fish.state.all.species.dropdown)]

      plotOutput("fish.state.all.species.abundance.plot", height = 600)
    })

  ####### ►  Leaflet All Species ----
  output$fish.state.all.species.leaflet <- renderLeaflet({

    dat <- fish_abundance()[scientific %in% c(input$fish.state.all.species.dropdown)]

    overzero.ta <- dplyr::filter(dat, maxn > 0)
    equalzero.ta <- dplyr::filter(dat, maxn == 0)
    max.ta <- max(overzero.ta$maxn)

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
        position = "bottomleft",
        group = "Marine Parks"
      ) %>%
      addLayersControl(
        overlayGroups = c(
          "Marine Parks",
          "Abundance"
        ),
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
          data = overzero.ta,
          lat = ~latitude,
          lng = ~longitude,
          radius = ~ (((maxn / max(maxn)) * 20)),
          fillOpacity = 0.5,
          stroke = FALSE,
          label = ~ as.character(maxn),
          group = "Abundance",
          color = "blue"
        )
    }

    if (nrow(equalzero.ta)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero.ta,
          lat = ~latitude,
          lng = ~longitude,
          radius = 2,
          fillOpacity = 0.5,
          color = "black",
          stroke = FALSE,
          label = ~ as.character(maxn),
          group = "Abundance"
        )
    }
    map
  })

  #   #----------------------------------------------------------------------------#
  #### MARINE PARK PLOTS ----
  # Start of marine park plots ----
  ####### ►  Sampling effort leaflet ----
  output$fish.park.sampling.leaflet <- renderLeaflet({

    iconSet <- awesomeIconList(
      'consistent' = makeAwesomeIcon(
        icon = 'surf',
        iconColor = 'white',
        library = 'fa',
        markerColor = 'green' #Possible values are "red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray", "lightgray", "black"
      ),
      'intermittent' = makeAwesomeIcon(
        icon = 'surf',
        iconColor = 'white',
        library = 'fa',
        markerColor = 'orange'
      )
    )
    # data needed
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
      )

    # If the method = DOVs then split based off complete or incomplete
    if(input$fish.park.method.dropdown %in% "stereo-DOVs"){

      map <- map %>%
        leaflegend::addLegendAwesomeIcon(iconSet = iconSet,
                                         orientation = 'horizontal',
                                         title = htmltools::tags$div(
                                           style = 'font-size: 15px;',
                                           'Site visted:'),
                                         labelStyle = 'font-size: 12px;',
                                         position = 'bottomleft',
                                         group = 'Sampling locations')

    complete <- dat %>%
      dplyr::filter(complete %in% "Yes")

    incomplete <- dat %>%
      dplyr::filter(!complete %in% "Yes")

    if (nrow(complete)) {
      map <- map %>%
        leaflet::addAwesomeMarkers(data = complete,
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
    }

    if (nrow(incomplete)) {
      map <- map %>%
        leaflet::addAwesomeMarkers(data = incomplete,
                                   lng = ~longitude,
                                   lat = ~latitude,
                                   icon = leaflet::awesomeIcons(
                                     icon = 'surf',
                                     iconColor = 'white',
                                     library = 'fa',
                                     markerColor = 'orange' #Possible values are "red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray", "lightgray", "black"
                                   ),
                                   popup = ~content,
                                   label = ~as.character(sample),
                                   group = "Sampling locations"
        )
    }

    }

    if(!input$fish.park.method.dropdown %in% "stereo-DOVs"){
        map <- map %>%
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
      }
    map
  })

  # TODO TURN THIS BACK ON WHEN IT DOESN'T BREAK EVERYTHING
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
          data = overzero.ta, lat = ~latitude, lng = ~longitude,
          radius = ~ (((value / max(value)) * 20)), fillOpacity = 0.5, stroke = FALSE,
          label = ~ as.character(value), group = "Total abundance", color = "yellow"
        )
    }

    if (nrow(equalzero.ta)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero.ta, lat = ~latitude, lng = ~longitude,
          radius = 2, fillOpacity = 0.5, color = "black", stroke = FALSE,
          label = ~ as.character(value), group = "Total abundance"
        )
    }

    if (nrow(overzero.sr)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero.sr, lat = ~latitude, lng = ~longitude,
          radius = ~ ((value / max(value)) * 20), fillOpacity = 0.5, stroke = FALSE,
          label = ~ as.character(value), group = "Species richness", color = "green"
        )
    }

    if (nrow(equalzero.sr)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero.sr, lat = ~latitude, lng = ~longitude,
          radius = 2, fillOpacity = 0.5, color = "black", stroke = FALSE,
          label = ~ as.character(value), group = "Species richness"
        )
    }

    map %>%
      hideGroup("Species richness")
  })

  ####### ►  Total abundance ----
  output$fish.park.total.plot <- renderPlot({

    ta <- fish_park_ta()
    dat <- ta[complete %in% c("Yes")]

    # label <- grobTree(textGrob(as.character("Total abundance"),
    #   x = 0.01, y = 0.97, hjust = 0,
    #   gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    gazetted <- unique(dat$gazetted)
    re.zoned <- unique(dat$re.zoned)

    p <- ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      # annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      ggplot_mpatheme() #+

    # THESE ARE HOW TO ADD RE_ZONED AND GAZETTED, THEY CAN BE TURNED OFF
    # if(!gazetted %in% c("NA", NA, NULL)){
    #   p <- p + geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
    #     geom_label(
    #       x = gazetted,
    #       y = +Inf,
    #       label = "\n\n gazetted",
    #       size = 5,
    #       fill = "white",
    #       check_overlap = TRUE,
    #       label.size = NA
    #     )}
    #
    # if(!re.zoned %in% c("NA", NA, NULL)){
    #   p <- p + geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
    #     geom_label(
    #       x = re.zoned,
    #       y = +Inf,
    #       label = "\n\n rezoned",
    #       size = 5,
    #       fill = "white",
    #       check_overlap = TRUE,
    #       label.size = NA
    #     )}
    p
  }) #%>%
    #bindCache(fish_park_ta())

  ####### ►  Total abundance by site ----
  output$fish.park.total.site.plot <- renderPlot({

    dat <- fish_park_ta()

    # label <- grobTree(textGrob(as.character("Total abundance"),
    #                            x = 0.01, y = 0.97, hjust = 0,
    #                            gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    if(input$fish.park.method.dropdown %in% "stereo-DOVs"){

      ggplot(dat, aes(x = year, y = value, fill = status)) +
        stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
        stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
        xlab("Year") +
        ylab("Average total abundance per sample \n(+/- SE)") +
        # annotation_custom(label) +
        stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
          expand = expand_scale(mult = c(0, 0.05))
        ) +
        scale_fill_manual(values = c("#b9e6fb",
                                     "#7bbc63")) +
        ggplot_mpatheme() +
        facet_wrap(site ~ ., scales = "free", ncol = 3)
    }
  }) %>%
    bindCache(fish_park_ta())

  output$ui.fish.park.total.site.plot <- renderUI({
    dat <- fish_park_ta()

    if(input$fish.park.method.dropdown %in% "stereo-DOVs"){

      if (length(unique(dat$site)) %in% c(1,2,3) ){
        p.height <- 250
      } else {
        p.height <- 250 * ceiling(length(unique(dat$site))/3)
      }

      tagList(h4("Total abundance by site:"),
              plotOutput("fish.park.total.site.plot", height = p.height))
    }
  }) %>%
    bindCache(fish_park_ta())

  ####### ►  Total abundance by Sanctuary ----
  output$fish.park.total.sanctuary.plot <- renderPlot({

    dat <- fish_park_ta()

    # label <- grobTree(textGrob(as.character("Total abundance"),
    #                            x = 0.01, y = 0.97, hjust = 0,
    #                            gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      # annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      ggplot_mpatheme() +
      facet_wrap(dbca_sanctuary ~ ., scales = "free", ncol = 3)
  }) %>%
    bindCache(fish_park_ta())

  output$ui.fish.park.total.sanctuary.plot <- renderUI({
    dat <- fish_park_ta()

    if (length(unique(dat$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 250 * ceiling(length(unique(dat$dbca_sanctuary))/3)
    }

    plotOutput("fish.park.total.sanctuary.plot", height = p.height)
  }) %>%
    bindCache(fish_park_ta())

  ####### ►  Total abundance by Zone ----
  output$fish.park.total.zone.plot <- renderPlot({
    dat <- fish_park_ta()

    # label <- grobTree(textGrob(as.character("Total abundance"),
    #                            x = 0.01, y = 0.97, hjust = 0,
    #                            gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      # annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      ggplot_mpatheme() +
      facet_wrap(dbca_zone ~ ., scales = "free", ncol = 3)
  }) %>%
    bindCache(fish_park_ta())

  output$ui.fish.park.total.zone.plot <- renderUI({
    dat <- fish_park_ta()

    if (length(unique(dat$dbca_zone)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 250 * ceiling(length(unique(dat$dbca_zone))/3)
    }

    plotOutput("fish.park.total.zone.plot", height = p.height)
  }) %>%
    bindCache(fish_park_ta())

  ####### ►  Species richness ----
  output$fish.park.rich.plot <- renderPlot({

    dat <- fish_park_sr()

    # label <- grobTree(textGrob(as.character("Species richness"),
    #   x = 0.01, y = 0.97, hjust = 0,
    #   gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      #annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      ggplot_mpatheme()
  }) #%>% bindCache(fish_park_sr())

  ####### ►  Species richness by site ----
  output$fish.park.rich.site.plot <- renderPlot({

    dat <- fish_park_sr()

    # label <- grobTree(textGrob(as.character("Species richness"),
    #                            x = 0.01, y = 0.97, hjust = 0,
    #                            gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    if(input$fish.park.method.dropdown %in% "stereo-DOVs"){

      ggplot(dat, aes(x = year, y = value, fill = status)) +
        stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
        stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
        xlab("Year") +
        ylab("Average number of species per sample \n(+/- SE)") +
        #annotation_custom(label) +
        stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
          expand = expand_scale(mult = c(0, 0.05))
        ) +
        scale_fill_manual(values = c("#b9e6fb",
                                     "#7bbc63")) +
        ggplot_mpatheme() +
        facet_wrap(site ~ ., scales = "free", ncol = 3)
    }
  }) %>%
    bindCache(fish_park_sr())

  output$ui.fish.park.rich.site.plot <- renderUI({
    dat <- fish_park_sr()

    if(input$fish.park.method.dropdown %in% "stereo-DOVs"){

      if (length(unique(dat$site)) %in% c(1,2,3) ){
        p.height <- 250
      } else {
        p.height <- 250 * ceiling(length(unique(dat$site))/3)
      }
      tagList(h4("Species richness by site:"),
              plotOutput("fish.park.rich.site.plot", height = p.height))
    }
  }) %>%
    bindCache(fish_park_sr())

  ####### ►  Species richness by Sanctuary ----
  output$fish.park.rich.sanctuary.plot <- renderPlot({

    dat <- fish_park_sr()

    # label <- grobTree(textGrob(as.character("Species richness"),
    #                            x = 0.01, y = 0.97, hjust = 0,
    #                            gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      #annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      ggplot_mpatheme() +
      facet_wrap(dbca_sanctuary ~ ., scales = "free", ncol = 3)
  }) %>%
    bindCache(fish_park_sr())

  output$ui.fish.park.rich.sanctuary.plot <- renderUI({
    dat <- fish_park_sr()

    if (length(unique(dat$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 250 * ceiling(length(unique(dat$dbca_sanctuary))/3)
    }

    plotOutput("fish.park.rich.sanctuary.plot", height = p.height)
  }) %>%
    bindCache(fish_park_sr())

  ####### ►  Species richness by Zone ----
  output$fish.park.rich.zone.plot <- renderPlot({

    dat <- fish_park_sr()

    # label <- grobTree(textGrob(as.character("Species richness"),
    #                            x = 0.01, y = 0.97, hjust = 0,
    #                            gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    # ))

    ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      #annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      ggplot_mpatheme() +
      facet_wrap(dbca_zone ~ ., scales = "free", ncol = 3)
  }) %>%
    bindCache(fish_park_sr())

  output$ui.fish.park.rich.zone.plot <- renderUI({
    dat <- fish_park_sr()

    if (length(unique(dat$dbca_zone)) %in% c(1,2,3) ){
      p.height <- 250
    } else {
      p.height <- 250 * ceiling(length(unique(dat$dbca_zone))/3)
    }

    plotOutput("fish.park.rich.zone.plot", height = p.height)
  }) %>%
    bindCache(fish_park_sr())

  ####### ►  Stacked Abundance Plot ----
  output$fish.park.stack.plot <- renderPlot({

    maxn.sum <- fish_park_abundance() %>%
      mutate(scientific = paste(genus, species, sep = " ")) %>%
      group_by(scientific) %>%
      dplyr::summarise(maxn = sum(maxn)) %>%
      ungroup() %>%
      arrange(desc(maxn)) %>%
      top_n(10)

    ## ►  Total frequency of occurrence ----
    ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      coord_flip() +
      xlab("Species") +
      ylab("Overall abundance") +
      ggplot_mpatheme() +
      theme(axis.text.y = element_text(face = "italic")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  }) #%>% bindCache(fish_park_abundance())

  ####### ►  Trophic group ----
  output$fish.park.trophic.plot <- renderPlot({

    dat <- fish_park_trophicabundance()[trophic.group %in% c(input$fish.park.trophic.dropdown)]

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      facet_wrap(trophic.group ~ ., scales = "free", ncol = 1) +
      ggplot_mpatheme()
  }) #%>% bindCache(fish_park_trophicabundance())

  ####### ►  KDE plot ----
  output$fish.park.fished.species.kde.plot <- renderPlot({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.fished.species.dropdown) #, input$fish.park.site.dropdown

    more.than.20 <- fish_park_fishedcompletelength() %>%
      dplyr::group_by(marine.park, method, campaignid, status, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::filter(number > 20) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(marine.park, method, campaignid, status, scientific)

    dat <- fish_park_fishedcompletelength()[scientific %in% c(input$fish.park.fished.species.dropdown)]
    dat <- dat[length > 0]
    dat <- dat[!is.na(length)]

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
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      ylab("Weighted KDE (*1000)") +
      xlab("Total Length (mm)") +
      facet_grid(year ~ scientific)
  }) #%>% bindCache(fish_park_fishedcompletelength())

  ####### ►  Fished abundance ----
  output$fish.park.fished.species.abundance.plot <- renderPlot({

    dat <- fish_park_fishedabundance()[scientific %in% c(input$fish.park.fished.species.dropdown)]

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      facet_wrap(scientific ~ ., scales = "free", ncol = 1) +
      ggplot_mpatheme()
  }) #%>% bindCache(fish_park_fishedabundance())

  ####### ►  All species abundance ----
  output$fish.park.all.species.abundance.plot <- renderPlot({

    req(fish_park_abundance_species())
    dat <- fish_park_abundance_species()

    ggplot(dat, aes(x = year, y = maxn, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_fill_manual(values = c("#b9e6fb",
                                   "#7bbc63")) +
      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
      facet_wrap(scientific ~ ., scales = "free", ncol = 1) +
      ggplot_mpatheme()
  }) #%>% bindCache(fish_park_abundance_species())


  # TODO THIS APPEARS TO BREAK INDIVIDUAL SPECIES PLOT
  ###### ►  Leaflet - All species abundance ----
  output$fish.park.all.species.leaflet <- renderLeaflet({

    req(fish_park_abundance_species())

    dat <- fish_park_abundance_species()

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
          radius = ~ (((maxn / max(maxn)) * 20)), fillOpacity = 0.5, stroke = FALSE,
          label = ~ as.character(maxn), group = "Abundance", color = "blue"
        )
    }

    if (nrow(equalzero.ta)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero.ta, lat = ~latitude, lng = ~longitude,
          radius = 2, fillOpacity = 0.5, color = "black", stroke = FALSE,
          label = ~ as.character(maxn), group = "Abundance"
        )
    }

    map
  })


  # EXTRA CONTENT ----

  # Marine Park image ----
  # NOTE this depends on the image filename being identical to the data folders
  # in the source data (data/Ningaloo or data/Ningaloo Marine Park)
  output$ui.fish.park.image <- renderUI({
    park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
    print(park)

    img(src = paste0("www/images/fish_", park, ".jpg"), align = "right", width = "100%") # removed www from URL to get rid of golem
  }) #%>% bindCache(input$fish.park.dropdown)

  output$ui.benthic.park.image <- renderUI({
    park <- stringr::str_replace_all(tolower(input$benthic.park.coralcover.dropdown), c("marine park" = "", "island marine reserve" = "", " " = ""))
    print(park)

    img(src = paste0("www/images/coral_", park, ".jpg"), align = "right", width = "100%")  # removed www from URL to get rid of golem
  }) #%>% bindCache(input$benthic.park.coralcover.dropdown)

  # Species iFrames -----
  ####### ►  State All species ----
  output$fish.state.all.species.iframe <- renderUI({

    dat <- mpa_data$foa.codes[scientific %in% c(input$fish.state.all.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), height = 600, width = "100%")
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

    frame <- tags$iframe(src = paste0(dat), height = 600, width = "100%")
    frame
  })

  ####### ►  Marine Park Fished species ----
  output$fish.park.fished.species.iframe <- renderUI({

    dat <- mpa_data$foa.codes[scientific %in% c(input$fish.park.fished.species.dropdown)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")

    frame <- tags$iframe(src = paste0(dat), height = 600, width = "100%")
    frame

  })

  ####### ►  Info buttons ----
  observeEvent(input$state.ta,
               showModal(modalDialog(
                 title = "How do we measure total abundance?",
                 htmltools::includeMarkdown(paste0("inst/app/www/popups/total.abundance.md"))))
  )

  observeEvent(input$state.sr,
               showModal(modalDialog(
                 title = "How do we measure species richness?",
                 htmltools::includeMarkdown(paste0("inst/app/www/popups/species.richness.md"))))
  )


  observeEvent(input$park.ta,
               showModal(modalDialog(
                 title = "How do we measure total abundance?",
                 htmltools::includeMarkdown(paste0("inst/app/www/popups/total.abundance.md"))))
  )

  observeEvent(input$park.sr,
               showModal(modalDialog(
                 title = "How do we measure species richness?",
                 htmltools::includeMarkdown(paste0("inst/app/www/popups/species.richness.md"))))
  )

  # MARINE PARK ----
  # FOR FISH
  observeEvent(
    input$alert.marinepark,

    showModal(modalDialog(
      title = input$fish.park.dropdown,
      htmltools::includeMarkdown(paste0("inst/app/www/popups/fish_",
                                        stringr::str_replace_all(tolower(input$fish.park.dropdown), c("marine park" = "", " " = "")), ".md"))
    ))

  )

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
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
          expand = expand_scale(mult = c(0, 0.05))
        ) +
        ggplot_mpatheme() +
        scale_y_continuous(expand = c(0, 0.1)) +
        facet_wrap(marine.park ~ ., scales = "free", ncol = 1)

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
        stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, size = 1, col = "black", linetype = "solid") +
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
        stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = TRUE, size = 1, col = "black", linetype = "solid") +
        geom_point(size = 2) +
        xlab("") +
        ylab("% Coral Cover (mean ± SE)") +
        ggplot_mpatheme() +
        scale_y_continuous(expand = c(0, 0.1)) +
        facet_wrap(sector ~ ., scales = "free", ncol = 1)

      p
    })

    ####### ►  Create plot for coral cover for one marine park by site
    output$benthic.site.coralcover.plot <- renderPlot({
      req(benthic_park_coral_cover_sector())

      p <- ggplot(data = subset(benthic_park_coral_cover_sector(), !plot_year %in% c("1999")), aes(x = plot_year, y = mean)) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .025) + # error bars
        stat_smooth(method = "gam", formula = y ~ poly(x, 3), se = F, size = 0.3, col = "black", linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = 2, colour = "grey") +
        geom_point(size=1) +
        xlab("") +
        ylab(" % Coral Cover (mean ? SE)") +
        ggplot_mpatheme() +
        scale_y_continuous(expand = c(0, 0.1)) +
        facet_wrap(site ~ ., scales = "free", ncol = 3)

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
        geom_smooth(method = "gam", formula = y ~  s(x, k=length(unique(benthic_rec_3c2()$year)-2)), se=F, size = 1, col="black") +
        geom_point(position = pd, size = 2)+
        xlab("") +
        ylab("Number of coral recruits per tile") +
        scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2),
          expand = expand_scale(mult = c(0, 0.05))
        ) +ggplot_mpatheme() +
        scale_y_continuous(expand = c(0, 0.1)) +
        facet_wrap(marine.park ~ ., scales = "free", ncol = 1)

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
