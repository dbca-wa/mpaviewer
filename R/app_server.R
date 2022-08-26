#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny leaflet dplyr grid ggplot2 leafgl rgdal forcats
#' @noRd
app_server <- function(input, output, session) {
  # mod_assetmap_server("assetmap_ui_1")
  #
  # Load data: generate with generate_data()
  fn_mpa_data <- here::here("inst/data/mpa_data.rds")
  mpa_data <- if (!fs::file_exists(fn_mpa_data)) {
    # Download mpa_data.rds from the DBCA CKAN data catalogue
    # TODO schedule download_data() to run e.g. every hour
    # TODO automate generate_data() after moving all data assets to DBCA's CKAN
    message("Data file not found, downloading from DBCA data catalogue...")
    download_data()
  } else {
    reactiveFileReader(
      1000, # Poll data file for changes every 1 second
      NULL, # across sessions
      fn_mpa_data, # relative to project or Dockerfile workdir
      readRDS # using function readRDS
    )
  }

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

  # State image
  # NOTE this depends on the image filename being identical to the data folders
  # in the source data (data/Ningaloo or data/Ningaloo Marine Park)
  output$ui.fish.park.image <- renderUI({
    park <- stringr::str_replace_all(tolower(input$fish.park.dropdown), " ", ".")
    img(src = paste0("www/", park, ".jpg"), align = "right", width = "100%")
  })

  # State data ----
  # Create a marine park dropdown ----
  output$fish.state.park.dropdown <- renderUI({
    choices <- mpa_data()$metadata %>%
      dplyr::distinct(marine.park) %>%
      dplyr::arrange(marine.park) %>%
      dplyr::pull("marine.park")

    pickerInput(
      inputId = "fish.state.park.dropdown",
      label = "Choose Marine Parks to include:",
      choices = choices,
      multiple = TRUE,
      selected = choices,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  # Create method dropdown ----
  output$fish.state.method.dropdown <- renderUI({
    choices <- mpa_data()$metadata %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::distinct(method) %>%
      dplyr::pull("method")

    create_dropdown("fish.state.method.dropdown", choices, "Choose a method:", FALSE)
  })

  # Create a fished species dropdown ----
  output$fish.state.fished.species.dropdown <- renderUI({
    req(mpa_data())

    choices <- mpa_data()$fished.complete.length %>%
      dplyr::filter(number > 0) %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::group_by(scientific) %>%
      dplyr::summarise(total = sum(number)) %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::distinct(scientific) %>%
      dplyr::pull("scientific")

    pickerInput(
      inputId = "fish.state.fished.species.dropdown",
      label = "Choose target species to plot:",
      choices = choices,
      multiple = TRUE,
      selected = choices[1:3],
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  # Create a fished species dropdown ----
  output$fish.state.all.species.dropdown <- renderUI({
    choices <- mpa_data()$abundance %>%
      dplyr::filter(maxn > 0) %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::group_by(scientific) %>%
      dplyr::summarise(total = sum(maxn)) %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::distinct(scientific) %>%
      dplyr::pull("scientific")

    shinyWidgets::pickerInput(
      inputId = "fish.state.all.species.dropdown",
      label = "Choose species to plot:",
      choices = choices,
      multiple = TRUE,
      selected = choices[1:3],
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  # Create a trophic group dropdown ----
  output$fish.state.trophic.dropdown <- renderUI({
    choices <- mpa_data()$trophic.abundance %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::group_by(trophic.group) %>%
      dplyr::arrange() %>%
      dplyr::distinct(trophic.group) %>%
      dplyr::pull("trophic.group")

    pickerInput(
      inputId = "fish.state.trophic.dropdown",
      label = "Choose trophic groups to plot:",
      choices = choices,
      multiple = TRUE,
      selected = choices,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  # Marine Park dropdowns ----
  # Create a marine park dropdown ----
  output$fish.park.dropdown <- renderUI({
    options <- mpa_data()$metadata %>%
      dplyr::distinct(marine.park) %>%
      dplyr::pull("marine.park")

    create_dropdown("fish.park.dropdown", options, "Choose a marine park:", FALSE)
  })

  # Create method dropdown ----
  output$fish.park.method.dropdown <- renderUI({
    options <- mpa_data()$metadata %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::distinct(method) %>%
      dplyr::pull("method")

    create_dropdown("fish.park.method.dropdown", options, "Choose a method:", FALSE)
  })

  # Create a site dropdown ----
  output$fish.park.site.dropdown <- renderUI({
    options <- mpa_data()$metadata %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::distinct(site) %>%
      dplyr::arrange() %>%
      dplyr::pull("site")

    pickerInput(
      inputId = "fish.park.site.dropdown",
      label = "Choose sites include:",
      choices = options,
      multiple = TRUE,
      selected = options,
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      width = "100%"
    )
  })

  # Create a fished species dropdown ----
  output$fish.park.fished.species.dropdown <- renderUI({
    choices <- mpa_data()$fished.complete.length %>%
      dplyr::filter(number > 0) %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::group_by(scientific) %>%
      dplyr::summarise(total = sum(number)) %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::distinct(scientific) %>%
      dplyr::pull("scientific")

    pickerInput(
      inputId = "fish.park.fished.species.dropdown",
      label = "Choose target species to plot:",
      choices = choices,
      multiple = TRUE,
      selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  # Create an all species dropdown ----
  output$fish.park.all.species.dropdown <- renderUI({
    choices <- mpa_data()$abundance %>%
      dplyr::filter(maxn > 0) %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::group_by(scientific) %>%
      dplyr::summarise(total = sum(maxn)) %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::distinct(scientific) %>%
      dplyr::pull("scientific")

    pickerInput(
      inputId = "fish.park.all.species.dropdown",
      label = "Choose species to plot:",
      choices = choices,
      multiple = TRUE,
      selected = choices[1:3],
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  # Create a trophic group dropdown ----
  output$fish.park.trophic.dropdown <- renderUI({
    choices <- mpa_data()$trophic.abundance %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::group_by(trophic.group) %>%
      arrange() %>%
      distinct(trophic.group) %>%
      pull("trophic.group")

    pickerInput(
      inputId = "fish.park.trophic.dropdown",
      label = "Choose trophic groups to plot:",
      choices = choices,
      multiple = TRUE,
      selected = choices,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  # State plots ----
  # Sampling effort ----
  output$fish.state.sampling.leaflet <- renderLeaflet({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

    fish.dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown))

    ta <- fish.dat %>%
      dplyr::filter(metric %in% c("Total abundance"))

    sr <- fish.dat %>%
      dplyr::filter(metric %in% c("Species richness"))

    dat <- mpa_data()$sampling.effort %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::mutate(content = paste(
        sep = " ",
        "<b>Sample:", sample, "</b>", "<br/>",
        "<b>Status:</b>", status, "<br/>",
        "<b>Depth:</b>", depth, "m", "<br/>",
        "<b>Site:</b>", site, "<br/>",
        "<b>Location:</b>", location, "<br/>",
        "<b>Number of times sampled:</b>", number.of.times.sampled, "<br/>"
      ))

    overzero.ta <- dplyr::filter(ta, value > 0)
    equalzero.ta <- dplyr::filter(ta, value == 0)
    max.ta <- max(overzero.ta$value)

    overzero.sr <- dplyr::filter(sr, value > 0)
    equalzero.sr <- dplyr::filter(sr, value == 0)
    max.sr <- max(overzero.sr$value)

    map <- leaflet_basemap(dat) %>%
      fitBounds(~ min(longitude), ~ min(latitude), ~ max(longitude), ~ max(latitude)) %>%
      # addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~as.character(sample), popup = ~content,
      #                  radius = 1, fillOpacity = 1, color = "black") %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~ as.character(sample), popup = ~content, group = "Sampling locations") %>%
      addGlPolygons(
        data =  mpa_data()$state.mp,
        color = ~ mpa_data()$state.pal(zone),
        popup =  mpa_data()$state.mp$COMMENTS,
        group = "Marine Parks"
      ) %>%
      addLegend(
        pal = mpa_data()$state.pal, values = mpa_data()$state.mp$zone, opacity = 1,
        title = "Zones",
        position = "bottomright",
        group = "Marine Parks"
      ) %>%
      addLayersControl(overlayGroups = c("Sampling locations", "Marine Parks", "Total abundance", "Species richness"), options = layersControlOptions(collapsed = FALSE)) %>%
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
      hideGroup("Total abundance") %>%
      hideGroup("Species richness")
  })

  # Total abundance ----
  output$fish.state.total.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::filter(metric %in% c("Total abundance"))

    label <- grobTree(textGrob(as.character("Total abundance"),
      x = 0.01, y = 0.97, hjust = 0,
      gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    ))

    p <- ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.min = se.min, fun.max = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      # scale_y_continuous(expand = expansion(mult = 10)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      ggplot_mpatheme() +
      facet_wrap(marine.park ~ ., scales = "free", ncol = 1) +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA) # +
    # annotate(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 6, fill = "white", geom = "text")

    p
    # ggplotly(p)
  })

  # Make total abundance plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.total.plot <- renderUI({
    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown))

    plotOutput("fish.state.total.plot", height = 300 * length(unique(dat$marine.park)))
  })

  # Species richness ----
  output$fish.state.rich.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::filter(metric %in% c("Species richness"))

    label <- grobTree(textGrob(as.character("Species richness"),
      x = 0.01, y = 0.97, hjust = 0,
      gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    ))

    ggplot(dat, aes(x = year, y = value, fill = status, group = status)) + # , col = status
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      annotation_custom(label) +
      ggplot_mpatheme() +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      facet_wrap(marine.park ~ ., scales = "free", ncol = 1) +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })

  # Make species richness plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.rich.plot <- renderUI({
    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown))

    plotOutput("fish.state.rich.plot", height = 300 * length(unique(dat$marine.park)))
  })

  ## ►  Stacked Abundance Plot ----
  output$fish.state.stack.plot <- renderPlot({
    maxn.sum <- mpa_data()$abundance %>%
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
      ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
      ggplot_mpatheme() +
      theme(axis.text.y = element_text(face = "italic")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  })

  # Trophic group ----
  output$fish.state.trophic.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.trophic.dropdown)

    dat <- mpa_data()$trophic.abundance %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::filter(trophic.group %in% c(input$fish.state.trophic.dropdown))

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      facet_wrap(marine.park ~ trophic.group, scales = "free", ncol = length(unique(dat$trophic.group))) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })

  # Make species richness plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.trophic.plot <- renderPlot({
    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown))

    plotOutput("fish.state.trophic.plot", height = 300 * length(unique(dat$marine.park)))
  })

  # Fished species KDE ----
  output$fish.state.fished.species.kde.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)

    more.than.20 <- mpa_data()$fished.complete.length %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::group_by(marine.park, method, campaignid, status, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::filter(number > 20) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(marine.park, method, campaignid, status, scientific)

    dat <- mpa_data()$fished.complete.length %>%
      dplyr::semi_join(more.than.20) %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::filter(!is.na(length)) %>%
      # dplyr::filter(length > 0) %>% # Might need to check if this is right?
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
        ylab("Weighted KDE (*1000)") +
        xlab("Total Length (mm)") +
        ggtitle(dat$marine.park) +
        facet_grid(year ~ scientific)
    })

    validate(
      need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
    )


    plot_grid(plotlist = plot_list, ncol = 1)
  })

  # Make KDE plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.fished.species.kde.plot <- renderPlot({
    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown))

    plotOutput("fish.state.fished.species.kde.plot", height = 300 * length(unique(dat$marine.park)))
  })

  # Make fished abundance plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.fished.species.abundance.plot <- renderPlot({
    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown))

    plotOutput("fish.state.fished.species.abundance.plot", height = 300 * length(unique(dat$marine.park)))
  })

  # Fished species abundance plot -----
  output$fish.state.fished.species.abundance.plot <- renderPlot({
    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)

    dat <- mpa_data()$fished.abundance %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::filter(scientific %in% c(input$fish.state.fished.species.dropdown))

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      facet_wrap(marine.park ~ scientific, scales = "free", ncol = length(unique(dat$scientific))) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })


  # Make all species abundance plot interactive so the height changes with the number of inputs ----
  output$ui.fish.state.all.species.abundance.plot <- renderPlot({
    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown))

    plotOutput("fish.state.all.species.abundance.plot", height = 300 * length(unique(dat$marine.park)))

    # plotOutput("fish.state.all.species.abundance.plot", height = 300 * length(input$fish.state.all.species.dropdown))
  })

  # All species abundance ----
  output$fish.state.all.species.abundance.plot <- renderPlot({
    req(input$fish.state.method.dropdown, input$fish.state.park.dropdown, input$fish.state.all.species.dropdown)

    dat <- mpa_data()$abundance %>%
      dplyr::filter(method %in% c(input$fish.state.method.dropdown)) %>%
      dplyr::filter(marine.park %in% c(input$fish.state.park.dropdown)) %>%
      dplyr::filter(scientific %in% c(input$fish.state.all.species.dropdown))

    ggplot(dat, aes(x = year, y = maxn, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      facet_wrap(marine.park ~ scientific, scales = "free", ncol = length(unique(dat$scientific))) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })


  # Start of marine park plots ----
  # Sampling effort leaflet ----
  output$fish.park.sampling.leaflet <- renderLeaflet({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown)

    fish.dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown))

    ta <- fish.dat %>%
      dplyr::filter(metric %in% c("Total abundance"))

    sr <- fish.dat %>%
      dplyr::filter(metric %in% c("Species richness"))

    dat <- mpa_data()$sampling.effort %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::mutate(content = paste(
        sep = " ",
        "<b>Sample:", sample, "</b>", "<br/>",
        "<b>Status:</b>", status, "<br/>",
        "<b>Depth:</b>", depth, "m", "<br/>",
        "<b>Site:</b>", site, "<br/>",
        "<b>Location:</b>", location, "<br/>",
        "<b>Number of times sampled:</b>", number.of.times.sampled, "<br/>"
      ))

    overzero.ta <- filter(ta, value > 0)
    equalzero.ta <- filter(ta, value == 0)
    max.ta <- max(overzero.ta$value)

    overzero.sr <- filter(sr, value > 0)
    equalzero.sr <- filter(sr, value == 0)
    max.sr <- max(overzero.sr$value)

    map <- leaflet_basemap(dat) %>%
      fitBounds(~ min(longitude), ~ min(latitude), ~ max(longitude), ~ max(latitude)) %>%
      addMarkers(lng = ~longitude, lat = ~latitude, label = ~ as.character(sample), popup = ~content, group = "Sampling locations") %>%
      addGlPolygons(
        data =  mpa_data()$state.mp,
        color = ~ mpa_data()$state.pal(zone),
        popup =  mpa_data()$state.mp$COMMENTS,
        group = "Marine Parks"
      ) %>%
      addLegend(
        pal = mpa_data()$state.pal, values = mpa_data()$state.mp$zone, opacity = 1,
        title = "Zones",
        position = "bottomright", group = "Marine Parks"
      ) %>%
      addLayersControl(
        overlayGroups = c("Sampling locations", "Marine Parks", "Total abundance", "Species richness"),
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
      hideGroup("Total abundance") %>%
      hideGroup("Species richness")
  })

  # Total abundance ----
  output$fish.park.total.plot <- renderPlot({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown)

    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::filter(metric %in% c("Total abundance"))

    label <- grobTree(textGrob(as.character("Total abundance"),
      x = 0.01, y = 0.97, hjust = 0,
      gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    ))

    ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average total abundance per sample \n(+/- SE)") +
      annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })

  # Species richness ----
  output$fish.park.rich.plot <- renderPlot({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown)

    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::filter(metric %in% c("Species richness"))

    label <- grobTree(textGrob(as.character("Species richness"),
      x = 0.01, y = 0.97, hjust = 0,
      gp = gpar(col = "black", fontsize = 13, fontface = "italic")
    ))

    ggplot(dat, aes(x = year, y = value, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average number of species per sample \n(+/- SE)") +
      annotation_custom(label) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })

  ## ►  Stacked Abundance Plot ----
  output$fish.park.stack.plot <- renderPlot({
    maxn.sum <- mpa_data()$abundance %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
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
  })

  # Trophic group ----
  output$fish.park.trophic.plot <- renderPlot({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown, input$fish.park.trophic.dropdown)

    dat <- mpa_data()$trophic.abundance %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::filter(trophic.group %in% c(input$fish.park.trophic.dropdown))

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      facet_wrap(trophic.group ~ ., scales = "free", ncol = 1) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })


  # Total abundance leaflet ----
  output$fish.park.total.leaflet <- renderLeaflet({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown)

    dat <- mpa_data()$all.data %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(metric %in% c("Total abundance"))

    map <- leaflet_basemap(dat) %>%
      fitBounds(~ min(longitude), ~ min(latitude), ~ max(longitude), ~ max(latitude)) %>%
      addGlPolygons(
        data =  mpa_data()$state.mp,
        color = ~ state.pal(zone),
        popup =  mpa_data()$state.mp$COMMENTS,
        group = "Marine Parks"
      ) %>%
      addLegend(
        pal = mpa_data()$state.pal, values = mpa_data()$state.mp$zone, opacity = 1,
        title = "Zones",
        position = "bottomright", group = "Marine Parks"
      ) %>%
      addLayersControl(overlayGroups = c("Marine Parks"), options = layersControlOptions(collapsed = FALSE))

    overzero <- filter(dat, value > 0)
    equalzero <- filter(dat, value == 0)

    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~latitude, lng = ~longitude,
          radius = ~ ((value / max(value)) * 15), fillOpacity = 0.5, stroke = FALSE,
          label = ~ as.character(value)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~latitude, lng = ~longitude,
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
          label = ~ as.character(value)
        )
    }
    map
  })

  output$fish.park.fished.species.kde.plot <- renderPlot({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown, input$fish.park.fished.species.dropdown)

    more.than.20 <- mpa_data()$fished.complete.length %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::group_by(marine.park, method, campaignid, status, scientific) %>%
      dplyr::summarise(number = sum(number)) %>%
      dplyr::filter(number > 20) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(marine.park, method, campaignid, status, scientific)

    dat <- mpa_data()$fished.complete.length %>%
      dplyr::semi_join(more.than.20) %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown)) %>%
      dplyr::filter(!is.na(length)) %>%
      dplyr::filter(length > 0) %>% # Might need to check if this is right?
      dplyr::filter(scientific %in% c(input$fish.park.fished.species.dropdown))

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
      ylab("Weighted KDE (*1000)") +
      xlab("Total Length (mm)") +
      facet_grid(year ~ scientific)
  })

  # Make fished abundance plot interactive so the height changes with the number of inputs ----
  output$ui.fish.park.fished.species.abundance.plot <- renderUI({
    plotOutput("fish.park.fished.species.abundance.plot", height = 300 * length(input$fish.park.fished.species.dropdown))
  })

  # Fished abundance ----
  output$fish.park.fished.species.abundance.plot <- renderPlot({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown, input$fish.park.fished.species.dropdown)

    dat <- mpa_data()$fished.abundance %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(scientific %in% c(input$fish.park.fished.species.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown))

    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      facet_wrap(scientific ~ ., scales = "free", ncol = 1) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })

  # Make all species abundance plot interactive so the height changes with the number of inputs ----
  output$ui.fish.park.all.species.abundance.plot <- renderUI({
    plotOutput("fish.park.all.species.abundance.plot", height = 300 * length(input$fish.park.all.species.dropdown))
  })

  # All species abundance ----
  output$fish.park.all.species.abundance.plot <- renderPlot({
    req(input$fish.park.method.dropdown, input$fish.park.dropdown, input$fish.park.site.dropdown, input$fish.park.all.species.dropdown)

    dat <- mpa_data()$abundance %>%
      dplyr::filter(method %in% c(input$fish.park.method.dropdown)) %>%
      dplyr::filter(marine.park %in% c(input$fish.park.dropdown)) %>%
      dplyr::filter(scientific %in% c(input$fish.park.all.species.dropdown)) %>%
      dplyr::filter(site %in% c(input$fish.park.site.dropdown))

    ggplot(dat, aes(x = year, y = maxn, fill = status)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
      xlab("Year") +
      ylab("Average abundance of target species per sample \n(+/- SE)") +
      scale_y_continuous(expand = c(0, 0.1)) +
      scale_x_continuous(breaks = c(unique(dat$year))) +
      stat_smooth(method = "gam", formula = y ~ mgcv::s(x, k = 3), size = 1, col = "black") +
      facet_wrap(scientific ~ ., scales = "free", ncol = 1) +
      ggplot_mpatheme() +
      geom_vline(aes(xintercept = year.zoned), linetype = "dashed") +
      geom_label(x = dat$year.zoned, y = +Inf, label = "\n zoned", size = 5, fill = "white", check_overlap = TRUE, label.size = NA)
  })



  # Info buttons ----

  observeEvent(
    input$alert.marinepark,
    shinyalert::shinyalert(
      title = input$fish.park.dropdown,
      text = filter(mpa_data()$park.popups, marine.park %in% c(input$fish.park.dropdown))$info,
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )
  )
}
