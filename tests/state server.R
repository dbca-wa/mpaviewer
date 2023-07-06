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

# shinyOptions(cache = cachem::cache_disk("./bind-cache"))


# #### STATE DROPDOWNS ----
# ####### ►  Create a marine park dropdown ----
# output$fish.state.park.dropdown <- renderUI({
#
#   lats <- mpa_data$lats %>%
#     dplyr::arrange(desc(mean.lat))
#
#   pickerInput(
#     inputId = "fish.state.park.dropdown",
#     label = "Choose Marine Parks to include:",
#     choices = c(unique(lats$marine.park)), #choices,
#     multiple = TRUE,
#     selected = c(unique(lats$marine.park)), # choices,
#     options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
#   )
# })
#
# ####### ►  Create method dropdown ----
# output$fish.state.method.dropdown <- renderUI({
#
#   req(input$fish.state.park.dropdown)
#
#   dat <- mpa_data$metadata[marine.park %in% c(input$fish.state.park.dropdown)]
#
#   choices <- dat %>%
#     dplyr::distinct(method) %>%
#     dplyr::pull("method")
#
#   create_dropdown("fish.state.method.dropdown", choices, "Choose a method:", FALSE)
#
# })# %>% bindCache(input$fish.state.park.dropdown)
#
# ####### ►  Create a fished species dropdown ----
# output$fish.state.fished.species.dropdown <- renderUI({
#
#   dat <- mpa_data$fished.complete.length[marine.park %in% c(input$fish.state.park.dropdown) &
#                                            method %in% c(input$fish.state.method.dropdown) &
#                                            number > 0]
#   choices <- dat %>%
#     dplyr::group_by(scientific) %>%
#     dplyr::summarise(total = sum(number)) %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(desc(total)) %>%
#     dplyr::distinct(scientific) %>%
#     dplyr::pull("scientific")
#
#   pickerInput(
#     inputId = "fish.state.fished.species.dropdown",
#     label = "Choose target species to plot:",
#     choices = choices,
#     multiple = FALSE,
#     selected = choices[1],
#     options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
#   )
# }) # %>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
#
# ####### ►  Create all species dropdown ----
# output$fish.state.all.species.dropdown <- renderUI({
#
#   dat <- mpa_data$abundance[marine.park %in% c(input$fish.state.park.dropdown) &
#                               method %in% c(input$fish.state.method.dropdown) &
#                               maxn > 0]
#
#   choices <- dat %>%
#     dplyr::group_by(scientific) %>%
#     dplyr::summarise(total = sum(maxn)) %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(desc(total)) %>%
#     dplyr::distinct(scientific) %>%
#     dplyr::pull("scientific")
#
#   shinyWidgets::pickerInput(
#     inputId = "fish.state.all.species.dropdown",
#     label = "Choose species to plot:",
#     choices = choices,
#     multiple = FALSE,
#     selected = choices[1],
#     options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
#   )
# }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
#
# # ####### ►  Create a trophic group dropdown ----
# # output$fish.state.trophic.dropdown <- renderUI({
# #
# #   dat <- mpa_data$trophic.abundance[marine.park %in% c(input$fish.state.park.dropdown) &
# #                               method %in% c(input$fish.state.method.dropdown)]
# #
# #   choices <- dat %>%
# #     dplyr::distinct(trophic.group) %>%
# #     dplyr::pull("trophic.group")
# #
# #   pickerInput(
# #     inputId = "fish.state.trophic.dropdown",
# #     label = "Choose trophic groups to plot:",
# #     choices = sort(choices),
# #     multiple = TRUE,
# #     selected = sort(choices)[1:3],
# #     options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
# #   )
# # }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)




# fish_alldata <- reactive({
#   req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
#
#   dat <- mpa_data$all.data[marine.park %in% c(input$fish.state.park.dropdown)]
#   dat <- dat[method %in% c(input$fish.state.method.dropdown)]
#
#   dat
#
# }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

# fish_ta <- reactive({
#   req(fish_alldata())
#
#   fish_alldata()[metric %in% c("Total abundance")]
#
# })
#
# fish_sr <- reactive({
#   req(fish_alldata())
#
#   fish_alldata()[metric %in% c("Species richness")]
#
# })


# Sampling effort for leaflet ----
# fish_samplingeffort <- reactive({
#   req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
#
#   dat <- mpa_data$sampling.effort[marine.park %in% c(input$fish.state.park.dropdown)]
#   dat <- dat[method %in% c(input$fish.state.method.dropdown)]
#
#   dat %>%
#     dplyr::mutate(content = paste(
#       sep = " ",
#       "<b>Sample:", sample, "</b>", "<br/>",
#       "<b>Status:</b>", status, "<br/>",
#       "<b>Depth:</b>", depth, "m", "<br/>",
#       "<b>Site:</b>", site, "<br/>",
#       "<b>Location:</b>", location, "<br/>",
#       "<b>Number of times sampled:</b>", number.of.times.sampled, "<br/>"
#     ))
# }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)


# fish_abundance <- reactive({
#   req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
#
#   dat <- mpa_data$abundance[marine.park %in% c(input$fish.state.park.dropdown)]
#   dat <- dat[method %in% c(input$fish.state.method.dropdown)]
#
#   dat
#
# }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)

# fish_fishedabundance <- reactive({
#   req(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
#
#   dat <- mpa_data$fished.abundance[marine.park %in% c(input$fish.state.park.dropdown)]
#   dat <- dat[method %in% c(input$fish.state.method.dropdown)]
#
#   dat
#
# }) #%>% bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown)
#
#----------------------------------------------------------------------------#
#  #### STATE PLOTS ----
#  ####### ►  Sampling effort leaflet ----
#  output$fish.state.sampling.leaflet <- renderLeaflet({
#
#    map <- leaflet_basemap(fish_samplingeffort()) %>%
#      leaflet::fitBounds(
#        ~ min(longitude),
#        ~ min(latitude),
#        ~ max(longitude),
#        ~ max(latitude)
#      ) %>%
#      leaflet::addAwesomeMarkers(lng = ~longitude,
#                                 lat = ~latitude,
#                                 icon = leaflet::awesomeIcons(
#                                   icon = 'surf',
#                                   iconColor = 'white',
#                                   library = 'fa',
#                                   markerColor = 'green'
#                                 ),
#                                 popup = ~content,
#                                 label = ~as.character(sample),
#                                 group = "Sampling locations"
#      ) %>%
#      # leaflet::addMarkers(
#      #   lng = ~longitude,
#      #   lat = ~latitude,
#      #   label = ~ as.character(sample),
#      #   popup = ~content,
#      #   group = "Sampling locations"
#      # ) %>%
#      addGlPolygons(
#        data =  mpa_data$state.mp,
#        color = ~ mpa_data$state.pal(zone),
#        popup =  mpa_data$state.mp$COMMENTS,
#        group = "Marine Parks"
#      ) %>%
#      # TODO fix this
#      addLegend(
#        pal = mpa_data$state.pal,
#        values = mpa_data$state.mp$zone,
#        opacity = 1,
#        title = "Zones",
#        position = "bottomleft",
#        group = "Marine Parks"
#      ) %>%
#      addLayersControl(
#        overlayGroups = c(
#          "Sampling locations",
#          "Marine Parks"),
#        options = layersControlOptions(collapsed = FALSE)
#      )
#
#    map
#  }) #%>% bindCache(fish_samplingeffort())
#
#  ####### ►  Leaflet Total Abundance and Species Richness ----
#  output$fish.state.metric.leaflet <- renderLeaflet({
#
#    overzero.ta <- dplyr::filter(fish_ta(), value > 0)
#    equalzero.ta <- dplyr::filter(fish_ta(), value == 0)
#    max.ta <- max(overzero.ta$value)
#
#    overzero.sr <- dplyr::filter(fish_sr(), value > 0)
#    equalzero.sr <- dplyr::filter(fish_sr(), value == 0)
#    max.sr <- max(overzero.sr$value)
#
#    map <- leaflet_basemap(fish_samplingeffort()) %>%
#      fitBounds(
#        ~ min(longitude),
#        ~ min(latitude),
#        ~ max(longitude),
#        ~ max(latitude)
#      ) %>%
#      addGlPolygons(
#        data =  mpa_data$state.mp,
#        color = ~ mpa_data$state.pal(zone),
#        popup =  mpa_data$state.mp$COMMENTS,
#        group = "Marine Parks"
#      ) %>%
#      addLegend(
#        pal = mpa_data$state.pal,
#        values = mpa_data$state.mp$zone,
#        opacity = 1,
#        title = "Zones",
#        position = "bottomleft",
#        group = "Marine Parks"
#      ) %>%
#      addLayersControl(
#        overlayGroups = c(
#          "Marine Parks",
#          "Total abundance",
#          "Species richness"
#        ),
#        options = layersControlOptions(collapsed = FALSE)
#      ) %>%
#      add_legend_ta(
#        colors = c("black", "yellow", "yellow"),
#        labels = c(0, round(max.ta / 2), max.ta),
#        sizes = c(5, 20, 40), group = "Total abundance"
#      ) %>%
#      add_legend_sr(
#        colors = c("black", "green", "green"),
#        labels = c(0, round(max.sr / 2), max.sr),
#        sizes = c(5, 20, 40), group = "Species richness"
#      )
#
#    if (nrow(overzero.ta)) {
#      map <- map %>%
#        addCircleMarkers(
#          data = overzero.ta,
#          lat = ~latitude,
#          lng = ~longitude,
#          radius = ~ (((value / max(value)) * 20)),
#          fillOpacity = 0.5,
#          stroke = FALSE,
#          label = ~ as.character(value),
#          group = "Total abundance",
#          color = "yellow"
#        )
#    }
#
#    if (nrow(equalzero.ta)) {
#      map <- map %>%
#        addCircleMarkers(
#          data = equalzero.ta,
#          lat = ~latitude,
#          lng = ~longitude,
#          radius = 2,
#          fillOpacity = 0.5,
#          color = "black",
#          stroke = FALSE,
#          label = ~ as.character(value),
#          group = "Total abundance"
#        )
#    }
#
#    if (nrow(overzero.sr)) {
#      map <- map %>%
#        addCircleMarkers(
#          data = overzero.sr,
#          lat = ~latitude,
#          lng = ~longitude,
#          radius = ~ ((value / max(value)) * 20),
#          fillOpacity = 0.5,
#          stroke = FALSE,
#          label = ~ as.character(value),
#          group = "Species richness",
#          color = "green"
#        )
#    }
#
#    if (nrow(equalzero.sr)) {
#      map <- map %>%
#        addCircleMarkers(
#          data = equalzero.sr,
#          lat = ~latitude,
#          lng = ~longitude,
#          radius = 2,
#          fillOpacity = 0.5,
#          color = "black",
#          stroke = FALSE,
#          label = ~ as.character(value),
#          group = "Species richness"
#        )
#    }
#
#    map %>%
#      # hideGroup("Total abundance") %>%
#      hideGroup("Species richness")
#  }) #%>% bindCache(fish_ta())
#
#  ####### ►  Total abundance ----
#  output$fish.state.total.plot <- renderPlot({
#    req(fish_ta())
#
#    label <- grobTree(textGrob(as.character("Total abundance"),
#                               x = 0.01, y = 0.97, hjust = 0,
#                               gp = gpar(col = "black", fontsize = 13, fontface = "italic")
#    ))
#
#    p <- ggplot(
#      fish_ta(),
#      aes(x = year, y = value, fill = status)
#    ) +
#      stat_summary(
#        fun = mean,
#        geom = "point",
#        shape = 23,
#        size = 6,
#        col = "black",
#        position = position_dodge(width = 0.5)
#      ) +
#      stat_summary(
#        fun.min = se.min,
#        fun.max = se.max,
#        geom = "errorbar",
#        width = 0.1,
#        col = "black",
#        position = position_dodge(width = 0.5)
#      ) +
#      xlab("Year") +
#      ylab("Average total abundance per sample \n(+/- SE)") +
#      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
#      scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
#      ggh4x::facet_wrap2(vars(marine.park), axes = "all", ncol = 1, scales = "free_y") +
#      scale_x_continuous(
#         breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
#         expand = expand_scale(mult = c(0, 0.05)))+
#      ggplot_mpatheme()
#
#
#    p
#
#  }) %>% bindCache(fish_ta())
#
#  output$ui.fish.state.total.plot <- renderUI({
#
#    plotOutput("fish.state.total.plot", height = length(unique(fish_ta()$marine.park))*200)
#
#  }) %>% bindCache(fish_ta())
#
#  ####### ►  Species richness ----
#  output$fish.state.rich.plot <- renderPlot({
#    req(fish_sr())
#
#    label <- grobTree(textGrob(as.character("Species richness"),
#                               x = 0.01, y = 0.97, hjust = 0,
#                               gp = gpar(col = "black", fontsize = 13, fontface = "italic")
#    ))
#
#    ggplot(fish_sr(), aes(x = year, y = value, fill = status, group = status)) + # , col = status
#      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
#      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
#      xlab("Year") +
#      ylab("Average number of species per sample \n(+/- SE)") +
#      scale_y_continuous(expand = c(0, 0.1)) +
#      scale_fill_manual(values = c("#b9e6fb",
#                                   "#7bbc63")) +
#      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
#      ggh4x::facet_wrap2(vars(marine.park), axes = "all", ncol = 1, scales = "free_y") +
#      scale_x_continuous(
#        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
#        expand = expand_scale(mult = c(0, 0.05))
#      )+
#      ggplot_mpatheme()
#  }) %>% bindCache(fish_sr())
#
# ####### ►  Make species richness plot interactive so the height changes with the number of inputs ----
#  output$ui.fish.state.rich.plot <- renderUI({
#
#    plotOutput("fish.state.rich.plot", height = length(unique(fish_sr()$marine.park))*200)
#
#  }) %>% bindCache(fish_sr())
#
#  ####### ►  Trophic group ----
#  output$fish.state.trophic.plot <- renderPlot({
#    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.trophic.dropdown)
#
#    dat <- mpa_data$trophic.abundance[marine.park %in% c(input$fish.state.park.dropdown)]
#    dat <- dat[method %in% c(input$fish.state.method.dropdown)]
#
#    metadata <- fish_samplingeffort()
#
#    dat <- dat[trophic.group %in% c(input$fish.state.trophic.dropdown)]
#    dat <- dplyr::full_join(dat, metadata) %>%
#      tidyr::complete(tidyr::nesting(marine.park, method), trophic.group) %>%
#      tidyr::replace_na(list(total.abundance = 0)) %>%
#      dplyr::filter(!is.na(trophic.group))
#
#    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
#      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
#      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
#      xlab("Year") +
#      ylab("Average abundance per sample \n(+/- SE)") +
#      scale_y_continuous(expand = c(0, 0.1)) +
#      scale_fill_manual(values = c("#b9e6fb",
#                                   "#7bbc63")) +
#      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
#      ggh4x::facet_wrap2(vars(marine.park, trophic.group), axes = "all", ncol = length(unique(dat$trophic.group)), scales = "free_y") +
#      scale_x_continuous(
#        breaks = function(x) seq(ceiling(x[1]), floor(x[1]), by = 1),
#        expand = expand_scale(mult = c(0, 0.05))
#      ) +
#      ggplot_mpatheme()
#
#  }) #%>%
#    #TODO change this to be only one dataset
#    #bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.trophic.dropdown)
#
#  ####### ►  Make trophic plot interactive so the height changes with the number of inputs ----
#  output$ui.fish.state.trophic.plot <- renderUI({
#    plotOutput("fish.state.trophic.plot", height = length(unique(input$fish.state.park.dropdown))*200)
#  })
#
#
#  ####### ►  Fished species KDE ----
#  output$fish.state.fished.species.kde.plot <- renderPlot({
#    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)
#
#    dat <- mpa_data$fished.complete.length[marine.park %in% c(input$fish.state.park.dropdown)]
#    dat <- dat[method %in% c(input$fish.state.method.dropdown)]
#
#    more.than.20 <- dat %>%
#      dplyr::group_by(marine.park, method, campaignid, status, scientific) %>%
#      dplyr::summarise(number = sum(number)) %>%
#      dplyr::filter(number > 20) %>%
#      dplyr::ungroup() %>%
#      dplyr::distinct(marine.park, method, campaignid, status, scientific)
#
#    dat <- dat %>%
#      dplyr::semi_join(more.than.20) %>%
#      dplyr::filter(!is.na(length)) %>%
#      dplyr::filter(scientific %in% c(input$fish.state.fished.species.dropdown))
#
#    split.dat <- split(dat, f = dat$marine.park, drop = TRUE)
#
#    plot_list <- lapply(split.dat, function(dat) {
#      ggplot(dat, aes(x = length, fill = status)) +
#        geom_density(aes(y = ..density.. * 1000), alpha = 0.5, size = 0.7) +
#        theme(legend.position = ("bottom")) +
#        theme(
#          strip.text.y = element_text(size = 12, angle = 270),
#          strip.background = element_blank(),
#          axis.title = element_text(face = "bold"),
#          plot.title = element_text(face = "italic", hjust = 0.5),
#          strip.text.x = element_text(size = 14),
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank()
#        ) +
#        scale_y_continuous(expand = c(0, 0.1)) +
#        scale_fill_manual(values = c("#b9e6fb",
#                                     "#7bbc63")) +
#        ylab("Weighted KDE (*1000)") +
#        xlab("Total Length (mm)") +
#        ggtitle(dat$marine.park) +
#        facet_grid(year ~ scientific)
#    })
#
#    validate(
#      need(nrow(dat) > 0, "Sorry, there is not enough data to create a KDE for the species you requested. Please change your input selections")
#    )
#
#    cowplot::plot_grid(plotlist = plot_list, ncol = 1)
#  }) #%>%
#    #TODO change this to be only one dataset
#    #bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)
#
#  # #   ####### ►  Make KDE plot interactive so the height changes with the number of inputs ----
#  # output$ui.fish.state.fished.species.kde.plot <- renderPlot({
#  #   plotOutput("fish.state.fished.species.kde.plot", height = 300 * length(unique(fish_alldata()$marine.park)))
#  # })
#  #
#  ####### ►  Fished species abundance plot -----
#  output$fish.state.fished.species.abundance.plot <- renderPlot({
#    req(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)
#
#    dat <- fish_fishedabundance()[scientific %in% c(input$fish.state.fished.species.dropdown)]
#
#    ggplot(dat, aes(x = year, y = total.abundance, fill = status)) +
#      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
#      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
#      xlab("Year") +
#      ylab("Average abundance of target species per sample \n(+/- SE)") +
#      scale_y_continuous(expand = c(0, 0.1)) +
#
#      scale_fill_manual(values = c("#b9e6fb",
#                                   "#7bbc63")) +
#      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
#      ggh4x::facet_wrap2(vars(marine.park, scientific), axes = "all", ncol = 1, scales = "free_y") +
#      scale_x_continuous(
#        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
#        expand = expand_scale(mult = c(0, 0.05))
#      ) +
#      ggplot_mpatheme()
#  }) #%>%
#    #TODO change this to be only one dataset
#    #bindCache(input$fish.state.park.dropdown, input$fish.state.method.dropdown, input$fish.state.fished.species.dropdown)
#
#  # ####### ►  Make fished abundance plot interactive so the height changes with the number of inputs ----
#  output$ui.fish.state.fished.species.abundance.plot <- renderUI({
#
#    dat <- fish_fishedabundance()[scientific %in% c(input$fish.state.fished.species.dropdown)]
#
#    plotOutput("fish.state.fished.species.abundance.plot", height = length(unique(dat$marine.park))*200)
#  })
#
#  ####### ►  All species abundance ----
#  output$fish.state.all.species.abundance.plot <- renderPlot({
#
#    req(fish_abundance())
#
#    dat <- fish_abundance()[scientific %in% c(input$fish.state.all.species.dropdown)]
#
#    ggplot(dat, aes(x = year, y = maxn, fill = status)) +
#      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 6, col = "black", position = position_dodge(width = 0.5)) +
#      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1, col = "black", position = position_dodge(width = 0.5)) +
#      xlab("Year") +
#      ylab("Average abundance of species per sample \n(+/- SE)") +
#      scale_y_continuous(expand = c(0, 0.1)) +
#      scale_fill_manual(values = c("#b9e6fb",
#                                   "#7bbc63")) +
#      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
#      ggh4x::facet_wrap2(vars(marine.park, scientific), axes = "all", ncol = 1, scales = "free_y") +
#      scale_x_continuous(
#        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
#        expand = expand_scale(mult = c(0, 0.05))
#      ) +
#      ggplot_mpatheme()
#
#
#  }) #%>%
#    #bindCache(fish_abundance())
#
#  # TODO COME BACK AND MAKE THIS ONE WORK - FOR SOME REASON STOPS DROPDOWN FROM SHOWING UP.
#  # NEED TO CHANGE IN UI TOO
#    ####### ►  Make all species abundance plot interactive so the height changes with the number of inputs ----
#    output$ui.fish.state.all.species.abundance.plot <- renderPlot({
#
#      dat <- fish_abundance()[scientific %in% c(input$fish.state.all.species.dropdown)]
#
#      plotOutput("fish.state.all.species.abundance.plot", height = 600)
#    })
#
#  ####### ►  Leaflet All Species ----
#  output$fish.state.all.species.leaflet <- renderLeaflet({
#
#    dat <- fish_abundance()[scientific %in% c(input$fish.state.all.species.dropdown)]
#
#    overzero.ta <- dplyr::filter(dat, maxn > 0)
#    equalzero.ta <- dplyr::filter(dat, maxn == 0)
#    max.ta <- max(overzero.ta$maxn)
#
#    map <- leaflet_basemap(fish_samplingeffort()) %>%
#      fitBounds(
#        ~ min(longitude),
#        ~ min(latitude),
#        ~ max(longitude),
#        ~ max(latitude)
#      ) %>%
#      addGlPolygons(
#        data =  mpa_data$state.mp,
#        color = ~ mpa_data$state.pal(zone),
#        popup =  mpa_data$state.mp$COMMENTS,
#        group = "Marine Parks"
#      ) %>%
#      addLegend(
#        pal = mpa_data$state.pal,
#        values = mpa_data$state.mp$zone,
#        opacity = 1,
#        title = "Zones",
#        position = "bottomleft",
#        group = "Marine Parks"
#      ) %>%
#      addLayersControl(
#        overlayGroups = c(
#          "Marine Parks",
#          "Abundance"
#        ),
#        options = layersControlOptions(collapsed = FALSE)
#      ) %>%
#      add_legend_ta(
#        colors = c("black", "blue", "blue"),
#        labels = c(0, round(max.ta / 2), max.ta),
#        sizes = c(5, 20, 40), group = "Abundance"
#      )
#
#    if (nrow(overzero.ta)) {
#      map <- map %>%
#        addCircleMarkers(
#          data = overzero.ta,
#          lat = ~latitude,
#          lng = ~longitude,
#          radius = ~ (((maxn / max(maxn)) * 20)),
#          fillOpacity = 0.5,
#          stroke = FALSE,
#          label = ~ as.character(maxn),
#          group = "Abundance",
#          color = "blue"
#        )
#    }
#
#    if (nrow(equalzero.ta)) {
#      map <- map %>%
#        addCircleMarkers(
#          data = equalzero.ta,
#          lat = ~latitude,
#          lng = ~longitude,
#          radius = 2,
#          fillOpacity = 0.5,
#          color = "black",
#          stroke = FALSE,
#          label = ~ as.character(maxn),
#          group = "Abundance"
#        )
#    }
#    map
#  })
