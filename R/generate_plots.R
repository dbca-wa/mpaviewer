#' Generate plots to display in the dashboard
#'
#'
#' @return All plots
#'
#' @examples
#' \dontrun{
#' x <- generate_plots()
#' }
generate_plots <- function() {
  options(ragg.max_dim = 1000000)

  message("This function takes a couple minutes to run")

  # LOAD DATA ----
  load("inst/data/mpa_data.Rdata")

  # PALETTE FOR ZONE PLOTS ----
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

  # BEGIN PLOTTING ----
  # FISH PLOTS ----
  ## TOTAL ABUNDANCE ----
  ta <- mpa_data$ta_sr[metric %in% c("Total abundance")]

  # Filter to consistently sampled
  dat <- ta[complete %in% c("Consistently sampled")]

  unique(dat$marine_park)
  unique(dat$method)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
        ggplot2::ylab("Average total abundance\nper sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 4,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
            ggplot2::geom_label(
              x = 2021,
              y = +Inf,
              label = "\n\n method\nchange",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA)
      }

      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))
#
#       png(paste0("inst/app/www/plots/", park.name, "_", methods, "_total_abundance.png"), width = 1000, height = 250, res = 300, pointsize = 4)
#       print(p)
#       dev.off()

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_total_abundance.png"),
        p,
        width = 10,
        height = 3,
        dpi = 300
      )

    }
  }


  #### TOTAL ABUNDANCE - BY SANCTUARY ----
  ta <- mpa_data$ta_sr_sanctuary[metric %in% c("Total abundance")]
  dat <- ta
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average total abundance\n per sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
        ggh4x::facet_wrap2(ggplot2::vars(dbca_sanctuary), axes = "all", ncol = 3) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }

      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      if (length(unique(temp2$dbca_sanctuary)) %in% c(1,2,3) ){
        p.height <- 3
      } else {
        p.height <- 3 * ceiling(length(unique(temp2$dbca_sanctuary))/3)
      }

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_total_abundance_sanctuary.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

    }
  }

  #### TOTAL ABUNDANCE - BY SITE ----
  ta <- mpa_data$ta_sr_site[metric %in% c("Total abundance")]
  dat <- ta
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    print(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      print(methods)

      if(methods %in% c("stereo-DOVs", "stereo-ROVs")){

        temp2 <- temp[method %in% c(methods)]

        p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
          ggplot2::geom_point(ggplot2::aes(shape = complete), size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
          ggplot2::xlab("Year") +
          ggplot2::ylab("Average total abundance \nper sample (+/- SE)") +
          ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
          ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                      expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
          ggplot2::scale_shape_manual(values = c("Consistently sampled" = 21, "Intermittently sampled" = 22)) +
          ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
          ggh4x::facet_wrap2(ggplot2::vars(site), axes = "all", ncol = 3) +
          ggplot_mpatheme()

        gazetted <- unique(temp2$gazetted)
        re_zoned <- unique(temp2$re_zoned)
        min_year <- min(temp2$year)

        # Add gazettal and rezoned dates if they occured after sampling
        if(!gazetted %in% c("NA", NA, NULL)){

          if(min_year < gazetted) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
              ggplot2::geom_label(
                x = gazetted,
                y = +Inf,
                label = "\n\n gazetted",
                size = 5,
                fill = "white",
                check_overlap = TRUE,
                label.size = NA
              )}
        }

        if(!re_zoned %in% c("NA", NA, NULL)){
          if(min_year < re_zoned) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
              ggplot2::geom_label(
                x = re_zoned,
                y = +Inf,
                label = "\n\n rezoned",
                size = 5,
                fill = "white",
                check_overlap = TRUE,
                label.size = NA
              )}
        }

        if(methods %in% c("stereo-ROVs+UVC")){
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
            ggplot2::geom_label(
              x = 2021,
              y = +Inf,
              label = "\n\n method\nchange",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA)
        }

        p

        park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

        if (length(unique(temp2$site)) %in% c(1,2,3) ){
          p.height <- 3
        } else {
          p.height <- 3 * ceiling(length(unique(temp2$site))/3)
        }

        if (length(unique(temp2$site)) > 30 ){ # To fix ningaloo sites
          p.height <- 2 * ceiling(length(unique(temp2$site))/3)
        }

        ggplot2::ggsave(
          paste0("inst/app/www/plots/", park.name, "_", methods, "_total_abundance_site.png"),
          p,
          width = 10,
          height = p.height,
          dpi = 300
        )

      }
    }
  }

  #### TOTAL ABUNDANCE - BY ZONE ----
  ta <- mpa_data$ta_sr_zone[metric %in% c("Total abundance")]
  dat <- ta
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = dbca_zone)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average total abundance \n per sample (+/- SE)") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c(pal)) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }

      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      # if (length(unique(temp2$dbca_sanctuary)) %in% c(1,2,3) ){
      p.height <- 3
      # } else {
      #   p.height <- 3 * ceiling(length(unique(temp2$dbca_sanctuary))/3)
      # }


      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_total_abundance_zone.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

    }
  }

  ## SPECIES RICHNESS ----
  sr <- mpa_data$ta_sr[metric %in% c("Species richness")]

  # Filter to consistently sampled

  # dat <- sr[complete %in% c("Consistently sampled")] # Turned off after meeting with Jordan 6th Nov 2023

  dat <- sr

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
        ggplot2::ylab("Average species richness\n per sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }

      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      # png(paste0("inst/app/www/plots/", park.name, "_", methods, "_species_richness.png"), width = 1000, height = 250, res = 1200, pointsize = 4)
      # print(p)
      # dev.off()

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_species_richness.png"),
        p,
        width = 10,
        height = 3,
        dpi = 300
      )

    }
  }

  #### SPECIES RICHNESS - BY SANCTAURY ----
  sr <- mpa_data$ta_sr_sanctuary[metric %in% c("Species richness")]
  dat <- sr
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average species richness\n per sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
        ggh4x::facet_wrap2(ggplot2::vars(dbca_sanctuary), axes = "all", ncol = 3) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }
      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      if (length(unique(temp2$dbca_sanctuary)) %in% c(1,2,3) ){
        p.height <- 3
      } else {
        p.height <- 3 * ceiling(length(unique(temp2$dbca_sanctuary))/3)
      }

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_species_richness_sanctuary.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

    }
  }

  #### SPECIES RICHNESS - BY SITE ----
  sr <- mpa_data$ta_sr_site[metric %in% c("Species richness")]
  dat <- sr
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    print(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      print(methods)

      if(methods %in% c("stereo-DOVs", "stereo-ROVs")){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(ggplot2::aes(shape = complete), size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average number of species per sample \n(+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                           expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
        ggplot2::scale_shape_manual(values = c("Consistently sampled" = 21, "Intermittently sampled" = 22)) +
        ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        ggh4x::facet_wrap2(ggplot2::vars(site), axes = "all", ncol = 3) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }
      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      if (length(unique(temp2$site)) %in% c(1,2,3) ){
        p.height <- 3
      } else {
        p.height <- 3 * ceiling(length(unique(temp2$site))/3)
      }

      if (length(unique(temp2$site)) > 30 ){ # To fix ningaloo sites
        p.height <- 2 * ceiling(length(unique(temp2$site))/3)
      }

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_species_richness_site.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

      }
    }
  }

  #### SPECIES RICHNESS - BY ZONE ----
  sr <- mpa_data$ta_sr_zone[metric %in% c("Species richness")]
  dat <- sr
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = dbca_zone)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average species richness \n per sample (+/- SE)") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                           expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c(pal)) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }
      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      # if (length(unique(temp2$dbca_sanctuary)) %in% c(1,2,3) ){
        p.height <- 3
      # } else {
      #   p.height <- 3 * ceiling(length(unique(temp2$dbca_sanctuary))/3)
      # }


      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_species_richness_zone.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

    }
  }



  ## STACKED ABUNDANCE PLOT ----
  dat <- mpa_data$top_ten

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      maxn_sum <- temp2 %>%
        dplyr::arrange(desc(maxn))

      p <-   ggplot2::ggplot(maxn_sum, ggplot2::aes(x = reorder(scientific_name, maxn), y = maxn)) +
        ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
        ggplot2::coord_flip() +
        ggplot2::xlab("Species") +
        ggplot2::ylab("Overall abundance") +
        ggplot_mpatheme() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")) +
        ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0, .1)))

      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_top10.png"),
        p,
        width = 10,
        height = 4,
        dpi = 300
      )

    }
  }

  ## SUM ALL FISHED ABUNDANCE ----
  dat <- mpa_data$fished_sum
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average abundance\nof target species\nper sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }
      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_sum_targets.png"),
        p,
        width = 10,
        height = 3,
        dpi = 300
      )

    }
  }

  ## SUM ALL FISHED ABUNDANCE PLUS ----
  dat <- mpa_data$fished_all_sum
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average abundance\nof target species\nper sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }
      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_sum_all_targets.png"),
        p,
        width = 10,
        height = 3,
        dpi = 300
      )

    }
  }

  ## SUM ALL FISHED ABUNDANCE - BY SANCTAURY ----
  dat <- mpa_data$fished_sum_sanctuary
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average abundance\nof target species\nper sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        ggh4x::facet_wrap2(ggplot2::vars(dbca_sanctuary), axes = "all", ncol = 3) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }
      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      if (length(unique(temp2$dbca_sanctuary)) %in% c(1,2,3) ){
        p.height <- 3
      } else {
        p.height <- 3 * ceiling(length(unique(temp2$dbca_sanctuary))/3)
      }

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_sum_targets_sanctuary.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

    }
  }

  ## SUM ALL FISHED ABUNDANCE PLUS - BY SANCTAURY ----
  dat <- mpa_data$fished_all_sum_sanctuary
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average abundance\nof target species\nper sample (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        ggh4x::facet_wrap2(ggplot2::vars(dbca_sanctuary), axes = "all", ncol = 3) +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }
      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      if (length(unique(temp2$dbca_sanctuary)) %in% c(1,2,3) ){
        p.height <- 3
      } else {
        p.height <- 3 * ceiling(length(unique(temp2$dbca_sanctuary))/3)
      }

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_sum_all_targets_sanctuary.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

    }
  }



  ### TROPHIC GROUP ----
  dat <- mpa_data$trophic_sum
  dat <- dat[complete %in% c("Consistently sampled")]

  dat <- dat[!trophic_group %in% c("Unknown")]

  #TODO figure out why trophic has two gazettal (NA and 2018) for Ngari Capes
  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-se, ymax=mean+se), width=.2, position=ggplot2::position_dodge(.5)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab("Average abundance per sample \n(+/- SE)") +
        ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
        ggplot2::scale_x_continuous(
          breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
          expand = ggplot2::expand_scale(mult = c(0, 0.05))
        ) +
        ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
        ggplot_mpatheme()

      gazetted <- unique(temp2$gazetted)
      re_zoned <- unique(temp2$re_zoned)
      min_year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min_year < gazetted) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
            ggplot2::geom_label(
              x = gazetted,
              y = +Inf,
              label = "\n\n gazetted",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(!re_zoned %in% c("NA", NA, NULL)){
        if(min_year < re_zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re_zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      if(methods %in% c("stereo-ROVs+UVC")){
        p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
          ggplot2::geom_label(
            x = 2021,
            y = +Inf,
            label = "\n\n method\nchange",
            size = 5,
            fill = "white",
            check_overlap = TRUE,
            label.size = NA)
      }

      p <- p +
        ggh4x::facet_wrap2(ggplot2::vars(trophic_group), axes = "all", ncol = 1, scales = "free_y")

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      p.height <- 3 * length(unique(temp2$trophic_group))

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_trophic.png"),
        p,
        width = 10,
        height = p.height,
        dpi = 300
      )

    }
  }

  ## INDIVIDUAL SPECIES ----
  #### ABUNDANCE ----
  dat <- mpa_data$abundance_sum
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      # TODO think about changing the cut off

      species_above_zero <- temp2 %>%
        dplyr::group_by(scientific_name) %>%
        #dplyr::summarise(total = sum(mean)) %>%
        dplyr::filter(total > 20) %>%
        dplyr::ungroup()

      for(species in unique(species_above_zero$scientific_name)){

        temp3 <- temp2[scientific_name %in% species]

        p <- ggplot2::ggplot(temp3, ggplot2::aes(x = year, y = mean, fill = status)) +
          ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
          ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
          ggplot2::xlab("Year") +
          ggplot2::ylab("Average abundance \n per sample (+/- SE)") +
          ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                      expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
          ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
          ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
          ggh4x::facet_wrap2(ggplot2::vars(scientific_name), axes = "all", ncol = 1) +
          ggplot_mpatheme()

        gazetted <- unique(temp3$gazetted)
        re_zoned <- unique(temp3$re_zoned)
        min_year <- min(temp3$year)

        # Add gazettal and rezoned dates if they occured after sampling
        if(!gazetted %in% c("NA", NA, NULL)){

          if(min_year < gazetted) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
              ggplot2::geom_label(
                x = gazetted,
                y = +Inf,
                label = "\n\n gazetted",
                size = 5,
                fill = "white",
                check_overlap = TRUE,
                label.size = NA
              )}
        }

        if(!re_zoned %in% c("NA", NA, NULL)){
          if(min_year < re_zoned) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
              ggplot2::geom_label(
                x = re_zoned,
                y = +Inf,
                label = "\n\n rezoned",
                size = 5,
                fill = "white",
                check_overlap = TRUE,
                label.size = NA
              )}
        }

        if(methods %in% c("stereo-ROVs+UVC")){
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
            ggplot2::geom_label(
              x = 2021,
              y = +Inf,
              label = "\n\n method\nchange",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA)
        }

        p

        park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

        ggplot2::ggsave(
          paste0("inst/app/www/plots/species/", park.name, "_", methods, "_", species, ".png"),
          p,
          width = 10,
          height = 3,
          dpi = 300 #1200
        )
      }
    }
  }

  #### ABUNDANCE BY SANCTUARY ----
  dat <- mpa_data$abundance_sum_sanctuary
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      # TODO think about changing the cut off

      species_above_zero <- temp2 %>%
        dplyr::group_by(scientific_name) %>%
        #dplyr::summarise(total = sum(mean)) %>%
        dplyr::filter(total > 10) %>%
        dplyr::ungroup()

      for(species in unique(species_above_zero$scientific_name)){

        temp3 <- temp2[scientific_name %in% species]

        p <- ggplot2::ggplot(temp3, ggplot2::aes(x = year, y = mean, fill = status)) +
          ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) +
          ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black") +
          ggplot2::xlab("Year") +
          ggplot2::ylab("Average abundance \n per sample (+/- SE)") +
          ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                      expand = ggplot2::expand_scale(mult = c(0, 0.05))) +
          ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
          ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
          ggh4x::facet_wrap2(ggplot2::vars(dbca_sanctuary), axes = "all", ncol = 3) +
          ggplot_mpatheme()

        gazetted <- unique(temp3$gazetted)
        re_zoned <- unique(temp3$re_zoned)
        min_year <- min(temp3$year)

        # Add gazettal and rezoned dates if they occured after sampling
        if(!gazetted %in% c("NA", NA, NULL)){

          if(min_year < gazetted) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = gazetted), linetype = "dashed") +
              ggplot2::geom_label(
                x = gazetted,
                y = +Inf,
                label = "\n\n gazetted",
                size = 5,
                fill = "white",
                check_overlap = TRUE,
                label.size = NA
              )}
        }

        if(!re_zoned %in% c("NA", NA, NULL)){
          if(min_year < re_zoned) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re_zoned), linetype = "dashed") +
              ggplot2::geom_label(
                x = re_zoned,
                y = +Inf,
                label = "\n\n rezoned",
                size = 5,
                fill = "white",
                check_overlap = TRUE,
                label.size = NA
              )}
        }

        if(methods %in% c("stereo-ROVs+UVC")){
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 2021), linetype = "dashed") +
            ggplot2::geom_label(
              x = 2021,
              y = +Inf,
              label = "\n\n method\nchange",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA)
        }

        p

        park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

        if (length(unique(temp3$dbca_sanctuary)) %in% c(1,2,3) ){
          p.height <- 3
        } else {
          p.height <- 3 * ceiling(length(unique(temp3$dbca_sanctuary))/3)
        }


        ggplot2::ggsave(
          paste0("inst/app/www/plots/species/", park.name, "_", methods, "_", species, "_sanctuary.png"),
          p,
          width = 10,
          height = 3,
          dpi = 300
        )
      }
    }
  }

  ## KERNEL DENSIY ESTIMATE ----
  #
  # dat <- mpa_data$fished_complete_length
  #
  # unique(dat$marine_park)
  #
  # for(marinepark in unique(dat$marine_park)){
  #
  #   temp <- dat[marine_park %in% c(marinepark)]
  #
  #   for(methods in unique(temp$method)){
  #
  #     temp2 <- temp[method %in% c(methods)] #%>% dplyr::glimpse()
  #
  #     more_than_20 <- temp2 %>%
  #       dplyr::mutate(latin = paste(family, genus, species)) %>%
  #       dplyr::group_by(marine_park, method, campaignid, status, year, scientific_name, latin) %>%
  #       dplyr::summarise(number = sum(number)) %>%
  #       dplyr::filter(number > 20) %>%
  #       dplyr::ungroup() %>%
  #       dplyr::distinct(marine_park, method, campaignid, status, year, scientific_name, latin)
  #
  #     to_plot <- c(more_than_20$scientific_name)
  #
  #     for(i in to_plot){
  #
  #       unique(temp2$scientific_name)
  #       print(i)
  #
  #       temp_new <- temp2[scientific_name %in% i]
  #
  #       temp3 <- temp_new[length > 0]
  #       temp3 <- temp3[!is.na(length)]
  #       temp3 <- temp3[complete %in% c("Consistently sampled")]
  #
  #       temp3 <- temp3 %>% dplyr::semi_join(more_than_20)
  #
  #       p <- ggplot2::ggplot(temp3, ggplot2::aes(x = length, fill = status)) +
  #         ggplot2::geom_density(ggplot2::aes(y = ..density.. * 1000), alpha = 0.5, size = 0.7) +
  #         ggplot2::theme(legend.position = ("bottom")) +
  #         ggplot2::theme(
  #           strip.text.y = ggplot2::element_text(size = 12, angle = 270),
  #           strip.background = ggplot2::element_blank(),
  #           axis.title = ggplot2::element_text(face = "bold"),
  #           plot.title = ggplot2::element_text(face = "italic", hjust = 0.5),
  #           strip.text.x = ggplot2::element_text(size = 14),
  #           panel.grid.major = ggplot2::element_blank(),
  #           panel.grid.minor = ggplot2::element_blank()
  #         ) +
  #         ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
  #         ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
  #         ggplot2::ylab("Weighted KDE (*1000)") +
  #         ggplot2::xlab("Total Length (mm)") +
  #         ggplot_mpatheme()
  #         #ggplot2::facet_grid(rows = ggplot2::vars(year))
  #
  #       park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))
  #
  #       # p.height <- 3 * length(unique(temp3$year))
  #
  #       ggplot2::ggsave(
  #         paste0("inst/app/www/plots/species/", park.name, "_", methods, "_", i, "_KDE.png"),
  #         p,
  #         width = 10,
  #         height = 3,
  #         dpi = 300
  #       )
  #     }
  #   }
  # }


 ## CTI Plots ----

  library(ggimage)

  dat <- mpa_data$cti_park %>%
    dplyr::mutate(year = as.numeric(year),
                  legend = "All sites")

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat[marine_park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = cti, fill = legend)) +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black", fill ="darkslategrey", alpha = 0.3) +
        ggplot2::geom_point(shape = 23, size = 4, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::scale_fill_manual(values = c("cadetblue"))+
        ggplot2::geom_errorbar(ggplot2::aes(ymin = cti - se, ymax = cti + se), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
        ggplot2::ylab("Average CTI (°C)\nper sample (+/- SE)") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot_mpatheme()

      min_year <- min(temp2$year)
      heatwave <- 2011

      # Add 2011 heatwave if they occured after sampling
      if(!heatwave %in% c("NA", NA, NULL)){

        if(min_year < heatwave) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = heatwave), linetype = "dashed", colour = "red") +
            ggplot2::geom_label(
              x = heatwave,
              y = +Inf,
              label = "\n\n Marine heat wave",
              size = 3,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }

      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", park.name, "_", methods, "_cti.png"),
        p,
        width = 10,
        height = 3,
        dpi = 300
      )

    }
  }


# CORAL PLOTS ----
## PERCENT COVER ----

  library(dplyr)
  library(plotrix)
  install.packages("fishualize")
  library(fishualize)
  library(paletteer)

  #### ALL PARKS BY YEAR Faceted ----

  dat <- mpa_data$coral_cover_transect %>%
    mutate(year = plot_year) %>%
    group_by(marine_park, year) %>%
    summarise(coral_cover = mean(percent_cover), se = plotrix::std.error(percent_cover), sd = sd(percent_cover)) %>%
    ungroup()

    p <- ggplot2::ggplot(dat, ggplot2::aes(x = year, y = coral_cover)) +
      ggplot2::geom_point(shape = 23, size = 4, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = coral_cover - se, ymax = coral_cover + se), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
      ggplot2::ylab("Percent Coral Cover (+/- SE)") +
      ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black", fill = "lightblue") +
      ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                  expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
      ggh4x::facet_wrap2(ggplot2::vars(marine_park), axes = "all", ncol = 1) +
      ggplot_mpatheme()

    p

    park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

    parknum <- dat %>% distinct(marine_park) %>% summarise(n=n())

    ggplot2::ggsave(
      paste0("inst/app/www/plots/", "Coral_All_Parks_coral_cover.png"),
      p,
      width = 10,
      height = 3*parknum$n,
      dpi = 300)


  ### ALL PARKS - BY YEAR ----


  dat <- mpa_data$coral_cover_transect %>%
    mutate(year = plot_year) %>%
    group_by(marine_park, year) %>%
    summarise(coral_cover = mean(percent_cover), se = plotrix::std.error(percent_cover), sd = sd(percent_cover)) %>%
    ungroup()

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat %>% filter(marine_park == marinepark)

      p <- ggplot2::ggplot(temp, ggplot2::aes(x = year, y = coral_cover)) +
        ggplot2::geom_point(shape = 23, size = 4, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = coral_cover - se, ymax = coral_cover + se), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
        ggplot2::ylab("Percent Coral Cover (+/- SE)") +
        ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black", fill = "lightblue") +
        ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
        ggplot_mpatheme()

      p

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", "Coral_",park.name,"_coral_cover.png"),
        p,
        width = 10,
        height = 3,
        dpi = 300
      )

  }

  ### ALL PARKS - BY SITE & YEAR ----

  dat <- mpa_data$coral_cover_transect %>%
    mutate(year = plot_year) %>%
    group_by(marine_park, site, year) %>%
    summarise(coral_cover = mean(percent_cover), se = plotrix::std.error(percent_cover), sd = sd(percent_cover)) %>%
    ungroup() %>%
    filter(!(is.na(se)))

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat %>% filter(marine_park == marinepark)

    # for(sites in unique(temp$site)){
    #
    #   temp2 <- temp %>% filter(site == sites)

    p <- ggplot2::ggplot(temp, ggplot2::aes(x = year, y = coral_cover)) +
      ggplot2::geom_point(shape = 23, size = 4, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = coral_cover - sd, ymax = coral_cover + sd), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
      ggplot2::ylab("Percent Coral Cover (+/- SD)") +
      ggplot2::expand_limits(y=c(0, NA)) +
      ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black", fill = "lightblue") +
      ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                  expand = ggplot2::expansion(mult = c(0, 0.05))) +
      #ggplot2::scale_y_continuous(limits = c(0, NA))+
      ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
      ggh4x::facet_wrap2(ggplot2::vars(site), axes = "all", ncol = 2) +
      ggplot_mpatheme()

    p

    park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

    sitenum <- temp %>% distinct(site) %>% summarise(n=n())

    ggplot2::ggsave(
      paste0("inst/app/www/plots/", "Coral_",park.name,"_site_coral_cover.png"),
      p,
      width = 20,
      height = (3*sitenum$n)/2,
      dpi = 300
    )

  }

  ### Reef Zone - Per Park/Per Year ----

  dat <- coral_cover_reefzone

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat %>% filter(marine_park == marinepark)

    # for(sites in unique(temp$site)){
    #
    #   temp2 <- temp %>% filter(site == sites)

    p <- ggplot2::ggplot(temp, ggplot2::aes(x = plot_year, y = percent_cover)) +
      ggplot2::geom_point(shape = 23, size = 4, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = percent_cover - se, ymax = percent_cover + se), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
      ggplot2::ylab("Percent Coral Cover (+/- SE)") +
      ggplot2::expand_limits(y=c(0, NA)) +
      ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, col = "black", fill = "lightblue") +
      ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                  expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::coord_cartesian(ylim = c(0, NA))+
      ggplot2::scale_fill_manual(values = c("#b9e6fb", "#7bbc63")) +
      ggh4x::facet_wrap2(ggplot2::vars(reef_zone), axes = "all", ncol = 2) +
      ggplot_mpatheme()

    p

    park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

    sitenum <- temp %>% distinct(reef_zone) %>% summarise(n=n())

    ggplot2::ggsave(
      paste0("inst/app/www/plots/", "Coral_",park.name,"_reefzone_coral_cover.png"),
      p,
      width = 20,
      height = ceiling(sitenum$n/2)*4,
      dpi = 300
    )

  }

  # STACKED BAR PLOTS ----
  ### ALL PARKS - PER YEAR ----
  pal <- c("darkblue", "dodgerblue3", "darkturquoise", "purple", "gold", "coral","darkgreen", "#1fab89")

  dat <- mpa_data$coral_cover_species %>%
    #dplyr::rename(genus = level3class) %>%
    #dplyr::filter(!(level3class == "Hard coral")) %>%
    dplyr::group_by(marine_park, year, level3class) %>%
    dplyr::summarise(percent_cover = mean(cover)) %>%
    #dplyr::filter(min_rank(desc(percent_cover)) <= 4) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(park_family = paste(marine_park, level3class, sep="_"))

  other <-
    dplyr::anti_join(dat, coral_top_families, by = "park_family") %>%
    dplyr::group_by(marine_park, year) %>%
    dplyr::summarise(cover = sum(percent_cover)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(level3class = "Other Hard Coral")

  top_fams <-
    dplyr::semi_join(dat, coral_top_families, by = "park_family") %>%
    dplyr::group_by(marine_park, year, level3class) %>%
    dplyr::summarize(cover = mean(percent_cover)) %>%
    dplyr::ungroup()

  dat <- dplyr::bind_rows(other, top_fams)

  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    message(marinepark)

    temp <- dat %>% dplyr::filter(marine_park == marinepark)

    p <- ggplot2::ggplot(temp, ggplot2::aes(x = year, y = cover, fill = level3class)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::ylab("Mean Coral Cover (%)") +
      ggplot2::xlab("Year")+
      ggplot2::scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
                                  expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::scale_fill_manual(values = pal) +
      #ggh4x::facet_wrap2(ggplot2::vars(site), axes = "all", ncol = 2) +
      ggplot_mpatheme()

    p

    park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

    ggplot2::ggsave(
      paste0("inst/app/www/plots/", "Coral_", park.name,"_species_coral_cover.png"),
      p,
      width = 10,
      height = 4,
      dpi = 300)

  }

  ### ALL PARKS - PER SITE ----

  drop_sites <- coral_cover_species %>%
    dplyr::distinct(marine_park, site, year) %>%
    dplyr::group_by(marine_park, site) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 2) %>%
    dplyr::mutate(park_site = paste(marine_park, site, sep= "_")) %>%
    dplyr::select(park_site)

  dat <- coral_cover_species %>%
    dplyr::mutate(park_family = paste(marine_park, level3class, sep = "_")) %>%
    dplyr::group_by(marine_park, year, site, park_family, level3class) %>%
    dplyr::summarise(percent_cover = mean(cover)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(park_site_year = paste(marine_park, site, year, sep="_"),
                  park_family = paste(marine_park, level3class, sep = "_"))

  other <- dplyr::anti_join(dat, coral_top_families, by = "park_family") %>%
    dplyr::group_by(marine_park, year, site, level3class) %>%
    dplyr::summarize(cover = mean(percent_cover)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(level3class) %>%
    dplyr::group_by(marine_park, year, site) %>%
    dplyr::summarise(percent_cover = sum(cover)) %>%
    dplyr::mutate(level3class = "Other Hard Coral") %>%
    dplyr::ungroup()

  top_fams <- dplyr::semi_join(dat, coral_top_families, by = "park_family") %>%
    dplyr::group_by(marine_park, year, site, level3class) %>%
    dplyr::summarize(cover = mean(percent_cover)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(percent_cover = cover)

  all <- dplyr::bind_rows(top_fams, other) %>%
    dplyr::mutate(park_site = paste(marine_park, site, sep = "_")) %>%
    dplyr::semi_join(drop_sites)


  unique(dat$marine_park)

  for(marinepark in unique(dat$marine_park)){

    temp <- all %>% dplyr::filter(marine_park == marinepark)

    message(marinepark)

    for(sites in unique(temp$site)){

      years <- temp %>%
        dplyr::select(year) %>%
        dplyr::distinct() %>%
        dplyr::mutate(marine_park = marinepark, site = sites, percent_cover = 0, level3class = "")

      temp2 <- temp %>% dplyr::filter(site == sites) #%>%

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = percent_cover, fill = level3class)) +
        ggplot2::geom_bar(position="stack", stat="identity", width = 0.7) +
        ggplot2::ylab("Mean Coral Cover (%)") +
        ggplot2::xlab("Year")+
        #ggplot2::ylim(min(temp$percent_cover), max(temp$percent_cover))+
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::scale_x_continuous(breaks=seq(min(years$year), max(years$year), 1)) +
        #ggplot2::coord_cartesian(xlim = c(min(years$year), max(years$year))) +
        #ggplot2::geom_col(width = 0.4) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.title = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank()) +
        ggplot_mpatheme() +
        ggplot2::ggtitle(paste0(sites))

      p

        # ggplot2::geom_text(data = labs2, mapping = ggplot2::aes(x=id,y=cover, label=site ,hjust=hjust), size=4, angle=labs2$angle,inherit.aes = FALSE)
        # p <- p + ggplot2::coord_polar(start=0)

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      # sitenum <- temp %>% distinct(year) %>% summarise(n=n())

      ggplot2::ggsave(
        paste0("inst/app/www/plots/", "Coral_", park.name, "_site_", sites, "_species_coral_cover.png"),
        p,
        width = 10,
        height = 4,
        dpi = 300)

      message(sites)
    }
  }

  }

