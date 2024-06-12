#' Generate one data object to use in server.R
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

  # BEGIN PLOTTING ----
  ## TOTAL ABUNDANCE ----
  ta <- mpa_data$ta.sr[metric %in% c("Total abundance")]

  # Filter to consistently sampled
  dat <- ta[complete %in% c("Consistently sampled")]

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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
  ta <- mpa_data$ta.sr.sanctuary[metric %in% c("Total abundance")]
  dat <- ta
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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
  ta <- mpa_data$ta.sr.site[metric %in% c("Total abundance")]
  dat <- ta
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    print(marinepark)

    temp <- dat[marine.park %in% c(marinepark)]

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
        re.zoned <- unique(temp2$re.zoned)
        min.year <- min(temp2$year)

        # Add gazettal and rezoned dates if they occured after sampling
        if(!gazetted %in% c("NA", NA, NULL)){

          if(min.year < gazetted) {
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

        if(!re.zoned %in% c("NA", NA, NULL)){
          if(min.year < re.zoned) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
              ggplot2::geom_label(
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
  ta <- mpa_data$ta.sr.zone[metric %in% c("Total abundance")]
  dat <- ta
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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
  sr <- mpa_data$ta.sr[metric %in% c("Species richness")]

  # Filter to consistently sampled
  dat <- sr[complete %in% c("Consistently sampled")]

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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
  sr <- mpa_data$ta.sr.sanctuary[metric %in% c("Species richness")]
  dat <- sr
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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
  sr <- mpa_data$ta.sr.site[metric %in% c("Species richness")]
  dat <- sr
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    print(marinepark)

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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
  sr <- mpa_data$ta.sr.zone[metric %in% c("Species richness")]
  dat <- sr
  # dat <- sr[complete %in% c("Consistently sampled")]  # Turned off after meeting with Jordan 6th Nov 2023

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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
  dat <- mpa_data$top.ten

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      maxn.sum <- temp2 %>%
        dplyr::arrange(desc(maxn))

      p <-   ggplot2::ggplot(maxn.sum, ggplot2::aes(x = reorder(scientific, maxn), y = maxn)) +
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
  dat <- mpa_data$fished.sum
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    message(marinepark)

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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

  ## SUM ALL FISHED ABUNDANCE - BY SANCTAURY ----
  dat <- mpa_data$fished.sum.sanctuary
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    message(marinepark)

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
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


  ### TROPHIC GROUP ----
  dat <- mpa_data$trophic.sum
  dat <- dat[complete %in% c("Consistently sampled")]

  #TODO figure out why trophic has two gazettal (NA and 2018) for Ngari Capes
  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

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
      re.zoned <- unique(temp2$re.zoned)
      min.year <- min(temp2$year)

      # Add gazettal and rezoned dates if they occured after sampling
      if(!gazetted %in% c("NA", NA, NULL)){

        if(min.year < gazetted) {
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

      if(!re.zoned %in% c("NA", NA, NULL)){
        if(min.year < re.zoned) {
          p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
            ggplot2::geom_label(
              x = re.zoned,
              y = +Inf,
              label = "\n\n rezoned",
              size = 5,
              fill = "white",
              check_overlap = TRUE,
              label.size = NA
            )}
      }
      p <- p +
        ggh4x::facet_wrap2(ggplot2::vars(trophic.group), axes = "all", ncol = 1, scales = "free_y")

      park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

      p.height <- 3 * length(unique(temp2$trophic.group))

      # if (length(unique(temp2$trophic.group)) %in% c(1,2,3) ){
      #   p.height <- 3
      # } else {
      #   p.height <- 3 * ceiling(length(unique(temp2$trophic.group))/3)
      # }

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
  dat <- mpa_data$abundance.sum
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      species_above_zero <- temp2 %>%
        dplyr::group_by(scientific) %>%
        dplyr::summarise(total = sum(mean)) %>%
        dplyr::filter(total > 0) %>%
        dplyr::ungroup()

      for(species in unique(species_above_zero$scientific)){

        temp3 <- temp2[scientific %in% species]

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
          ggh4x::facet_wrap2(ggplot2::vars(scientific), axes = "all", ncol = 1) +
          ggplot_mpatheme()

        gazetted <- unique(temp3$gazetted)
        re.zoned <- unique(temp3$re.zoned)
        min.year <- min(temp3$year)

        # Add gazettal and rezoned dates if they occured after sampling
        if(!gazetted %in% c("NA", NA, NULL)){

          if(min.year < gazetted) {
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

        if(!re.zoned %in% c("NA", NA, NULL)){
          if(min.year < re.zoned) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
              ggplot2::geom_label(
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
  dat <- mpa_data$abundance.sum.sanctuary
  dat <- dat[complete %in% c("Consistently sampled")]

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)]

      species_above_zero <- temp2 %>%
        dplyr::group_by(scientific) %>%
        dplyr::summarise(total = sum(mean)) %>%
        dplyr::filter(total > 0) %>%
        dplyr::ungroup()

      for(species in unique(species_above_zero$scientific)){

        temp3 <- temp2[scientific %in% species]

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
        re.zoned <- unique(temp3$re.zoned)
        min.year <- min(temp3$year)

        # Add gazettal and rezoned dates if they occured after sampling
        if(!gazetted %in% c("NA", NA, NULL)){

          if(min.year < gazetted) {
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

        if(!re.zoned %in% c("NA", NA, NULL)){
          if(min.year < re.zoned) {
            p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = re.zoned), linetype = "dashed") +
              ggplot2::geom_label(
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
  dat <- mpa_data$fished.complete.length

  unique(dat$marine.park)

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

    for(methods in unique(temp$method)){

      temp2 <- temp[method %in% c(methods)] #%>% dplyr::glimpse()

      more_than_20 <- temp2 %>%
        dplyr::mutate(latin = paste(family, genus, species)) %>%
        dplyr::group_by(marine.park, method, campaignid, status, year, scientific, latin) %>%
        dplyr::summarise(number = sum(number)) %>%
        dplyr::filter(number > 20) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(marine.park, method, campaignid, status, year, scientific, latin)

      to_plot <- c(more_than_20$scientific)

      for(i in to_plot){

        unique(temp2$scientific)
        print(i)

        temp_new <- temp2[scientific %in% i]

        temp3 <- temp_new[length > 0]
        temp3 <- temp3[!is.na(length)]
        temp3 <- temp3[complete %in% c("Consistently sampled")]

        temp3 <- temp3 %>% dplyr::semi_join(more_than_20)

        p <- ggplot2::ggplot(temp3, ggplot2::aes(x = length, fill = status)) +
          ggplot2::geom_density(ggplot2::aes(y = ..density.. * 1000), alpha = 0.5, size = 0.7) +
          ggplot2::theme(legend.position = ("bottom")) +
          ggplot2::theme(
            strip.text.y = ggplot2::element_text(size = 12, angle = 270),
            strip.background = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(face = "bold"),
            plot.title = ggplot2::element_text(face = "italic", hjust = 0.5),
            strip.text.x = ggplot2::element_text(size = 14),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
          ggplot2::scale_fill_manual(values = c("Fished" = "#b9e6fb", "No-take" = "#7bbc63")) +
          ggplot2::ylab("Weighted KDE (*1000)") +
          ggplot2::xlab("Total Length (mm)") +
          ggplot_mpatheme() +
          ggplot2::facet_grid(rows = ggplot2::vars(year))

        p

        park.name <- stringr::str_replace_all(tolower(marinepark), c("marine park" = "", "island marine reserve" = "", " " = ""))

        p.height <- 3 * length(unique(temp3$year))

        ggplot2::ggsave(
          paste0("inst/app/www/plots/species/", park.name, "_", methods, "_", i, "_KDE.png"),
          p,
          width = 10,
          height = p.height,
          dpi = 300
        )
      }
    }
  }





}

