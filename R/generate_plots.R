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
  message("This function takes a couple minutes to run")

  load("inst/data/mpa_data.Rdata")

  # TOTAL ABUNDANCE ----
  ta <- mpa_data$ta.sr[metric %in% c("Total abundance")]

  # Filter to consistently sampled
  dat <- ta[complete %in% c("Consistently sampled")]

  for(marinepark in unique(dat$marine.park)){

    temp <- dat[marine.park %in% c(marinepark)]

    for(method in unique(temp$method)){

      temp2 <- dat[marine.park %in% c(marinepark)]

      p <- ggplot2::ggplot(temp2, ggplot2::aes(x = year, y = mean, fill = status)) +
        ggplot2::geom_point(shape = 23, size = 6, col = "black", position = ggplot2::position_dodge(width = 0.5)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width=.2, position = ggplot2::position_dodge(.5)) + ggplot2::xlab("Year") +
        ggplot2::ylab("Average total abundance per sample \n(+/- SE)") +
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
          p <- p + ggplot2::geom_vline(aes(xintercept = gazetted), linetype = "dashed") +
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
          p <- p + ggplot2::geom_vline(aes(xintercept = re.zoned), linetype = "dashed") +
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

      png(paste0("inst/app/www/plots/", park.name, "_", method, "_total_abundance.png"), width = 800, height = 400)
      print(p)
      dev.off()

    }
  }

}

