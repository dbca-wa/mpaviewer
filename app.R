# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Google Sheets Auth
# options(gargle_oauth_cache = ".secrets")
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# googlesheets4::gs4_auth()
# 1

# Step 1. Load all functions from package

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# library(data.table)
# Step 2. Download all data from GoogleDrive (only if it has been updated)
# mpaviewer::googledrive_download_data()

# Step 3. Generate new data
# Sys.time()
# mpaviewer::generate_data()
# Sys.time()

# Step 4. Run demo app
options("golem.app.prod" = TRUE)
mpaviewer::run_app()

# Step 5. Deploy to shiny server for testing
# rsconnect::deployApp()
# test <- readRDS(here::here("inst/data/mpa_data.rds"))$all.data
# test <-


# TESTING - TODO DELETE
# data.table::key(test)
# t <- load(here::here("inst/data/data.Rdata"))
#
# data.table::key(x$abundance)
#
#
# dat <- mpa_data$metadata
# data.table::key(dat)
#
# choices <- dat[marine.park %in% c("Marmion Marine Park", "Montebello Islands")] %>% dplyr::glimpse()
# choices <- choices %>%
#   dplyr::distinct(method) %>%
#   dplyr::pull("method")
#
#
# test <- t$all.data
#
# unique(test$marine.park)
# unique(test$method)
# unique(test$metric)
#
# fish_ta <- test %>%
#   dplyr::filter(method %in% "stereo-BRUVs") %>%
#   dplyr::filter(metric %in% "Total abundance") %>%
#   dplyr::filter(is.na(year))
#
# unique(fish_ta$year)
#
# library(ggplot2)
#
# p <- ggplot(
#   fish_ta,
#   aes(x = year, y = value, fill = status))+
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,
#     size = 6,
#     col = "black",
#     position = position_dodge(width = 0.5)
#   ) +
#   # stat_summary(
#   #   fun.min = se.min,
#   #   fun.max = se.max,
#   #   geom = "errorbar",
#   #   width = 0.1,
#   #   col = "black",
#   #   position = position_dodge(width = 0.5)
#   # ) +
#   xlab("Year") +
#   ylab("Average total abundance per sample \n(+/- SE)") +
#   # annotation_custom(label) +
#   stat_smooth(
#     method = "gam",
#     formula = y ~ s(x, k = 3), # Removed all the mgcv:: as it was breaking the plots
#     size = 1,
#     col = "black"
#   ) +
#   # scale_y_continuous(expand = expansion(mult = 10)) +
#   # scale_x_continuous(breaks = c(unique(fish_ta()$year))) +
#   scale_x_continuous(breaks = seq(min(fish_ta$year)-1, max(fish_ta$year)+1, 2)) +
#   scale_fill_manual(values = c("#b9e6fb", "#7bbc63"))# +
#   #ggplot_mpatheme() +
#
#   # ggbreak::scale_x_break(c(1989, 2006)) + Maybe investigate this later
#
#   facet_wrap(marine.park ~ ., scales = "free", ncol = 1)
# p
# ggpl
