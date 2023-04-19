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
library(data.table)
# Step 2. Download all data from GoogleDrive (only if it has been updated)
# mpaviewer::googledrive_download_data()

# Step 3. Generate new data
# Sys.time()
# mpaviewer::generate_data()
# Sys.time()


load("inst/data/mpa_data.Rdata")
load("example.Rdata")

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
