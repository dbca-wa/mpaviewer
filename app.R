## Authenticate GoogleDrive and GoogleSheets ----
# options(gargle_oauth_cache = ".secrets")
## check the value of the option, if you like
# gargle::gargle_oauth_cache()
# googlesheets4::gs4_auth()

## If CheckEM package has changed you will need to remove the package and reinstall ----
# remove.packages("CheckEM")
# renv::install("GlobalArchiveManual/CheckEM")
# devtools::install_github("GlobalArchiveManual/CheckEM")
# library(CheckEM)

# Step 1. Load all functions from package ----
 pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Set raw and project directory
# raw_dir = paste0("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Department of Biodiversity, Conservation and Attractions/iLab_fish/!Essential_Files/Dashboard_Data")
# project_dir = "C:/Users/damon/Documents/Work/Github/mpaviewer"

# Step 2. Download all data from GoogleDrive (only run this section if it has been updated) ----
# mpaviewer::googledrive_download_data(raw_dir = "C:/Users/damon/Documents/Work/Github/mpaviewer_data", project_dir = "C:/Users/damon/Documents/Work/Github/mpaviewer") # takes ~15 minutes to run

# source(paste0(project_dir, "/R/projectfolder_data_download.R"))

# Step 3. Generate new summarised data ----
# mpaviewer::generate_data(raw_dir = raw_dir) # takes ~7 minutes to run

# Step 4. Generate new plots
# Sys.time() # Takes a few hours to run
# mpaviewer::generate_plots() # TODO figure out how to get this to work
# Sys.time()

# Step 5. Run demo app
# options("golem.app.prod" = TRUE)

# RUN app
 mpaviewer::run_app()

# Step 6. Deploy to shiny server for testing
# rsconnect::deployApp()
