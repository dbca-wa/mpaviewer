## Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# # Google Sheets Auth ----
# options(gargle_oauth_cache = ".secrets")
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# googlesheets4::gs4_auth()
# 2

## If CheckEM package has changed
# remove.packages("CheckEM")
# renv::install("GlobalArchiveManual/CheckEM")
# devtools::install_github("GlobalArchiveManual/CheckEM")

# remove.packages("GlobalArchive")
# renv::install("UWAMEGFisheries/GlobalArchive")
# devtools::install_github("UWAMEGFisheries/GlobalArchive")

# Step 1. Load all functions from package
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Step 2. Download all data from GoogleDrive (only if it has been updated)
# mpaviewer::googledrive_download_data() # takes 11 minutes

# Step 3. Generate new data
# Sys.time() # takes 7 mins
# mpaviewer::generate_data()
# Sys.time()

# Step 4. Generate new plots
# Sys.time() # Takes a few hours to run
# mpaviewer::generate_plots() # TODO figure out how to get this to work
# Sys.time()

# Step 5. Run demo app
options("golem.app.prod" = TRUE)

# RUN app
mpaviewer::run_app()

# Step 6. Deploy to shiny server for testing
# rsconnect::deployApp()

# # For profiling app
# profvis::profvis({
#   print(
#     mpaviewer::run_app()
#   )
# })
