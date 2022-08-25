# Setup -----------------------------------------------------------------------#
library(mpaviewer)
library(ckanr)
library(fs)
library(glue)
library(magrittr)

# Absolute paths to saved data and config files -------------------------------#
fn_renv <- "/app/inst/data/.Renviron"

# Gate checks -----------------------------------------------------------------#
"[{Sys.time()}] Cronjob startup and gatechecks" %>% glue::glue() %>% print()
# There are two mounted persistent volumes, one for data, the other for config.
# If the data directory does not exist, the save functions below will fail.
print("Shared data and config directory exists:")
print(fs::dir_exists("/app/inst/data"))

# The file .Renviron has to be created once manually through a shell into the
# running container, pasting in any environment variables we need for the script.
print("Persisted .Renviron exists:")
print(fs::file_exists(fn_renv))

# Since our .Renviron is not in a default location, we have to read it explicitly.
# The non-default location of .Renviron allows to mount a persistent volume
# containing the .Renviron to a (non-standard) folder, here "/app/config".
# This avoids collisions between the mounted volume and other files in the
# target folder inside the running Docker container.
readRenviron(fn_renv)

# This should indicate whether the environment variables have been read.
print("ckanr settings:")
print(ckanr::ckanr_settings())

# CKAN to local download ------------------------------------------------------#
"[{Sys.time()}] Downloading source data from CKAN" %>% glue::glue() %>% print()
# TODO this does nothing yet
mpaviewer::download_source_data()

# Local source data processing ------------------------------------------------#
# "[{Sys.time()}] Generating data for dashboard app" %>% glue::glue() %>% print()
# TODO enable this once download from CKAN is live
# mpaviewer::generate_data()
# For now, we download mpa_data.rds from CKAN:
mpaviewer::download_data(data_dir = "/app/inst/data")

"[{Sys.time()}] Data ETL finished." %>% glue::glue() %>% print()
