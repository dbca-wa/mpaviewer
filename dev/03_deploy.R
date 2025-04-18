# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
# golem::add_rstudioconnect_file()
# golem::add_shinyappsio_file()
# golem::add_shinyserver_file()

## Docker ----
## Update Dockerfile
golem::add_dockerfile()
# Git commit
# Trigger a new Image build through pushing a new tag
usethis::use_version(which="major")
usethis::use_version(which="minor")
usethis::use_version(which="patch")
devtools::document()
v <- packageVersion("mpaviewer")
system(glue::glue("git tag -a v{v} -m 'v{v}'"))
system(glue::glue("git push --tags"))
# Update mpaviewer in rancher

## If you want to deploy to ShinyProxy
# golem::add_dockerfile_shinyproxy()

## If you want to deploy to Heroku
# golem::add_dockerfile_heroku()

# Release new docker image
# mpaviewer::make_docker() # superseded by GH package
