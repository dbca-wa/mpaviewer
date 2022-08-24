
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpaviewer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dbca-wa/mpaviewer/workflows/R-CMD-check/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions)
<!-- badges: end -->

The goal of mpaviewer is to visualise marine monitoring data to DBCA
staff.

## Architecture

-   Data is loaded from a locally saved `.rds` file, or freshly loaded
    from Google Drive or the DBCA data catalogue (and then saved as
    `.rds`). The locally saved file lives on a persistent volume.

## Development

`mpaviewer` is an RShiny app using [Shiny
modules](https://shiny.rstudio.com/articles/modules.html), developed
using [`golem`](https://mastering-shiny.org/scaling-modules.html).

The commands to develop new functionality are in `dev/02_dev.R`.

## Deployment

GitHub Actions are configured to build a new Docker image for every tag
starting with “v”, such as “v1.0.0”. The images are pushed to the GitHub
container registry `ghcr.io`. The DBCA maintainer of `mpaviewer` then
updates the tag number (e.g. `1.0.0`) in the image run by Kubernetes,
which will trigger a hot reload of the image.

The commands to deploy a new version are in `dev/03_deploy.R`.
