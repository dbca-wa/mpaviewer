
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

  - Data is loaded from a locally saved `.rds` file, or freshly loaded
    from the DBCA data catalogue (and then saved as `.rds`). The locally
    saved file lives on a persistent volume.
  - The main area shows a map and a data area.
  - The map displays all MPAs, sites, and transects once ready.
  - The user can filter data through cascading dropdown menus:
      - All MPAs or one selected MPA (one or many)
      - All site within selected MPAs or selected sites (one or many)
      - All transects within selected sites or selected transects (one
        or many)
      - Time - no filtering as we show timeseries
  - The map shows selected locations, and data summarised for all
    selected locations.
  - The map has popups for each selected location with some useful
    content.
  - The data area shows tabs, one for each asset.
  - Each asset has a “display data” module that knows how to generate
    data summaries for each asset. The individual products (tables,
    plots) are created with helper functions.

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
