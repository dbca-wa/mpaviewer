
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpaviewer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Package
checks](https://github.com/dbca-wa/mpaviewer/workflows/R-CMD-check/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions)
[![Docker
app](https://github.com/dbca-wa/mpaviewer/actions/workflows/docker.yaml/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions/workflows/docker.yaml)
[![Docker
data](https://github.com/dbca-wa/mpaviewer/actions/workflows/docker_cron.yaml/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions/workflows/docker_cron.yaml)
[![Docs](https://github.com/dbca-wa/mpaviewer/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions/workflows/pkgdown.yaml)

<!-- badges: end -->

[mpaviewer](https://mpaviewer.dbca.wa.gov.au/) is a dashboard of marine
monitoring data for DBCA staff.

## Architecture

- Data is loaded from a locally-created `.RData` file. The team is in
  the process of transitioning to Azure blob storage for data.

## Data

The dashboard runs off a single `mpa_data.rds` R save file which is
produced from a range of source data files (CSV, TXT, SHP). This file is
created by running the functions `mpaviewer::copy_data()`,
`mpaviewer::generate_data()` and `mpaviewer::generate_plots()`, which
pull the raw data from the Marine Science SharePoint.

### Update source data and create dashboard data

Data owners can update the data by correcting data in the original
SharePoint source, and then requesting an analyst to run the code
defined in `dev/02_dev.R`. The functions contained in this script will
pull new source data, update the `mpa_data.RData` file, and create new
static plots to be rendered by the dashboard.

### Update the dashboard with new data

A Github Action is triggered by a new tag being pushed. This action runs
a Dockerfile which creates a new image, stored in the Github Container
Registry.

The dashboard can be tested locally by using `mpaviewer::run_app()`.

## Development

`mpaviewer` is an RShiny app using [Shiny
modules](https://shiny.rstudio.com/articles/modules.html), developed
using [`golem`](https://mastering-shiny.org/scaling-modules.html).

The commands to develop new functionality are in `dev/02_dev.R`.

- Create a GH issue for each task, keep it as small and self-contained
  as possible.
- Work on one issue at a time.
- Once R CMD Check passes and all build errors and warnings are
  addressed, commit changes.
- Write descriptive and well-formed commit messages and reference the GH
  issue.
- Push and see whether the R CMD Check GH action passes.

## Release

Once the app and Docker image work, follow the steps in `03_deploy.R` to
create a new version, tag, and push the tag.

## Deployment

GitHub Actions are configured to build a new Docker image for every tag
starting with “v”, such as “v1.0.0”. The images are pushed to the GitHub
container registry `ghcr.io`. The DBCA maintainer of `mpaviewer` then
updates the tag number (e.g. `1.0.0`) in Rancher, which will trigger a
hot reload of the image.

The commands to deploy a new version are in `dev/03_deploy.R`.
