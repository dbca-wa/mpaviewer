
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpaviewer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dbca-wa/mpaviewer/workflows/R-CMD-check/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions)
[![docker](https://github.com/dbca-wa/mpaviewer/actions/workflows/docker.yaml/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions/workflows/docker.yaml)
[![pkgdown](https://github.com/dbca-wa/mpaviewer/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/dbca-wa/mpaviewer/actions/workflows/pkgdown.yaml)
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

## Release

Once app and Docker image work, create a new version, tag, and push the
tag.

``` r
styler::style_pkg()
spelling::spell_check_package()
spelling::update_wordlist()

# Code and docs tested, working, committed
usethis::use_version(which="patch")
usethis::use_version(which="minor")
usethis::use_version(which="major")
usethis::edit_file("NEWS.md")

# Document to load new package version. Git commit, tag, and push.
devtools::document()
v <- packageVersion("mpaviewer")
system(glue::glue("git tag -a v{v} -m 'v{v}'"))
system(glue::glue("git push && git push --tags"))
```

## Deployment

GitHub Actions are configured to build a new Docker image for every tag
starting with “v”, such as “v1.0.0”. The images are pushed to the GitHub
container registry `ghcr.io`. The DBCA maintainer of `mpaviewer` then
updates the tag number (e.g. `1.0.0`) in the image run by Kubernetes,
which will trigger a hot reload of the image.

The commands to deploy a new version are in `dev/03_deploy.R`.
