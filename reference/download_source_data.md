# Download source data files from CKAN to the local data directory

This method can be configured to point to the data file through
environment variables. It will work with defaults unless the location of
the data file changes.

## Usage

``` r
download_source_data(
  ckan_url = Sys.getenv("CKAN_URL", unset = "https://data.dbca.wa.gov.au"),
  data_dir = here::here("inst/data")
)
```

## Arguments

- ckan_url:

  The data catalogue URL, default: `Sys.getenv("CKAN_URL")` or the
  current DBCA CKAN server.

- data_dir:

  A local directory to download files to, default:
  `here::here("inst/data")`.

## Value

None.
