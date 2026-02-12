# Download the application data file mpa_data.rds from CKAN

This method can be configured to point to the data file through
environment variables. It will work with defaults unless the location of
the data file changes.

## Usage

``` r
download_data(
  ckan_url = Sys.getenv("CKAN_URL", unset = "https://data.dbca.wa.gov.au"),
  data_rid = Sys.getenv("MPAVIEWER_RID", unset = "29183116-2409-4213-90c7-69f00888f280"),
  data_dir = here::here("inst/data")
)
```

## Arguments

- ckan_url:

  The data catalogue URL, default: `Sys.getenv("CKAN_URL")` or the
  current DBCA CKAN server.

- data_rid:

  The resource ID of the data file, default:
  `Sys.getenv("MPAVIEWER_RID")` or the current resource ID.

- data_dir:

  A local directory to download files to, default:
  `here::here("inst/data")`.

## Value

None.
