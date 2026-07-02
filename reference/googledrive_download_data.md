# Download the application data file mpa_data.rds from CKAN

This method can be configured to point to the data file through
environment variables. It will work with defaults unless the location of
the data file changes.

## Usage

``` r
googledrive_download_data(raw_dir, project_dir)
```

## Arguments

- raw_dir:

  A local directory to download files to.

- project_dir:

  A local directory containing the Github repository.

## Value

None.
