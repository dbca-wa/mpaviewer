# Generate one data object to use in server.R

Generate one data object to use in server.R

## Usage

``` r
generate_data(
  raw_dir,
  save = TRUE,
  dest = here::here("inst/data/mpa_data.rds")
)
```

## Arguments

- save:

  Boolean whether to save data to rds

- dest:

  The target file to save rds as, default:
  here::here("inst/data/mpa_data.rds")

## Value

An object of class "mpa_data" containing all data tibbles and objects
used by server.R

## Examples

``` r
if (FALSE) { # \dontrun{
x <- generate_data(save = FALSE) # only returns data
x <- generate_data() # returns data and saves data to local file
} # }
```
