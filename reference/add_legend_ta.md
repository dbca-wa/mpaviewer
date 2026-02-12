# Add a legend to a Leaflet map

This function wraps
[`leaflet::addLegend`](https://rstudio.github.io/leaflet/reference/addLegend.html)
with some defaults.

## Usage

``` r
add_legend_ta(map, colors, labels, sizes, opacity = 1, group)
```

## Arguments

- map:

  A Leaflet map object

- colors:

  A string of colours, e.g. `c("black", "yellow", "yellow")`.

- labels:

  A list of labels, e.g. `c(0:10)`.

- sizes:

  A list of sizes, e.g. `c(5, 20, 40)`.

- opacity:

  The legend's opacity as decimal number between 0 and 1, default: `1`.

- group:

  A string containing the intended layer name, e.g. `"Total abundance"`.

## Value

The output of
[`leaflet::addLegend`](https://rstudio.github.io/leaflet/reference/addLegend.html).
