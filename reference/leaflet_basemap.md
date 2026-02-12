# Create a Leaflet basemap for Western Australia

Create a Leaflet basemap for Western Australia

## Usage

``` r
leaflet_basemap(data = NULL, l_width = NULL, l_height = NULL)
```

## Arguments

- data:

  The map data, default: NULL

- l_width:

  The leaflet map width, default: NULL

- l_height:

  The leaflet map height, default: NULL

## Value

A Leaflet map focused on WA

## Examples

``` r
leaflet_basemap()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldImagery",null,"Basemap",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addProviderTiles","args":["OpenStreetMap.Mapnik",null,"Basemap",{"errorTileUrl":"","noWrap":false,"opacity":0.35,"detectRetina":false}]},{"method":"addScaleBar","args":[{"maxWidth":200,"metric":true,"imperial":false,"updateWhenIdle":true,"position":"bottomleft"}]},{"method":"addMiniMap","args":[null,null,"bottomright",150,150,19,19,-5,false,false,false,true,false,true,{"color":"#ff7800","weight":1,"clickable":false},{"color":"#000000","weight":1,"clickable":false,"opacity":0,"fillOpacity":0},{"hideText":"Hide MiniMap","showText":"Show MiniMap"},[]]}],"setView":[[-20,130],5,[]]},"evals":[],"jsHooks":[]}
```
