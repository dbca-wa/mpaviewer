test_that("leaflet_basemap works", {
  themap <- leaflet_basemap()
  testthat::expect_equal(class(themap), c("leaflet", "htmlwidget"))
})
