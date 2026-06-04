testthat::test_that("app ui", {
  ui <- mpaviewer:::app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mpaviewer:::app_ui)
  for (i in c("request")) {
    testthat::expect_true(i %in% names(fmls))
  }
})

testthat::test_that("app server", {
  server <- mpaviewer:::app_server
  testthat::expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(mpaviewer:::app_server)
  for (i in c("input", "output", "session")) {
    testthat::expect_true(i %in% names(fmls))
  }
})

# Configure this test to fit your need
testthat::test_that(
  "app launches",
  {
    golem::expect_running(sleep = 5)
  }
)
