# Smoke tests for the asset modules. They need inst/data/mpa_data.Rdata (gitignored),
# so skip when it is not available.
skip_if_no_data <- function() {
  skip_if(!file.exists(test_path("../../inst/data/mpa_data.Rdata")) && !nzchar(app_sys("data/mpa_data.Rdata")),
          "mpa_data.Rdata not available")
}

# TRUE if the output exists and renders without error (req() "silent" errors are fine)
output_ok <- function(output, id) {
  tryCatch({ output[[id]]; TRUE }, shiny.silent.error = function(e) TRUE)
}

# ids of the outputs a module UI asks for, e.g. "fish.park.total.plot"
ui_output_ids <- function(ui, id) {
  tags <- regmatches(as.character(ui), gregexpr("<[^>]+>", as.character(ui)))[[1]]
  tags <- tags[grepl('class="[^"]*shiny-[a-z-]*output', tags) & grepl(paste0('id="', id, '-'), tags, fixed = TRUE)]
  sub(paste0('.*id="', id, '-([^"]+)".*'), "\\1", tags)
}

# ids a conditionalPanel waits on, e.g. input['fish-fishparkmetric']
ui_condition_ids <- function(ui) {
  html <- as.character(ui)
  unique(sub("input\\[&#39;(.*)&#39;\\].*", "\\1", regmatches(html, gregexpr("input\\[&#39;[^&]+&#39;\\]", html))[[1]]))
}

test_that("conditionalPanel conditions point at inputs that exist in the UI", {
  for (m in list(list("fish", mod_fish_ui), list("coral", mod_coral_ui))) {
    html <- as.character(m[[2]](m[[1]]))
    for (cond in ui_condition_ids(html)) {
      expect_true(grepl(sprintf('id="%s"', cond), html, fixed = TRUE), info = cond)
    }
  }
})

test_that("every output in the fish UI is defined by the server", {
  skip_if_no_data()
  mpa_data <- load_mpa_data()
  ui_ids <- ui_output_ids(mod_fish_ui("fish"), "fish")
  expect_gt(length(ui_ids), 30)

  shiny::testServer(mod_fish_server, args = list(mpa_data = mpa_data), {
    session$setInputs(fish.park.dropdown = "Ningaloo Marine Park")
    session$setInputs(fish.park.method.dropdown = "stereo-DOVs")
    session$setInputs(fish.park.all.species.dropdown = mpa_data$ordered_top_species$scientific_name[1],
                      fish.park.fished.species.dropdown = mpa_data$ordered_top_fished_species$scientific_name[1])
    for (id in ui_ids) expect_true(output_ok(output, id), info = id)
  })
})

test_that("every output in the coral UI is defined by the server", {
  skip_if_no_data()
  mpa_data <- load_mpa_data()
  ui_ids <- ui_output_ids(mod_coral_ui("coral"), "coral")
  expect_gte(length(ui_ids), 10)

  shiny::testServer(mod_coral_server, args = list(mpa_data = mpa_data), {
    session$setInputs(benthic.park.coralcover.dropdown = "Ningaloo Marine Park")
    session$setInputs(benthic.park.site.coralcover.dropdown = mpa_data$coral_cover_metadata$site[1])
    for (id in ui_ids) expect_true(output_ok(output, id), info = id)
  })
})

test_that("plot_img shows a message, not a broken image, when the file is missing", {
  html <- as.character(plot_img("does_not_exist.png", "Title"))
  expect_match(html, "Plot not available")
  expect_false(grepl("<img", html))
})

test_that("park_slug matches the PNG / banner file names", {
  expect_equal(park_slug("Ningaloo Marine Park"), "ningaloo")
  expect_equal(park_slug("Montebello Islands Marine Park"), "montebelloislands")
  expect_equal(park_slug("Rowley Shoals Marine Park"), "rowleyshoals")
})

test_that("fish_png_spec ids are unique and used by the UI", {
  expect_false(anyDuplicated(fish_png_spec$id) > 0)
  expect_true(all(fish_png_spec$id %in% ui_output_ids(mod_fish_ui("fish"), "fish")))
})
