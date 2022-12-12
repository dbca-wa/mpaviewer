# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# mpaviewer::generate_data()

options("golem.app.prod" = TRUE)
mpaviewer::run_app() # add parameters here (if any)


# Testing
# test <- mpa_data$metadata
# unique(test$year)
# test2 <- mpa_data$all.data
