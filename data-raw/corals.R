## code to prepare `corals` dataset goes here
corals <- readr::read_csv(here::here("inst/corals.csv"))

usethis::use_data(corals, overwrite = TRUE)
