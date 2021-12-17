## code to prepare `fish` dataset goes here
fish <- readr::read_csv(here::here("inst/fish.csv"))
usethis::use_data(fish, overwrite = TRUE)
