test_that("Which geo works", {

  # Main maps

  d <- dplyr::tribble(
    ~country,~value,
    "Morocco",1,
    "Lybia",2,
    "Egypt",3
  )
  expect_equal(which_geocode_col(d, "world_countries"), NA)
  expect_equal(which_geoname_col(d, "world_countries"), "country")

  # Region maps
  map_name <- "world_countries_northern_africa"
  expect_equal(which_geocode_col(d, map_name), NA)
  expect_equal(which_geoname_col(d, map_name), "country")





})
