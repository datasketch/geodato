

test_that("Match works",{

  d <- geodato::sample_data$col_departments[[1]]
  gd_match(d, "col_departments")

  d <- dplyr::tribble(
    ~ciudad,~value,
    "Bogotá",1,
    "Pasto",2,
    "Buga",3
  )
  map_name <- "col_departments"
  gd_match(d, map_name)

  map_name <- "col_municipalities"
  gd_match(d, map_name)

  # A maps that is not available
  map_name <- "xxxx"
  expect_error(gd_match(d, map_name))

  # A region map
  map_name <- "world_countries_northern_africa"
  d <- dplyr::tribble(
    ~country,~value,
    "Morocco",1,
    "Liberia",2,
    "Libia",2,
    "Egypt",3,
    "South Africa",4,
    "Algeria",6
  )
  #gd_match_names(d, map_name = map_name, col = "country")
  matches <- gd_match(d, map_name)
  no_match <- gd_no_match(d, map_name)
  expect_true("South Africa" %in% no_match$country)

  tj <- gd_tj(map_name) # make sure tj returns the filtered region
  expect_equal(nrow(tj), 6)



})





