

test_that("Match works",{

  d <- geodato::sample_data$col_departments[[1]]$data
  map_name <- "col_departments"
  gd_match(d, map_name)

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


  ## A map with codes

  map_name <- "gtm_municipalities"
  d <- dplyr::tribble(
    ~cod_municipio,~value,
    101,1,
    201,2,
    301,2,
    401,3
  )

  col <- "cod_municipio"
  gd_match_codes(d, map_name = map_name, col = col)

  gd_match(d, map_name = map_name)



})





