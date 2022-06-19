test_that("Matching works",{

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

})


test_that("Matching geocodes and geonames work",{

  map_name <- "col_departments"

  d <- data.frame(cod_dane = c("11", "05", "20","500"))
  match <- gd_match_codes(d, map_name = map_name)
  expect_equal(match$..gd_id, c("11","05","20", NA))

  gd_match(d, map_name)

  d <- data.frame(depto = c("Bogotá", "Antioquia", "Chocó"))
  gd_match_names(d, map_name = map_name)

  gd_match(d, map_name)

  ###

  map_name <- "col_municipalities"

  d <- dplyr::tribble(
    ~ciudad,~value,
    "Bogotá",1,
    "Pasto",2,
    "Buga",3
  )
  col <- "ciudad"
  gd_match_names(d, map_name = map_name, col = col)

  d <- dplyr::tribble(
    ~ciudad, ~dept, ~value,
    "La Unión", "Valle",1,
    "La Unión", "Cauca",2,
    "La Unión", "Narino",3,
    "Alban", "Cundinamarca", 4,
    "Alban", "nariño", 5
  )
  #gd_match_names(d, map_name = map_name)
  col <- c("ciudad", "dept")
  gd_match_names2(d, map_name = map_name, col = col)

  gd_match(d, map_name = map_name, col = col)

  #x <- gd_codes("col_municipalities")

  ###

  map_name <- "world_countries"

  d <- data.frame(value = 1:4, code = c("CPV", "COD", "COG", "SWZ"))
  match1 <- gd_match_codes(d, map_name = map_name, col = 2)
  match2 <- gd_match_codes(d, map_name = map_name, col = "code")

  expect_equal(match1, match2)

  d <- data.frame(country = c("Cabo verde", "Congo (Kinshasa)",
                              "Congo (Brazzaville)", "Eswatini","Holy See"))
  gd_match_names(d, map_name = map_name, col = NULL)





})




test_that("Name and id match works", {

  d <- geodato::sample_data$col_departments[[1]]

  geoname_col <- which_geoname_col(d, map_name = "col_departments")
  geocode_col <- which_geocode_col(d, map_name = "col_departments")

  d <- dplyr::tribble(
    ~ciudad,~value,
    "Bogotá",1,
    "Pasto",2,
    "Buga",3
  )
  which_geoname_col(d, map_name = "col_municipalities")
  which_geocode_col(d, map_name = "col_municipalities")




})
