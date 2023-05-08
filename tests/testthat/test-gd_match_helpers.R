

test_that("Possible names", {

  map_name <- "world_countries"
  countries <- gd_possiblenames(map_name)

  # Make sure it pulls the names of the parent region
  map_name <- "col_municipalities"
  munis <- gd_possiblenames(map_name)
  expect_true("choco" %in% munis)

})


test_that("Guess geo columns", {

  map_name <- "col_departments"
  d <- data.frame(depa = "bogota", valor = 1)
  expect_equal(which_geoname_col(d, "col_departments"), "depa")

  d <- data.frame(ciudad = 11001, valor = 1)
  expect_equal(which_geocode_col(d, "col_municipalities"), "ciudad")

  map_name <- "gtm_municipalities"
  data <- geodato::sample_data$gtm_municipalities |>
    dplyr::rename(vacunados = `Vacunados con primera dosis`)
  var <- "vacunados"
  data$..var <- data[[var]]
  d <- data

  expect_true(is.na(which_geocode_col(d, map_name)))


})


test_that("Matching by name works",{

  map_name <- "col_departments"
  d <- data.frame(depto = c("Bogotá", "Antioquia", "Chocó", "Colombia"))
  match <- gd_match_names(d, map_name = map_name)
  expect_equal(match$..gd_id, c("11","05","27", NA))


  map_name <- "col_municipalities"
  d <- dplyr::tribble(
    ~ciudad, ~dept, ~value,
    "La Unión", "Valle",1,
    "La Unión", "Cauca",2,
    "La Unión", "Narino",3,
    "Alban", "Cundinamarca", 4,
    "Alban", "nariño", 5
  )
  #gd_match_names(d, map_name = map_name)
  col <- which_geoname_col(d, map_name)
  #col <- c("ciudad", "dept")
  match <- gd_match_names2(d, map_name = map_name, col = col)
  expect_equal(match$..gd_id, c(NA, "25019","52399","76400","52019"))


  map_name <- "world_countries"
  d <- data.frame(country = c("Cabo verde", "Congo (Kinshasa)",
                              "Congo (Brazzaville)", "Eswatini","Holy See"))
  match <- gd_match_names(d, map_name = map_name, col = NULL)
  expect_equal(match$..gd_id, c("CPV", "COD", "COG", "SWZ", "VAT"))


  map_name <- "col_municipalities"
  library(dplyr)
  d <- sample_data$col_municipalities$censo_municipios
  d <- d |> filter(municipio == "ALBANIA")
  col <- which_geoname_col(d, map_name)

  gd_match_names2(d, map_name = map_name, col = col)


})


test_that("Matching by code works",{

  map_name <- "col_departments"
  d <- data.frame(cod_dane = c("11", "05", "20","500"))
  match <- gd_match_codes(d, map_name = map_name)
  expect_equal(match$..gd_id, c("11","05","20", NA))

  d <- data.frame(cod_dane = c(11, 5, 20, 500))
  match <- gd_match_codes(d, map_name = map_name)
  expect_equal(match$..gd_id, c("11","05","20", NA))


  map_name <- "world_countries"
  d <- data.frame(value = 1:4, code = c("CPV", "COD", "COG", "SWZ"))
  match1 <- gd_match_codes(d, map_name = map_name, col = 2)
  match2 <- gd_match_codes(d, map_name = map_name, col = "code")
  expect_equal(match1, match2)

})





