test_that("Maps Col", {

  library(tidyverse)

  map_name <- "col_departments"
  d <- data.frame(cod_dane = c(11, 5, 20,500))
  match1 <- gd_match_codes(d, map_name = map_name)
  d <- data.frame(cod_dane = c("11", "5", "08","20","500"))
  match_a <- gd_match_codes(d, map_name = map_name)
  match_a

  d <- data.frame(depto =  c("Bogotá", "Antioquia", "Cesar", "Otro"))
  match2 <- gd_match_names(d, map_name = map_name)

  expect_equal(match1 %>% select(-1), match2 %>% select(-1))



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


  ###

  map_name <- "col_municipalities"
  d <- sample_data$col_municipalities$plebiscito_2016_municipios
  d <- gd_match(d, map_name)




})
