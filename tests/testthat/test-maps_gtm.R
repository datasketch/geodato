test_that("GTM", {

  library(tidyverse)

  # Departamentos

  map_name <- "gtm_departments"
  d <- data.frame(depto = "quezaltenango")
  gd_altnames(map_name)
  gd_match_names(d, map_name = "gtm_departments")

  depto_codes <- gd_codes("gtm_departments")
  muni_codes <- gd_codes("gtm_municipalities")
  # Municipios

  d <- read_csv(geodato_sys("sample/gtm/vacunados-muni.csv"))

  map_name <- "gtm_municipalities"
  col <- c("municipio", "departamento")
  match <- gd_match_names2(d, map_name = map_name, col = col)

  no_match <- match %>% filter(is.na(..gd_id)) %>% arrange(departamento)

})
