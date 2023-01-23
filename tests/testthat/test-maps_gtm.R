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
  d <- sample_data$gtm_muncipalities$`vacunados-muni`

  map_name <- "gtm_municipalities"
  col <- c("municipio", "departamento")
  match_names2 <- gd_match_names2(d, map_name = map_name, col = col)

  match <- gd_match(d, map_name)
  no_match <- gd_no_match(d, map_name)
  codes <- gd_codes(map_name)


})
