

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



})





