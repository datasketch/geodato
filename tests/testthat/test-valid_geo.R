test_that("multiplication works", {

  map_name <- "gtm_departments"

  v <- c(16,15,4,9)
  valid_geoids(v, map_name)


  map_name <- "col_departments"
  v <- c(05, 11)
  valid_geoids(v, map_name)

  v <- c("05", "11")
  valid_geoids(v, map_name)


})
