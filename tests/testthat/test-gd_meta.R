
context("Metadata")


test_that("All geofiles have the right metadata",{

  l <- geodato:::maps

  # Required fields: centroids, topojson, tj
  purrr::walk(l, function(m){
    # m <- l[[1]]
    message(m$map_name)
    expect_true(nrow(m$centroids) > 0)
    expect_true("sf" %in% class(m$tj))
    #expect_true(file.exists(m$topojson$path))
    expect_true(all(c("id", "name", "lat", "lon") %in% names(m$centroids)))
    expect_true(all(c("id", "name") %in% names(m$tj)))
    expect_true(all(!is.na(m$centroids$id)))
    expect_true(all(!is.na(m$centroids$name)))
  })

  # Optional fields altnames, altids, regions
  # All regions have correct ids

})



test_that("Functions for extracting meta data work", {

  map_name <- "col_departments"
  tj <- gd_tj(map_name)
  tj

})

