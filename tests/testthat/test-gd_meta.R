
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

  # Required fields: centroids, topojson, tj
  purrr::walk(l, function(m){
    # m <- l[[3]]
    message(m$map_name)
    region_exists <- !is.null(m$regions)
    if(region_exists){
      regs <- gd_regions(m$map_name)
      expect_true(all(c("region_code", "region_name", "id") %in% names(regs)))
    }
  })

})



test_that("Functions for extracting meta data work", {

  map_name <- "col_departments"
  tj <- gd_tj(map_name)
  tj

  map_name <- "col_departments_antioquia"
  expect_error(gd_tj(map_name))



  available_maps_df
  map_name <- "col_departments_amazonas"
  codes <- gd_codes(map_name)
  expect_equal(nrow(codes), 6)

  an <- gd_altnames(map_name)
  expect_true(all(an$id %in% codes$id))

})

