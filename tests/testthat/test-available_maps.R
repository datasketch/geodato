test_that("Available maps", {

  av_maps <- available_maps(type = "main")
  expect_equal(av_maps, names(geodato:::maps))

  regs <- available_maps(type = "regions")

  purrr::walk(av_maps, function(map_name){
    message(map_name)
    #map_name <- av_maps[[3]]
    regs_map_name <- regs[grepl(map_name, regs)]
    regs <- gd_region_codes(map_name)
    if(!is.null(regs)){
      expect_equal(
        regs_map_name,
        paste(map_name,regs, sep = "_")
      )
    }
  })


})
