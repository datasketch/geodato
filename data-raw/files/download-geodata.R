
# Check if resources exist
basename <- "col-adm1-departments"
required <- c(".csv",".topojson")
optional <- c("-regions.csv","-altnames.csv")

geodato <- geodatoMeta() |>  purrr::map(`[`,c("basename","geoname")) %>%
  keep(~!is.null(.$basename))

basepath <- "https://rawgit.com/jpmarindiaz/geo-collection/master"
reqExists <- geodato |>  purrr::map(function(x){
  files <- paste0(file.path(basepath,x$geoname, x$basename),required)
  map_lgl(unlist(files), url_exists, printUrl = TRUE)
})

stopifnot(all(unlist(reqExists)))


# Download files
geonames <- geodato |>  map_chr("geoname") %>% unique()
file.path("inst","geodato",geonames) |>  purrr::map(dir.create)
geodato |>  purrr::map(function(x){
  reqfiles <- paste0(file.path(basepath,x$geoname, x$basename),required)
  optfiles <- paste0(file.path(basepath,x$geoname, x$basename),optional)
  optfiles <- optfiles[map_lgl(unlist(optfiles), url_exists)]
  files <- c(reqfiles,optfiles)
  purrr::map(files, function(y){
    message("Reading: ",y)
    dest <- file.path("inst","geodato",basename(dirname(y)),basename(y))
    download.file(y, destfile = dest,method="curl")
  })
})



