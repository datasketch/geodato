
library(fs)
library(tidyverse)
library(readr)

devtools::load_all()


## Meta data

dirs <- fs::dir_ls("data-raw/geodato", recurse = 1, type = "directory",
                   regexp = "/.*/.*/")
dirs

map_names <- basename(dirs)

path <- dirs[4]
read_meta_path(path)

map_name <- "col_municipalities"
l <- gd_meta(map_name)


#### OJO gd_centroids SE ESTÁ LEYENDO COMO CHR PARA LON LAT

l <- map(dirs, purrr::safely(function(x){
  ## Read and validate metadata
  message(x)
  l <- read_meta_path(x)
  l
})) %>% purrr::set_names(map_names)

message("maps = ", length(l))

errors <- l %>% keep(~!is.null(.$error)) %>% map(~.$error)
errors
message("maps with errors = ", length(errors))
message( paste0(paste0(" - ", names(errors)), collapse = "\n - "))

maps <- l %>% keep(~!is.null(.$result))  %>% map(~.$result)
pryr::object_size(maps)

### Sample DATA

dirs <- list.files("data-raw/sample_data", full.names = FALSE)

sample_data <- map(dirs, function(dir){
  # dir <- dirs[[1]]
  samples <- list.files(file.path("data-raw/sample_data",dir), full.names = TRUE)
  tables <- map(samples, read_csv)
  names(tables) <-  basename(tools::file_path_sans_ext(samples))
  tables
})
names(sample_data) <- dirs


### Save

usethis::use_data(maps, internal = TRUE, overwrite = TRUE)
usethis::use_data(sample_data, internal = FALSE, overwrite = TRUE)

