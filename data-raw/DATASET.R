
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
path
read_meta_path(path)


map_name <- "col_municipalities"
l <- gd_meta(map_name)
l$regions

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


# Available maps
# TODO Add title and descriptions in english and other languages

all_maps <- names(maps)
names(all_maps) <- all_maps

main_maps <- tibble(
  map_name = all_maps
)
main_maps$type <- "main"
region_maps <- imap(all_maps, function(m, nm){
  reg_codes <- gd_regions(m)
  if(is.null(reg_codes)) return(NULL)
  d <- tibble(region = reg_codes)
  d$main_map <- nm
  d$map_name <- paste(nm, reg_codes, sep = "_")
  d$type <- "region"
  d
}) |> bind_rows()
available_maps_df <- bind_rows(main_maps, region_maps) |>
  relocate(main_map, .after = type)



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
pryr::object_size(sample_data)

#
map_name <- "col_municipalities"





### Save

usethis::use_data(maps, internal = TRUE, overwrite = TRUE)
usethis::use_data(sample_data, internal = FALSE, overwrite = TRUE)
usethis::use_data(available_maps_df, internal = FALSE, overwrite = TRUE)


