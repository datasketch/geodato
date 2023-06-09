
library(fs)
library(tidyverse)
library(readr)

devtools::load_all()


## Meta data

dirs <- gd_dir()
dirs

map_names <- basename(dirs)

path <- gd_dir(map_name = map_names[3])
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
})) |>  purrr::set_names(map_names)

message("maps = ", length(l))

errors <- l |>  keep(~!is.null(.$error)) %>% map(~.$error)
errors
message("maps with errors = ", length(errors))
message( paste0(paste0(" - ", names(errors)), collapse = "\n - "))

maps <- l |>  keep(~!is.null(.$result))  %>% map(~.$result)
pryr::object_size(maps)


# Available maps
# TODO Add title and descriptions in english and other languages

all_maps <- names(maps)
names(all_maps) <- all_maps

main_maps <- tibble(
  map_name = all_maps
)
main_maps$type <- "main"

available_maps_df <- main_maps
usethis::use_data(available_maps_df, internal = FALSE, overwrite = TRUE)

devtools::load_all()


region_maps <- imap(all_maps, function(m, nm){
  reg_codes <- unique(gd_regions(m)$region_code)
  if(is.null(reg_codes)) return(NULL)
  d <- tibble(region = unique(reg_codes))
  d$main_map <- nm
  d$map_name <- paste(nm, reg_codes, sep = "_")
  d$type <- "region"
  d
}) |> bind_rows()
available_maps_df <- bind_rows(main_maps, region_maps) |>
  relocate(main_map, .after = type)



### Sample DATA

dirs <- list.files("data-raw/sample_data", full.names = FALSE)

sample_data <- purrr::map(dirs, function(dir){
  # dir <- dirs[[4]]
  message(dir)
  samples_path <- file.path("data-raw/sample_data",dir)
  if(!dir.exists(samples_path)){
    stop("Sample path does not exists")
  }

  samples <-  fs::dir_ls(samples_path,
                          regexp = "^(?=.*csv)(?!.*dic)", perl = TRUE)
  #tables <- map(samples, read_csv)
  tables_meta <- purrr::map(samples, function(x){
    #x <- samples[[1]]
    message(x)
    table <- read_csv(x)
    dic <- readr::read_csv(gsub("csv$","dic\\.csv", x))
    meta <- yaml::yaml.load_file(gsub("csv$","meta\\.yaml", x))
    meta$data <- table
    meta$dic <- dic
    #meta$more$map_name <- dstools::create_slug(basename(tools::file_path_sans_ext(x)))
    meta
  })

  names(tables_meta) <-  basename(tools::file_path_sans_ext(samples))
  tables_meta
})
names(sample_data) <- dirs
pryr::object_size(sample_data)



### Save


usethis::use_data(maps, internal = TRUE, overwrite = TRUE)
usethis::use_data(available_maps_df, internal = FALSE, overwrite = TRUE)
usethis::use_data(sample_data, internal = FALSE, overwrite = TRUE)

