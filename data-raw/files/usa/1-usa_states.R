

##  ~ States shapefile  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(geojsonio)
library(rgdal)
library(sf)
devtools::load_all()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    Paths                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

file_path <- "data-raw/files/usa"
geodato_path <-  "data-raw/geodato/usa"

map_name <- "usa_states"

# Load shape file
temp <- "data-raw/files/usa/tmp/states.zip"
URL <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip"
download.file(URL, temp)
states_path <- "data-raw/files/usa/tmp/states"
unzip(zipfile = temp, exdir = states_path)


# US REGIONS
# https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
# temp_regions <- "data-raw/files/usa/tmp/states-regions.zip"
# URL <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_500k.zip"
# download.file(URL, temp_regions)
# states_regions_path <- "data-raw/files/usa/tmp/states_regions"
# unzip(zipfile = temp_regions, exdir = states_regions_path)

#https://www.50states.com/city/regions.htm



####

library(sf)

x0 <- sf::st_read(states_path)

#property_to_remove <- c("REGIONCE", "AFFGEOID", "LSAD", "ALAND", "AWATER")

tj <- x0 |>
  select(id = STUSPS, name = NAME, geometry)

write_topojson(tj, file.path(file_path, "auxi/states.topojson"))
write_topojson(tj,
               file.path(geodato_path,
                         glue::glue("{map_name}/{map_name}.topojson")))

pryr::object_size(tj)
#tj2 <- gd_simplify(x)
#pryr::object_size(x2)


## CODES

codes <- st_drop_geometry(tj)
write_csv(codes, file.path(file_path, glue::glue("usa_states-codes.csv")))
write_csv(codes,
          file.path(geodato_path, glue::glue("{map_name}/{map_name}-codes.csv")))

## CENTROIDS
centroids <- centroids_from_topojson(tj)
write_csv(centroids,
          file.path(geodato_path,
                    glue::glue("{map_name}/{map_name}-centroids.csv")))

## ALTNAME

altnames0 <- read_csv("data-raw/files/usa/usa_states-altnames.csv")
alts <- read_csv("data-raw/files/usa/us_states_abreviations.csv")
altnames <- alts |>
  select(id = code, altname = abbreviation)
altnames <- bind_rows(altnames, altnames0)
write_csv(altnames,
          file.path(geodato_path,
                    glue::glue("{map_name}/{map_name}-altnames.csv")))

# ## REGIONS
## region_code, region_name, id, name
# regions <- sf::st_read(states_regions_path)
regions <- read_csv("data-raw/files/usa/usa_states_regions.csv")
names(regions)
main_regions <- regions
main_regions <- main_regions |>
  mutate(region_code = dstools::create_slug(region, sep = "_")) |>
  select(region_code, region_name = region, id, name = state) |>
  mutate(type = "region") |>
  distinct()
division_regions <- regions |>
  mutate(region_code = dstools::create_slug(division, sep = "_")) |>
  select(region_code, region_name = division, id, name = state, region = region) |>
  mutate(type = "division")
all_regions <- bind_rows(main_regions, division_regions)
write_csv(all_regions,
          file.path(geodato_path,
                    glue::glue("{map_name}/{map_name}-regions.csv")))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Testing geographies                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(sf)

ggplot(data = tj) +
  geom_sf()

## Validation

tj2 <- read_sf("data-raw/geodato/usa/usa_states/usa_states.topojson")
codes <- read_csv(file.path(file_path, "usa_states-codes.csv"))
check_names_in_topojson(tj = tj2, codes = codes)









# ### Check files inside the .zip
# files <- unzip(zipfile = temp, exdir = temp2, list = TRUE)
#
# file <- files %>%
#   filter(str_detect(Name, ".shp$")) %>%
#   pull(Name) %>%
#   str_remove(., ".shp")
#
# shapefile_data <- readOGR(dsn = temp2, layer = file)
# geojson_data <- geojson_list(shapefile_data)
#
#
#
# # Remove other propierties than id and name
# for (i in 1:length(geojson_data$features)) {
#   geojson_data$features[[i]]$properties[property_to_remove] <- NULL
#
#   names(geojson_data$features[[i]]$properties)[1] <- "id"
#   names(geojson_data$features[[i]]$properties)[2] <- "name"
# }

# # Write the modified GeoJSON file
# topojson_data <- geojsonio::geojson_json(geojson_data, object_name = "myTopoJSON")
#
# geojsonio::geojson_write(topojson_data,
#                          file = file.path(file_path, glue::glue("usa_{adm1}.topojson")),
#                          convert_topojson = TRUE)
#
# ## Write topojson
# topojson <- sf::read_sf(file.path(file_path, glue::glue("usa_{adm1}.topojson")))
# geodato::write_topojson(tj = topojson ,
#                         path = file.path(geodato_path, glue::glue("usa_{adm1}.topojson")))
#
# file.remove(file.path(file_path, glue::glue("usa_{adm1}.topojson")))
#
# ### Create and Write centroid file
# centroids <- geodato::centroids_from_topojson(file.path(geodato_path, glue::glue("usa_{adm1}.topojson")))
#
# write_csv(centroids, file.path(geodato_path, glue::glue("usa_{adm1}-centroids.csv")))

