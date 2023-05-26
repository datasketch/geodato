

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
country <- "usa"
adm1 <- "states"

file_path <- glue::glue("data-raw/files/{country}/{country}_{adm1}")
geodato_path <- glue::glue("data-raw/geodato/{country}/{country}_{adm1}")


# Load shape file
temp <- tempfile()
temp2 <- tempfile()

# Download the zip file and save to 'temp'
URL <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip"
download.file(URL, temp)

unzip(zipfile = temp, exdir = temp2)
### Check files inside the .zip
files <- unzip(zipfile = temp, exdir = temp2, list = TRUE)

file <- files %>%
  filter(str_detect(Name, ".shp$")) %>%
  pull(Name) %>%
  str_remove(., ".shp")

shapefile_data <- readOGR(dsn = temp2, layer = file)
geojson_data <- geojson_list(shapefile_data)


property_to_remove <- c("REGIONCE", "AFFGEOID", "LSAD", "ALAND", "AWATER")

# Remove other propierties than id and name
for (i in 1:length(geojson_data$features)) {
  geojson_data$features[[i]]$properties[property_to_remove] <- NULL

  names(geojson_data$features[[i]]$properties)[1] <- "id"
  names(geojson_data$features[[i]]$properties)[2] <- "name"
}



# Write the modified GeoJSON file
topojson_data <- geojsonio::geojson_json(geojson_data, object_name = "myTopoJSON")

geojsonio::geojson_write(topojson_data,
                         file = file.path(file_path, glue::glue("usa_{adm1}.topojson")),
                         convert_topojson = TRUE)

## Write topojson
topojson <- sf::read_sf(file.path(file_path, glue::glue("usa_{adm1}.topojson")))
geodato::write_topojson(tj = topojson ,
                        path = file.path(geodato_path, glue::glue("usa_{adm1}.topojson")))

file.remove(file.path(file_path, glue::glue("usa_{adm1}.topojson")))

### Create and Write centroid file
centroids <- geodato::centroids_from_topojson(file.path(geodato_path, glue::glue("usa_{adm1}.topojson")))

write_csv(centroids, file.path(geodato_path, glue::glue("usa_{adm1}-centroids.csv")))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Testing geographies                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(sf)

ggplot(data = topojson) +
  geom_sf()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    Codes                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#topojson <- sf::read_sf(file.path(geodato_path, glue::glue("usa_{adm1}.topojson")))

codes <- st_drop_geometry(topojson) %>%
  select(-NAME) %>%
  janitor::clean_names()

write_csv(codes, file.path(file_path, glue::glue("usa_{adm1}-codes.csv")))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                 Validation                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#topojson <- read_sf("data-raw/geodato/usa/usa_regions/usa_regions.topojson")
#codes <- read_csv(file.path(file_path, "usa_regions-codes.csv"))
#centroids <- read_csv(file.path(geodato_path, glue::glue("usa_{adm1}-codes.csv")))


# Make sure topojsons and codes have the same names
check_names_in_topojson(tj = topojson, codes = codes)
