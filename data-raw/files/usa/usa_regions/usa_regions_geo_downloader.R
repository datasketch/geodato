# observablehq
# mapshaper.rg (se carga a mano el shape para simplificar)
# Buscar función que limpia el geojson
# Ver capturar
# link de

##  ~ Regions shapefile  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(geojsonio)
library(rgdal)
library(sf)
devtools::load_all()

# Load shape file
temp <- tempfile()
temp2 <- tempfile()

# Download the zip file and save to 'temp'
URL <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_500k.zip"
download.file(URL, temp)

unzip(zipfile = temp, exdir = temp2)
### Check files inside the .zip
#unzip(zipfile = temp, exdir = temp2, list = TRUE)

shapefile_data <- readOGR(dsn = temp2, layer = "cb_2018_us_region_500k")
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
                         file = "data-raw/files/usa/usa_regions/usa_regions.topojson",
                         convert_topojson = TRUE)

## Write topojson
topojson <- sf::read_sf("data-raw/files/usa/usa_regions/usa_regions.topojson")
geodato::write_topojson(tj = topojson , path = "data-raw/geodato/usa/usa_regions/usa_regions.topojson")

file.remove("data-raw/files/usa/usa_regions/usa_regions.topojson")

### Create and Write centroid file
centroids <- geodato::centroids_from_topojson("data-raw/geodato/usa/usa_regions/usa_regions.topojson")
write_csv(centroids, "data-raw/geodato/usa/usa_regions/usa_regions-centroids.csv")



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Testing geographies                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(sf)

ggplot(data = topojson) +
  geom_sf()


topojson_2 <- sf::read_sf("data-raw/files/usa/usa_regions/usa_test_otro.topojson")

ggplot(data = topojson_2) +
  geom_sf()



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    Codes                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
topojson <- sf::read_sf("data-raw/files/usa/usa_regions/usa_regions.topojson")

codes <- st_drop_geometry(topojson)

write_csv(codes, file.path("data-raw/files/usa/usa_regions", "usa_regions-codes.csv"))

ARTofR::xxx_title2("Validation")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                 Validation                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### paths
file_path <- "data-raw/files/usa/usa_regions"
geodato_path <- "data-raw/geodato/usa/usa_regions"

topojson <- read_sf("data-raw/geodato/usa/usa_regions/usa_regions.topojson")
codes <- read_csv(file.path(file_path, "usa_regions-codes.csv"))
centroids <- read_csv(file.path(geodato_path, "usa_regions-centroids.csv"))


# Make sure topojsons and codes have the same names
check_names_in_topojson(tj = topojson, codes = codes)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  Altnames                                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

codes <- st_drop_geometry(topojson)

altnames <- codes %>%
  select(id, altname = name) %>%
  distinct()
write_csv(altnames, "data-raw/geodato/usa/usa_regions/usa_regions-altnames.csv")
