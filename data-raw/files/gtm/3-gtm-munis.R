
library(tidyverse)
devtools::load_all()

map_name <- "gtm_municipalities"


file_path <- "data-raw/files/gtm"
geodato_path <-  "data-raw/geodato/gtm"

# Make sure topojsons and codes have the same names

codes <- read_csv(file.path(file_path, "gtm_municipalities-codes.csv"))

topojson_path <- file.path(file_path, "auxi/gtm_municipios-0.topojson")
tj0 <- read_sf(topojson_path)

check_names_in_topojson2(tj0, codes)

write_topojson(tj0, file.path(file_path, "gtm_municipalities.topojson"))

write_topojson(tj0,  file.path(geodato_path,map_name,
                              "gtm_municipalities.topojson"))

# Write centroids

centroids <- centroids_from_topojson(tj0)
write_csv(centroids,
          file.path(geodato_path, map_name, paste0(map_name, "-centroids.csv")))



