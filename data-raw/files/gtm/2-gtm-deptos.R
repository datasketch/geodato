
library(tidyverse)
devtools::load_all()

map_name <- "gtm_departments"

file_path <- "data-raw/files/gtm"
geodato_path <-  "data-raw/geodato/gtm"

### DEPARMENTS

# Make sure topojsons and codes have the same names

codes <- read_csv("data-raw/files/gtm/gtm_departments-codes.csv")
topojson_path <- "data-raw/files/gtm/auxi/gtm-departments-0.topojson"
tj0 <- read_sf(topojson_path)

check_names_in_topojson(tj0, codes)

new_names <- tibble(name = "Quezaltenango",
                    new_name = "Quetzaltenango")

tj1 <- change_topojson_names(topojson_path, new_names)
check_names_in_topojson(tj1, codes)


# altids <-  codes_deptos |>
#   bind_rows(st_drop_geometry(tj1))
#
# write_csv(altids, file.path(file_path, "gtm_departments-altids.csv"))
# write_csv(altids, file.path(geodato_path, map_name,"gtm_departments-altids.csv"))

# Make sure topojsons and codes have the same ids

check_ids_in_topojson(tj0, codes)
check_ids_in_topojson(tj1, codes)


tj2 <- change_topojson_ids(tj1, codes)
check_ids_in_topojson(tj2, codes)

# Write correct topojson

pryr::object_size(tj2)
write_topojson(tj2, "data-raw/files/gtm/gtm_departments.topojson")

write_topojson(tj2,file.path(geodato_path, map_name,
                             paste0(map_name,".topojson")))

# Write centroids

centroids <- centroids_from_topojson(tj2)
write_csv(centroids, file.path(geodato_path, map_name,
                               paste0(map_name,"-centroids.csv")))

