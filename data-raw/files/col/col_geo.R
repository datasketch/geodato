
library(sf)
library(dplyr)
library(tidyverse)
library(ggplot2)

options(timeout = 1200)
# Colombia: EPSG::9377

# DANE
# MGN Marco Geoestadístico Nacional
url <- "https://www.dane.gov.co/files/geoportal-provisional/SHP_MGN2021_COLOMBIA.zip"
download.file(url, "data-raw/files/col/SHP_MGN2021_COLOMBIA.zip")
unzip("data-raw/files/col/SHP_MGN2021_COLOMBIA.zip",
      exdir = "data-raw/files/col/SHP_MGN2021_COLOMBIA")

depto <- sf::read_sf("data-raw/files/col/SHP_MGN2021_COLOMBIA/MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_DPTO_POLITICO.shp")
depto <- st_transform(depto, crs = 4326)

departamentos <- depto |>
  select(
    cod_departamento = DPTO_CCDGO,
    departamento = DPTO_CNMBR,
    geometry
  )
object.size(departamentos)
st_crs(departamentos)

muni <- sf::read_sf("data-raw/files/col/SHP_MGN2021_COLOMBIA/MGN_2021_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
muni <- st_transform(muni, crs = 4326)

municipio <- muni |>
  select(
    cod_departamento = DPTO_CCDGO,
    departamento = DPTO_CNMBR,
    cod_municipio = MPIO_CCDGO,
    municipio = MPIO_CNMBR,
    geometry
  ) |>
  mutate(cod_municipio = paste0(cod_departamento, cod_municipio))

sanandres_prov <- municipio |>
  filter(grepl("ARCHI", departamento))

departamentos <- departamentos |>
  st_simplify(preserveTopology = TRUE, dTolerance = 100) |>
  mutate(geometry = st_make_valid(geometry)) |>
  filter(cod_departamento != "88")
object.size(departamentos)

ggplot(data = departamentos) +
  geom_sf(fill = "lightblue", color = "black") +
  theme_minimal()

place_geometry_wilke <- function(geometry, position, scale = 1,
                                 centroid = sf::st_centroid(geometry)) {
  (geometry - centroid) * scale + sf::st_sfc(st_point(position))
}


sanandres <- municipio |>
  filter(cod_municipio == "88001")
centroid <- sanandres |> sf::st_geometry() |> sf::st_centroid()
sanandres2 <- sanandres
sanandres2$geometry <- place_geometry_wilke(sf::st_geometry(sanandres),
                     c(-80, 13), scale = 20,
                     centroid = centroid)
sanandres2 <- st_set_crs(sanandres2, 4326)
sanandres2 <- sanandres2 |> select(-cod_municipio, -municipio)
departamentos2 <- rbind(departamentos, sanandres2)


prov <- municipio |>
  filter(cod_municipio == "88564")
centroid <- sanandres |> sf::st_geometry() |> sf::st_centroid()
prov2 <- prov
prov2$geometry <- place_geometry_wilke(sf::st_geometry(prov),
                                            c(-82.5, 5), scale = 10,
                                            centroid = centroid)
prov2 <- st_set_crs(prov2, 4326)
prov2 <- prov2 |> select(-cod_municipio, -municipio)
departamentos3 <- rbind(departamentos2, prov2)

departamentos4 <- departamentos3 |>
  group_by(cod_departamento, departamento) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

ggplot(data = departamentos4) +
  geom_sf(fill = "lightblue", color = "black") +
  theme_minimal()

sf::write_sf(departamentos4, "data-raw/files/col/maps/col_departments.geojson")
sf::write_sf(departamentos, "data-raw/files/col/maps/col_departments2.geojson")









# Veredas
url_veredas <- "https://www.dane.gov.co/files/geoportal-provisional/SHP_CRVEREDAS_2020.zip"
download.file(url_veredas, "data-raw/files/col/shp_veredas_2020.zip")
unzip("data-raw/files/col/shp_veredas_2020.zip",
      exdir = "data-raw/files/col/shp_veredas_2020")

col <- sf::read_sf("data-raw/files/col/shp_veredas_2020/CRVeredas_2020.shp")



col_veredas <- col |>
  mutate(geometry = st_make_valid(geometry)) |> # Fix weird interseccions
  ## removing small irregularities, Might help to simplify
  # mutate(geometry = st_simplify(geometry, preserveTopology = TRUE)) |>
  ## A common trick in GIS to handle minor errors in polygon geometries is to apply a zero-width buffer:
  # mutate(geometry = st_buffer(geometry, dist = 0)) |>
  rename(
    cod_municipio = DPTOMPIO,
    cod_vereda = CODIGO_VER,
    cod_departamento = COD_DPTO,
    departamento = NOM_DEP,
    municipio = NOMB_MPIO,
    vereda = NOMBRE_VER
  )


col_veredas1 <- col_veredas |>
  mutate(geometry = st_simplify(geometry, preserveTopology = TRUE))

st_crs(col_veredas)


col_departamentos0 <- col_veredas1 |>
  group_by(cod_departamento, departamento) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

col_departamentos1 <- nngeo::st_remove_holes(col_departamentos0)

col_departamentos2 <- st_simplify(col_departamentos1, preserveTopology = TRUE,
                                  dTolerance = 200) |>
  mutate(geometry = st_make_valid(geometry))
col_departamentos2 <- nngeo::st_remove_holes(col_departamentos2)

object.size(col_departamentos2)

ggplot(data = col_departamentos2) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_minimal()


ggplot(data = bog) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_minimal()


library(nngeo)
library(mapview)



mapview()

# Remove holes
col_departamentos2 <- col_departamentos1 |>
  mutate(geometry = st_buffer(geometry, dist = 0.01) %>%  # Slightly expand the geometries
           st_buffer(dist = -0.01))  # Immediately contract them





# Object size
# Average number of points per feature
average_points <- col_departamentos %>%
  st_geometry() %>%
  lapply(st_coordinates) %>%
  sapply(nrow) %>%  # Get number of rows in coordinates matrix, which corresponds to number of points
  mean()
average_points


st_crs(col_departamentos)


ggplot(data = col_departamentos1) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_minimal()

col_departamentos_projected <- st_transform(col_departamentos, crs = 9377)
st_crs(col_departamentos_projected)

ggplot(fl_counties) +
  geom_sf() +
  coord_sf(crs = 9377)





















# All Census Bureau datasets are stored in the “NAD83”
fl_counties <- counties("FL", cb = TRUE)
st_crs(fl_counties)
## Coordinate Reference System:
##   User input: NAD83
##   wkt:
## GEOGCRS["NAD83",
##     DATUM["North American Datum 1983",
##         ELLIPSOID["GRS 1980",6378137,298.257222101,
##             LENGTHUNIT["metre",1]]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433]],
##     CS[ellipsoidal,2],
##         AXIS["latitude",north,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433]],
##         AXIS["longitude",east,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433]],
##     ID["EPSG",4269]]
# All Census Bureau datasets are stored in the “NAD83” geographic coordinate system,
# which refers to the North American Datum of 1983. Other relevant information includes
# the ellipsoid used (GRS 1980, which is a generalized three-dimensional model of the Earth’s shape),
# the prime meridian of the CRS (Greenwich is used here),
# and the EPSG (European Petroleum Survey Group) ID of 4269, which is a special code that can be used to represent the CRS in more concise terms.
# crsuggest package can help make suggestions of coordinates

library(crsuggest)
fl_crs <- suggest_crs(fl_counties)
fl_projected <- st_transform(fl_counties, crs = 3087)
st_crs(fl_projected)

ggplot(fl_counties) +
  geom_sf() +
  coord_sf(crs = 3087)

#the underlying graticule (the grid lines and axis tick labels) default to longitude/latitude.
#To show the coordinates of the projected coordinate reference system, the argument datum can be used which controls the gridlines.
ggplot(fl_counties) +
  geom_sf() +
  coord_sf(crs = 3087, datum = 3087)



# continental US Albers Equal Area projection:
ggplot(us_states) +
  geom_sf() +
  coord_sf(crs = 'ESRI:102003') +
  theme_void()


# tigris offers a solution to this problem with the shift_geometry()
us_states_shifted <- shift_geometry(us_states)

ggplot(us_states_shifted) +
  geom_sf() +
  theme_void()

us_states_outside <- shift_geometry(us_states,
                                    preserve_area = TRUE,
                                    position = "outside")

ggplot(us_states_outside) +
  geom_sf() +
  theme_void()


# Places
tx_places <- places("TX", cb = TRUE) %>%
  filter(NAME %in% c("Dallas", "Fort Worth", "Houston",
                     "Austin", "San Antonio", "El Paso")) %>%
  st_transform(6580)

tx_outline <- states(cb = TRUE) %>%
  filter(NAME == "Texas") %>%
  st_transform(6580)

ggplot() +
  geom_sf(data = tx_outline) +
  geom_sf(data = tx_places, fill = "red", color = NA) +
  theme_void()

tx_centroids <- st_centroid(tx_places)

ggplot() +
  geom_sf(data = tx_outline) +
  geom_sf(data = tx_centroids, color = "red", size = 3) +
  theme_void()


