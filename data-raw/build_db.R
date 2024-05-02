
library(duckdb)
library(DBI)
library(sf)

con <- DBI::dbConnect(duckdb(config=list('allow_unsigned_extensions'='true'),
                      dbdir = "data-raw/db/geodato.duckdb"))
dbExecute(con, "INSTALL spatial; LOAD spatial;")


# Col maps

col_departments <- read_sf("data-raw/files/col/maps/col_departments.geojson")

dbWriteTable(con, "col_departments",col_departments)

q <- "CREATE TABLE col_departments AS SELECT * FROM ST_Read('data-raw/files/col/maps/col_departments.geojson');"

dbSendQuery(con, q)

dbSendQuery(con, "DROP TABLE col_departments⟩")

dbListTables(con)

col_deparments <- DBI::dbReadTable(con, "col_departments")

duckdb::dbDisconnect(con)
