
library(duckdb)
library(DBI)
library(sf)
library(tidyverse)
library(dbplyr)

con <- DBI::dbConnect(duckdb(config=list('allow_unsigned_extensions'='true'),
                      dbdir = "data-raw/db/geodato.duckdb"))
dbExecute(con, "INSTALL spatial; LOAD spatial;")

dbListTables(con)

# Get all column names from the table
columns <- dbGetQuery(con, "PRAGMA table_info(col_departments)")

col_departments <- dplyr::tbl(con, "col_departments") |> collect()
str(col_departments)

q <- "SELECT * FROM col_departments"
q <- "SELECT *, ST_AsText(geom) as geometry FROM col_departments WHERE ST_IsValid(geom)"
table <- dbGetQuery(con, q)


duckdb::dbDisconnect(con)
