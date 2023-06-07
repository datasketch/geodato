file_path <- "data-raw/files/usa/usa_regions"
geodato_path <- "data-raw/geodato/usa/usa_regions"

codes <- read_csv(file.path(file_path, "usa_regions-codes.csv"))

# region_code,
# region_name,
# id,
# name

regions <- codes %>%
  mutate(region_code = tolower(name),
         region_name = name,
         id = stringr::str_pad(id, width = 2, side = "left", pad = "0"),
         name = toupper(name)) %>%
  select(region_code, region_name, id, name)

readr::write_csv(regions, "data-raw/geodato/usa/usa-adm1-regions.csv")

