
library(readxl)

# World bank
worldbank_regions0 <- readxl::read_excel("data-raw/helperdata/world_countries/worldbank-CLASS.xlsx",
                        sheet = 2)

worldbank_regions <- worldbank_regions0 |>
  select(region_name = GroupName, region_custom_code = GroupCode,
         id = CountryCode, name = CountryName) |>
  mutate(region_code = mop::create_slug(region_name)) |>
  mutate(region_code = gsub("-", "_", region_code)) |>
  mutate(region_custom_code_source = "worldbank") |>
  select(region_code, region_name, id, name,
         region_custom_code, region_custom_code_source)


library(eurostat)
ea <- eurostat::ea_countries
efta <- eurostat::efta_countries
candidates <- eurostat::eu_candidate_countries
eu <- eurostat::eu_countries
#eurostat::harmonize_country_code()

eu_region <- geodato::gd_match(eu, "world_countries") |>
  select(name, id = ..gd_id) |>
  distinct() |>
  mutate(region_name = "European Union Countries",
         region_code = "eu_countries",
         region_custom_code = "EU28",
         region_custom_code_source = "european commision") |>
  select(region_code, region_name, id, name, region_custom_code)

###
# Continents

# https://worldpopulationreview.com/country-rankings/list-of-countries-by-continent
# Download CSV

world_pop <- read_csv("data-raw/helperdata/world_countries/world-population-review-continents.csv")

x <- world_pop |>
  select(name = "country", code = "cca3", subregion, continent)

world_codes <- geodato::gd_codes("world_countries")
all(x$code %in% world_codes$id)
not_codes <- mop::which_not_in(x$code, world_codes$id)
#[1] "MYT" "REU" "SSD" "ESH" "MDV" "PSE" "GIB" "GLP" "MTQ" "TKL" "TUV" "GUF"
x |> filter(code %in% not_codes)

continents <- x |>
  select(name, code, region_name = continent) |>
  mutate(region_code = mop::create_slug(region_name)) |>
  mutate(region_code = gsub("-", "_", region_code)) |>
  select(region_code, region_name, id = code, name)

america <- continents |>
  filter(region_code %in% c("north_america", "south_america")) |>
  mutate(region_code = "america") |>
  mutate(region_name = "America")

eurasia <- continents |>
  filter(region_code %in% c("europe", "asia")) |>
  mutate(region_code = "eurasia") |>
  mutate(region_name = "Eurasia")

continents <- bind_rows(list(continents, america, eurasia))


subcontinents0 <- x |>
  select(name, code, region_name = subregion) |>
  tidyr::separate(region_name, into = c("r1","r2","r3","r4"), sep = ",") |>
  mutate(r2 = stringr::str_trim(r2)) |>
  mop::discard_all_na_cols()
subcontinents1 <-  subcontinents0 |>
  select(name, code, region_name = r1) |>
  filter(!is.na(region_name)) |>
  mutate(region_code = mop::create_slug(tolower(region_name))) |>
  mutate(region_code = gsub("-", "_", region_code)) |>
  select(region_code, region_name, id = code, name) |>
  arrange(region_code)
subcontinents2 <-  subcontinents0 |>
  select(name, code, region_name = r2) |>
  filter(!is.na(region_name)) |>
  mutate(region_code = mop::create_slug(tolower(region_name))) |>
  mutate(region_code = gsub("-", "_", region_code)) |>
  select(region_code, region_name, id = code, name) |>
  arrange(region_code)
subcontinents <- bind_rows(subcontinents1, subcontinents2)

###

world_countries_regions <- bind_rows(
  worldbank_regions,
  eu_region,
  continents,
  subcontinents
)

write_csv(world_countries_regions,
          "data-raw/geodato/world/world_countries/world_countries-regions.csv")




