library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

total_population_20 <- get_decennial(
  geography = "state",
  variables = "P001001",
  year = 2020
)


# 2020 Decennial Census data are available from the PL 94-171 Redistricting summary file,
# which is specified with sumfile = "pl" and is also available for 2010.


born_in_mexico <- get_acs(
  geography = "state",
  variables = "B05006_150",
  year = 2020
)
# If the year is not specified, get_acs() defaults to the most recent five-year ACS sample,
# which at the time of this writing is 2016-2020.

# estimate column (for the ACS estimate) and moe column (for the margin of error around that estimate)
# Different years and different surveys are available by adjusting the year and survey parameters.
# survey defaults to the 5-year ACS; however this can be changed to the 1-year ACS by
# using the argument survey = "acs1".

born_in_mexico_1yr <- get_acs(
  geography = "state",
  variables = "B05006_150",
  survey = "acs1",
  year = 2019
)


# In this case, only the states with the largest Mexican-born populations have data available
# for that variable in the 1-year ACS, meaning that the 5-year ACS should be used to make full
# state-wise comparisons if desired.


# To get all variables associated with table B01001, which covers sex broken down by age,
# from the 2016-2020 5-year ACS:

age_table <- get_acs(
  geography = "state",
  table = "B01001",
  year = 2020
)


# geographies:
# https://walker-data.com/census-r/an-introduction-to-tidycensus.html#geography-and-variables-in-tidycensus
# "us"	United States		get_acs(), get_decennial(), get_estimates()
# "region"	Census region		get_acs(), get_decennial(), get_estimates()
# "division"	Census division		get_acs(), get_decennial(), get_estimates()
# "state"	State or equivalent	state	get_acs(), get_decennial(), get_estimates(), get_flows()
# "county"	County or equivalent	state, county	get_acs(), get_decennial(), get_estimates(), get_flows()
# "county subdivision"	County subdivision	state, county	get_acs(), get_decennial(), get_estimates(), get_flows()
# "tract"	Census tract	state, county	get_acs(), get_decennial()
# "block group"	Census block group	state, county	get_acs() (2013-), get_decennial()
# "block"	Census block	state, county	get_decennial()
# "place"	Census-designated place	state	get_acs(), get_decennial(), get_estimates()
# "congressional district"	Congressional district for the year-appropriate Congress	state	get_acs(), get_decennial()
#"state legislative district (upper chamber)"	State senate districts	state	get_acs(), get_decennial()
#"state legislative district (lower chamber)"	State house districts	state	get_acs(), get_decennial()
#"voting district"	Voting districts (2020 only)	state	get_decennial()


# Variables
# There are thousands of variables available across the different datasets and summary files.
# To make searching easier for R users, tidycensus offers the load_variables()

vars <- load_variables(year = "2020", dataset = "pl")
write_csv(vars, "aux_data/vars.csv")
detailed_vars <- load_variables(year = "2022", dataset = "acs5" )
write_csv(detailed_vars, "aux_data/detailed_vars.csv")
# geography column: specifies the smallest geography at which a given variable is available from the Census API.



#400259503001110. The GEOID value breaks down as follows:
#The first two digits, 40, correspond to the Federal Information Processing Series (FIPS) code for the state of Oklahoma. All states and US territories, along with other geographies at which the Census Bureau tabulates data, will have a FIPS code that can uniquely identify that geography.
#Digits 3 through 5, 025, are representative of Cimarron County. These three digits will uniquely identify Cimarron County within Oklahoma. County codes are generally combined with their corresponding state codes to uniquely identify a county within the United States, as three-digit codes will be repeated across states. Cimarron County’s code in this example would be 40025.
#The next six digits, 950300, represent the block’s Census tract. The tract name in the NAME column is Census Tract 9503; the six-digit tract ID is right-padded with zeroes.
#The twelfth digit, 1, represents the parent block group of the Census block. As there are no more than nine block groups in any Census tract, the block group name will not exceed 9.
#The last three digits, 110, represent the individual Census block, though these digits are combined with the parent block group digit to form the block’s name.

# Renaming variables
ga <- get_acs(
  geography = "county",
  state = "Georgia",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  year = 2020,
  output = "wide"
)

# PEP
# Population Estimates Program
queens_components <- get_estimates(
  geography = "county",
  product = "components",
  state = "NY",
  county = "Queens",
  year = 2019
)

louisiana_sex_hisp <- get_estimates(
  geography = "state",
  product = "characteristics",
  breakdown = c("SEX", "HISP"),
  breakdown_labels = TRUE,
  state = "LA",
  year = 2019
)

honolulu_migration <- get_flows(
  geography = "county",
  state = "HI",
  county = "Honolulu",
  year = 2019
)

cbsa_bachelors <- get_acs(
  geography = "cbsa",
  variables = "DP02_0068P",
  year = 2019,
  show_call = TRUE
)


median_age <- get_acs(
  geography = "county",
  variables = "B01002_001",
  year = 2020
)

race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)

az_race <- get_acs(
  geography = "county",
  state = "AZ",
  variables = race_vars,
  summary_var = "B03002_001",
  year = 2020
)

# variables in ACS table B19001 represent groups of households whose household
# incomes fall into a variety of categories: less than $10,000/year, between $10,000/year...

# variable B19001_001, which represents the total number of households for each county.

# Census data longitudinally. One major issue that can emerge is geography changes over time.


# The safest option for time-series analysis in the ACS is to use the
# Comparison Profile Tables. These tables are available for both the 1-year and 5-year ACS,
# and allow for comparison of demographic indicators over the past five years for a given year.

ak_income_compare <- get_acs(
  geography = "county",
  variables = c(
    income15 = "CP03_2015_062",
    income20 = "CP03_2020_062"
  ),
  state = "AK",
  year = 2020
)



# estimates of populations age 25+ who have finished a 4-year degree or graduate degrees,
# by sex), we’ll request those variables directly rather than the entire B15002 table.

college_vars <- c("B15002_015",
                  "B15002_016",
                  "B15002_017",
                  "B15002_018",
                  "B15002_032",
                  "B15002_033",
                  "B15002_034",
                  "B15002_035")
years <- 2010:2019
names(years) <- years

college_by_year <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = college_vars,
    state = "CO",
    summary_var = "B15002_001",
    survey = "acs1",
    year = .x
  )
}, .id = "year")



# The confidence level of the MOE can be controlled with the moe_level argument in get_acs().
# The default moe_level is 90, which is what the Census Bureau returns by default.
# tidycensus can also return MOEs at a confidence level of 95 or 99 which uses Census Bureau-recommended formulas to adjust the MOE.
# A stricter margin of error will increase the size of the MOE relative to its estimate.

get_acs(
  geography = "county",
  state = "Rhode Island",
  variables = "B19013_001",
  year = 2020,
  moe_level = 99
)


vars <- paste0("B01001_0", c(20:25, 44:49))

salt_lake <- get_acs(
  geography = "tract",
  variables = vars,
  state = "Utah",
  county = "Salt Lake",
  year = 2020
)

# Given that the smaller age bands in the Salt Lake City dataset are characterized by too much
# uncertainty for our analysis, we decide in this scenario to aggregate our data upwards
# to represent populations aged 65 and older by sex.

salt_lake_grouped <- salt_lake %>%
  mutate(sex = case_when(
    str_sub(variable, start = -2) < "26" ~ "Male",
    TRUE ~ "Female"
  )) %>%
  group_by(GEOID, sex) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe, estimate))



# The variable in the 2015-2019 ACS for “percent of the population age 25 and up
# with a bachelor’s degree” is DP02_0068P.




# Median household income and median age by county
ga_wide <- get_acs(
  geography = "county",
  state = "Georgia",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  output = "wide",
  year = 2020
)



# percent of commuters that take public transportation to work
# largest metropolitan areas in the United States.
# The data come from the 2019 1-year American Community Survey Data Profile, variable DP03_0021P.

metros <-  get_acs(
  geography = "cbsa",
  variables = "DP03_0021P",
  summary_var = "B01003_001",
  survey = "acs1",
  year = 2019
) %>%
  slice_max(summary_est, n = 20)



# Sixteen counties in Maine, ranging in population from a maximum of 303,069 to
# a minimum of 16,800.
maine_income <- get_acs(
  state = "Maine",
  geography = "county",
  variables = c(hhincome = "B19013_001"),
  year = 2020
) %>%
  mutate(NAME = str_remove(NAME, " County, Maine"))

#we will use the moe column to determine the lengths of the error bars.
ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe))



# Median home value overtime
years <- 2005:2019
names(years) <- years
deschutes_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001",
    state = "OR",
    county = "Deschutes",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")


# Population pyramids

utah <- get_estimates(
  geography = "state",
  state = "UT",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
)


housing_val <- get_acs(
  geography = "tract",
  variables = "B25077_001",
  state = "OR",
  county = c(
    "Multnomah",
    "Clackamas",
    "Washington",
    "Yamhill",
    "Marion",
    "Columbia"
  ),
  year = 2020
)



# TIGER
# the US Census Bureau’s TIGER/Line database, where the acronym TIGER stands for
# Topologically Integrated Geographic Encoding and Referencing.

library(tigris)

st <- states(resolution = '20m')
nm_counties <- counties("NM")
la_tracts <- tracts("NM", "Los Alamos")
la_water <- area_water("NM", "Los Alamos")
dc_landmarks <- landmarks("DC", type = "point")
dc_roads <- primary_secondary_roads("DC")
dc_block_groups <- block_groups("DC")


#   nation()	cartographic (1:5m; 1:20m)	2013-
#   divisions()	cartographic (1:500k; 1:5m; 1:20m)	2013-
#   regions()	cartographic (1:500k; 1:5m; 1:20m)	2013-
#   states()	TIGER/Line; cartographic (1:500k; 1:5m; 1:20m)	1990, 2000, 2010-
#   counties()	TIGER/Line; cartographic (1:500k; 1:5m; 1:20m)	1990, 2000, 2010-
#   tracts()	TIGER/Line; cartographic (1:500k)	1990, 2000, 2010-
#   block_groups()	TIGER/Line; cartographic (1:500k)	1990, 2000, 2010-
#   blocks()	TIGER/Line	2000, 2010-
#   places()	TIGER/Line; cartographic (1:500k)	2011-
#   pumas()	TIGER/Line; cartographic (1:500k)	2012-
#   school_districts()	TIGER/Line; cartographic	2011-
#   zctas()	TIGER/Line; cartographic (1:500k)	2000, 2010, 2012-
#   congressional_districts()	TIGER/Line; cartographic (1:500k; 1:5m; 1:20m)	2011-
#   state_legislative_districts()	TIGER/Line; cartographic (1:500k)	2011-
#   voting_districts()	TIGER/Line	2012, 2020-
#   area_water()	TIGER/Line	2011-
#   linear_water()	TIGER/Line	2011-
#   coastline()	TIGER/Line()	2013-
#   core_based_statistical_areas()	TIGER/Line; cartographic (1:500k; 1:5m; 1:20m)	2011-
#   combined_statistical_areas()	TIGER/Line; cartographic (1:500k; 1:5m; 1:20m)	2011-
#   metro_divisions()	TIGER/Line	2011-
#   new_england()	TIGER/Line; cartographic (1:500k)	2011-
#   county_subdivisions()	TIGER/Line; cartographic (1:500k)	2010-
#   urban_areas()	TIGER/Line; cartographic (1:500k)	2012-
#   primary_roads()	TIGER/Line	2011-
#   primary_secondary_roads()	TIGER/Line	2011-
#   roads()	TIGER/Line	2011-
#   rails()	TIGER/Line	2011-
#   native_areas()	TIGER/Line; cartographic (1:500k)	2011-
#   alaska_native_regional_corporations()	TIGER/Line; cartographic (1:500k)	2011-
#   tribal_block_groups()	TIGER/Line	2011-
#   tribal_census_tracts()	TIGER/Line	2011-
#   tribal_subdivisions_national()	TIGER/Line	2011-
#   landmarks()	TIGER/Line	2011-
#   military()	TIGER/Line	2011-


mi_counties <- counties("MI") # which include water area
# Cartographic boundaries
# generalized in the interior and clipped to the shoreline of the United States
mi_counties_cb <- counties("MI", cb = TRUE)

# Check coord system


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


# Specific parts of the multipolygon Lee County object can be extracted by exploding the multipart geometry into single parts.
lee <- fl_projected %>%
  filter(NAME == "Lee")
mapview(lee)
lee_singlepart <- st_cast(lee, "POLYGON")
lee_singlepart




# geometry = TRUE combines the automated data download functionality of tidycensus and tigris

dc_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "DC",
  year = 2020,
  geometry = TRUE
)

dc_income


us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()
ggplot(data = us_median_age, aes(fill = estimate)) +
  geom_sf()




hennepin_race <- get_decennial(
  geography = "tract",
  state = "MN",
  county = "Hennepin",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))



vote2020 <- read_csv("data/cook-political-report/Popular vote backend - Sheet1.csv")

names(vote2020)

us_states <- states(cb = TRUE, resolution = "20m") %>%
  filter(NAME != "Puerto Rico") %>%
  shift_geometry()

us_states_joined <- us_states %>%
  left_join(vote2020, by = c("NAME" = "state"))

ggplot(us_states_joined, aes(fill = called)) +
  geom_sf(color = "white", lwd = 0.2) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_void() +
  labs(fill = "Party",
       title = " 2020 US presidential election results by state",
       caption = "Note: Nebraska and Maine split electoral college votes by congressional district")

# The US Census Bureau allows for an approximation of zip code mapping with Zip Code Tabulation Areas,
# or ZCTAs. ZCTAs are shapes built from Census blocks in which the most common zip code
# for addresses in each block determines how blocks are allocated to corresponding ZCTAs.
# While ZCTAs are not recommended for spatial analysis due to these irregularities, they can be useful for visualizing data distributions when no other granular geographies are available.


irs_data <- read_csv("https://www.irs.gov/pub/irs-soi/18zpallnoagi.csv")
self_employment <- irs_data %>%
  select(ZIPCODE, self_emp = N09400, total = N1)
boston_zctas <- zctas(
  cb = TRUE,
  starts_with = c("021", "022", "024"),
  year = 2018
)
boston_se_data <- boston_zctas %>%
  left_join(self_employment, by = c("GEOID10" = "ZIPCODE")) %>%
  mutate(pct_self_emp = 100 * (self_emp / total)) %>%
  select(GEOID10, self_emp, pct_self_emp)



# Lower resolution

library(ggiraph)
library(scales)
us_value <- get_acs(
  geography = "state",
  variables = "B25077_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
)
us_value_shifted <- us_value %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, estimate, sep = ": "))
us_value_shifted <- us_value %>%
  shift_geometry(position = "outside") %>%
  mutate(tooltip = paste(NAME, estimate, sep = ": "))

gg <- ggplot(us_value_shifted, aes(fill = estimate)) +
  geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME),
                      size = 0.1) +
  scale_fill_viridis_c(option = "plasma", labels = label_dollar()) +
  labs(title = "Median housing value by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()

girafe(ggobj = gg) %>%
  girafe_options(opts_hover(css = "fill:cyan;"),
                 opts_zoom(max = 10))


# the most popular recent migration destinations in the United States: Travis County Texas, home to Austin.
travis_inflow <- get_flows(
  geography = "county",
  state = "TX",
  county = "Travis",
  geometry = TRUE
) %>%
  filter(variable == "MOVEDIN") %>%
  na.omit() %>%
  arrange(desc(estimate))

#
twin_cities_race <- get_acs(
  geography = "tract",
  variables = c(
    hispanic = "DP05_0071P",
    white = "DP05_0077P",
    black = "DP05_0078P",
    native = "DP05_0079P",
    asian = "DP05_0080P",
    year = 2019
  ),
  state = "MN",
  county = c("Hennepin", "Ramsey", "Anoka", "Washington",
             "Dakota", "Carver", "Scott"),
  geometry = TRUE
)

groups <- c("Hispanic" = "hispanic",
            "White" = "white",
            "Black" = "black",
            "Native American" = "native",
            "Asian" = "asian")


dc_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "DC",
  year = 2020,
  geometry = TRUE
)


