

# GCAM AR / Reforestation (AR) land and LULUCF emissions analysis ----


# This script (repo) processes detailed GCAM land use and emissions outputs
# to partition afforestation / reforestation (AR) land.
#
# Goal:
#   - Quantify land transitions to AR at the water basin level
#   - Estimate associated vegetation and soil carbon storage
#
# Approach:
#   - Combine detailed land allocation and LULUCF emissions outputs
#   - Apply carbon density assumptions (from gcamdata / CarbonInfo)
#   - Derive basin-level gross carbon storage trajectories
#
# Notes:
#   - Assumptions follow original GCAM (v7.1 here).
#   - Sensitivity tests can be done later by adjusting emission factors
#     (vegetation vs. soil carbon density, soil min values, etc.).



library(gcamdata)
library(dplyr)
library(tidyr)




# Load carbon data ----

# GCAM_land_carbon_info.rds derived in gcamdata (GCAM v7.1)
# Base year depends on GCAM version (2015 for v7.1).
# See CarbonInfo/get_carbon_info.R for detailed sources.
readRDS(file.path("data", "carboninfo", "GCAM_land_carbon_info.rds")) ->
  GCAM_land_carbon_info

# This dataset provides:
#   - Total carbon densities (veg + soil)
#   - Mature ages for vegetation
#   - Soil turnover times
#   - Minimum soil carbon density (based on cropland values)
# We will test using this "min soil" value for AR soil carbon assumptions.

# Load scenario (land & LULUCF) data ----
# scenario is also the data folder name
ascenario <- "cwf_highafforest"

# load land and emission data
readr::read_csv(file.path("data", "scenario", ascenario, "detailed land allocation.csv"),
                comment = "#") %>%
  gather_years() %>% filter(year >= 1975) ->
  DetailedLand

readr::read_csv(file.path("data", "scenario", ascenario, "LUC emissions by LUT.csv"),
                comment = "#")%>%
  gather_years() %>% filter(year >= 1975) ->
  LUC_LUT

# Check that land and emissions cover the same region-landleaf combinations
assertthat::assert_that(
  DetailedLand %>% distinct(region, LandLeaf) %>%
    dplyr::setdiff(LUC_LUT %>% distinct(region, LandLeaf)) %>%
    nrow() == 0, msg = "Emissions and land not matching in region and land leaf"
)

## Join data and update units ----
# Mha and MtCO2
DetailedLand %>%
  transmute(scenario = ascenario, region, LandLeaf, year, Mha = value / 10) %>%
  left_join_error_no_match(
    LUC_LUT %>%
      transmute(scenario = ascenario, region, LandLeaf, year, MtCO2 = value * 44 / 12),
    by = c("scenario", "region", "LandLeaf", "year")
  ) ->
  Land_Ems_basin


## split land leaf ----
# this version include biochar

# Non-forest leaves (including biochar)
Land_Ems_basin %>%
  filter(!grepl("Forest", LandLeaf)) %>%
  tidyr::separate(
    col = LandLeaf,
    into = c("land", "basin", "irr", "mgmt", "biochar"),
    sep = "_") ->
  Land_Ems_basin_nonForest

Land_Ems_basin_nonForest %>% distinct(land) %>% pull

# Forest leaves
Land_Ems_basin %>%
  filter(grepl("Forest", LandLeaf)) %>%
  tidyr::separate(
    col = LandLeaf,
    into = c("land", "forest", "basin"),
    sep = "_") %>%
  select(-forest)->
  Land_Ems_basin_Forest

Land_Ems_basin_Forest %>% distinct(land)

# Final objects:
#   - Land_Ems_basin_Forest: forest land and emissions by basin
#   - GCAM_land_carbon_info: vegetation & soil carbon density assumptions
# These will be combined to calculate AR area and associated carbon storage.
