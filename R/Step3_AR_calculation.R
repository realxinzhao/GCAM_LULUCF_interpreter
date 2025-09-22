
# Step 3: AR calculation ----

## 3.1 derived forest carbon density for LUC application ----
GCAM_land_carbon_info %>%
  filter(grepl("Forest", LandLeaf)) %>%
  tidyr::separate(
    col = LandLeaf,
    into = c("land", "forest", "basin"),
    sep = "_") %>%
  select(-forest) ->
  GCAM_land_carbon_info_Forest

### approach 1: original ----
GCAM_land_carbon_info_Forest %>%
  rowwise() %>%
  mutate(intensity_dist =
           list(LUC_flux_MtCO2_intensity_dist(
             E_soil_t = soil.carbon.density,
             s        = SoilTime,
             E_veg_t  = veg.carbon.density,
             M        = mature.age
           ))) %>%
  unnest(intensity_dist) ->
  CarbonDensity_Dist

# Adding carbon distributed back up for confirmation
CarbonDensity_Dist %>%
  gather_years() %>%
  filter(y_conv == 2025) %>%
  group_by_at(vars(-year, -value)) %>%
  summarize(value = sum(value))

CarbonDensity_Dist %>%
  gather_years() %>%
  group_by_at(vars(region, basin, land, y_conv, EmYear = year, source)) %>%
  summarize(value = sum(value), .groups = "drop") ->
  CarbonDensity_Dist_yr

### approach 2 updated with min.soil.carbon.density substracted ----
GCAM_land_carbon_info_Forest %>%
  rowwise() %>%
  mutate(intensity_dist =
           list(LUC_flux_MtCO2_intensity_dist(
             E_soil_t = soil.carbon.density - min.soil.carbon.density,
             s        = SoilTime,
             E_veg_t  = veg.carbon.density,
             M        = mature.age
           ))) %>%
  unnest(intensity_dist) ->
  CarbonDensity_Dist_MinSoil

CarbonDensity_Dist_MinSoil %>%
  gather_years() %>%
  group_by_at(vars(region, basin, land, y_conv, EmYear = year, source)) %>%
  summarize(value = sum(value), .groups = "drop") ->
  CarbonDensity_Dist_yr_MinSoil


## 3.2 report AR land area ----
Land_Ems_basin_Forest %>%
  filter(!grepl("^Protected", land)) %>%
  group_by(scenario, region, land, basin) %>%
  mutate(LUC = Mha - lag(Mha),
         y_conv = lag(year)) %>%
  ungroup() %>%
  filter(year != 1990) %>%
  # we only care forest increase; adding a floor of zero
  mutate(LUC = pmax(0, LUC)) %>%
  filter(year >= 2025) ->
  Land_Ems_basin_Forest_Yr


## 3.3 applications ----

### approach 1 ----
Land_Ems_basin_Forest_Yr %>%
  left_join(CarbonDensity_Dist_yr %>% filter(y_conv != 2100) %>%
              # no ems in year 0
              mutate(value = if_else(EmYear == y_conv, 0, value)),
            by = c("region", "land", "basin", "y_conv")) %>%
  mutate(Ems = -LUC * value,
         # probably not needed as we only have Ems <0
         Ems = if_else(source == "Veg_flux_tCO2PerHa" & Ems >0, 0, Ems)) ->
  Ems_flux


Ems_flux %>%
  group_by(scenario, region, land, basin, EmYear, source) %>%
  summarize(Ems = sum(Ems)) %>%
  filter(EmYear %in% seq(2010, 2100, 5)) ->
  df_original

### approach 2 ----

Land_Ems_basin_Forest_Yr %>%
  left_join(CarbonDensity_Dist_yr_MinSoil %>% filter(y_conv != 2100) %>%
              # no ems in year 0
              mutate(value = if_else(EmYear == y_conv, 0, value)),
            by = c("region", "land", "basin", "y_conv")) %>%
  mutate(Ems = -LUC * value,
         # probably not needed as we only have Ems <0
         Ems = if_else(source == "Veg_flux_tCO2PerHa" & Ems >0, 0, Ems)) ->
  Ems_flux


Ems_flux %>%
  group_by(scenario, region, land, basin, EmYear, source) %>%
  summarize(Ems = sum(Ems)) %>%
  filter(EmYear %in% seq(2010, 2100, 5)) ->
  df_updated


# Step 4 merge tables ----

df_original %>%
  group_by(region, scenario, land, year = EmYear) %>%
  summarize(original_AR_MtCO2 = sum(Ems), .groups = "drop") %>%
  left_join_error_no_match(
    df_updated %>%
      group_by(region, scenario, land, year = EmYear) %>%
      summarize(updated_AR_MtCO2 = sum(Ems), .groups = "drop"),
    by = c("region", "scenario", "land", "year")
  ) ->
  AR_MtCO2_Annual


Land_Ems_basin_Forest_Yr %>%
  group_by(scenario, region, land, year) %>%
  summarize(AR_Mha = sum(LUC), .groups = "drop") %>%
  group_by_at(vars(-year, -AR_Mha)) %>%
  mutate(Cum_AR_Mha = cumsum(AR_Mha)) %>% select(-AR_Mha) %>%
  ungroup() ->
  AR_Mha_cumulative

AR_MtCO2_Annual %>%
  left_join(AR_Mha_cumulative) %>%
  replace_na(list(Cum_AR_Mha = 0)) ->
  AR_Reporting


AR_Reporting %>%
  group_by(scenario, year) %>%
  summarize(across(where(is.numeric), sum), .groups = "drop") %>%
  # join LULUCF for comparison
  left_join_error_no_match(
    Land_Ems_basin %>%
      group_by(scenario, year) %>%
      summarize(Total_LULUCF_MtCO2 = sum(MtCO2), .groups = "drop")
  ) ->
  AR_Reporting_World

# export data ----

AR_Reporting %>% readr::write_csv("output/AR_Reporting.csv")

AR_Reporting_World %>% readr::write_csv("output/AR_Reporting_World.csv")
