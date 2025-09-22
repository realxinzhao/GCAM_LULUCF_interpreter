
# this script is for gcamdata to get GCAM_land_carbon_info

# load gcamdata
devtools::load_all()

# unmanaged land carbon
MODULE_INPUTS_UnmgdCarbon <-
  c("L222.LN2_UnmgdCarbon",
    "L2231.LN3_UnmgdCarbon")
# managed land carbon
MODULE_INPUTS_MgdCarbon <-
  c("L222.LN2_MgdCarbon",
    "L2231.LN3_MgdCarbon_noncrop",
    "L2252.LN5_MgdCarbon_crop",
    "L2252.LN5_MgdCarbon_bio")

MODULE_INPUTS <-
  c(MODULE_INPUTS_UnmgdCarbon,
    MODULE_INPUTS_MgdCarbon,
    "L221.LN0_SoilTimeScale")

MODULE_INPUTS %>% load_from_cache() -> all_data
# Load required inputs ----
get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

# bind all carbon info together
lapply(c(MODULE_INPUTS_UnmgdCarbon,
         MODULE_INPUTS_MgdCarbon),
       function(d){
         get(d) %>%
           dplyr::rename_with(~ gsub("UnmanagedLandLeaf", "LandLeaf", .x, fixed = TRUE)) %>%
           select(region, LandNode1, LandLeaf,
                  veg.carbon.density,
                  soil.carbon.density,
                  mature.age,
                  min.soil.carbon.density)
       }) %>%
  bind_rows() ->
  GCAM_land_carbon_info_1

# quick unit conversion and cleaning
# also join soil time
GCAM_land_carbon_info_1 %>%
  # convert density to tCO2 per ha from kgC/m2
  mutate(across(contains("density"), ~ .x * 10 * 44 / 12)) %>%
  # get basin name
  mutate(basin = gsub("AgroForestLand_", "", LandNode1), .after = region) %>%
  select(-LandNode1) %>%
  left_join_error_no_match(
    L221.LN0_SoilTimeScale %>% select(region, SoilTime = soilTimeScale),
    by = "region"
  ) ->
  GCAM_land_carbon_info

saveRDS(GCAM_land_carbon_info, "GCAM_land_carbon_info.rds")
