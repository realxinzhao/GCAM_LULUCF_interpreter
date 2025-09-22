# Step 2 Generate functions for emission density distribution


library(ggplot2)

# GCAM: yearly allocation of vegetation carbon change using a sigmoid function ----
veg_alloc_S_curve <-
  function(y, y_conv, E_veg_t, M) {
  # y:        calendar year
  # y_conv:   year of land-use conversion
  # E_veg_t:  total potential vegetation carbon change (tCO2 per ha)
  # M:        mature age of vegetation (years)

  age1 <- (y - y_conv) + 1
  age0 <- (y - y_conv)

  # cumulative fractions from the sigmoid growth function
  frac1 <- (1 - exp(-3.0 * age1 / M))^2
  frac0 <- (1 - exp(-3.0 * age0 / M))^2

  # yearly allocation as the incremental difference
  alloc_frac <- ifelse(age0 < 0, 0, frac1 - frac0)

  E_veg_t * alloc_frac
}

## veg_alloc Example: ----
# For unmanaged forest in Brazil AmazonR with 40-year mature age
# and vegetation carbon density of 623 tCO2/ha,
# if conversion occurred in 2020, the annual sequestration in 2030 is:
veg_alloc_S_curve(2030, y_conv = 2020, E_veg_t = 623, M = 40)

## plots ----

y_conv   <- 2020
E_veg_t  <- 1     # normalize to 1 (fraction of total vegetation potential)
M        <- 40
years    <- 0:50 + y_conv

# Use the function to get yearly increments
increments <- sapply(years, function(y) veg_alloc_S_curve(y, y_conv, E_veg_t, M))
# Compute cumulative uptake
cumulative <- cumsum(increments)

df <- data.frame(
  Years_since_conversion = years - y_conv,
  Annual_increment = increments,
  Cumulative_fraction_released = cumulative
)

ggplot(df, aes(x = Years_since_conversion)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 1) +
  geom_vline(xintercept = 40, linetype = 2) +
  geom_line(aes(y = Cumulative_fraction_released, color = "Cumulative uptake"), size = 1.2) +
  geom_point(aes(y = Cumulative_fraction_released), size = 1, shape = 21, fill = "grey") +
  geom_line(aes(y = Annual_increment, color = "Annual increment"), size = 0.8) +
  geom_point(aes(y = Annual_increment), size = 1, shape = 21, fill = "grey") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = paste("Vegetation carbon uptake trajectory (M =", M, "yrs)"),
    subtitle = "Mature age = 40 years (90% growth by 40th year); e.g., Brazil AmazonR forest",
    x = "Years since conversion",
    y = "Fraction of total vegetation carbon potential",
    color = "Legend"
  ) +
  theme_bw(base_size = 14) -> p

ggsave("figure/veg_alloc_S_curve_example.png", width = 8, height = 6)





# GCAM: yearly allocation of soil carbon change using an exponential function ----
soil_alloc_exp_func <- function(y, y_conv, E_soil_t, s) {
  # y:        calendar year
  # y_conv:   year of land-use conversion
  # E_soil_t: total potential soil carbon change (tCO2)
  # s:        soil time scale parameter (years)

  kappa <- log(2) / (s / 10.0)
  age1  <- (y - y_conv) + 1
  age0  <- (y - y_conv)

  frac1 <- ifelse(age1 < 0, 0, (1 - exp(-kappa * age1)))
  frac0 <- ifelse(age0 < 0, 0, (1 - exp(-kappa * age0)))

  alloc_frac <- frac1 - frac0
  E_soil_t * alloc_frac
}

## Example usage: ----
# For unmanaged forest in Brazil AmazonR with soil carbon density of 330 tCO2/ha
# and a soil time scale of 25 years, if conversion occurred in 2020,
# the annual soil carbon flux in 2030 is:
soil_alloc_exp_func(2030, y_conv = 2025, E_soil_t = 330, s = 25)

## plots ----

y_conv   <- 2020
E_soil_t <- 1     # normalize to 1 for fraction
s        <- 25
years    <- 0:50 + y_conv

# Use your function to get yearly increments
increments <- sapply(years, function(y) soil_alloc_exp_func(y, y_conv, E_soil_t, s))
# Compute cumulative released
cumulative <- cumsum(increments)

df <- data.frame(
  Years_since_conversion = years - y_conv,
  Annual_increment = increments,
  Cumulative_fraction_released = cumulative
)

ggplot(df, aes(x = Years_since_conversion)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 1) +
  geom_line(aes(y = Cumulative_fraction_released, color = "Cumulative released"), size = 1.2) +
  geom_point(aes(y = Cumulative_fraction_released), size = 1, shape = 21, fill = "grey") +
  geom_line(aes(y = Annual_increment, color = "Annual increment"), size = 0.8) +
  geom_point(aes(y = Annual_increment), size = 1, shape = 21, fill = "grey") +
  geom_vline(xintercept = s/10 -1 , linetype = 2, color = "red") +
  annotate("text", x = s/10, y = 0.55, label = paste("Half-life ≈", s/10, "yrs?"),
           hjust = -0.1, color = "red") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = paste("Soil carbon uptake/release trajectory (s =", s, ")"),
    subtitle = "Soil time = 25 years (Half-time = 2.5 ?); e.g., Brazil AmazonR forest",
    x = "Years since conversion",
    y = "Fraction of total soil carbon potential",
    color = "Legend"
  ) +
  theme_bw(base_size = 14) -> p;p

ggsave("figure/soil_alloc_exp_func_example.png", width = 8, height = 6)




# Combined function: LUC flux intensity distributions ----

LUC_flux_MtCO2_intensity_dist <- function(E_soil_t, s, E_veg_t, M){

  # Generate annual intensity distributions (soil + vegetation) per ha
  # across GCAM model years (1990–2100).
  #
  # Args:
  #   E_soil_t: soil carbon potential (tCO2/ha)
  #   s:        soil time scale (years)
  #   E_veg_t:  vegetation carbon potential (tCO2/ha)
  #   M:        vegetation mature age (years)
  #
  # Returns:
  #   Data frame with flux intensity by source (soil/veg) and year


  # Inputs ----
  years <- 1990:2100
  # GCAM model years: 1990, 2005, then 5-year steps
  y_convs <- c(1990, 2005, seq(2010, 2100, by = 5))


  do.call(rbind, lapply(y_convs, function(conv) {

    data.frame(
      EmYear = years,
      y_conv = conv,
      value = sapply(years, function(y) soil_alloc_exp_func(y, conv, E_soil_t, s))
    )
  })) ->
    Soil_flux_MtCO2_intensity_dist


  do.call(rbind, lapply(y_convs, function(conv) {

    data.frame(
      EmYear = years,
      y_conv = conv,
      value = sapply(years, function(y) veg_alloc_S_curve(y, conv, E_veg_t, M))
    )
  })) ->
    Veg_flux_tCO2_intensity_dist

  Soil_flux_MtCO2_intensity_dist %>%
    mutate(source = "Soil_flux_tCO2PerHa") %>%
    bind_rows(
      Veg_flux_tCO2_intensity_dist %>%
        mutate(source = "Veg_flux_tCO2PerHa")
    ) ->
    LUC_flux_MtCO2_intensity_dist

  return(LUC_flux_MtCO2_intensity_dist %>% spread(EmYear, value))

}





