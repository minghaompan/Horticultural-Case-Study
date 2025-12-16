library(dplyr)

# ----------------------------
# 0) Read inputs
# ----------------------------
price <- read.csv("data/simulated_peat_prices_lognormal_t.csv")   # has: year, iteration, price_t
costs <- read.csv("data/simulated_costs.csv")                    # has: year, iteration, costs
vol   <- read.csv("data/simulated_peat_volume_triangular.csv")   # has: year, iteration, yield (m3/ha)

# ----------------------------
# 1) Simulate bulk density ONCE and save
# ----------------------------
set.seed(123)
bd <- vol %>%
  select(year, iteration) %>%
  mutate(bd_tpm3 = runif(n(), min = 0.226, max = 0.261))

write.csv(bd, "data/simulated_bulk_density_uniform.csv", row.names = FALSE)

# ----------------------------
# 2) Convert volume (harvest m3/ha -> sale m3/ha) and save
# ----------------------------
fluffy_bd <- 0.10  # t/m3 for fluffy harvested peat

vol_conv <- vol %>%
  mutate(
    harv_m3_ha = yield,
    mass_t_ha  = harv_m3_ha * fluffy_bd
  ) %>%
  left_join(bd, by = c("year", "iteration")) %>%
  mutate(
    yield = mass_t_ha / bd_tpm3
  ) %>%
  select(year, iteration, harv_m3_ha, mass_t_ha, bd_tpm3, yield)

summary(vol_conv)

write.csv(
  vol_conv[, c("year", "iteration", "yield")],
  "data/simulated_peat_volume_compressed.csv",
  row.names = FALSE
)

# ----------------------------
# 3) Convert price ($/t -> $/m3 using simulated bd) and save
# ----------------------------
price_conv <- price %>%
  left_join(bd, by = c("year", "iteration")) %>%
  mutate(price_m3 = price_t * bd_tpm3) %>%
  select(year, iteration, price_t, bd_tpm3, price_m3)

summary(price_conv)

write.csv(
  price_conv [, c("year", "iteration", "price_m3")],
  "data/simulated_peat_prices_lognormal_m3.csv",
  row.names = FALSE
)


# ----------------------------
# 4) Convert costs ($/t -> $/m3 using simulated bd) and save
# ----------------------------
costs_conv <- costs %>%
  left_join(bd, by = c("year", "iteration")) %>%
  mutate(costs_m3 = costs * bd_tpm3) %>%
  select(year, iteration, costs, bd_tpm3, costs_m3)

summary(costs_conv)

write.csv(
  costs_conv [, c("year", "iteration", "costs_m3")],
  "data/simulated_costs_m3.csv",
  row.names = FALSE
)
