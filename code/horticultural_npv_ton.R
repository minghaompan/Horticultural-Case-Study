# ============================================================
# Monte Carlo NPV with STOCHASTIC two-leg freight
# TON BASIS:
# - Yields are tons
# - Prices & costs are $/ton
# - Freight is generated as $/m3, then converted to $/ton using bale-based m3/ton
#   Leg 1 uses FLUFFY m3/ton
#   Leg 2 uses COMPRESSED m3/ton
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

# --------------------------
# 0) Helper
# --------------------------
km_to_cost_per_m3 <- function(km, rate_per_km, capacity_m3_per_TL) {
  (rate_per_km * km) / capacity_m3_per_TL
}

expand_to_all_iters <- function(mean_path_tbl, value_cols, iter_ids) {
  stopifnot("year" %in% names(mean_path_tbl))
  stopifnot(all(value_cols %in% names(mean_path_tbl)))
  
  grid <- tidyr::crossing(
    iteration = iter_ids,
    year      = sort(unique(mean_path_tbl$year))
  )
  
  out <- dplyr::left_join(grid, mean_path_tbl, by = "year") |>
    dplyr::arrange(iteration, year)
  
  dplyr::select(out, iteration, year, dplyr::all_of(value_cols))
}

# --------------------------
# 1) Load inputs (T years x N iterations)
# --------------------------
costs_data  <- read.csv("data/simulated_costs.csv")
prices_data <- read.csv("data/simulated_peat_prices_lognormal_t.csv")
yield_t     <- read.csv("data/simulated_peat_volume_t.csv")  # yield is TON/ha now

# Ensure types
costs_data  <- costs_data  |> mutate(iteration = as.integer(iteration), year = as.integer(year))
prices_data <- prices_data |> mutate(iteration = as.integer(iteration), year = as.integer(year))
yield_t     <- yield_t     |> mutate(iteration = as.integer(iteration), year = as.integer(year))

# Standardize names (adjust here if your column names differ)
# Expect: price_t ($/ton), costs_t ($/ton), tons_ha (ton/ha)
costs_data  <- costs_data  |> rename(costs_t = costs)   # <- change to your actual cost column if needed
yield_t     <- yield_t     |> rename(tons_ha = yield_t)

# Build a balanced base panel (intersection of all series)
base_panel <- prices_data |> select(iteration, year, price_t) |>
  inner_join(costs_data  |> select(iteration, year, costs_t), by = c("iteration","year")) |>
  inner_join(yield_t     |> select(iteration, year, tons_ha), by = c("iteration","year")) |>
  arrange(iteration, year)

iter_ids   <- sort(unique(base_panel$iteration))
years_vec  <- sort(unique(base_panel$year))
N_iter     <- length(iter_ids)
final_year <- max(years_vec)

# --------------------------
# 2) Key parameters
# --------------------------
area_ha       <- 100
discount_rate <- 0.07
depr_rate     <- 0.08

# --- Bale-based volume conversion ---
# 6 ft3/bale, compression_factor=2 (loose = compressed * 2), 26 bales = 1 ton
bale_ft3            <- 6
compression_factor  <- 2
ft3_to_m3           <- 0.028317
bales_per_ton       <- 26

m3_per_bale_loose <- bale_ft3 * compression_factor * ft3_to_m3            # loose m3 per bale
m3_per_ton_fluffy <- m3_per_bale_loose * bales_per_ton                    # loose (fluffy) m3 per ton
m3_per_ton_comp   <- m3_per_ton_fluffy / compression_factor               # compressed m3 per ton

# Royalty (originally per m3 fluffy) -> convert to $/ton using fluffy m3/ton
royalty_per_cy <- 0.11
m3_to_cy       <- 1.30795
royalty_per_m3 <- royalty_per_cy * m3_to_cy  # $/m3 (fluffy)
royalty_per_t  <- royalty_per_m3 * m3_per_ton_fluffy

# Restoration (paid in final year, per ha)
restoration_cost_per_ha <- 3485

# Salvage factor (declining-balance over project length)
salvage_factor <- (1 - depr_rate)^final_year

# --------------------------
# 3) Year-0 fixed costs (draw per iteration)
# --------------------------
set.seed(123)
initial_costs  <- runif(N_iter, min = 2051500, max = 2653000)
env_app_costs  <- runif(N_iter, min = 300000,  max = 1000000)

initial_df <- tibble(
  iteration              = iter_ids,
  initial_cost_equipment = initial_costs,
  env_app_cost           = env_app_costs
) |>
  mutate(
    total_initial_cost = initial_cost_equipment + env_app_cost,
    salvage_equipment  = initial_cost_equipment * salvage_factor
  )

# --------------------------
# 4) Freight assumptions (per TL)
# --------------------------
# Leg 1: Bog -> Plant (bulk TLs) [FLUFFY volume basis]
leg1_rate_per_km <- 2.00
leg1_cap_m3_TL   <- 125
leg1_dist_km     <- c(379, 429, 505, 278, 207)

# Leg 2: Plant -> Customer (palletized TLs) [COMPRESSED volume basis]
leg2_rate_per_km <- 0.66
leg2_cap_m3_TL   <- 71
bc_dist_km       <- 1023
ca_dist_km_vec   <- c(1964, 2148, 2284)
share_CA         <- 0.50

# --------------------------
# 5) Build STOCHASTIC freight per (iteration, year)
# --------------------------
set.seed(456)
iy_grid <- tidyr::crossing(iteration = iter_ids, year = years_vec)

freight_draws <- iy_grid |>
  mutate(
    leg1_km = sample(leg1_dist_km, size = n(), replace = TRUE),
    is_CA   = runif(n()) < share_CA,
    leg2_km = ifelse(is_CA, sample(ca_dist_km_vec, size = n(), replace = TRUE), bc_dist_km),
    
    # draw freight as $/m3 (as before)
    freight1_m3_fluffy     = km_to_cost_per_m3(leg1_km, leg1_rate_per_km, leg1_cap_m3_TL),
    freight2_m3_compressed = km_to_cost_per_m3(leg2_km, leg2_rate_per_km, leg2_cap_m3_TL),
    
    # convert to $/ton using bale-based m3/ton
    freight1_t_fluffy      = freight1_m3_fluffy     * m3_per_ton_fluffy,
    freight2_t_compressed  = freight2_m3_compressed * m3_per_ton_comp
  ) |>
  select(iteration, year, is_CA, freight1_t_fluffy, freight2_t_compressed)

summary(freight_draws)
num_cols_fd <- sapply(freight_draws, is.numeric)
sapply(freight_draws[, num_cols_fd, drop = FALSE], sd, na.rm = TRUE)

# --------------------------
# 6) Baseline cash flows (TON BASIS)
# --------------------------
all_data <- base_panel |>
  inner_join(freight_draws, by = c("iteration","year")) |>
  left_join(initial_df,    by = "iteration") |>
  mutate(
    # area totals (tons)
    total_tons = tons_ha * area_ha,
    
    # optional volumes for reporting only
    total_fluffy_m3     = total_tons * m3_per_ton_fluffy,
    total_compressed_m3 = total_tons * m3_per_ton_comp,
    
    # revenue & costs ($/ton basis)
    revenue             = price_t * total_tons,
    variable_cost_total = costs_t * total_tons,
    
    # royalty ($/ton basis converted from $/m3 fluffy)
    royalty_total       = royalty_per_t * total_tons,
    
    # restoration at final year
    restoration_total   = if_else(year == final_year, restoration_cost_per_ha * area_ha, 0),
    
    # separated freight totals ($/ton basis)
    freight1_total      = freight1_t_fluffy     * total_tons,
    freight2_total      = freight2_t_compressed * total_tons,
    freight_total       = freight1_total + freight2_total,
    
    net_cash            = revenue - variable_cost_total - royalty_total - restoration_total - freight_total,
    
    discount_factor     = (1 + discount_rate)^year,
    discounted_net_cash = net_cash / discount_factor
  )

summary(all_data)
num_cols_ad <- sapply(all_data, is.numeric)
sapply(all_data[, num_cols_ad, drop = FALSE], sd, na.rm = TRUE)

pv_flows <- all_data |>
  group_by(iteration) |>
  summarise(PV_net_cash = sum(discounted_net_cash), .groups = "drop")

NPV_results <- initial_df |>
  inner_join(pv_flows, by = "iteration") |>
  mutate(
    PV_salvage        = salvage_equipment / ((1 + discount_rate)^final_year),
    NPV_total_project = PV_net_cash + PV_salvage - total_initial_cost,
    NPV_per_ha        = NPV_total_project / area_ha
  )

cat("\n--- Baseline ---\n")
print(summary(NPV_results$NPV_per_ha))
cat("Mean:", mean(NPV_results$NPV_per_ha, na.rm = TRUE), "\n")
cat("SD:",   sd(NPV_results$NPV_per_ha,   na.rm = TRUE), "\n")
cat("P(NPV>0):", mean(NPV_results$NPV_per_ha > 0, na.rm = TRUE), "\n")

hist(
  NPV_results$NPV_per_ha, breaks = 30,
  main = "Distribution of NPV per hectare",
  xlab = "NPV per ha ($)"
)
