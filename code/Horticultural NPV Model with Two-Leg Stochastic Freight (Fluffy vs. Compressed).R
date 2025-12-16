# ============================================================
# Monte Carlo NPV with STOCHASTIC two-leg freight (SEPARATED)
# Leg 1 (Bog -> Plant): applied to FLUFFY volume (m3)
# Leg 2 (Plant -> Customer): applied to COMPRESSED volume (m3)
# Prices & costs are on COMPRESSED $/m3 basis
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
costs_data  <- read.csv("data/simulated_costs_m3.csv")
prices_data <- read.csv("data/simulated_peat_prices_lognormal_m3.csv")

yield_compressed <- read.csv("data/simulated_peat_volume_compressed.csv")      # m3/ha (compressed)
yield_fluf       <- read.csv("data/simulated_peat_volume_triangular.csv")      # m3/ha (fluffy)

# Ensure types
costs_data       <- costs_data       |> mutate(iteration = as.integer(iteration), year = as.integer(year))
prices_data      <- prices_data      |> mutate(iteration = as.integer(iteration), year = as.integer(year))
yield_compressed <- yield_compressed |> mutate(iteration = as.integer(iteration), year = as.integer(year))
yield_fluf       <- yield_fluf       |> mutate(iteration = as.integer(iteration), year = as.integer(year))

# Standardize names
yield_compressed <- yield_compressed |> rename(compressed_m3_ha = yield)
yield_fluf       <- yield_fluf       |> rename(fluffy_m3_ha     = yield)

# Build a balanced base panel (intersection of all series)
base_panel <- prices_data |> select(iteration, year, price_m3) |>
  inner_join(costs_data |> select(iteration, year, costs_m3), by = c("iteration","year")) |>
  inner_join(yield_compressed |> select(iteration, year, compressed_m3_ha), by = c("iteration","year")) |>
  inner_join(yield_fluf |> select(iteration, year, fluffy_m3_ha), by = c("iteration","year")) |>
  arrange(iteration, year)

iter_ids  <- sort(unique(base_panel$iteration))
years_vec <- sort(unique(base_panel$year))
N_iter    <- length(iter_ids)
T_years   <- length(years_vec)
final_year <- max(years_vec)

# --------------------------
# 2) Key parameters
# --------------------------
area_ha       <- 100
discount_rate <- 0.07
depr_rate     <- 0.08

# Royalty (per m3 harvested)
royalty_per_cy <- 0.11
m3_to_cy       <- 1.30795
royalty_per_m3 <- royalty_per_cy * m3_to_cy  # $/m3 (fluffy)

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
    
    freight1_m3_fluffy     = km_to_cost_per_m3(leg1_km, leg1_rate_per_km, leg1_cap_m3_TL),
    freight2_m3_compressed = km_to_cost_per_m3(leg2_km, leg2_rate_per_km, leg2_cap_m3_TL)
  ) |>
  select(iteration, year, is_CA, freight1_m3_fluffy, freight2_m3_compressed)

# --------------------------
# 6) Baseline cash flows
# --------------------------
all_data <- base_panel |>
  inner_join(freight_draws, by = c("iteration","year")) |>
  left_join(initial_df,    by = "iteration") |>
  mutate(
    # area totals
    total_compressed_m3 = compressed_m3_ha * area_ha,
    total_fluffy_m3     = fluffy_m3_ha     * area_ha,
    
    # revenue & costs (COMPRESSED basis)
    revenue             = price_m3 * total_compressed_m3,
    variable_cost_total = costs_m3 * total_compressed_m3,
    
    # royalty (FLUFFY basis, as you intended)
    royalty_total       = royalty_per_m3 * total_fluffy_m3,
    
    # restoration at final year
    restoration_total   = if_else(year == final_year, restoration_cost_per_ha * area_ha, 0),
    
    # separated freight totals
    freight1_total      = freight1_m3_fluffy     * total_fluffy_m3,
    freight2_total      = freight2_m3_compressed * total_compressed_m3,
    freight_total       = freight1_total + freight2_total,
    
    net_cash            = revenue - variable_cost_total - royalty_total - restoration_total - freight_total,
    
    discount_factor     = (1 + discount_rate)^year,
    discounted_net_cash = net_cash / discount_factor
  )

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

# ============================================================
# 7) Variance decomposition (fix inputs to mean paths)
# ============================================================

# Mean paths
price_mean_by_year <- prices_data |>
  group_by(year) |>
  summarise(price_m3 = mean(price_m3, na.rm = TRUE), .groups = "drop")

comp_mean_by_year <- yield_compressed |>
  group_by(year) |>
  summarise(compressed_m3_ha = mean(compressed_m3_ha, na.rm = TRUE), .groups = "drop")

fluf_mean_by_year <- yield_fluf |>
  group_by(year) |>
  summarise(fluffy_m3_ha = mean(fluffy_m3_ha, na.rm = TRUE), .groups = "drop")

freight_mean_by_year <- freight_draws |>
  group_by(year) |>
  summarise(
    freight1_m3_fluffy     = mean(freight1_m3_fluffy,     na.rm = TRUE),
    freight2_m3_compressed = mean(freight2_m3_compressed, na.rm = TRUE),
    .groups = "drop"
  )

# Expand mean paths to all iterations
price_fixed_all <- expand_to_all_iters(price_mean_by_year, c("price_m3"), iter_ids)
comp_fixed_all  <- expand_to_all_iters(comp_mean_by_year,  c("compressed_m3_ha"), iter_ids)
fluf_fixed_all  <- expand_to_all_iters(fluf_mean_by_year,  c("fluffy_m3_ha"), iter_ids)
freight_fixed_all <- expand_to_all_iters(
  freight_mean_by_year,
  c("freight1_m3_fluffy","freight2_m3_compressed"),
  iter_ids
)

# Stochastic (tidy) inputs
prices_stoch  <- prices_data |> select(iteration, year, price_m3)
comp_stoch    <- yield_compressed |> select(iteration, year, compressed_m3_ha)
fluf_stoch    <- yield_fluf       |> select(iteration, year, fluffy_m3_ha)
freight_stoch <- freight_draws    |> select(iteration, year, freight1_m3_fluffy, freight2_m3_compressed)
costs_stoch   <- costs_data       |> select(iteration, year, costs_m3)

recompute_NPV <- function(prices_tbl, comp_tbl, fluf_tbl, freight_tbl) {
  
  panel <- prices_tbl |>
    inner_join(costs_stoch, by = c("iteration","year")) |>
    inner_join(comp_tbl,    by = c("iteration","year")) |>
    inner_join(fluf_tbl,    by = c("iteration","year")) |>
    inner_join(freight_tbl, by = c("iteration","year")) |>
    arrange(iteration, year) |>
    mutate(
      total_compressed_m3 = compressed_m3_ha * area_ha,
      total_fluffy_m3     = fluffy_m3_ha     * area_ha,
      
      revenue             = price_m3 * total_compressed_m3,
      variable_cost_total = costs_m3 * total_compressed_m3,
      
      royalty_total       = royalty_per_m3 * total_fluffy_m3,
      restoration_total   = if_else(year == final_year, restoration_cost_per_ha * area_ha, 0),
      
      freight1_total      = freight1_m3_fluffy     * total_fluffy_m3,
      freight2_total      = freight2_m3_compressed * total_compressed_m3,
      freight_total       = freight1_total + freight2_total,
      
      net_cash            = revenue - variable_cost_total - royalty_total - restoration_total - freight_total,
      discounted_net_cash = net_cash / ((1 + discount_rate)^year)
    )
  
  pv <- panel |>
    group_by(iteration) |>
    summarise(PV_net_cash = sum(discounted_net_cash), .groups = "drop")
  
  initial_df |>
    inner_join(pv, by = "iteration") |>
    mutate(
      PV_salvage        = salvage_equipment / ((1 + discount_rate)^final_year),
      NPV_total_project = PV_net_cash + PV_salvage - total_initial_cost,
      NPV_per_ha        = NPV_total_project / area_ha
    )
}

# Experiments
NPV_E0  <- NPV_results
NPV_E1  <- recompute_NPV(price_fixed_all, comp_stoch,  fluf_stoch,  freight_stoch)       # Fix price
NPV_E2  <- recompute_NPV(prices_stoch,    comp_fixed_all, fluf_fixed_all, freight_stoch) # Fix volume (both)
NPV_E3  <- recompute_NPV(prices_stoch,    comp_stoch,  fluf_stoch,  freight_fixed_all)   # Fix freight (both)

NPV_E12 <- recompute_NPV(price_fixed_all, comp_fixed_all, fluf_fixed_all, freight_stoch)
NPV_E13 <- recompute_NPV(price_fixed_all, comp_stoch,     fluf_stoch,     freight_fixed_all)
NPV_E23 <- recompute_NPV(prices_stoch,    comp_fixed_all, fluf_fixed_all, freight_fixed_all)

sd_tbl <- tibble(
  Experiment = c("E0 Baseline",
                 "E1 Fix Price",
                 "E2 Fix Volume (compressed+fluffy)",
                 "E3 Fix Freight (leg1+leg2)",
                 "E12 Fix Price+Volume",
                 "E13 Fix Price+Freight",
                 "E23 Fix Volume+Freight"),
  SD_NPV_per_ha = c(sd(NPV_E0$NPV_per_ha,  na.rm = TRUE),
                    sd(NPV_E1$NPV_per_ha,  na.rm = TRUE),
                    sd(NPV_E2$NPV_per_ha,  na.rm = TRUE),
                    sd(NPV_E3$NPV_per_ha,  na.rm = TRUE),
                    sd(NPV_E12$NPV_per_ha, na.rm = TRUE),
                    sd(NPV_E13$NPV_per_ha, na.rm = TRUE),
                    sd(NPV_E23$NPV_per_ha, na.rm = TRUE))
)
cat("\n--- SD(NPV per ha) by experiment ---\n")
print(sd_tbl)

Var_all     <- var(NPV_E0$NPV_per_ha, na.rm = TRUE)
Var_noPrice <- var(NPV_E1$NPV_per_ha, na.rm = TRUE)
Var_noVol   <- var(NPV_E2$NPV_per_ha, na.rm = TRUE)
Var_noFre   <- var(NPV_E3$NPV_per_ha, na.rm = TRUE)

Contr_price   <- max(0, Var_all - Var_noPrice)
Contr_volume  <- max(0, Var_all - Var_noVol)
Contr_freight <- max(0, Var_all - Var_noFre)
Interaction   <- Var_all - (Contr_price + Contr_volume + Contr_freight)

share <- function(x) ifelse(Var_all > 0, x / Var_all, NA_real_)

contr_tbl <- tibble(
  Component = c("Price", "Volume", "Freight", "Interactions/Other"),
  Variance  = c(Contr_price, Contr_volume, Contr_freight, Interaction),
  Share     = c(share(Contr_price), share(Contr_volume), share(Contr_freight), share(Interaction))
)
cat("\n--- Variance contributions (approx.) ---\n")
print(contr_tbl)

# ============================================================
# 8) Price floor sensitivities
# ============================================================

# A) CONSTANT floors at overall P10/P20
P10_const <- quantile(prices_data$price_m3, 0.10, na.rm = TRUE)
P20_const <- quantile(prices_data$price_m3, 0.20, na.rm = TRUE)

prices_floor_P10_const <- prices_stoch |> mutate(price_m3 = pmax(price_m3, P10_const))
prices_floor_P20_const <- prices_stoch |> mutate(price_m3 = pmax(price_m3, P20_const))

NPV_floor_P10_const <- recompute_NPV(prices_floor_P10_const, comp_stoch, fluf_stoch, freight_stoch)
NPV_floor_P20_const <- recompute_NPV(prices_floor_P20_const, comp_stoch, fluf_stoch, freight_stoch)

# B) PER-YEAR floors
peryear_P10 <- prices_data |>
  group_by(year) |>
  summarise(floor = quantile(price_m3, 0.10, na.rm = TRUE), .groups = "drop")

peryear_P20 <- prices_data |>
  group_by(year) |>
  summarise(floor = quantile(price_m3, 0.20, na.rm = TRUE), .groups = "drop")

prices_floor_P10_path <- prices_stoch |>
  left_join(peryear_P10, by = "year") |>
  mutate(price_m3 = pmax(price_m3, floor)) |>
  select(iteration, year, price_m3)

prices_floor_P20_path <- prices_stoch |>
  left_join(peryear_P20, by = "year") |>
  mutate(price_m3 = pmax(price_m3, floor)) |>
  select(iteration, year, price_m3)

NPV_floor_P10_path <- recompute_NPV(prices_floor_P10_path, comp_stoch, fluf_stoch, freight_stoch)
NPV_floor_P20_path <- recompute_NPV(prices_floor_P20_path, comp_stoch, fluf_stoch, freight_stoch)

# C) Margin-based dynamic floors (in $/COMPRESSED m3 units)
# Convert fluffy-based leg1 + royalty into per-compressed-m3 using ratio = fluffy/compressed
threshold_tbl <- costs_stoch |>
  left_join(comp_stoch,    by = c("iteration","year")) |>
  left_join(fluf_stoch,    by = c("iteration","year")) |>
  left_join(freight_stoch, by = c("iteration","year")) |>
  mutate(
    ratio = if_else(compressed_m3_ha > 0, fluffy_m3_ha / compressed_m3_ha, NA_real_),
    
    threshold_noRoyalty   = costs_m3 +
      freight2_m3_compressed +
      freight1_m3_fluffy * ratio,
    
    threshold_withRoyalty = costs_m3 +
      freight2_m3_compressed +
      freight1_m3_fluffy * ratio +
      royalty_per_m3 * ratio
  ) |>
  select(iteration, year, threshold_noRoyalty, threshold_withRoyalty)

prices_floor_margin_noR <- prices_stoch |>
  left_join(threshold_tbl |> select(iteration, year, threshold_noRoyalty), by = c("iteration","year")) |>
  mutate(price_m3 = pmax(price_m3, threshold_noRoyalty)) |>
  select(iteration, year, price_m3)

prices_floor_margin_withR <- prices_stoch |>
  left_join(threshold_tbl |> select(iteration, year, threshold_withRoyalty), by = c("iteration","year")) |>
  mutate(price_m3 = pmax(price_m3, threshold_withRoyalty)) |>
  select(iteration, year, price_m3)

NPV_floor_margin_noR   <- recompute_NPV(prices_floor_margin_noR,   comp_stoch, fluf_stoch, freight_stoch)
NPV_floor_margin_withR <- recompute_NPV(prices_floor_margin_withR, comp_stoch, fluf_stoch, freight_stoch)

floor_sd_tbl <- tibble(
  Scenario = c("Const floor P10", "Const floor P20",
               "Per-year floor P10", "Per-year floor P20",
               "Margin floor (cost+freight)", "Margin floor (cost+freight+royalty)"),
  SD_NPV_per_ha = c(
    sd(NPV_floor_P10_const$NPV_per_ha,  na.rm = TRUE),
    sd(NPV_floor_P20_const$NPV_per_ha,  na.rm = TRUE),
    sd(NPV_floor_P10_path$NPV_per_ha,   na.rm = TRUE),
    sd(NPV_floor_P20_path$NPV_per_ha,   na.rm = TRUE),
    sd(NPV_floor_margin_noR$NPV_per_ha, na.rm = TRUE),
    sd(NPV_floor_margin_withR$NPV_per_ha, na.rm = TRUE)
  ),
  Mean_NPV_per_ha = c(
    mean(NPV_floor_P10_const$NPV_per_ha,  na.rm = TRUE),
    mean(NPV_floor_P20_const$NPV_per_ha,  na.rm = TRUE),
    mean(NPV_floor_P10_path$NPV_per_ha,   na.rm = TRUE),
    mean(NPV_floor_P20_path$NPV_per_ha,   na.rm = TRUE),
    mean(NPV_floor_margin_noR$NPV_per_ha, na.rm = TRUE),
    mean(NPV_floor_margin_withR$NPV_per_ha, na.rm = TRUE)
  )
)

cat("\n--- Price-floor scenarios: SD and Mean of NPV/ha ---\n")
print(floor_sd_tbl)
