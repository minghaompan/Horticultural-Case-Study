# ============================================================
# Monte Carlo NPV with STOCHASTIC two-leg freight
# + Variance Decomp + Price-Floor (incl. cost+freight floors)
# ============================================================

# --------------------------
# Packages
# --------------------------
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(tidyselect)
})

# --------------------------
# 0) Helper
# --------------------------
km_to_cost_per_m3 <- function(km, rate_per_km, capacity_m3_per_TL) {
  (rate_per_km * km) / capacity_m3_per_TL
}

# --------------------------
# 1) Load inputs (18 years x 1000 iters)
data <- read.csv("data/peat_price_alberta.csv")
colnames(data) <- tolower(colnames(data))
# --------------------------
costs_data  <- read.csv("data/simulated_costs_m3.csv")
prices_data <- read.csv("data/simulated_peat_prices_lognormal_m3.csv")
yield_data  <- read.csv("data/simulated_peat_volume_triangular.csv")

# Ensure integer types for joins/sorting
costs_data  <- dplyr::mutate(costs_data,  iteration = as.integer(iteration), year = as.integer(year))
prices_data <- dplyr::mutate(prices_data, iteration = as.integer(iteration), year = as.integer(year))
yield_data  <- dplyr::mutate(yield_data,  iteration = as.integer(iteration), year = as.integer(year))

# Dimensions
N_iter  <- max(yield_data$iteration, na.rm = TRUE)
T_years <- max(yield_data$year,       na.rm = TRUE)

# --------------------------
# 2) Key parameters
# --------------------------
area_ha       <- 100
discount_rate <- 0.07
depr_rate     <- 0.08

# Royalty (per m3)
royalty_per_cy <- 0.11
m3_to_cy       <- 1.30795
royalty_per_m3 <- royalty_per_cy * m3_to_cy

# Restoration (paid in year 18, per ha)
restoration_cost_per_ha <- 3485

# Salvage factor (equipment, declining-balance over 18 yrs)
salvage_factor <- (1 - depr_rate)^18

# Year-0 fixed costs (draw per iteration)
set.seed(123)
n_iters        <- N_iter
initial_costs  <- runif(n_iters, min = 2051500, max = 2653000)
env_app_costs  <- runif(n_iters, min = 300000,  max = 1000000)
salvage_values <- initial_costs * salvage_factor

initial_df <- dplyr::tibble(
  iteration              = 1:n_iters,
  initial_cost_equipment = initial_costs,
  env_app_cost           = env_app_costs,
  total_initial_cost     = initial_costs + env_app_costs,
  salvage_equipment      = salvage_values
)

# --------------------------
# 3) Freight assumptions (per TL)
# --------------------------
# Leg 1: Bog -> Plant (bulk TLs)
leg1_rate_per_km <- 2.00
leg1_cap_m3_TL   <- 125
leg1_dist_km     <- c(379, 429, 505, 278, 207)

# Leg 2: Plant -> Customer (palletized TLs)
leg2_rate_per_km <- 0.66
leg2_cap_m3_TL   <- 71
bc_dist_km       <- 1023
ca_dist_km_vec   <- c(1964, 2148, 2284)
share_CA         <- 0.50  # P(market = CA)

# --------------------------
# 4) Build STOCHASTIC freight per (iteration, year)
# --------------------------
set.seed(123)

iy_grid <- dplyr::tibble(
  iteration = rep(1:N_iter, each = T_years),
  year      = rep(1:T_years, times = N_iter)
)

freight_draws <- iy_grid |>
  dplyr::mutate(
    leg1_km = sample(leg1_dist_km, size = dplyr::n(), replace = TRUE),
    is_CA   = runif(dplyr::n()) < share_CA,
    leg2_km = ifelse(is_CA, sample(ca_dist_km_vec, size = dplyr::n(), replace = TRUE), bc_dist_km),
    c_leg1_m3 = km_to_cost_per_m3(leg1_km, leg1_rate_per_km, leg1_cap_m3_TL),
    c_leg2_m3 = km_to_cost_per_m3(leg2_km, leg2_rate_per_km, leg2_cap_m3_TL)
  ) |>
  dplyr::transmute(iteration, year, freight_per_m3 = c_leg1_m3 + c_leg2_m3)

# --------------------------
# 5) Merge series and compute annual cash flows (baseline)
# --------------------------
all_data <- prices_data |>
  dplyr::inner_join(yield_data,   by = c("iteration", "year")) |>
  dplyr::inner_join(costs_data,   by = c("iteration", "year")) |>
  dplyr::left_join(freight_draws, by = c("iteration", "year")) |>
  dplyr::arrange(iteration, year) |>
  dplyr::mutate(
    total_yield_m3       = yield * area_ha,
    revenue              = price_m3       * total_yield_m3,
    variable_cost_total  = costs_m3       * total_yield_m3,
    royalty_total        = royalty_per_m3 * total_yield_m3,
    restoration_total    = dplyr::if_else(year == 18, restoration_cost_per_ha * area_ha, 0),
    freight_total        = freight_per_m3 * total_yield_m3,
    net_cash             = revenue - variable_cost_total - royalty_total - restoration_total - freight_total
  )

# --------------------------
# 6) Discounting (baseline)
# --------------------------
all_data <- all_data |>
  dplyr::mutate(
    discount_factor     = (1 + discount_rate)^year,
    discounted_net_cash = net_cash / discount_factor
  )

pv_flows <- all_data |>
  dplyr::group_by(iteration) |>
  dplyr::summarise(PV_net_cash = sum(discounted_net_cash), .groups = "drop")

# --------------------------
# 7) Add capex & salvage; compute NPV (baseline)
# --------------------------
NPV_results <- initial_df |>
  dplyr::inner_join(pv_flows, by = "iteration") |>
  dplyr::mutate(
    PV_salvage        = salvage_equipment / ((1 + discount_rate)^18),
    NPV_total_project = PV_net_cash + PV_salvage - total_initial_cost,
    NPV_per_ha        = NPV_total_project / area_ha
  )

# --------------------------
# 8) Inspect & summarize (baseline)
# --------------------------
print(utils::head(NPV_results))

summary_baseline <- summary(NPV_results$NPV_per_ha)
sd_NPV <- stats::sd(NPV_results$NPV_per_ha, na.rm = TRUE)
mean_NPV_per_ha      <- mean(NPV_results$NPV_per_ha)
positive_fraction_ha <- mean(NPV_results$NPV_per_ha > 0)

cat(sprintf("\n--- Baseline ---\nMean NPV per ha: %.2f\nSD NPV per ha: %.2f\nProportion NPV>0: %.3f\n",
            mean_NPV_per_ha, sd_NPV, positive_fraction_ha))

options(scipen = 999)

# ============================================================
# 9) Variance decomposition (hold-one-input-fixed experiments)
# ============================================================

# --- SAFER helper: expand per-year mean path to all iterations ---
expand_to_all_iters <- function(mean_path_tbl, value_col) {
  stopifnot("year" %in% names(mean_path_tbl), value_col %in% names(mean_path_tbl))
  grid <- dplyr::tibble(iteration = 1:N_iter) |>
    tidyr::crossing(year = sort(unique(mean_path_tbl$year)))
  out <- dplyr::left_join(grid, mean_path_tbl, by = "year") |>
    dplyr::arrange(iteration, year)
  dplyr::select(out, iteration, year, tidyselect::all_of(value_col))
}

# --- Build mean paths ---
price_mean_by_year <- prices_data |>
  dplyr::group_by(year) |>
  dplyr::summarise(price_m3 = mean(price_m3, na.rm = TRUE), .groups = "drop")

yield_mean_by_year <- yield_data |>
  dplyr::group_by(year) |>
  dplyr::summarise(yield = mean(yield, na.rm = TRUE), .groups = "drop")

freight_mean_by_year <- freight_draws |>
  dplyr::group_by(year) |>
  dplyr::summarise(freight_per_m3 = mean(freight_per_m3, na.rm = TRUE), .groups = "drop")

# --- Expand to all iterations using the safer helper ---
price_fixed_all   <- expand_to_all_iters(price_mean_by_year,   "price_m3")
yield_fixed_all   <- expand_to_all_iters(yield_mean_by_year,   "yield")
freight_fixed_all <- expand_to_all_iters(freight_mean_by_year, "freight_per_m3")

# --- Prepare stochastic versions (tidy)
prices_stoch  <- dplyr::select(prices_data,   iteration, year, price_m3)
yield_stoch   <- dplyr::select(yield_data,    iteration, year, yield)
freight_stoch <- dplyr::select(freight_draws, iteration, year, freight_per_m3)

# --- Helper: recompute NPV given replacements for (price, yield, freight)
recompute_NPV <- function(prices_tbl, yield_tbl, freight_tbl) {
  all_data_mod <- prices_tbl |>
    dplyr::inner_join(yield_tbl,   by = c("iteration","year")) |>
    dplyr::inner_join(costs_data,  by = c("iteration","year")) |>
    dplyr::left_join(freight_tbl,  by = c("iteration","year")) |>
    dplyr::arrange(iteration, year) |>
    dplyr::mutate(
      total_yield_m3      = yield * area_ha,
      revenue             = price_m3       * total_yield_m3,
      variable_cost_total = costs_m3       * total_yield_m3,
      royalty_total       = royalty_per_m3 * total_yield_m3,
      restoration_total   = dplyr::if_else(year == 18, restoration_cost_per_ha * area_ha, 0),
      freight_total       = freight_per_m3 * total_yield_m3,
      net_cash            = revenue - variable_cost_total - royalty_total - restoration_total - freight_total,
      discount_factor     = (1 + discount_rate)^year,
      discounted_net_cash = net_cash / discount_factor
    )
  
  pv_flows_mod <- all_data_mod |>
    dplyr::group_by(iteration) |>
    dplyr::summarise(PV_net_cash = sum(discounted_net_cash), .groups = "drop")
  
  initial_df |>
    dplyr::inner_join(pv_flows_mod, by = "iteration") |>
    dplyr::mutate(
      PV_salvage        = salvage_equipment / ((1 + discount_rate)^18),
      NPV_total_project = PV_net_cash + PV_salvage - total_initial_cost,
      NPV_per_ha        = NPV_total_project / area_ha
    )
}

# --- Experiments (variance decomp) ---
NPV_E0  <- NPV_results
NPV_E1  <- recompute_NPV(price_fixed_all, yield_stoch,  freight_stoch)   # Fix price
NPV_E2  <- recompute_NPV(prices_stoch,   yield_fixed_all, freight_stoch) # Fix volume
NPV_E3  <- recompute_NPV(prices_stoch,   yield_stoch,  freight_fixed_all)# Fix freight
NPV_E12 <- recompute_NPV(price_fixed_all, yield_fixed_all, freight_stoch)
NPV_E13 <- recompute_NPV(price_fixed_all, yield_stoch,     freight_fixed_all)
NPV_E23 <- recompute_NPV(prices_stoch,    yield_fixed_all, freight_fixed_all)

sd_tbl <- dplyr::tibble(
  Experiment = c("E0 Baseline (all stochastic)",
                 "E1 Fix Price (Vol, Freight stochastic)",
                 "E2 Fix Volume (Price, Freight stochastic)",
                 "E3 Fix Freight (Price, Volume stochastic)",
                 "E12 Fix Price+Volume (Freight stochastic)",
                 "E13 Fix Price+Freight (Volume stochastic)",
                 "E23 Fix Volume+Freight (Price stochastic)"),
  SD_NPV_per_ha = c(stats::sd(NPV_E0$NPV_per_ha,  na.rm = TRUE),
                    stats::sd(NPV_E1$NPV_per_ha,  na.rm = TRUE),
                    stats::sd(NPV_E2$NPV_per_ha,  na.rm = TRUE),
                    stats::sd(NPV_E3$NPV_per_ha,  na.rm = TRUE),
                    stats::sd(NPV_E12$NPV_per_ha, na.rm = TRUE),
                    stats::sd(NPV_E13$NPV_per_ha, na.rm = TRUE),
                    stats::sd(NPV_E23$NPV_per_ha, na.rm = TRUE))
)

cat("\n--- SD(NPV per ha) by experiment ---\n"); print(sd_tbl)

Var_all     <- stats::var(NPV_E0$NPV_per_ha,  na.rm = TRUE)
Var_noPrice <- stats::var(NPV_E1$NPV_per_ha,  na.rm = TRUE)
Var_noVol   <- stats::var(NPV_E2$NPV_per_ha,  na.rm = TRUE)
Var_noFre   <- stats::var(NPV_E3$NPV_per_ha,  na.rm = TRUE)

Contr_price   <- max(0, Var_all - Var_noPrice)
Contr_volume  <- max(0, Var_all - Var_noVol)
Contr_freight <- max(0, Var_all - Var_noFre)
Interaction   <- Var_all - (Contr_price + Contr_volume + Contr_freight)

share <- function(x) ifelse(Var_all > 0, x / Var_all, NA_real_)

contr_tbl <- dplyr::tibble(
  Component = c("Price", "Volume", "Freight", "Interactions/Other"),
  Variance  = c(Contr_price, Contr_volume, Contr_freight, Interaction),
  Share     = c(share(Contr_price), share(Contr_volume), share(Contr_freight), share(Interaction))
)

cat("\n--- Variance contributions (approx.) ---\n"); print(contr_tbl)

# ============================================================
# 10) Price floor sensitivities (constant, per-year, and margin-based)
# ============================================================

# --- A) CONSTANT floors at overall P10/P20 ---
P10_const <- stats::quantile(prices_data$price_m3, 0.10, na.rm = TRUE)
P20_const <- stats::quantile(prices_data$price_m3, 0.20, na.rm = TRUE)

prices_floor_P10_const <- prices_data |>
  dplyr::mutate(price_m3 = pmax(price_m3, P10_const)) |>
  dplyr::select(iteration, year, price_m3)

prices_floor_P20_const <- prices_data |>
  dplyr::mutate(price_m3 = pmax(price_m3, P20_const)) |>
  dplyr::select(iteration, year, price_m3)

NPV_floor_P10_const <- recompute_NPV(prices_floor_P10_const, yield_stoch, freight_stoch)
NPV_floor_P20_const <- recompute_NPV(prices_floor_P20_const, yield_stoch, freight_stoch)

# --- B) PER-YEAR floors (each year's P10/P20) ---
peryear_P10 <- prices_data |>
  dplyr::group_by(year) |>
  dplyr::summarise(floor = stats::quantile(price_m3, 0.10, na.rm = TRUE), .groups = "drop")

peryear_P20 <- prices_data |>
  dplyr::group_by(year) |>
  dplyr::summarise(floor = stats::quantile(price_m3, 0.20, na.rm = TRUE), .groups = "drop")

prices_floor_P10_path <- prices_data |>
  dplyr::left_join(peryear_P10, by = "year") |>
  dplyr::mutate(price_m3 = pmax(price_m3, floor)) |>
  dplyr::select(iteration, year, price_m3)

prices_floor_P20_path <- prices_data |>
  dplyr::left_join(peryear_P20, by = "year") |>
  dplyr::mutate(price_m3 = pmax(price_m3, floor)) |>
  dplyr::select(iteration, year, price_m3)

NPV_floor_P10_path <- recompute_NPV(prices_floor_P10_path, yield_stoch, freight_stoch)
NPV_floor_P20_path <- recompute_NPV(prices_floor_P20_path, yield_stoch, freight_stoch)

# --- C) Margin-based dynamic floors (costs + freight [+ royalty]) ---
threshold_tbl <- costs_data |>
  dplyr::select(iteration, year, costs_m3) |>
  dplyr::left_join(dplyr::select(freight_draws, iteration, year, freight_per_m3),
                   by = c("iteration","year")) |>
  dplyr::mutate(
    threshold_noRoyalty  = costs_m3 + freight_per_m3,
    threshold_withRoyalty= costs_m3 + freight_per_m3 + royalty_per_m3  # include royalty if desired
  )

prices_floor_margin_noR <- prices_data |>
  dplyr::left_join(dplyr::select(threshold_tbl, iteration, year, threshold_noRoyalty),
                   by = c("iteration","year")) |>
  dplyr::mutate(price_m3 = pmax(price_m3, threshold_noRoyalty)) |>
  dplyr::select(iteration, year, price_m3)

prices_floor_margin_withR <- prices_data |>
  dplyr::left_join(dplyr::select(threshold_tbl, iteration, year, threshold_withRoyalty),
                   by = c("iteration","year")) |>
  dplyr::mutate(price_m3 = pmax(price_m3, threshold_withRoyalty)) |>
  dplyr::select(iteration, year, price_m3)

NPV_floor_margin_noR   <- recompute_NPV(prices_floor_margin_noR,   yield_stoch, freight_stoch)
NPV_floor_margin_withR <- recompute_NPV(prices_floor_margin_withR, yield_stoch, freight_stoch)

# --- Summarize SDs (and means) for all floor variants ---
floor_sd_tbl <- dplyr::tibble(
  Scenario = c("Const floor P10", "Const floor P20",
               "Per-year floor P10", "Per-year floor P20",
               "Margin floor (cost+freight)", "Margin floor (cost+freight+royalty)"),
  SD_NPV_per_ha = c(
    stats::sd(NPV_floor_P10_const$NPV_per_ha,  na.rm = TRUE),
    stats::sd(NPV_floor_P20_const$NPV_per_ha,  na.rm = TRUE),
    stats::sd(NPV_floor_P10_path$NPV_per_ha,   na.rm = TRUE),
    stats::sd(NPV_floor_P20_path$NPV_per_ha,   na.rm = TRUE),
    stats::sd(NPV_floor_margin_noR$NPV_per_ha, na.rm = TRUE),
    stats::sd(NPV_floor_margin_withR$NPV_per_ha, na.rm = TRUE)
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

cat("\n--- Price-floor scenarios: SD and Mean of NPV/ha ---\n"); print(floor_sd_tbl)

summary(NPV_floor_margin_noR$NPV_per_ha)
sd_NPV <- sd(NPV_floor_margin_noR$NPV_per_ha, na.rm = TRUE)
mean_NPV_per_ha      <- mean(NPV_floor_margin_noR$NPV_per_ha)
positive_fraction_ha <- mean(NPV_floor_margin_noR$NPV_per_ha > 0)
cat(sprintf("Mean NPV per ha: %.2f\nSD NPV per ha: %.2f\nProportion NPV>0: %.3f\n",
            mean_NPV_per_ha, sd_NPV, positive_fraction_ha))
hist(
  NPV_floor_margin_noR$NPV_per_ha, breaks = 30,
  main = "Distribution of NPV per hectare (price threshold = variable + freight costs)",
  xlab = "NPV per ha ($)"
)