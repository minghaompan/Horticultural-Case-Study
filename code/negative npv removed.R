# ============================================================
# Monte Carlo NPV with STOCHASTIC two-leg freight
# + Sensitivity analysis: price, freight, and operating cost
#   each increase/decrease by 15%
#
# TON BASIS:
# - Yields are tons
# - Prices & costs are $/ton
# - Freight is generated as $/m3, then converted to $/ton
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
# 1) Read data
# --------------------------
costs_data  <- read.csv("data/simulated_costs.csv")
prices_data <- read.csv("data/simulated_peat_prices_lognormal_t.csv")
yield_t     <- read.csv("data/simulated_peat_volume_t.csv")  # yield is TON/ha

# Ensure types
costs_data  <- costs_data |>
  dplyr::mutate(
    iteration = as.integer(iteration),
    year = as.integer(year)
  )

prices_data <- prices_data |>
  dplyr::mutate(
    iteration = as.integer(iteration),
    year = as.integer(year)
  )

yield_t     <- yield_t |>
  dplyr::mutate(
    iteration = as.integer(iteration),
    year = as.integer(year)
  )

# Standardize names
costs_data <- costs_data |> dplyr::rename(costs_t = costs)
yield_t    <- yield_t    |> dplyr::rename(tons_ha = yield_t)

# Build balanced base panel
base_panel <- prices_data |> 
  dplyr::select(iteration, year, price_t) |>
  dplyr::inner_join(
    costs_data |> dplyr::select(iteration, year, costs_t),
    by = c("iteration", "year")
  ) |>
  dplyr::inner_join(
    yield_t |> dplyr::select(iteration, year, tons_ha),
    by = c("iteration", "year")
  ) |>
  dplyr::arrange(iteration, year)

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
bale_ft3           <- 6
compression_factor <- 2
ft3_to_m3          <- 0.028317
bales_per_ton      <- 26

m3_per_bale_loose <- bale_ft3 * compression_factor * ft3_to_m3
m3_per_ton_fluffy <- m3_per_bale_loose * bales_per_ton
m3_per_ton_comp   <- m3_per_ton_fluffy / compression_factor

# Royalty (originally per m3 fluffy) -> convert to $/ton using fluffy m3/ton
royalty_per_cy <- 0.11
m3_to_cy       <- 1.30795
royalty_per_m3 <- royalty_per_cy * m3_to_cy
royalty_per_t  <- royalty_per_m3 * m3_per_ton_fluffy

# Restoration (paid in final year, per ha)
restoration_cost_per_ha <- 3485

# Salvage factor
salvage_factor <- (1 - depr_rate)^final_year

# --------------------------
# 3) Year-0 fixed costs (draw per iteration)
# --------------------------
set.seed(123)
initial_costs <- runif(N_iter, min = 2051500, max = 2653000)
env_app_costs <- runif(N_iter, min = 300000,  max = 1000000)

initial_df <- tibble::tibble(
  iteration              = iter_ids,
  initial_cost_equipment = initial_costs,
  env_app_cost           = env_app_costs
) |>
  dplyr::mutate(
    total_initial_cost = initial_cost_equipment + env_app_cost,
    salvage_equipment  = initial_cost_equipment * salvage_factor
  )

# --------------------------
# 4) Freight assumptions
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
# 5) Build stochastic freight
# --------------------------
set.seed(456)
iy_grid <- tidyr::crossing(iteration = iter_ids, year = years_vec)

freight_draws <- iy_grid |>
  dplyr::mutate(
    leg1_km = sample(leg1_dist_km, size = dplyr::n(), replace = TRUE),
    is_CA   = runif(dplyr::n()) < share_CA,
    leg2_km = ifelse(
      is_CA,
      sample(ca_dist_km_vec, size = dplyr::n(), replace = TRUE),
      bc_dist_km
    ),
    
    freight1_m3_fluffy     = km_to_cost_per_m3(leg1_km, leg1_rate_per_km, leg1_cap_m3_TL),
    freight2_m3_compressed = km_to_cost_per_m3(leg2_km, leg2_rate_per_km, leg2_cap_m3_TL),
    
    freight1_t_fluffy     = freight1_m3_fluffy     * m3_per_ton_fluffy,
    freight2_t_compressed = freight2_m3_compressed * m3_per_ton_comp
  ) |>
  dplyr::select(iteration, year, is_CA, freight1_t_fluffy, freight2_t_compressed)

summary(freight_draws)
num_cols_fd <- sapply(freight_draws, is.numeric)
sapply(freight_draws[, num_cols_fd, drop = FALSE], sd, na.rm = TRUE)

# ============================================================
# 6) Carbon emissions (fixed across sensitivity scenarios)
# ============================================================

# ---- 6.1 Carbon-accounting parameters ----
k_decay    <- 0.054
C_frac_dry <- 0.46

moisture_frac <- 0.50
dry_frac      <- 1 - moisture_frac

EF_onsite_value <- 1.4
EF_unit         <- "tC"

# ---- 6.2 Conversion ----
tC_to_tCO2 <- function(tC) tC * (44 / 12)

# ---- 6.3 Off-site emissions ----
offsite_emissions_time_integrated <- function(m_tC, k) {
  Tn <- length(m_tC)
  R  <- numeric(Tn)
  E  <- numeric(Tn)
  
  for (t in seq_len(Tn)) {
    R[t] <- m_tC[t] + (1 - k) * ifelse(t == 1, 0, R[t - 1])
    E[t] <- k * R[t]
  }
  E
}

# ---- 6.4 Emissions depend only on physical production, not price/cost scenarios ----
emissions_df <- base_panel |>
  dplyr::group_by(iteration) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    area_ha    = area_ha,
    total_tons = tons_ha * area_ha,
    m_tC       = total_tons * dry_frac * C_frac_dry,
    offsite_tC = offsite_emissions_time_integrated(m_tC, k_decay),
    offsite_tCO2e = tC_to_tCO2(offsite_tC),
    
    onsite_tCO2e = if (EF_unit == "tC") {
      tC_to_tCO2(EF_onsite_value * area_ha)
    } else {
      EF_onsite_value * area_ha
    },
    
    total_tCO2e = onsite_tCO2e + offsite_tCO2e,
    disc_tCO2e  = total_tCO2e / ((1 + discount_rate)^year)
  ) |>
  dplyr::ungroup()

# ---- 6.5 PV emissions per iteration ----
pv_emis <- emissions_df |>
  dplyr::group_by(iteration) |>
  dplyr::summarise(
    T = max(year),
    area_ha = first(area_ha),
    
    PV_onsite_1T  = sum(onsite_tCO2e  / ((1 + discount_rate)^year), na.rm = TRUE),
    PV_offsite_1T = sum(offsite_tCO2e / ((1 + discount_rate)^year), na.rm = TRUE),
    
    offsite_T = offsite_tCO2e[year == T][1],
    
    PV_offsite_tail = (offsite_T / ((1 + discount_rate)^T)) *
      ((1 - k_decay) / (discount_rate + k_decay)),
    
    PV_tCO2e_total  = PV_onsite_1T + PV_offsite_1T + PV_offsite_tail,
    PV_tCO2e_per_ha = PV_tCO2e_total / area_ha,
    .groups = "drop"
  )

cat("\n--- PV emissions (tCO2e/ha), INCLUDING infinite off-site tail ---\n")
print(summary(pv_emis$PV_tCO2e_per_ha))

# ============================================================
# 7) Scenario runner
# ============================================================

run_project_scenario <- function(price_mult = 1,
                                 freight_mult = 1,
                                 cost_mult = 1) {
  
  all_data <- base_panel |>
    dplyr::inner_join(freight_draws, by = c("iteration", "year")) |>
    dplyr::left_join(initial_df, by = "iteration") |>
    dplyr::mutate(
      # scenario-adjusted unit values
      price_t_scn     = price_t * price_mult,
      costs_t_scn     = costs_t * cost_mult,
      freight1_t_scn  = freight1_t_fluffy * freight_mult,
      freight2_t_scn  = freight2_t_compressed * freight_mult,
      
      # quantities
      total_tons          = tons_ha * area_ha,
      total_fluffy_m3     = total_tons * m3_per_ton_fluffy,
      total_compressed_m3 = total_tons * m3_per_ton_comp,
      
      # cash flow
      revenue             = price_t_scn * total_tons,
      variable_cost_total = costs_t_scn * total_tons,
      royalty_total       = royalty_per_t * total_tons,
      restoration_total   = dplyr::if_else(year == final_year,
                                           restoration_cost_per_ha * area_ha,
                                           0),
      
      freight1_total = freight1_t_scn * total_tons,
      freight2_total = freight2_t_scn * total_tons,
      freight_total  = freight1_total + freight2_total,
      
      net_cash            = revenue - variable_cost_total - royalty_total -
        restoration_total - freight_total,
      discount_factor     = (1 + discount_rate)^year,
      discounted_net_cash = net_cash / discount_factor
    )
  
  pv_flows <- all_data |>
    dplyr::group_by(iteration) |>
    dplyr::summarise(
      PV_net_cash = sum(discounted_net_cash),
      .groups = "drop"
    )
  
  NPV_results <- initial_df |>
    dplyr::inner_join(pv_flows, by = "iteration") |>
    dplyr::mutate(
      PV_salvage        = salvage_equipment / ((1 + discount_rate)^final_year),
      NPV_total_project = PV_net_cash + PV_salvage - total_initial_cost,
      NPV_per_ha        = NPV_total_project / area_ha
    )
  
  BEP_results <- NPV_results |>
    dplyr::left_join(pv_emis, by = "iteration") |>
    dplyr::mutate(
      BEP = dplyr::if_else(
        NPV_per_ha > 0,
        NPV_per_ha / PV_tCO2e_per_ha,
        NA_real_
      )
    )
  
  summary_row <- tibble::tibble(
    mean_NPV_per_ha   = mean(NPV_results$NPV_per_ha, na.rm = TRUE),
    sd_NPV_per_ha     = sd(NPV_results$NPV_per_ha, na.rm = TRUE),
    max_NPV_per_ha    = max(NPV_results$NPV_per_ha, na.rm = TRUE),
    p_NPV_positive    = mean(NPV_results$NPV_per_ha > 0, na.rm = TRUE),
    mean_BEP          = mean(BEP_results$BEP, na.rm = TRUE),
    sd_BEP            = sd(BEP_results$BEP, na.rm = TRUE),
    n_BEP             = sum(!is.na(BEP_results$BEP)),
    n_excluded_BEP    = sum(is.na(BEP_results$BEP))
  )
  
  list(
    all_data     = all_data,
    NPV_results  = NPV_results,
    BEP_results  = BEP_results,
    summary_row  = summary_row
  )
}

# ============================================================
# 8) Sensitivity scenarios
# ============================================================

scenario_tbl <- tibble::tibble(
  scenario     = c("Baseline",
                   "Price +15%",
                   "Price -15%",
                   "Freight +15%",
                   "Freight -15%",
                   "Operating cost +15%",
                   "Operating cost -15%"),
  price_mult   = c(1.00, 1.15, 0.85, 1.00, 1.00, 1.00, 1.00),
  freight_mult = c(1.00, 1.00, 1.00, 1.15, 0.85, 1.00, 1.00),
  cost_mult    = c(1.00, 1.00, 1.00, 1.00, 1.00, 1.15, 0.85)
)

scenario_outputs <- vector("list", nrow(scenario_tbl))
summary_list     <- vector("list", nrow(scenario_tbl))

for (i in seq_len(nrow(scenario_tbl))) {
  scn <- scenario_tbl[i, ]
  
  tmp <- run_project_scenario(
    price_mult   = scn$price_mult,
    freight_mult = scn$freight_mult,
    cost_mult    = scn$cost_mult
  )
  
  tmp$NPV_results <- tmp$NPV_results |>
    dplyr::mutate(scenario = scn$scenario, .before = 1)
  
  tmp$BEP_results <- tmp$BEP_results |>
    dplyr::mutate(scenario = scn$scenario, .before = 1)
  
  scenario_outputs[[i]] <- tmp
  
  summary_list[[i]] <- tmp$summary_row |>
    dplyr::mutate(scenario = scn$scenario, .before = 1)
}

NPV_all <- dplyr::bind_rows(lapply(scenario_outputs, function(x) x$NPV_results))
BEP_all <- dplyr::bind_rows(lapply(scenario_outputs, function(x) x$BEP_results))

sensitivity_summary <- dplyr::bind_rows(summary_list)

# Add change relative to baseline
baseline_mean_npv <- sensitivity_summary$mean_NPV_per_ha[
  sensitivity_summary$scenario == "Baseline"
]
baseline_max_npv <- sensitivity_summary$max_NPV_per_ha[
  sensitivity_summary$scenario == "Baseline"
]
baseline_mean_bep <- sensitivity_summary$mean_BEP[
  sensitivity_summary$scenario == "Baseline"
]

sensitivity_summary <- sensitivity_summary |>
  dplyr::mutate(
    delta_mean_NPV_per_ha = mean_NPV_per_ha - baseline_mean_npv,
    pct_change_mean_NPV   = 100 * (mean_NPV_per_ha / baseline_mean_npv - 1),
    
    delta_max_NPV_per_ha  = max_NPV_per_ha - baseline_max_npv,
    pct_change_max_NPV    = 100 * (max_NPV_per_ha / baseline_max_npv - 1),
    
    delta_mean_BEP        = mean_BEP - baseline_mean_bep,
    pct_change_mean_BEP   = 100 * (mean_BEP / baseline_mean_bep - 1)
  )

cat("\n--- Sensitivity summary: NPV and BEP ---\n")
print(sensitivity_summary)

# ============================================================
# 9) Baseline outputs
# ============================================================

baseline_out <- scenario_outputs[[which(scenario_tbl$scenario == "Baseline")]]

cat("\n--- Baseline NPV (includes negative values) ---\n")
print(summary(baseline_out$NPV_results$NPV_per_ha))
cat("Mean:", mean(baseline_out$NPV_results$NPV_per_ha, na.rm = TRUE), "\n")
cat("SD:",   sd(baseline_out$NPV_results$NPV_per_ha,   na.rm = TRUE), "\n")
cat("Max:",  max(baseline_out$NPV_results$NPV_per_ha,  na.rm = TRUE), "\n")
cat("P(NPV>0):", mean(baseline_out$NPV_results$NPV_per_ha > 0, na.rm = TRUE), "\n")

cat("\n--- Baseline break-even carbon price ($/tCO2e), only for simulations with positive NPV ---\n")
print(summary(baseline_out$BEP_results$BEP))
cat("Mean BEP:", mean(baseline_out$BEP_results$BEP, na.rm = TRUE), "\n")
cat("SD BEP:",   sd(baseline_out$BEP_results$BEP,   na.rm = TRUE), "\n")
cat("Number of BEP observations:", sum(!is.na(baseline_out$BEP_results$BEP)), "\n")
cat("Excluded negative-NPV observations:", sum(is.na(baseline_out$BEP_results$BEP)), "\n")

# ============================================================
# 10) Plots
# ============================================================

hist(
  baseline_out$NPV_results$NPV_per_ha,
  breaks = 30,
  main = "Baseline distribution of NPV per hectare",
  xlab = "NPV per ha ($)"
)

hist(
  baseline_out$BEP_results$BEP,
  breaks = 30,
  main = "Baseline distribution of break-even carbon price",
  xlab = "BEP ($/tCO2e)"
)

boxplot(
  NPV_per_ha ~ scenario,
  data = NPV_all,
  las = 2,
  main = "NPV per hectare by sensitivity scenario",
  ylab = "NPV per ha ($)"
)

boxplot(
  BEP ~ scenario,
  data = BEP_all,
  las = 2,
  main = "Break-even carbon price by sensitivity scenario",
  ylab = "BEP ($/tCO2e)"
)

# ============================================================
# 11) Diagnostics for emissions
# ============================================================

check_components <- emissions_df |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    mean_onsite  = mean(onsite_tCO2e,  na.rm = TRUE),
    mean_offsite = mean(offsite_tCO2e, na.rm = TRUE),
    mean_total   = mean(total_tCO2e,   na.rm = TRUE),
    .groups = "drop"
  )

print(
  check_components |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ sprintf("%.2f", .x))
    )
)

mass_balance <- emissions_df |>
  dplyr::group_by(iteration) |>
  dplyr::summarise(
    T = max(year),
    sum_m_tC   = sum(m_tC),
    sum_E_1T   = sum(offsite_tC),
    tail_undisc = offsite_tC[year == T][1] * (1 - k_decay) / k_decay,
    sum_E_inf   = sum_E_1T + tail_undisc,
    diff        = sum_E_inf - sum_m_tC,
    .groups = "drop"
  )

cat("\n--- Mass balance check (tC): diff should be ~0 ---\n")
print(summary(mass_balance$diff))

tail_share <- pv_emis |>
  dplyr::mutate(
    tail_share = PV_offsite_tail / (PV_offsite_1T + PV_offsite_tail)
  )

cat("\n--- PV off-site tail share (tail / total off-site PV) ---\n")
print(summary(tail_share$tail_share))