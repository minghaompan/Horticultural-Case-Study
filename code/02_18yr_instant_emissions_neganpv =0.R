# ============================================================
# 02_18yr_instant_emissions_negative_npv=0
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(triangle)
  library(fitdistrplus)
  library(actuar)
})

set.seed(123)


# ============================================================
# 1. Global settings
# ============================================================

n_iter  <- 10000
n_years <- 18

project_area_ha <- 100
discount_rate   <- 0.07
depr_rate       <- 0.08
final_year      <- n_years

input_price_path <- "data/peat_price_alberta.csv"

output_dir      <- "peat_extraction_18yr_instant_emissions_negative_npv=0"
output_data_dir <- file.path(output_dir, "data")

dir.create(dirname(input_price_path), showWarnings = FALSE, recursive = TRUE)
dir.create(output_data_dir, showWarnings = FALSE, recursive = TRUE)

sim_grid <- tidyr::crossing(
  iteration = seq_len(n_iter),
  year      = seq_len(n_years)
)


# ============================================================
# 2. Helper function for summary tables
# ============================================================

make_summary <- function(data, vars, dataset_name) {
  data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        list(
          n    = ~ sum(is.finite(.x)),
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ stats::sd(.x, na.rm = TRUE),
          min  = ~ min(.x, na.rm = TRUE),
          p10  = ~ as.numeric(stats::quantile(.x, 0.10, na.rm = TRUE)),
          p50  = ~ as.numeric(stats::quantile(.x, 0.50, na.rm = TRUE)),
          p90  = ~ as.numeric(stats::quantile(.x, 0.90, na.rm = TRUE)),
          max  = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.+)_(n|mean|sd|min|p10|p50|p90|max)"
    ) %>%
    dplyr::mutate(dataset = dataset_name, .before = 1)
}


# ============================================================
# 3. Unit conversion assumptions
# ============================================================

ft3_to_m3 <- 0.0283168
m3_to_cy  <- 1.30795

# Loose peat assumptions
bale_ft3_loose     <- 6
compression_factor <- 2
bales_per_ton      <- 26

m3_per_bale_loose <- bale_ft3_loose * compression_factor * ft3_to_m3
m3_per_ton_loose  <- m3_per_bale_loose * bales_per_ton

# Freight conversion factor
m3_per_ton_bog_to_plant <- m3_per_ton_loose


# ============================================================
# 4. Operating cost simulation, $/t
# ============================================================

op_min  <- 130.97
op_mean <- 171.79
op_max  <- 206.03

# Calibrate mode so triangular mean equals historical mean
op_mode <- 3 * op_mean - op_min - op_max

cat("\n--- Operating cost distribution, $/t ---\n")
print(
  data.frame(
    Min  = op_min,
    Mode = op_mode,
    Max  = op_max,
    Mean = op_mean
  )
)

# Simulate operating costs
cost_sim <- sim_grid %>%
  dplyr::mutate(
    cost_t = triangle::rtriangle(
      dplyr::n(),
      a = op_min,
      b = op_max,
      c = op_mode
    )
  )

# Summary
cost_summary <- make_summary(
  cost_sim,
  vars = "cost_t",
  dataset_name = "simulated_costs"
)

# Export
utils::write.csv(
  cost_sim,
  file.path(output_data_dir, "simulated_costs.csv"),
  row.names = FALSE
)

# ============================================================
# 5. Peat yield simulation
# ============================================================

depth_min <- 0.05
depth_c   <- 0.086
depth_max <- 0.14

area_per_ha_m2 <- 10000

yield_sim <- sim_grid %>%
  dplyr::mutate(
    depth_m = triangle::rtriangle(
      n = dplyr::n(),
      a = depth_min,
      b = depth_max,
      c = depth_c
    ),
    yield_m3_ha = depth_m * area_per_ha_m2,
    yield_t_ha  = yield_m3_ha / m3_per_ton_loose
  )

yield_summary <- make_summary(
  data = yield_sim,
  vars = c("depth_m", "yield_m3_ha", "yield_t_ha"),
  dataset_name = "simulated_peat_yield"
)

utils::write.csv(
  yield_sim,
  file.path(output_data_dir, "simulated_peat_yield.csv"),
  row.names = FALSE
)


# ============================================================
# 6. Peat price simulation, $/t
# Log-logistic distribution
# Historical data: 1996 onward
# ============================================================


# ------------------------------------------------------------
# 6.1 Load historical peat-price data
# ------------------------------------------------------------

peat_data <- read.csv(input_price_path) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::filter(
    years >= 1996,
    is.finite(prices),
    prices > 0
  ) %>%
  dplyr::arrange(years)

peat_price <- peat_data$prices


# ------------------------------------------------------------
# 6.2 Fit Log-logistic distribution
# ------------------------------------------------------------

fit_price <- fitdistrplus::fitdist(
  peat_price,
  "llogis"
)

price_shape <- unname(
  fit_price$estimate["shape"]
)

price_scale <- unname(
  fit_price$estimate["scale"]
)

cat("\n--- Log-logistic peat-price parameters ---\n")
cat("Shape:", price_shape, "\n")
cat("Scale:", price_scale, "\n")


# ------------------------------------------------------------
# 6.3 Simulate annual peat prices
# ------------------------------------------------------------

price_sim <- sim_grid %>%
  dplyr::mutate(
    price_t = actuar::rllogis(
      n = dplyr::n(),
      shape = price_shape,
      scale = price_scale
    )
  )


# ------------------------------------------------------------
# 6.4 Summary
# ------------------------------------------------------------

price_summary <- make_summary(
  data = price_sim,
  vars = "price_t",
  dataset_name = "simulated_peat_price"
)

cat("\n--- Simulated peat price, $/t ---\n")
print(summary(price_sim$price_t))


# ------------------------------------------------------------
# 6.5 Simulated prices relative to historical range
# ------------------------------------------------------------

hist_min <- min(peat_price)
hist_max <- max(peat_price)

price_range_check <- tibble::tibble(
  category = c(
    "Within historical range",
    "Below historical minimum",
    "Above historical maximum"
  ),
  
  count = c(
    sum(
      price_sim$price_t >= hist_min &
        price_sim$price_t <= hist_max
    ),
    
    sum(
      price_sim$price_t < hist_min
    ),
    
    sum(
      price_sim$price_t > hist_max
    )
  )
) %>%
  dplyr::mutate(
    percent = 100 * count / nrow(price_sim)
  )

cat("\n--- Simulated prices relative to historical range ---\n")
print(price_range_check)


# ------------------------------------------------------------
# 6.6 Export
# ------------------------------------------------------------

utils::write.csv(
  price_sim,
  file.path(
    output_data_dir,
    "simulated_peat_price.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  price_range_check,
  file.path(
    output_data_dir,
    "peat_price_range_check.csv"
  ),
  row.names = FALSE
)
# ============================================================
# 7. Build base simulation panel
# ============================================================
base_panel <- price_sim %>%
  dplyr::inner_join(
    cost_sim %>%
      dplyr::select(iteration, year, cost_t),
    by = c("iteration", "year")
  ) %>%
  dplyr::inner_join(
    yield_sim %>%
      dplyr::select(iteration, year, yield_t_ha),
    by = c("iteration", "year")
  ) %>%
  dplyr::rename(
    costs_t = cost_t,
    tons_ha = yield_t_ha
  ) %>%
  dplyr::arrange(iteration, year)

base_panel_summary <- make_summary(
  data = base_panel,
  vars = c("price_t", "costs_t", "tons_ha"),
  dataset_name = "base_panel"
)

iter_ids <- sort(unique(base_panel$iteration))
n_iter_actual <- length(iter_ids)


# ============================================================
# 8. Initial costs, royalty, restoration, and salvage
# ============================================================

salvage_factor <- (1 - depr_rate)^final_year

initial_df <- tibble::tibble(
  iteration = iter_ids,
  initial_cost_equipment = stats::runif(
    n = n_iter_actual,
    min = 1843694,
    max = 2703888
  ),
  env_app_cost = stats::runif(
    n = n_iter_actual,
    min = 300000,
    max = 1000000
  )
) %>%
  dplyr::mutate(
    total_initial_cost = initial_cost_equipment + env_app_cost,
    salvage_equipment  = initial_cost_equipment * salvage_factor
  )

initial_cost_summary <- make_summary(
  data = initial_df,
  vars = c(
    "initial_cost_equipment",
    "env_app_cost",
    "total_initial_cost",
    "salvage_equipment"
  ),
  dataset_name = "initial_costs"
)

royalty_per_cy <- 0.11
royalty_per_m3 <- royalty_per_cy * m3_to_cy
royalty_per_t  <- royalty_per_m3 * m3_per_ton_loose

restoration_cost_per_ha <- 3485


# ============================================================
# 9. Freight simulation: bog to plant only, $/t
#    Area-weighted triangular distribution
# ============================================================

# Freight assumptions
leg1_rate_per_km <- 2.00
leg1_cap_m3_TL   <- 125

# Premier Tech Alberta harvesting sites
# 404 km = midpoint of 379-429 km for the combined
# Babette/Beaver, Ferguson, Paxson East operations
site_area_ha <- c(
  1928.07,  # Babette/Beaver, Ferguson, Paxson East
  217.72,   # Valleyview
  860.48,   # Pembina
  102.00    # Bucklake
)

site_dist_km <- c(
  404,
  505,
  278,
  207
)

# ------------------------------------------------------------
# Area-weighted mean distance
# ------------------------------------------------------------

weighted_dist_km <- weighted.mean(
  site_dist_km,
  site_area_ha
)

# Triangular distance parameters
dist_min  <- min(site_dist_km)
dist_max  <- max(site_dist_km)

# Calibrate mode so triangular mean equals
# the area-weighted mean:
# mean = (min + mode + max) / 3
dist_mode <- 3 * weighted_dist_km -
  dist_min -
  dist_max

# ------------------------------------------------------------
# Convert distance parameters to freight cost, $/t
# ------------------------------------------------------------

freight_min <- (
  leg1_rate_per_km *
    dist_min /
    leg1_cap_m3_TL
) * m3_per_ton_bog_to_plant

freight_mode <- (
  leg1_rate_per_km *
    dist_mode /
    leg1_cap_m3_TL
) * m3_per_ton_bog_to_plant

freight_max <- (
  leg1_rate_per_km *
    dist_max /
    leg1_cap_m3_TL
) * m3_per_ton_bog_to_plant

freight_mean <- (
  freight_min +
    freight_mode +
    freight_max
) / 3

# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------

freight_range_summary <- tibble::tibble(
  component          = "Bog to plant",
  weighted_dist_km   = round(weighted_dist_km, 2),
  min_cost_t         = round(freight_min, 2),
  mode_cost_t        = round(freight_mode, 2),
  mean_cost_t        = round(freight_mean, 2),
  max_cost_t         = round(freight_max, 2)
)

cat("\n--- Bog-to-plant freight distribution, $/t ---\n")
print(freight_range_summary)

utils::write.csv(
  freight_range_summary,
  file.path(
    output_data_dir,
    "bog_to_plant_freight_range_summary.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# Monte Carlo simulation
# ------------------------------------------------------------

freight_draws <- sim_grid %>%
  dplyr::mutate(
    freight_t = triangle::rtriangle(
      n = dplyr::n(),
      a = freight_min,
      b = freight_max,
      c = freight_mode
    )
  )

freight_summary <- make_summary(
  data = freight_draws,
  vars = "freight_t",
  dataset_name = "simulated_bog_to_plant_freight"
)

utils::write.csv(
  freight_draws,
  file.path(
    output_data_dir,
    "simulated_bog_to_plant_freight.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 10. Carbon emissions settings
# ============================================================

# Carbon content of dry peat mass
C_frac_dry <- 0.46

# Moisture adjustment: convert wet/as-sold tons to dry tons
moisture_frac <- 0.50
dry_frac      <- 1 - moisture_frac

# On-site emission factors, t CO2-C/ha/year.
# Both are converted to t CO2e below using 44/12.
EF_onsite_low_tC_ha_yr  <- 1.4
EF_onsite_high_tC_ha_yr <- 3.1

# Baseline value used only for stand-alone diagnostics.
EF_onsite_tC_ha_yr <- EF_onsite_low_tC_ha_yr


# ============================================================
# 11. Emissions functions
# ============================================================

calc_annual_emissions <- function(data, EF_onsite_tC_ha_yr_scn) {
  data %>%
    dplyr::group_by(iteration) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      area_ha = project_area_ha,
      m_tC = total_tons * dry_frac * C_frac_dry,
      # Instant off-site emissions: all extracted peat carbon is assigned
      # to the harvest/use year.
      offsite_tC = m_tC,
      offsite_tCO2e = offsite_tC * (44 / 12),
      onsite_tCO2e  = EF_onsite_tC_ha_yr_scn * project_area_ha * (44 / 12),
      total_tCO2e = onsite_tCO2e + offsite_tCO2e
    ) %>%
    dplyr::ungroup()
}

calc_emissions_for_BEP <- function(emissions_data, discount_rate_scn) {
  emissions_data %>%
    dplyr::group_by(iteration) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::summarise(
      T       = max(year),
      area_ha = dplyr::first(area_ha),
      onsite_tCO2e_1T  = sum(onsite_tCO2e, na.rm = TRUE),
      offsite_tCO2e_1T = sum(offsite_tCO2e, na.rm = TRUE),
      emissions_for_BEP_tCO2e_total = onsite_tCO2e_1T + offsite_tCO2e_1T,
      emissions_for_BEP_tCO2e_per_ha =
        emissions_for_BEP_tCO2e_total / area_ha,
      .groups = "drop"
    )
}

# Stand-alone baseline emissions diagnostics use the low on-site EF.
emissions_df <- base_panel %>%
  dplyr::mutate(total_tons = tons_ha * project_area_ha) %>%
  calc_annual_emissions(EF_onsite_tC_ha_yr_scn = EF_onsite_low_tC_ha_yr)

emissions_summary <- make_summary(
  data = emissions_df,
  vars = c(
    "total_tons",
    "m_tC",
    "offsite_tC",
    "offsite_tCO2e",
    "onsite_tCO2e",
    "total_tCO2e"
  ),
  dataset_name = "simulated_emissions_baseline_low_EF"
)


# ============================================================
# 12. Undiscounted emissions for BEP
# ============================================================

bep_emis <- calc_emissions_for_BEP(
  emissions_data = emissions_df,
  discount_rate_scn = discount_rate
)

emissions_for_BEP_summary <- make_summary(
  data = bep_emis,
  vars = c('onsite_tCO2e_1T', 'offsite_tCO2e_1T', 'emissions_for_BEP_tCO2e_total', 'emissions_for_BEP_tCO2e_per_ha'),
  dataset_name = "undiscounted_emissions_for_BEP"
)

cat("\n--- Undiscounted emissions for BEP, tCO2e/ha ---\n")
print(summary(bep_emis$emissions_for_BEP_tCO2e_per_ha))


# ============================================================
# 13. Scenario calculation function
# ============================================================

run_project_scenario <- function(price_mult = 1,
                                 yield_mult = 1,
                                 freight_mult = 1,
                                 cost_mult = 1,
                                 discount_rate_scn = discount_rate,
                                 EF_onsite_tC_ha_yr_scn = EF_onsite_low_tC_ha_yr) {
  
  annual_cashflow <- base_panel %>%
    dplyr::inner_join(
      freight_draws,
      by = c("iteration", "year")
    ) %>%
    dplyr::left_join(
      initial_df,
      by = "iteration"
    ) %>%
    dplyr::mutate(
      price_t_scn = price_t * price_mult,
      tons_ha_scn = tons_ha * yield_mult,
      costs_t_scn = costs_t * cost_mult,
      
      freight_t_scn = freight_t * freight_mult,
      
      total_tons = tons_ha_scn * project_area_ha,
      
      revenue             = price_t_scn * total_tons,
      variable_cost_total = costs_t_scn * total_tons,
      royalty_total       = royalty_per_t * total_tons,
      freight_total       = freight_t_scn * total_tons,
      
      restoration_total = as.numeric(year == final_year) *
        restoration_cost_per_ha *
        project_area_ha,
      
      net_cash = revenue -
        variable_cost_total -
        royalty_total -
        freight_total -
        restoration_total,
      
      discounted_net_cash = net_cash / ((1 + discount_rate_scn)^year)
    )
  
  pv_flows <- annual_cashflow %>%
    dplyr::group_by(iteration) %>%
    dplyr::summarise(
      PV_net_cash = sum(discounted_net_cash, na.rm = TRUE),
      .groups = "drop"
    )
  
  npv_results <- initial_df %>%
    dplyr::inner_join(
      pv_flows,
      by = "iteration"
    ) %>%
    dplyr::mutate(
      PV_salvage = salvage_equipment / ((1 + discount_rate_scn)^final_year),
      NPV_total_project = PV_net_cash + PV_salvage - total_initial_cost,
      NPV_per_ha = NPV_total_project / project_area_ha
    )
  
  scenario_emissions <- annual_cashflow %>%
    calc_annual_emissions(EF_onsite_tC_ha_yr_scn = EF_onsite_tC_ha_yr_scn)
  
  emissions_for_BEP <- calc_emissions_for_BEP(
    emissions_data = scenario_emissions,
    discount_rate_scn = discount_rate_scn
  )
  
  bep_results <- npv_results %>%
    dplyr::left_join(
      emissions_for_BEP,
      by = "iteration"
    ) %>%
    dplyr::mutate(
      # Keep all NPV draws in the simulation.
      # For BEP only, set negative NPV values to zero.
      NPV_used_for_BEP_per_ha = pmax(NPV_per_ha, 0),
      BEP = NPV_used_for_BEP_per_ha / emissions_for_BEP_tCO2e_per_ha
    )
  
  summary_row <- tibble::tibble(
    emissions_accounting = "Instant emissions",
    project_horizon_years = n_years,
    discount_rate_scn = discount_rate_scn,
    EF_onsite_tC_ha_yr = EF_onsite_tC_ha_yr_scn,
    
    mean_NPV_per_ha = mean(npv_results$NPV_per_ha, na.rm = TRUE),
    sd_NPV_per_ha   = stats::sd(npv_results$NPV_per_ha, na.rm = TRUE),
    min_NPV_per_ha  = min(npv_results$NPV_per_ha, na.rm = TRUE),
    p10_NPV_per_ha  = as.numeric(stats::quantile(npv_results$NPV_per_ha, 0.10, na.rm = TRUE)),
    p50_NPV_per_ha  = as.numeric(stats::quantile(npv_results$NPV_per_ha, 0.50, na.rm = TRUE)),
    p90_NPV_per_ha  = as.numeric(stats::quantile(npv_results$NPV_per_ha, 0.90, na.rm = TRUE)),
    max_NPV_per_ha  = max(npv_results$NPV_per_ha, na.rm = TRUE),
    p_NPV_positive  = mean(npv_results$NPV_per_ha > 0, na.rm = TRUE),
    
    mean_emissions_for_BEP_tCO2e_per_ha =
      mean(emissions_for_BEP$emissions_for_BEP_tCO2e_per_ha, na.rm = TRUE),
    sd_emissions_for_BEP_tCO2e_per_ha =
      stats::sd(emissions_for_BEP$emissions_for_BEP_tCO2e_per_ha, na.rm = TRUE),
    min_emissions_for_BEP_tCO2e_per_ha =
      min(emissions_for_BEP$emissions_for_BEP_tCO2e_per_ha, na.rm = TRUE),
    p10_emissions_for_BEP_tCO2e_per_ha =
      as.numeric(stats::quantile(emissions_for_BEP$emissions_for_BEP_tCO2e_per_ha, 0.10, na.rm = TRUE)),
    p50_emissions_for_BEP_tCO2e_per_ha =
      as.numeric(stats::quantile(emissions_for_BEP$emissions_for_BEP_tCO2e_per_ha, 0.50, na.rm = TRUE)),
    p90_emissions_for_BEP_tCO2e_per_ha =
      as.numeric(stats::quantile(emissions_for_BEP$emissions_for_BEP_tCO2e_per_ha, 0.90, na.rm = TRUE)),
    max_emissions_for_BEP_tCO2e_per_ha =
      max(emissions_for_BEP$emissions_for_BEP_tCO2e_per_ha, na.rm = TRUE),
    
    mean_NPV_used_for_BEP_per_ha = mean(bep_results$NPV_used_for_BEP_per_ha, na.rm = TRUE),
    sd_NPV_used_for_BEP_per_ha   = stats::sd(bep_results$NPV_used_for_BEP_per_ha, na.rm = TRUE),
    min_NPV_used_for_BEP_per_ha  = min(bep_results$NPV_used_for_BEP_per_ha, na.rm = TRUE),
    p10_NPV_used_for_BEP_per_ha  = as.numeric(stats::quantile(bep_results$NPV_used_for_BEP_per_ha, 0.10, na.rm = TRUE)),
    p50_NPV_used_for_BEP_per_ha  = as.numeric(stats::quantile(bep_results$NPV_used_for_BEP_per_ha, 0.50, na.rm = TRUE)),
    p90_NPV_used_for_BEP_per_ha  = as.numeric(stats::quantile(bep_results$NPV_used_for_BEP_per_ha, 0.90, na.rm = TRUE)),
    max_NPV_used_for_BEP_per_ha  = max(bep_results$NPV_used_for_BEP_per_ha, na.rm = TRUE),
    
    mean_BEP = mean(bep_results$BEP, na.rm = TRUE),
    sd_BEP   = stats::sd(bep_results$BEP, na.rm = TRUE),
    min_BEP  = min(bep_results$BEP, na.rm = TRUE),
    p10_BEP  = as.numeric(stats::quantile(bep_results$BEP, 0.10, na.rm = TRUE)),
    p50_BEP  = as.numeric(stats::quantile(bep_results$BEP, 0.50, na.rm = TRUE)),
    p90_BEP  = as.numeric(stats::quantile(bep_results$BEP, 0.90, na.rm = TRUE)),
    max_BEP  = max(bep_results$BEP, na.rm = TRUE),
    
    n_BEP          = sum(!is.na(bep_results$BEP)),
    n_excluded_BEP = sum(is.na(bep_results$BEP))
  )
  
  list(
    annual_cashflow   = annual_cashflow,
    scenario_emissions = scenario_emissions,
    emissions_for_BEP = emissions_for_BEP,
    NPV_results       = npv_results,
    BEP_results       = bep_results,
    summary_row       = summary_row
  )
}


# ============================================================
# 14. Scenario tables
# ============================================================

# Economic scenarios. These change only economic or production assumptions.
scenario_tbl <- tibble::tibble(
  scenario = c(
    "Baseline",
    "Price +15%",
    "Price -15%",
    "Yield +15%",
    "Yield -15%",
    "Freight +15%",
    "Freight -15%",
    "Operating cost +15%",
    "Operating cost -15%",
    "Discount rate 3%",
    "Discount rate 10%"
  ),
  price_mult = c(
    1.00,
    1.15,
    0.85,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00
  ),
  yield_mult = c(
    1.00,
    1.00,
    1.00,
    1.15,
    0.85,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00
  ),
  freight_mult = c(
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.15,
    0.85,
    1.00,
    1.00,
    1.00,
    1.00
  ),
  cost_mult = c(
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.00,
    1.15,
    0.85,
    1.00,
    1.00
  ),
  discount_rate_scn = c(
    discount_rate,
    discount_rate,
    discount_rate,
    discount_rate,
    discount_rate,
    discount_rate,
    discount_rate,
    discount_rate,
    discount_rate,
    0.03,
    0.10
  )
)

# Carbon-accounting cases for on-site emissions.
# These are not economic scenarios. Each economic scenario is run once
# with 1.4 t CO2-C/ha/year and once with 3.1 t CO2-C/ha/year.
onsite_ef_tbl <- tibble::tibble(
  onsite_EF_case = c(
    "On-site EF 1.4 t CO2-C/ha/yr",
    "On-site EF 3.1 t CO2-C/ha/yr"
  ),
  EF_onsite_tC_ha_yr_scn = c(
    EF_onsite_low_tC_ha_yr,
    EF_onsite_high_tC_ha_yr
  )
)

scenario_grid <- tidyr::crossing(
  onsite_ef_tbl,
  scenario_tbl
)


# ============================================================
# 15. Run all economic scenarios under both on-site emission factors
# ============================================================

scenario_outputs <- vector("list", nrow(scenario_grid))
summary_list     <- vector("list", nrow(scenario_grid))

for (i in seq_len(nrow(scenario_grid))) {
  
  scn <- scenario_grid[i, ]
  
  scenario_result <- run_project_scenario(
    price_mult             = scn$price_mult,
    yield_mult             = scn$yield_mult,
    freight_mult           = scn$freight_mult,
    cost_mult              = scn$cost_mult,
    discount_rate_scn      = scn$discount_rate_scn,
    EF_onsite_tC_ha_yr_scn = scn$EF_onsite_tC_ha_yr_scn
  )
  
  add_scenario_cols <- function(data) {
    data %>%
      dplyr::mutate(
        emissions_accounting = "Instant emissions",
        project_horizon_years = n_years,
        onsite_EF_case = scn$onsite_EF_case,
        EF_onsite_tC_ha_yr = scn$EF_onsite_tC_ha_yr_scn,
        scenario = scn$scenario,
        .before = 1
      )
  }
  
  scenario_result$NPV_results <- add_scenario_cols(scenario_result$NPV_results)
  scenario_result$BEP_results <- add_scenario_cols(scenario_result$BEP_results)
  scenario_result$annual_cashflow <- add_scenario_cols(scenario_result$annual_cashflow)
  scenario_result$scenario_emissions <- add_scenario_cols(scenario_result$scenario_emissions)
  scenario_result$emissions_for_BEP <- add_scenario_cols(scenario_result$emissions_for_BEP)
  
  scenario_outputs[[i]] <- scenario_result
  
  summary_list[[i]] <- scenario_result$summary_row %>%
    dplyr::mutate(
      onsite_EF_case = scn$onsite_EF_case,
      scenario = scn$scenario,
      .before = 1
    )
}

NPV_all <- dplyr::bind_rows(
  lapply(scenario_outputs, function(x) x$NPV_results)
)

BEP_all <- dplyr::bind_rows(
  lapply(scenario_outputs, function(x) x$BEP_results)
)

cashflow_all <- dplyr::bind_rows(
  lapply(scenario_outputs, function(x) x$annual_cashflow)
)

emissions_all <- dplyr::bind_rows(
  lapply(scenario_outputs, function(x) x$scenario_emissions)
)

emissions_for_BEP_all <- dplyr::bind_rows(
  lapply(scenario_outputs, function(x) x$emissions_for_BEP)
)

sensitivity_summary <- dplyr::bind_rows(summary_list)


# ============================================================
# 16. Sensitivity summary relative to baseline within each on-site EF case
# ============================================================

baseline_summary <- sensitivity_summary %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::select(
    onsite_EF_case,
    baseline_mean_NPV_per_ha = mean_NPV_per_ha,
    baseline_max_NPV_per_ha = max_NPV_per_ha,
    baseline_mean_NPV_used_for_BEP_per_ha = mean_NPV_used_for_BEP_per_ha,
    baseline_mean_emissions_for_BEP_tCO2e_per_ha = mean_emissions_for_BEP_tCO2e_per_ha,
    baseline_mean_BEP = mean_BEP
  )

sensitivity_summary <- sensitivity_summary %>%
  dplyr::left_join(
    baseline_summary,
    by = "onsite_EF_case"
  ) %>%
  dplyr::mutate(
    delta_mean_NPV_per_ha = mean_NPV_per_ha - baseline_mean_NPV_per_ha,
    pct_change_mean_NPV   = 100 * (mean_NPV_per_ha / baseline_mean_NPV_per_ha - 1),
    
    delta_max_NPV_per_ha = max_NPV_per_ha - baseline_max_NPV_per_ha,
    pct_change_max_NPV   = 100 * (max_NPV_per_ha / baseline_max_NPV_per_ha - 1),
    
    delta_mean_NPV_used_for_BEP_per_ha =
      mean_NPV_used_for_BEP_per_ha - baseline_mean_NPV_used_for_BEP_per_ha,
    pct_change_mean_NPV_used_for_BEP =
      100 * (mean_NPV_used_for_BEP_per_ha / baseline_mean_NPV_used_for_BEP_per_ha - 1),
    
    delta_mean_emissions_for_BEP_tCO2e_per_ha =
      mean_emissions_for_BEP_tCO2e_per_ha - baseline_mean_emissions_for_BEP_tCO2e_per_ha,
    pct_change_mean_emissions_for_BEP =
      100 * (mean_emissions_for_BEP_tCO2e_per_ha / baseline_mean_emissions_for_BEP_tCO2e_per_ha - 1),
    
    delta_mean_BEP      = mean_BEP - baseline_mean_BEP,
    pct_change_mean_BEP = 100 * (mean_BEP / baseline_mean_BEP - 1)
  )

cat("\n--- Sensitivity summary, by on-site EF case ---\n")
print(sensitivity_summary)


# ============================================================
# 17. Combined summary for simulated datasets
# ============================================================

simulation_summary <- dplyr::bind_rows(
  cost_summary,
  yield_summary,
  price_summary,
  base_panel_summary,
  initial_cost_summary,
  freight_summary,
  emissions_summary,
  emissions_for_BEP_summary
) %>%
  dplyr::mutate(
    dplyr::across(
      c(mean, sd, min, p10, p50, p90, max),
      ~ round(.x, 4)
    )
  )

cat("\n--- Summary for each simulated dataset ---\n")
print(simulation_summary)


# ============================================================
# 18. Save main output files
# ============================================================

utils::write.csv(
  NPV_all,
  file.path(output_data_dir, "npv_all_scenarios.csv"),
  row.names = FALSE
)

utils::write.csv(
  BEP_all,
  file.path(output_data_dir, "bep_all_scenarios.csv"),
  row.names = FALSE
)

utils::write.csv(
  cashflow_all,
  file.path(output_data_dir, "cashflow_all_scenarios.csv"),
  row.names = FALSE
)

utils::write.csv(
  emissions_all,
  file.path(output_data_dir, "emissions_all_scenarios.csv"),
  row.names = FALSE
)

utils::write.csv(
  emissions_for_BEP_all,
  file.path(output_data_dir, "emissions_for_BEP_all_scenarios.csv"),
  row.names = FALSE
)

utils::write.csv(
  sensitivity_summary,
  file.path(output_data_dir, "sensitivity_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  simulation_summary,
  file.path(output_data_dir, "simulated_dataset_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  emissions_df,
  file.path(output_data_dir, "annual_emissions_onsite_offsite_baseline_low_EF.csv"),
  row.names = FALSE
)

utils::write.csv(
  bep_emis,
  file.path(output_data_dir, "emissions_for_BEP_by_iteration_baseline_low_EF.csv"),
  row.names = FALSE
)


# ============================================================
# 19. Baseline results under both on-site emission factors
# ============================================================

baseline_results_by_EF <- sensitivity_summary %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::select(
    emissions_accounting,
    project_horizon_years,
    onsite_EF_case,
    EF_onsite_tC_ha_yr,
    mean_NPV_per_ha,
    sd_NPV_per_ha,
    min_NPV_per_ha,
    p50_NPV_per_ha,
    max_NPV_per_ha,
    p_NPV_positive,
    mean_emissions_for_BEP_tCO2e_per_ha,
    sd_emissions_for_BEP_tCO2e_per_ha,
    p50_emissions_for_BEP_tCO2e_per_ha,
    mean_BEP,
    sd_BEP,
    p50_BEP,
    n_BEP,
    n_excluded_BEP
  )

cat("\n--- Baseline summary under both on-site EF cases ---\n")
print(baseline_results_by_EF)

utils::write.csv(
  baseline_results_by_EF,
  file.path(output_data_dir, "baseline_results_by_onsite_EF.csv"),
  row.names = FALSE
)


# ============================================================
# 20. Emissions diagnostics under both on-site emission factors
# ============================================================

baseline_emissions <- emissions_all %>%
  dplyr::filter(scenario == "Baseline")

emissions_by_year <- baseline_emissions %>%
  dplyr::group_by(onsite_EF_case, EF_onsite_tC_ha_yr, year) %>%
  dplyr::summarise(
    mean_onsite_tCO2e  = mean(onsite_tCO2e, na.rm = TRUE),
    mean_offsite_tCO2e = mean(offsite_tCO2e, na.rm = TRUE),
    mean_total_tCO2e   = mean(total_tCO2e, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Annual emissions components for baseline, by on-site EF case ---\n")
print(emissions_by_year)

utils::write.csv(
  emissions_by_year,
  file.path(output_data_dir, "annual_emissions_summary_by_year_by_onsite_EF.csv"),
  row.names = FALSE
)

mass_balance <- baseline_emissions %>%
  dplyr::group_by(onsite_EF_case, EF_onsite_tC_ha_yr, iteration) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::summarise(
    T = max(year),
    sum_m_tC = sum(m_tC, na.rm = TRUE),
    sum_offsite_tC = sum(offsite_tC, na.rm = TRUE),
    diff = sum_offsite_tC - sum_m_tC,
    .groups = "drop"
  )

cat("\n--- Instant off-site emissions check: diff should be 0 ---\n")
print(summary(mass_balance$diff))

utils::write.csv(
  mass_balance,
  file.path(output_data_dir, "emissions_mass_balance_check_by_onsite_EF.csv"),
  row.names = FALSE
)
