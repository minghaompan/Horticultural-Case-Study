# ============================================================
# Peat extraction Monte Carlo simulation
# Operating cost, yield, price, freight, NPV, emissions, and BEP
# 10,000 simulations x 18 years
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

output_dir      <- "peat_extraction_18yr_instant_emissions"
output_data_dir <- file.path(output_dir, "data")

dir.create(dirname(input_price_path), showWarnings = FALSE, recursive = TRUE)
dir.create(output_data_dir, showWarnings = FALSE, recursive = TRUE)

sim_grid <- tidyr::crossing(
  iteration = seq_len(n_iter),
  year      = seq_len(n_years)
)


# ============================================================
# 2. Unit conversion assumptions
# ============================================================

ft3_to_m3 <- 0.0283168
m3_to_cy  <- 1.30795

# Loose peat assumptions
bale_ft3_loose     <- 6
compression_factor <- 2
bales_per_ton      <- 26

m3_per_bale_loose <- bale_ft3_loose * compression_factor * ft3_to_m3
m3_per_ton_loose  <- m3_per_bale_loose * bales_per_ton

# Compressed peat assumptions
m3_per_ton_comp <- m3_per_ton_loose / compression_factor

# Freight conversion factors
m3_per_ton_bog_to_plant      <- m3_per_ton_loose
m3_per_ton_plant_to_customer <- m3_per_ton_comp


# ============================================================
# 3. Operating cost simulation, $/t
# ============================================================

cost_min <- 104.25
cost_c   <- 164.32
cost_max <- 206.03

cost_sim <- sim_grid %>%
  dplyr::mutate(
    cost_t = triangle::rtriangle(
      n = dplyr::n(),
      a = cost_min,
      b = cost_max,
      c = cost_c
    )
  )

cost_summary <- cost_sim %>%
  dplyr::summarise(
    dataset  = "simulated_costs",
    variable = "cost_t",
    n        = sum(is.finite(cost_t)),
    mean     = mean(cost_t, na.rm = TRUE),
    sd       = stats::sd(cost_t, na.rm = TRUE),
    min      = min(cost_t, na.rm = TRUE),
    p10      = as.numeric(stats::quantile(cost_t, 0.10, na.rm = TRUE)),
    p50      = as.numeric(stats::quantile(cost_t, 0.50, na.rm = TRUE)),
    p90      = as.numeric(stats::quantile(cost_t, 0.90, na.rm = TRUE)),
    max      = max(cost_t, na.rm = TRUE)
  )

utils::write.csv(
  cost_sim,
  file.path(output_data_dir, "simulated_costs.csv"),
  row.names = FALSE
)


# ============================================================
# 4. Peat yield simulation
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

yield_summary <- yield_sim %>%
  dplyr::summarise(
    dplyr::across(
      c(depth_m, yield_m3_ha, yield_t_ha),
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
  dplyr::mutate(dataset = "simulated_peat_yield", .before = 1)

utils::write.csv(
  yield_sim,
  file.path(output_data_dir, "simulated_peat_yield.csv"),
  row.names = FALSE
)


# ============================================================
# 5. Peat price fitting and simulation, $/t
# ============================================================

peat_data <- utils::read.csv(input_price_path) %>%
  dplyr::rename_with(tolower)

peat_price <- as.numeric(peat_data$price)

cat("\n--- Historical peat price summary, $/t ---\n")
print(summary(peat_price))

fit_norm    <- fitdistrplus::fitdist(peat_price, "norm", method = "mle")
fit_lnorm   <- fitdistrplus::fitdist(peat_price, "lnorm", method = "mle")
fit_llogis  <- fitdistrplus::fitdist(peat_price, "llogis", method = "mle")
fit_unif    <- fitdistrplus::fitdist(peat_price, "unif", method = "mle")
fit_gamma   <- fitdistrplus::fitdist(peat_price, "gamma", method = "mle")
fit_weibull <- fitdistrplus::fitdist(peat_price, "weibull", method = "mle")

fits <- list(
  fit_norm,
  fit_lnorm,
  fit_llogis,
  fit_unif,
  fit_gamma,
  fit_weibull
)

fit_names <- c(
  "Normal",
  "Lognormal",
  "Log-logistic",
  "Uniform",
  "Gamma",
  "Weibull"
)

gof_results <- fitdistrplus::gofstat(
  fits,
  fitnames = fit_names
)

cat("\n--- Price distribution goodness-of-fit ---\n")
print(gof_results)

fit_table <- tibble::tibble(
  distribution = fit_names,
  AIC = sapply(fits, function(x) x$aic),
  BIC = sapply(fits, function(x) x$bic)
) %>%
  dplyr::arrange(AIC)

cat("\n--- Price distribution fit table ---\n")
print(fit_table)

utils::write.csv(
  fit_table,
  file.path(output_data_dir, "peat_price_distribution_fit_table.csv"),
  row.names = FALSE
)

# Truncated lognormal simulation
ln_meanlog <- as.numeric(fit_lnorm$estimate["meanlog"])
ln_sdlog   <- as.numeric(fit_lnorm$estimate["sdlog"])

min_price <- min(peat_price, na.rm = TRUE)
max_price <- max(peat_price, na.rm = TRUE)

F_min <- stats::plnorm(
  min_price,
  meanlog = ln_meanlog,
  sdlog   = ln_sdlog
)

F_max <- stats::plnorm(
  max_price,
  meanlog = ln_meanlog,
  sdlog   = ln_sdlog
)

price_sim <- sim_grid %>%
  dplyr::mutate(
    u = stats::runif(
      n = dplyr::n(),
      min = F_min,
      max = F_max
    ),
    price_t = stats::qlnorm(
      p = u,
      meanlog = ln_meanlog,
      sdlog   = ln_sdlog
    )
  ) %>%
  dplyr::select(iteration, year, price_t)

price_summary <- price_sim %>%
  dplyr::summarise(
    dataset  = "simulated_peat_price",
    variable = "price_t",
    n        = sum(is.finite(price_t)),
    mean     = mean(price_t, na.rm = TRUE),
    sd       = stats::sd(price_t, na.rm = TRUE),
    min      = min(price_t, na.rm = TRUE),
    p10      = as.numeric(stats::quantile(price_t, 0.10, na.rm = TRUE)),
    p50      = as.numeric(stats::quantile(price_t, 0.50, na.rm = TRUE)),
    p90      = as.numeric(stats::quantile(price_t, 0.90, na.rm = TRUE)),
    max      = max(price_t, na.rm = TRUE)
  )

price_year_stats <- price_sim %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    mean = mean(price_t, na.rm = TRUE),
    sd   = stats::sd(price_t, na.rm = TRUE),
    p10  = as.numeric(stats::quantile(price_t, 0.10, na.rm = TRUE)),
    p50  = as.numeric(stats::quantile(price_t, 0.50, na.rm = TRUE)),
    p90  = as.numeric(stats::quantile(price_t, 0.90, na.rm = TRUE)),
    .groups = "drop"
  )

utils::write.csv(
  price_sim,
  file.path(output_data_dir, "simulated_peat_price_lognormal_t.csv"),
  row.names = FALSE
)


# ============================================================
# 6. Build base simulation panel
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

base_panel_summary <- base_panel %>%
  dplyr::summarise(
    dplyr::across(
      c(price_t, costs_t, tons_ha),
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
  dplyr::mutate(dataset = "base_panel", .before = 1)

iter_ids <- sort(unique(base_panel$iteration))
n_iter_actual <- length(iter_ids)


# ============================================================
# 7. Initial costs, royalty, restoration, and salvage
# ============================================================

salvage_factor <- (1 - depr_rate)^final_year

initial_df <- tibble::tibble(
  iteration = iter_ids,
  initial_cost_equipment = stats::runif(
    n = n_iter_actual,
    min = 2051500,
    max = 2653000
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

initial_cost_summary <- initial_df %>%
  dplyr::summarise(
    dplyr::across(
      c(initial_cost_equipment, env_app_cost, total_initial_cost, salvage_equipment),
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
  dplyr::mutate(dataset = "initial_costs", .before = 1)

royalty_per_cy <- 0.11
royalty_per_m3 <- royalty_per_cy * m3_to_cy
royalty_per_t  <- royalty_per_m3 * m3_per_ton_loose

restoration_cost_per_ha <- 3485


# ============================================================
# 8. Freight simulation, $/t
# ============================================================

# Bog to plant
leg1_rate_per_km <- 2.00
leg1_cap_m3_TL   <- 125
leg1_dist_km     <- c(379, 429, 505, 278, 207)

# Plant to customer
leg2_rate_per_km <- 0.66

bale_ft3_comp     <- 3.8
bales_per_pallet  <- 30
pallets_per_truck <- 22

leg2_cap_m3_TL <- bale_ft3_comp *
  bales_per_pallet *
  pallets_per_truck *
  ft3_to_m3

leg2_dist_km <- c(1023, 1964, 2148, 2284)

# Convert freight distances to $/t directly
leg1_cost_t_vec <- (
  leg1_rate_per_km *
    leg1_dist_km /
    leg1_cap_m3_TL
) * m3_per_ton_bog_to_plant

leg2_cost_t_vec <- (
  leg2_rate_per_km *
    leg2_dist_km /
    leg2_cap_m3_TL
) * m3_per_ton_plant_to_customer

freight1_min <- min(leg1_cost_t_vec)
freight1_max <- max(leg1_cost_t_vec)
freight1_c   <- (freight1_min + freight1_max) / 2

freight2_min <- min(leg2_cost_t_vec)
freight2_max <- max(leg2_cost_t_vec)
freight2_c   <- (freight2_min + freight2_max) / 2

freight1_mean <- (freight1_min + freight1_c + freight1_max) / 3
freight2_mean <- (freight2_min + freight2_c + freight2_max) / 3

freight_range_summary <- tibble::tibble(
  component = c("Bog to plant", "Plant to customer", "Total freight"),
  min_cost_t = c(
    freight1_min,
    freight2_min,
    freight1_min + freight2_min
  ),
  c_cost_t = c(
    freight1_c,
    freight2_c,
    freight1_c + freight2_c
  ),
  mean_cost_t = c(
    freight1_mean,
    freight2_mean,
    freight1_mean + freight2_mean
  ),
  max_cost_t = c(
    freight1_max,
    freight2_max,
    freight1_max + freight2_max
  )
) %>%
  dplyr::mutate(
    dplyr::across(
      c(min_cost_t, c_cost_t, mean_cost_t, max_cost_t),
      ~ round(.x, 2)
    )
  )

cat("\n--- Freight range summary, $/t ---\n")
print(freight_range_summary)

utils::write.csv(
  freight_range_summary,
  file.path(output_data_dir, "freight_range_summary.csv"),
  row.names = FALSE
)

freight_draws <- sim_grid %>%
  dplyr::mutate(
    freight1_t = triangle::rtriangle(
      n = dplyr::n(),
      a = freight1_min,
      b = freight1_max,
      c = freight1_c
    ),
    freight2_t = triangle::rtriangle(
      n = dplyr::n(),
      a = freight2_min,
      b = freight2_max,
      c = freight2_c
    ),
    freight_total_t = freight1_t + freight2_t
  ) %>%
  dplyr::select(
    iteration,
    year,
    freight1_t,
    freight2_t,
    freight_total_t
  )

freight_summary <- freight_draws %>%
  dplyr::summarise(
    dplyr::across(
      c(freight1_t, freight2_t, freight_total_t),
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
  dplyr::mutate(dataset = "simulated_freight_costs", .before = 1)

utils::write.csv(
  freight_draws,
  file.path(output_data_dir, "simulated_freight_costs.csv"),
  row.names = FALSE
)


# ============================================================
# 9. Carbon emissions simulation
# ============================================================

# Carbon content of dry peat mass
C_frac_dry <- 0.46

# Moisture adjustment: convert wet/as-sold tons to dry tons
moisture_frac <- 0.50
dry_frac      <- 1 - moisture_frac

# On-site emission factor, tC/ha/year.
# It is converted to tCO2e below using 44/12.
EF_onsite_tC_ha_yr <- 1.4

emissions_df <- base_panel %>%
  dplyr::group_by(iteration) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(
    area_ha = project_area_ha,
    total_tons = tons_ha * project_area_ha,
    
    m_tC = total_tons * dry_frac * C_frac_dry,
    
    # Instant off-site emissions: all extracted peat carbon is emitted
    # in the same year it is harvested/used.
    offsite_tC = m_tC,
    
    offsite_tCO2e = offsite_tC * (44 / 12),
    onsite_tCO2e  = EF_onsite_tC_ha_yr * project_area_ha * (44 / 12),
    
    total_tCO2e = onsite_tCO2e + offsite_tCO2e
  ) %>%
  dplyr::ungroup()

emissions_summary <- emissions_df %>%
  dplyr::summarise(
    dplyr::across(
      c(total_tons, m_tC, offsite_tC, offsite_tCO2e, onsite_tCO2e, total_tCO2e),
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
  dplyr::mutate(dataset = "simulated_emissions", .before = 1)


# ============================================================
# 10. Undiscounted emissions for BEP
# ============================================================

bep_emis <- emissions_df %>%
  dplyr::group_by(iteration) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::summarise(
    T       = max(year),
    area_ha = dplyr::first(area_ha),
    
    onsite_tCO2e_1T  = sum(onsite_tCO2e, na.rm = TRUE),
    offsite_tCO2e_1T = sum(offsite_tCO2e, na.rm = TRUE),
    
    total_tCO2e_project = onsite_tCO2e_1T + offsite_tCO2e_1T,
    total_tCO2e_per_ha  = total_tCO2e_project / area_ha,
    
    .groups = "drop"
  )

bep_emis_summary <- bep_emis %>%
  dplyr::summarise(
    dplyr::across(
      c(onsite_tCO2e_1T, offsite_tCO2e_1T, total_tCO2e_project, total_tCO2e_per_ha),
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
  dplyr::mutate(dataset = "bep_undiscounted_emissions", .before = 1)

cat("\n--- Undiscounted emissions for BEP, tCO2e/ha ---\n")
print(summary(bep_emis$total_tCO2e_per_ha))


# ============================================================
# 11. Scenario calculation function
# ============================================================

run_project_scenario <- function(price_mult = 1,
                                 yield_mult = 1,
                                 freight_mult = 1,
                                 cost_mult = 1,
                                 discount_rate_scn = discount_rate) {
  
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
      
      freight1_t_scn      = freight1_t * freight_mult,
      freight2_t_scn      = freight2_t * freight_mult,
      freight_total_t_scn = freight_total_t * freight_mult,
      
      total_tons = tons_ha_scn * project_area_ha,
      
      revenue             = price_t_scn * total_tons,
      variable_cost_total = costs_t_scn * total_tons,
      royalty_total       = royalty_per_t * total_tons,
      freight_total       = freight_total_t_scn * total_tons,
      
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
    dplyr::group_by(iteration) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      area_ha = project_area_ha,
      
      m_tC = total_tons * dry_frac * C_frac_dry,
      
      offsite_tC = m_tC,
      
      offsite_tCO2e = offsite_tC * (44 / 12),
      onsite_tCO2e  = EF_onsite_tC_ha_yr * project_area_ha * (44 / 12),
      
      total_tCO2e = onsite_tCO2e + offsite_tCO2e
    ) %>%
    dplyr::ungroup()
  
  bep_emis_scn <- scenario_emissions %>%
    dplyr::group_by(iteration) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::summarise(
      T       = max(year),
      area_ha = dplyr::first(area_ha),
      
      onsite_tCO2e_1T  = sum(onsite_tCO2e, na.rm = TRUE),
      offsite_tCO2e_1T = sum(offsite_tCO2e, na.rm = TRUE),
      
      total_tCO2e_project = onsite_tCO2e_1T + offsite_tCO2e_1T,
      total_tCO2e_per_ha  = total_tCO2e_project / area_ha,
      
      .groups = "drop"
    )
  
  bep_results <- npv_results %>%
    dplyr::left_join(
      bep_emis_scn,
      by = "iteration"
    ) %>%
    dplyr::mutate(
      BEP = ifelse(
        NPV_per_ha > 0,
        NPV_per_ha / total_tCO2e_per_ha,
        NA_real_
      )
    )
  
  summary_row <- tibble::tibble(
    discount_rate_scn = discount_rate_scn,
    
    mean_NPV_per_ha = mean(npv_results$NPV_per_ha, na.rm = TRUE),
    sd_NPV_per_ha   = stats::sd(npv_results$NPV_per_ha, na.rm = TRUE),
    min_NPV_per_ha  = min(npv_results$NPV_per_ha, na.rm = TRUE),
    p10_NPV_per_ha  = as.numeric(stats::quantile(npv_results$NPV_per_ha, 0.10, na.rm = TRUE)),
    p50_NPV_per_ha  = as.numeric(stats::quantile(npv_results$NPV_per_ha, 0.50, na.rm = TRUE)),
    p90_NPV_per_ha  = as.numeric(stats::quantile(npv_results$NPV_per_ha, 0.90, na.rm = TRUE)),
    max_NPV_per_ha  = max(npv_results$NPV_per_ha, na.rm = TRUE),
    p_NPV_positive  = mean(npv_results$NPV_per_ha > 0, na.rm = TRUE),
    
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
    BEP_emissions     = bep_emis_scn,
    NPV_results       = npv_results,
    BEP_results       = bep_results,
    summary_row       = summary_row
  )
}


# ============================================================
# 12. Scenario table
# ============================================================

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


# ============================================================
# 13. Run all scenarios
# ============================================================

scenario_outputs <- vector("list", nrow(scenario_tbl))
summary_list     <- vector("list", nrow(scenario_tbl))

for (i in seq_len(nrow(scenario_tbl))) {
  
  scn <- scenario_tbl[i, ]
  
  scenario_result <- run_project_scenario(
    price_mult        = scn$price_mult,
    yield_mult        = scn$yield_mult,
    freight_mult      = scn$freight_mult,
    cost_mult         = scn$cost_mult,
    discount_rate_scn = scn$discount_rate_scn
  )
  
  scenario_result$NPV_results <- scenario_result$NPV_results %>%
    dplyr::mutate(scenario = scn$scenario, .before = 1)
  
  scenario_result$BEP_results <- scenario_result$BEP_results %>%
    dplyr::mutate(scenario = scn$scenario, .before = 1)
  
  scenario_result$annual_cashflow <- scenario_result$annual_cashflow %>%
    dplyr::mutate(scenario = scn$scenario, .before = 1)
  
  scenario_result$scenario_emissions <- scenario_result$scenario_emissions %>%
    dplyr::mutate(scenario = scn$scenario, .before = 1)
  
  scenario_result$BEP_emissions <- scenario_result$BEP_emissions %>%
    dplyr::mutate(scenario = scn$scenario, .before = 1)
  
  scenario_outputs[[i]] <- scenario_result
  
  summary_list[[i]] <- scenario_result$summary_row %>%
    dplyr::mutate(scenario = scn$scenario, .before = 1)
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

bep_emis_all <- dplyr::bind_rows(
  lapply(scenario_outputs, function(x) x$BEP_emissions)
)

sensitivity_summary <- dplyr::bind_rows(summary_list)


# ============================================================
# 14. Sensitivity summary relative to baseline
# ============================================================

baseline_mean_npv <- sensitivity_summary$mean_NPV_per_ha[
  sensitivity_summary$scenario == "Baseline"
]

baseline_max_npv <- sensitivity_summary$max_NPV_per_ha[
  sensitivity_summary$scenario == "Baseline"
]

baseline_mean_bep <- sensitivity_summary$mean_BEP[
  sensitivity_summary$scenario == "Baseline"
]

sensitivity_summary <- sensitivity_summary %>%
  dplyr::mutate(
    delta_mean_NPV_per_ha = mean_NPV_per_ha - baseline_mean_npv,
    pct_change_mean_NPV   = 100 * (mean_NPV_per_ha / baseline_mean_npv - 1),
    
    delta_max_NPV_per_ha = max_NPV_per_ha - baseline_max_npv,
    pct_change_max_NPV   = 100 * (max_NPV_per_ha / baseline_max_npv - 1),
    
    delta_mean_BEP      = mean_BEP - baseline_mean_bep,
    pct_change_mean_BEP = 100 * (mean_BEP / baseline_mean_bep - 1)
  )

cat("\n--- Sensitivity summary ---\n")
print(sensitivity_summary)


# ============================================================
# 15. Combined summary for simulated datasets
# ============================================================

simulation_summary <- dplyr::bind_rows(
  cost_summary,
  yield_summary,
  price_summary,
  base_panel_summary,
  initial_cost_summary,
  freight_summary,
  emissions_summary,
  bep_emis_summary
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
# 16. Save main output files
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
  bep_emis_all,
  file.path(output_data_dir, "bep_undiscounted_emissions_all_scenarios.csv"),
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


# ============================================================
# 17. Baseline results
# ============================================================

baseline_index <- which(scenario_tbl$scenario == "Baseline")
baseline_out   <- scenario_outputs[[baseline_index]]

cat("\n--- Baseline NPV per ha ---\n")
print(summary(baseline_out$NPV_results$NPV_per_ha))

cat("Mean:", mean(baseline_out$NPV_results$NPV_per_ha, na.rm = TRUE), "\n")
cat("SD:", stats::sd(baseline_out$NPV_results$NPV_per_ha, na.rm = TRUE), "\n")
cat("Min:", min(baseline_out$NPV_results$NPV_per_ha, na.rm = TRUE), "\n")
cat("Max:", max(baseline_out$NPV_results$NPV_per_ha, na.rm = TRUE), "\n")
cat("P(NPV > 0):", mean(baseline_out$NPV_results$NPV_per_ha > 0, na.rm = TRUE), "\n")

cat("\n--- Baseline BEP, positive NPV only ---\n")
print(summary(baseline_out$BEP_results$BEP))

cat("Mean BEP:", mean(baseline_out$BEP_results$BEP, na.rm = TRUE), "\n")
cat("SD BEP:", stats::sd(baseline_out$BEP_results$BEP, na.rm = TRUE), "\n")
cat("Number of BEP observations:", sum(!is.na(baseline_out$BEP_results$BEP)), "\n")
cat("Excluded negative-NPV observations:", sum(is.na(baseline_out$BEP_results$BEP)), "\n")


# ============================================================
# 18. Emissions diagnostics
# ============================================================

emissions_by_year <- emissions_df %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    mean_onsite_tCO2e  = mean(onsite_tCO2e, na.rm = TRUE),
    mean_offsite_tCO2e = mean(offsite_tCO2e, na.rm = TRUE),
    mean_total_tCO2e   = mean(total_tCO2e, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Annual emissions components ---\n")
print(emissions_by_year)

utils::write.csv(
  emissions_by_year,
  file.path(output_data_dir, "annual_emissions_summary_by_year.csv"),
  row.names = FALSE
)

mass_balance <- emissions_df %>%
  dplyr::group_by(iteration) %>%
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
  file.path(output_data_dir, "emissions_mass_balance_check.csv"),
  row.names = FALSE
)


# ============================================================
# 19. Final quick checks
# ============================================================

cat("\n--- Quick simulation checks ---\n")

cat("\nOperating cost, $/t:\n")
print(summary(cost_sim$cost_t))

cat("\nYield, t/ha:\n")
print(summary(yield_sim$yield_t_ha))

cat("\nPrice, $/t:\n")
print(summary(price_sim$price_t))

cat("\nFreight, $/t:\n")
print(summary(freight_draws$freight_total_t))

cat("\nBase panel dimensions:\n")
print(dim(base_panel))

cat("\nDone. Output CSV files were saved in the project-specific output_data_dir folder.\n")
