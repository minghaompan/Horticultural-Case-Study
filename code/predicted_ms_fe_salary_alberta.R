# --- Packages ---
library(dplyr)
library(broom)
library(lmtest)
library(sandwich)
library(writexl)

# ============================
# 0) Paths & settings
# ============================
input_path     <- "data/horti_peat_variable_costs.csv"
output_dir     <- "data"
fit_years      <- 1998:2015
forecast_years <- 2016:2023

# ============================
# 1) Read new dataset
# ============================
data <- read.csv(input_path, stringsAsFactors = FALSE)

# Sanity check (optional)
print(names(data))

# ============================
# 2) Build MS, FE, Salary data
# ============================
# We use the year column that goes with each AB/CA pair

## ---- MS ----
ab_ms <- data %>%
  transmute(
    year  = as.numeric(year),   # AB-MS year (col A)
    ab_ms = as.numeric(ab_ms)
  ) %>%
  filter(!is.na(year), !is.na(ab_ms))

ca_ms <- data %>%
  transmute(
    year  = as.numeric(year.1), # CA-MS year (col C)
    ca_ms = as.numeric(ca_ms)
  ) %>%
  filter(!is.na(year), !is.na(ca_ms))

df_ms <- full_join(ab_ms, ca_ms, by = "year") %>%
  arrange(year)


## ---- FE ----
ab_fe <- data %>%
  transmute(
    year  = as.numeric(year.2),  # AB-FE year
    ab_fe = as.numeric(ab_fe)
  ) %>%
  filter(!is.na(year), !is.na(ab_fe))

ca_fe <- data %>%
  transmute(
    year  = as.numeric(year.3),  # CA-FE year
    ca_fe = as.numeric(ca_fe)
  ) %>%
  filter(!is.na(year), !is.na(ca_fe))

df_fe <- full_join(ab_fe, ca_fe, by = "year") %>%
  arrange(year)


## ---- Salary ----
ab_salary <- data %>%
  transmute(
    year       = as.numeric(year.4),   # AB-salary year
    ab_salary  = as.numeric(ab_salary)
  ) %>%
  filter(!is.na(year), !is.na(ab_salary))

ca_salary <- data %>%
  transmute(
    year       = as.numeric(year.5),   # CA-salary year
    ca_salary  = as.numeric(ca_salary)
  ) %>%
  filter(!is.na(year), !is.na(ca_salary))

df_salary <- full_join(ab_salary, ca_salary, by = "year") %>%
  arrange(year)

# ============================
# 3) Helper: fit regression & predict AB
# ============================
fit_and_predict <- function(df, ab_name, ca_name,
                            fit_years, forecast_years,
                            ab_obs_name, ab_est_name) {
  
  df_mod <- df[, c("year", ab_name, ca_name)]
  names(df_mod) <- c("year", "ab", "ca")
  
  fit_df <- subset(df_mod,
                   year %in% fit_years &
                     !is.na(ab) & !is.na(ca))
  
  # OLS
  mod <- lm(ab ~ ca, data = fit_df)
  
  # Newey–West SEs (printed for info)
  nw_vcov <- sandwich::NeweyWest(mod, lag = 1, prewhite = FALSE)
  coefs_robust <- broom::tidy(mod)
  coefs_robust$robust_se <- sqrt(diag(nw_vcov))[match(coefs_robust$term,
                                                      names(coef(mod)))]
  coefs_robust$robust_t <- coefs_robust$estimate / coefs_robust$robust_se
  coefs_robust$robust_p <- 2 * pnorm(-abs(coefs_robust$robust_t))
  print(coefs_robust)
  
  # Forecast
  future <- subset(df_mod, year %in% forecast_years)
  future$ab_pred <- predict(mod, newdata = future)
  
  # Observed rows
  obs_out <- data.frame(
    year   = fit_df$year,
    ca     = fit_df$ca,
    source = "observed",
    stringsAsFactors = FALSE
  )
  obs_out[[ab_obs_name]] <- fit_df$ab
  obs_out[[ab_est_name]] <- fit_df$ab
  
  # Predicted rows
  pred_out <- data.frame(
    year   = future$year,
    ca     = future$ca,
    source = "predicted",
    stringsAsFactors = FALSE
  )
  pred_out[[ab_obs_name]] <- NA_real_
  pred_out[[ab_est_name]] <- future$ab_pred
  
  out <- rbind(obs_out, pred_out)
  out <- out[order(out$year), ]
  rownames(out) <- NULL
  out
}

# ============================
# 4) Run for MS, FE, Salary
# ============================
out_ms <- fit_and_predict(
  df              = df_ms,
  ab_name         = "ab_ms",
  ca_name         = "ca_ms",
  fit_years       = fit_years,
  forecast_years  = forecast_years,
  ab_obs_name     = "ab_ms_obs",
  ab_est_name     = "ab_ms_est"
)

out_fe <- fit_and_predict(
  df              = df_fe,
  ab_name         = "ab_fe",
  ca_name         = "ca_fe",
  fit_years       = fit_years,
  forecast_years  = forecast_years,
  ab_obs_name     = "ab_fe_obs",
  ab_est_name     = "ab_fe_est"
)

out_salary <- fit_and_predict(
  df              = df_salary,
  ab_name         = "ab_salary",
  ca_name         = "ca_salary",
  fit_years       = fit_years,
  forecast_years  = forecast_years,
  ab_obs_name     = "ab_salary_obs",
  ab_est_name     = "ab_salary_est"
)


# ============================
# 5) One combined, labeled CSV
# ============================
combined_long <- bind_rows(
  out_ms %>%
    transmute(
      series      = "MS",
      year,
      ca_cost     = ca,
      ab_cost_obs = ab_ms_obs,
      ab_cost_est = ab_ms_est,
      source
    ),
  out_fe %>%
    transmute(
      series      = "FE",
      year,
      ca_cost     = ca,
      ab_cost_obs = ab_fe_obs,
      ab_cost_est = ab_fe_est,
      source
    ),
  out_salary %>%
    transmute(
      series      = "Salary",
      year,
      ca_cost     = ca,
      ab_cost_obs = ab_salary_obs,
      ab_cost_est = ab_salary_est,
      source
    )
) %>%
  arrange(series, year)

write.csv(
  combined_long,
  file = file.path(output_dir, "horti_peat_variable_costs_predicted_AB_CA_combined.csv"),
  row.names = FALSE
)
