# ------------------------------------
# Peat yield simulation: triangular RNG
# 1000 simulations x 18 years
# Using package 'triangle' instead of custom rtri()
# ------------------------------------
library(writexl)
library(triangle)   # provides rtriangle()
set.seed(123)

# ----- PARAMETERS (from your figure) -----
depth_min  <- 0.05   # m
depth_average <- 0.086  # m (treat "Average" as the triangular average)
depth_max  <- 0.14   # m

area_per_ha <- 10000   # m^2 per ha
m_to_yield  <- area_per_ha  # depth (m) * 10,000 = m^3/ha (≈ t/ha if density = 1 t/m^3)

# Optional checks (t/ha or m^3/ha)
yield_min  <- depth_min  * m_to_yield
yield_average <- depth_average * m_to_yield
yield_max  <- depth_max  * m_to_yield

# ----- SIM SETTINGS -----
n_sims  <- 1000
n_years <- 18

# ----- SIMULATE DEPTHS, CONVERT TO YIELDS (t/ha) -----
# rtriangle(n, a = min, b = max, c = average)
depth_draws <- matrix(
  rtriangle(n_sims * n_years, a = depth_min, b = depth_max, c = depth_average),
  nrow = n_sims, ncol = n_years, byrow = TRUE
)

yield_draws <- depth_draws * m_to_yield  # t/ha (assuming 1 t/m^3)

# ----- LONG (TIDY) DATA FRAME OF ALL DRAWS -----
yield_df <- data.frame(
  year      = rep(seq_len(n_years), times = n_sims),
  iteration = rep(seq_len(n_sims), each  = n_years),
  yield_m3 = as.vector(yield_draws)
)

# ----- PER-YEAR SUMMARY ACROSS SIMS -----
year_stats <- aggregate(yield_m3 ~ year, data = yield_df, FUN = function(x) {
  c(
    mean = mean(x),
    sd   = sd(x),
    p10  = as.numeric(quantile(x, 0.10)),
    p50  = as.numeric(quantile(x, 0.50)),
    p90  = as.numeric(quantile(x, 0.90))
  )
})

# Unpack the matrix column safely
tmp <- year_stats
year_stats <- cbind(year = tmp$year, as.data.frame(tmp$yield))
names(year_stats) <- c("year", "mean", "sd", "p10", "p50", "p90")


# ---- Convert simulated volume (m3/ha) to bales and tons (your screenshot formula) ----
bale_ft3           <- 6
compression_factor <- 2
ft3_to_m3          <- 0.028317
bales_per_ton      <- 26

m3_per_bale_loose <- bale_ft3 * compression_factor * ft3_to_m3
m3_per_ton_loose  <- m3_per_bale_loose * bales_per_ton   # = 8.834904 m3 per ton

# yield_df$yield is your simulated volume (m3/ha)
yield_df$yield_t<- yield_df$yield / m3_per_ton_loose

# ----- QUICK PEEKS -----
head(yield_df)      # long table of simulated yearly yields (t/ha)
head(year_stats)    # per-year stats
summary(yield_df)
sapply(yield_df, sd, na.rm = TRUE)

write.csv(
  yield_df[, c("year", "iteration", "yield_m3")],
  "data/simulated_peat_volume_m3.csv",
  row.names = FALSE
)

write.csv(
  yield_df[, c("year", "iteration","yield_t" )],
  "data/simulated_peat_volume_t.csv",
  row.names = FALSE
)
