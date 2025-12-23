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
depth_mode <- 0.086  # m (treat "Average" as the triangular mode)
depth_max  <- 0.14   # m

area_per_ha <- 10000   # m^2 per ha
m_to_yield  <- area_per_ha  # depth (m) * 10,000 = m^3/ha (≈ t/ha if density = 1 t/m^3)

# Optional checks (t/ha or m^3/ha)
yield_min  <- depth_min  * m_to_yield
yield_mode <- depth_mode * m_to_yield
yield_max  <- depth_max  * m_to_yield

# ----- SIM SETTINGS -----
n_sims  <- 1000
n_years <- 18

# ----- SIMULATE DEPTHS, CONVERT TO YIELDS (t/ha) -----
# rtriangle(n, a = min, b = max, c = mode)
depth_draws <- matrix(
  rtriangle(n_sims * n_years, a = depth_min, b = depth_max, c = depth_mode),
  nrow = n_sims, ncol = n_years, byrow = TRUE
)

yield_draws <- depth_draws * m_to_yield  # t/ha (assuming 1 t/m^3)

# ----- LONG (TIDY) DATA FRAME OF ALL DRAWS -----
yield_df <- data.frame(
  year      = rep(seq_len(n_years), times = n_sims),
  iteration = rep(seq_len(n_sims), each  = n_years),
  yield = as.vector(yield_draws)
)

# ----- PER-YEAR SUMMARY ACROSS SIMS -----
year_stats <- aggregate(yield ~ year, data = yield_df, FUN = function(x) {
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

# ----- QUICK PEEKS -----
head(yield_df)      # long table of simulated yearly yields (t/ha)
head(year_stats)    # per-year stats
summary(yield_df)
sapply(yield_df, sd, na.rm = TRUE)

write.csv(
  yield_df[, c("year", "iteration", "yield")],
  "data/simulated_peat_volume_triangular.csv",
  row.names = FALSE
)