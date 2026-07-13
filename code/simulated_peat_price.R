# ============================
# Packages
# ============================
library(vars)
library(tseries)
library(readxl)
library(urca)
library(fitdistrplus)
library(pastecs)
library(stats)
library(actuar)

set.seed(123)
# ==========================================
# PART A — Load data & fit marginal models
# ==========================================
data <- read.csv("data/peat_price_alberta.csv")
colnames(data) <- tolower(colnames(data))
# Quick peek
head(data)

# --------------------------
# Peat prices fitting distribution
# --------------------------
fn.pp  <- fitdist(data$prices, "norm",    method = "mle"); summary(fn.pp)
fln.pp <- fitdist(data$prices, "lnorm",   method = "mle"); summary(fln.pp)
fll.pp <- fitdist(data$prices, "llogis",  method = "mle"); summary(fll.pp)
fu.pp  <- fitdist(data$prices, "unif",    method = "mle"); summary(fu.pp)
fg.pp  <- fitdist(data$prices, "gamma",   method = "mle"); summary(fg.pp)
fw.pp  <- fitdist(data$prices, "weibull", method = "mle"); summary(fw.pp)

gofstat(list(fn.pp, fln.pp, fll.pp, fu.pp, fg.pp, fw.pp),
        fitnames = c("Normal", "Lognormal", "Log-logistic", "uniform", "Gamma", "Weibull"))

# ------------------------
# Diagnostics: QQ & CDFs
# ------------------------
plot.legend <- c("Normal", "Lognormal", "Log-logistic", "uniform", "Gamma", "Weibull")
qqcomp(list(fn.pp, fln.pp, fll.pp, fu.pp, fg.pp, fw.pp), legendtext = plot.legend, main = "Peat Prices")
qqcomp(list(fn.pp, fln.pp, fll.pp, fu.pp, fg.pp, fw.pp),
       legendtext = c("Normal", "Lognormal", "Log-logistic", "Uniform", "Gamma", "Weibull"),
       main = "Peat Prices", fitcol = 1:6)
cdfcomp(list(fn.pp, fln.pp, fll.pp, fu.pp, fg.pp, fw.pp), legendtext = plot.legend, main = "Peat Prices")

# =====================================================
# PART B — Simulate peat prices with fitted Lognormal
# =====================================================

library(writexl)

# --- 1) Simulation settings ---
set.seed(123)          # reproducible
n_years <- 18
n_iter  <- 1000
n_sim   <- n_years * n_iter

# --- 2) Pull Lognormal parameters from the fitted object fln.pp ---
ln_meanlog <- as.numeric(fln.pp$estimate["meanlog"])
ln_sdlog   <- as.numeric(fln.pp$estimate["sdlog"])
cat(sprintf("Using fitted Lognormal: meanlog = %.6f | sdlog = %.6f\n", ln_meanlog, ln_sdlog))

# --- 3) Simulate draws (strictly positive by construction) ---
sim_draws_t <- rlnorm(n_sim, meanlog = ln_meanlog, sdlog = ln_sdlog)

# (Optional) light tail-trimming for plotting/NPV stability
# comment these three lines out if you want raw draws
lo <- quantile(sim_draws_t, 0.01, na.rm = TRUE)
hi <- quantile(sim_draws_t, 0.99, na.rm = TRUE)
sim_draws_t <- pmin(pmax(sim_draws_t, lo), hi)

# --- 4) Tidy panel: [year, iteration, price] ---
start_year <- if ("year" %in% names(data)) max(data$year, na.rm = TRUE) + 1 else 1
yrs <- seq(from = start_year, length.out = n_years)

sim_df <- data.frame(
  year      = rep(yrs, times = n_iter),
  iteration = rep(seq_len(n_iter), each = n_years),
  price_t     = sim_draws_t
)
summary(sim_df)

sapply(sim_df, sd, na.rm = TRUE)

# --- 6) Simple diagnostics ---
par(mfrow = c(1, 2))
hist(sim_df$price_t, breaks = 40,
     main = "Simulated Peat Prices ($/t)", xlab = "Price ($/t)")
boxplot(price_t ~ year, data = sim_df, outline = FALSE,
        main = "Yearly distribution across iterations",
        xlab = "Year", ylab = "Price")
par(mfrow = c(1, 1))

write.csv(
  sim_df[, c("year", "iteration", "price_t")],
  "data/simulated_peat_prices_lognormal_t.csv",
  row.names = FALSE
)

