# Load library
library(triangle)
library(dplyr)
library(writexl)

# Parameters from your table
min_cost <- 102.42
mode_cost <- 162.42  # average or mode
max_cost <- 209.87

years <- 18
iterations <- 1000

set.seed(123)  # reproducibility

bulk_density_t_per_m3 <- 0.168   # t per m^3

# Simulate in long format directly
sim_df <- expand.grid(
  year = 1:years,
  iteration = 1:iterations
) %>%
  mutate(costs = rtriangle(n = n(),
                           a = min_cost,
                           b = max_cost,
                           c = mode_cost),
         costs_m3 = costs * bulk_density_t_per_m3)

# Preview first 20 rows
head(sim_df, 20)
summary(sim_df)
sapply(sim_df, sd, na.rm = TRUE)

write.csv(
  sim_df[, c("year", "iteration", "costs_m3")],
  "data/simulated_costs_m3.csv",
  row.names = FALSE
)
