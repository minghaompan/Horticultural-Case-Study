# ============================================================
# Baseline NPV density plot: 18-year vs 30-year project
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# ============================================================
# 1. File paths and settings
# ============================================================

npv_18_path <- "peat_extraction_18yr_discounted_emissions/data/npv_all_scenarios.csv"
npv_30_path <- "peat_extraction_30yr_discounted_emissions/data/npv_all_scenarios.csv"

output_path <- "figures/baseline_npv_density_18_30.png"

project_levels <- c("18-year project", "30-year project")

project_colors <- c(
  "18-year project" = "#D55E00",
  "30-year project" = "#0072B2"
)

# ============================================================
# 2. Load baseline NPV draws
# ============================================================

npv_18 <- utils::read.csv(npv_18_path) %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::transmute(
    project = "18-year project",
    iteration = iteration,
    NPV_per_ha = as.numeric(NPV_per_ha)
  )

npv_30 <- utils::read.csv(npv_30_path) %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::transmute(
    project = "30-year project",
    iteration = iteration,
    NPV_per_ha = as.numeric(NPV_per_ha)
  )

baseline_npv <- dplyr::bind_rows(npv_18, npv_30) %>%
  dplyr::mutate(
    project = factor(project, levels = project_levels),
    NPV_thousand_ha = NPV_per_ha / 1000
  )

# ============================================================
# 3. Summary statistics
# ============================================================

baseline_summary <- baseline_npv %>%
  dplyr::group_by(project) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_NPV_per_ha = mean(NPV_per_ha, na.rm = TRUE),
    median_NPV_per_ha = median(NPV_per_ha, na.rm = TRUE),
    sd_NPV_per_ha = stats::sd(NPV_per_ha, na.rm = TRUE),
    min_NPV_per_ha = min(NPV_per_ha, na.rm = TRUE),
    p10_NPV_per_ha = as.numeric(stats::quantile(NPV_per_ha, 0.10, na.rm = TRUE)),
    p90_NPV_per_ha = as.numeric(stats::quantile(NPV_per_ha, 0.90, na.rm = TRUE)),
    max_NPV_per_ha = max(NPV_per_ha, na.rm = TRUE),
    p_NPV_positive = mean(NPV_per_ha > 0, na.rm = TRUE),
    mean_NPV_thousand_ha = mean_NPV_per_ha / 1000,
    .groups = "drop"
  )

print(baseline_summary)

mean_lines <- baseline_summary %>%
  dplyr::select(project, mean_NPV_thousand_ha)

x_breaks <- seq(
  from = floor(min(baseline_npv$NPV_thousand_ha, na.rm = TRUE) / 10) * 10,
  to = ceiling(max(baseline_npv$NPV_thousand_ha, na.rm = TRUE) / 10) * 10,
  by = 10
)

# ============================================================
# 4. Density plot
# ============================================================

p <- ggplot(
  baseline_npv,
  aes(x = NPV_thousand_ha)
) +
  geom_density(
    aes(fill = project),
    color = NA,
    alpha = 0.12,
    adjust = 1.10,
    trim = TRUE,
    show.legend = FALSE
  ) +
  geom_density(
    aes(color = project),
    linewidth = 1.15,
    adjust = 1.10,
    trim = TRUE,
    key_glyph = "path"
  ) +
  geom_vline(
    xintercept = 0,
    color = "grey35",
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_vline(
    data = mean_lines,
    aes(
      xintercept = mean_NPV_thousand_ha,
      color = project
    ),
    linewidth = 0.85,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = project_colors,
    breaks = project_levels
  ) +
  scale_fill_manual(
    values = project_colors,
    breaks = project_levels,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.001),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    x = "NPV (thousand CAD/ha)",
    y = "Density",
    color = NULL,
    fill = NULL
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1.4,
        linetype = "solid",
        fill = NA,
        alpha = 1
      )
    )
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.85),
      color = NA
    ),
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(1.0, "cm"),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(linewidth = 0.6, color = "black"),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    panel.grid = element_blank(),
    plot.margin = margin(8, 12, 8, 8)
  )

print(p)

# ============================================================
# 5. Save figure
# ============================================================

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

ggsave(
  filename = output_path,
  plot = p,
  width = 9.2,
  height = 4.6,
  units = "in",
  dpi = 300,
  bg = "white"
)

