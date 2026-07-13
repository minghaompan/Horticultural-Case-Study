# ============================================================
# BEP sensitivity bar chart
# Uses on-site emission factor = 3.1 t CO2-C/ha/year
# Styled to match the NPV sensitivity figure exactly
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# ============================================================
# 1. Settings
# ============================================================

selected_onsite_ef <- 3.1

sensitivity_18_discounted_path <- file.path(
  "peat_extraction_18yr_discounted_emissions",
  "data",
  "sensitivity_summary.csv"
)

sensitivity_18_instant_path <- file.path(
  "peat_extraction_18yr_instant_emissions",
  "data",
  "sensitivity_summary.csv"
)

sensitivity_30_discounted_path <- file.path(
  "peat_extraction_30yr_discounted_emissions",
  "data",
  "sensitivity_summary.csv"
)

sensitivity_30_instant_path <- file.path(
  "peat_extraction_30yr_instant_emissions",
  "data",
  "sensitivity_summary.csv"
)

output_png_path <- file.path(
  "figures",
  "bep_tornado_bar_3p1_EF_same_style.png"
)

x_limits <- c(-120, 300)
x_breaks <- seq(-100, 300, by = 50)

driver_levels <- c(
  "Peat price",
  "Operating cost",
  "Freight cost",
  "Discount rate",
  "Peat yield"
)

setting_colors <- c(
  "Higher parameter setting" = "#0072B2",
  "Lower parameter setting"  = "#D55E00"
)

# ============================================================
# 2. Helper functions
# ============================================================

read_sensitivity_summary <- function(path, project_label, emissions_label) {
  utils::read.csv(path) %>%
    dplyr::mutate(
      project = project_label,
      emissions_accounting = emissions_label
    )
}

common_plot_theme <- function() {
  theme_classic(base_size = 12) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key = element_blank(),
      legend.key.width = grid::unit(0.90, "cm"),
      legend.key.height = grid::unit(0.50, "cm"),
      legend.text = element_text(size = 10.5),
      
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12, lineheight = 1.05),
      
      axis.text = element_text(color = "black", size = 10.5),
      axis.title.x = element_text(size = 12),
      axis.line = element_blank(),
      axis.ticks = element_line(color = "grey45", linewidth = 0.40),
      
      panel.border = element_rect(color = "grey35", fill = NA, linewidth = 0.70),
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.45),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = grid::unit(0.8, "lines"),
      
      plot.caption = element_text(
        hjust = 0,
        size = 9.0,
        color = "grey25",
        lineheight = 1.1
      ),
      
      plot.margin = margin(10, 72, 16, 24)
    )
}

# ============================================================
# 3. Load and prepare data
# ============================================================

sensitivity_all <- dplyr::bind_rows(
  read_sensitivity_summary(sensitivity_18_instant_path,    "18-year project", "Instant emissions"),
  read_sensitivity_summary(sensitivity_18_discounted_path, "18-year project", "Discounted gradual decomposition"),
  read_sensitivity_summary(sensitivity_30_instant_path,    "30-year project", "Instant emissions"),
  read_sensitivity_summary(sensitivity_30_discounted_path, "30-year project", "Discounted gradual decomposition")
) %>%
  dplyr::mutate(
    EF_onsite_tC_ha_yr = as.numeric(EF_onsite_tC_ha_yr),
    mean_BEP = as.numeric(mean_BEP),
    pct_change_mean_BEP = as.numeric(pct_change_mean_BEP)
  ) %>%
  dplyr::filter(abs(EF_onsite_tC_ha_yr - selected_onsite_ef) < 1e-8)

baseline_tbl <- sensitivity_all %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::transmute(
    project,
    emissions_accounting,
    baseline_mean_BEP = mean_BEP,
    panel = paste0(
      project, ": ", emissions_accounting,
      "\nBaseline mean BEP = ",
      scales::number(mean_BEP, accuracy = 0.1),
      " CAD/t CO2e"
    )
  )

panel_levels <- c(
  baseline_tbl$panel[
    baseline_tbl$project == "18-year project" &
      baseline_tbl$emissions_accounting == "Instant emissions"
  ],
  baseline_tbl$panel[
    baseline_tbl$project == "18-year project" &
      baseline_tbl$emissions_accounting == "Discounted gradual decomposition"
  ],
  baseline_tbl$panel[
    baseline_tbl$project == "30-year project" &
      baseline_tbl$emissions_accounting == "Instant emissions"
  ],
  baseline_tbl$panel[
    baseline_tbl$project == "30-year project" &
      baseline_tbl$emissions_accounting == "Discounted gradual decomposition"
  ]
)

plot_data <- sensitivity_all %>%
  dplyr::filter(scenario != "Baseline") %>%
  dplyr::left_join(baseline_tbl, by = c("project", "emissions_accounting")) %>%
  dplyr::mutate(
    driver = dplyr::case_when(
      scenario %in% c("Price +15%", "Price -15%") ~ "Peat price",
      scenario %in% c("Operating cost +15%", "Operating cost -15%") ~ "Operating cost",
      scenario %in% c("Freight +15%", "Freight -15%") ~ "Freight cost",
      scenario %in% c("Discount rate 10%", "Discount rate 3%") ~ "Discount rate",
      scenario %in% c("Yield +15%", "Yield -15%") ~ "Peat yield",
      TRUE ~ scenario
    ),
    parameter_setting = dplyr::case_when(
      scenario %in% c("Price +15%", "Operating cost +15%", "Freight +15%", "Discount rate 10%", "Yield +15%") ~ "Higher parameter setting",
      scenario %in% c("Price -15%", "Operating cost -15%", "Freight -15%", "Discount rate 3%", "Yield -15%") ~ "Lower parameter setting",
      TRUE ~ NA_character_
    ),
    label = paste0(
      dplyr::if_else(pct_change_mean_BEP > 0, "+", ""),
      round(pct_change_mean_BEP),
      "%"
    ),
    label_x = dplyr::case_when(
      pct_change_mean_BEP >= 0 ~ pmin(pct_change_mean_BEP + 6, x_limits[2] - 2),
      TRUE ~ pmax(pct_change_mean_BEP - 6, x_limits[1] + 2)
    ),
    label_hjust = dplyr::case_when(
      pct_change_mean_BEP >= 0 ~ 0,
      TRUE ~ 1
    ),
    driver = factor(driver, levels = rev(driver_levels)),
    parameter_setting = factor(
      parameter_setting,
      levels = c("Higher parameter setting", "Lower parameter setting")
    ),
    panel = factor(panel, levels = panel_levels)
  )

bar_position <- position_dodge2(width = 0.78, preserve = "single")

# ============================================================
# 4. Plot
# ============================================================

p <- ggplot(
  plot_data,
  aes(
    x = pct_change_mean_BEP,
    y = driver,
    group = parameter_setting
  )
) +
  geom_vline(
    xintercept = 0,
    color = "grey30",
    linewidth = 0.70,
    linetype = "dashed"
  ) +
  geom_col(
    aes(fill = parameter_setting),
    width = 0.68,
    orientation = "y",
    position = bar_position
  ) +
  geom_text(
    aes(
      x = label_x,
      label = label,
      hjust = label_hjust
    ),
    position = bar_position,
    size = 3.2,
    color = "black",
    vjust = 0.5
  ) +
  facet_wrap(~ panel, ncol = 2) +
  scale_fill_manual(
    values = setting_colors,
    breaks = c("Higher parameter setting", "Lower parameter setting")
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = function(x) paste0(x, "%"),
    limits = x_limits,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Percentage change in mean BEP relative to baseline",
    y = NULL,
    fill = NULL,
    caption = paste0(
      "Lower parameter setting: -15% for peat price, peat yield, freight cost, and operating cost; 3% for the discount rate.\n",
      "Higher parameter setting: +15% for peat price, peat yield, freight cost, and operating cost; 10% for the discount rate."
    )
  ) +
  common_plot_theme() +
  coord_cartesian(clip = "off")

print(p)

# ============================================================
# 5. Save figure
# ============================================================

dir.create(dirname(output_png_path), showWarnings = FALSE, recursive = TRUE)

ggsave(
  filename = output_png_path,
  plot = p,
  width = 14.0,
  height = 8.0,
  units = "in",
  dpi = 300,
  bg = "white"
)

