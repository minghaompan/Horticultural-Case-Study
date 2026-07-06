# ============================================================
# Mean NPV sensitivity-check bar chart
# 18-year and 30-year peat extraction projects
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# ============================================================
# 1. File paths and settings
# ============================================================

sensitivity_18_path <- file.path(
  "peat_extraction_18yr_discounted_emissions",
  "data",
  "sensitivity_summary.csv"
)

sensitivity_30_path <- file.path(
  "peat_extraction_30yr_discounted_emissions",
  "data",
  "sensitivity_summary.csv"
)

output_path <- file.path(
  "figures",
  "npv_sensitivity_bar_18_30.png"
)

output_tiff_path <- file.path(
  "figures",
  "npv_sensitivity_bar_18_30.tiff"
)

project_levels <- c(
  "18-year project",
  "30-year project"
)

sensitivity_colors <- c(
  "Higher-NPV case" = "#0072B2",
  "Lower-NPV case"  = "#D55E00"
)

# ============================================================
# 2. Load sensitivity summaries
# ============================================================

sensitivity_18 <- utils::read.csv(sensitivity_18_path) %>%
  dplyr::mutate(project = "18-year project")

sensitivity_30 <- utils::read.csv(sensitivity_30_path) %>%
  dplyr::mutate(project = "30-year project")

sensitivity_all <- dplyr::bind_rows(
  sensitivity_18,
  sensitivity_30
) %>%
  dplyr::mutate(
    project = factor(project, levels = project_levels),
    mean_NPV_per_ha = as.numeric(mean_NPV_per_ha),
    p_NPV_positive = as.numeric(p_NPV_positive),
    p_NPV_positive = dplyr::if_else(
      p_NPV_positive > 1,
      p_NPV_positive / 100,
      p_NPV_positive
    )
  )

# ============================================================
# 3. Prepare plotting data
# ============================================================

baseline_values <- sensitivity_all %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::transmute(
    project,
    baseline_mean_NPV_per_ha = mean_NPV_per_ha,
    baseline_mean_NPV_thousand_ha = mean_NPV_per_ha / 1000,
    baseline_p_NPV_positive = p_NPV_positive
  )

plot_data <- sensitivity_all %>%
  dplyr::filter(scenario != "Baseline") %>%
  dplyr::left_join(
    baseline_values,
    by = "project"
  ) %>%
  dplyr::mutate(
    driver = dplyr::case_when(
      scenario %in% c("Price +15%", "Price -15%") ~ "Peat price",
      scenario %in% c("Yield +15%", "Yield -15%") ~ "Peat yield",
      scenario %in% c("Freight +15%", "Freight -15%") ~ "Freight cost",
      scenario %in% c("Operating cost +15%", "Operating cost -15%") ~ "Operating cost",
      scenario %in% c("Discount rate 3%", "Discount rate 10%") ~ "Discount rate",
      TRUE ~ scenario
    ),
    
    scenario_level = dplyr::case_when(
      scenario == "Price +15%" ~ "+15%",
      scenario == "Price -15%" ~ "-15%",
      scenario == "Yield +15%" ~ "+15%",
      scenario == "Yield -15%" ~ "-15%",
      scenario == "Freight +15%" ~ "+15%",
      scenario == "Freight -15%" ~ "-15%",
      scenario == "Operating cost +15%" ~ "+15%",
      scenario == "Operating cost -15%" ~ "-15%",
      scenario == "Discount rate 3%" ~ "3%",
      scenario == "Discount rate 10%" ~ "10%",
      TRUE ~ scenario
    ),
    
    sensitivity_case = dplyr::case_when(
      scenario %in% c("Price +15%", "Yield +15%") ~ "Higher-NPV case",
      scenario %in% c("Price -15%", "Yield -15%") ~ "Lower-NPV case",
      scenario %in% c("Freight -15%", "Operating cost -15%") ~ "Higher-NPV case",
      scenario %in% c("Freight +15%", "Operating cost +15%") ~ "Lower-NPV case",
      scenario == "Discount rate 3%" ~ "Higher-NPV case",
      scenario == "Discount rate 10%" ~ "Lower-NPV case",
      TRUE ~ "Lower-NPV case"
    ),
    
    mean_NPV_thousand_ha = mean_NPV_per_ha / 1000,
    
    label = paste0(
      scenario_level,
      " | ",
      scales::percent(p_NPV_positive, accuracy = 0.1)
    ),
    
    label_x = dplyr::case_when(
      sensitivity_case == "Higher-NPV case" ~ mean_NPV_thousand_ha + 1.5,
      sensitivity_case == "Lower-NPV case" ~ mean_NPV_thousand_ha - 1.5,
      mean_NPV_thousand_ha >= 0 ~ mean_NPV_thousand_ha + 1.5,
      TRUE ~ mean_NPV_thousand_ha - 1.5
    ),
    
    label_hjust = dplyr::case_when(
      sensitivity_case == "Higher-NPV case" ~ 0,
      sensitivity_case == "Lower-NPV case" ~ 1,
      mean_NPV_thousand_ha >= 0 ~ 0,
      TRUE ~ 1
    )
  )

driver_order <- plot_data %>%
  dplyr::group_by(driver) %>%
  dplyr::summarise(
    sensitivity_range = max(mean_NPV_thousand_ha, na.rm = TRUE) -
      min(mean_NPV_thousand_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(sensitivity_range) %>%
  dplyr::pull(driver)

plot_data <- plot_data %>%
  dplyr::mutate(
    driver = factor(driver, levels = driver_order),
    sensitivity_case = factor(
      sensitivity_case,
      levels = c("Higher-NPV case", "Lower-NPV case")
    )
  )

baseline_caption <- sensitivity_all %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::mutate(
    caption = paste0(
      as.character(project),
      ": baseline mean NPV = ",
      scales::dollar(mean_NPV_per_ha, accuracy = 1),
      "/ha; baseline P(NPV > 0) = ",
      scales::percent(p_NPV_positive, accuracy = 0.1)
    )
  ) %>%
  dplyr::pull(caption) %>%
  paste(collapse = "\n")

x_min <- min(
  plot_data$mean_NPV_thousand_ha,
  baseline_values$baseline_mean_NPV_thousand_ha,
  0,
  na.rm = TRUE
)

x_max <- max(
  plot_data$mean_NPV_thousand_ha,
  baseline_values$baseline_mean_NPV_thousand_ha,
  0,
  na.rm = TRUE
)

x_padding <- (x_max - x_min) * 0.15

x_min <- floor((x_min - x_padding) / 10) * 10
x_max <- ceiling((x_max + x_padding) / 10) * 10

x_breaks <- seq(
  from = x_min,
  to = x_max,
  by = 10
)

bar_position <- position_dodge2(
  width = 0.78,
  preserve = "single"
)

# ============================================================
# 4. Plot sensitivity-check bar chart
# ============================================================

p <- ggplot(
  plot_data,
  aes(
    x = mean_NPV_thousand_ha,
    y = driver,
    group = sensitivity_case
  )
) +
  geom_vline(
    xintercept = 0,
    color = "grey70",
    linewidth = 0.55
  ) +
  geom_vline(
    data = baseline_values,
    aes(xintercept = baseline_mean_NPV_thousand_ha),
    inherit.aes = FALSE,
    color = "grey20",
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_col(
    aes(fill = sensitivity_case),
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
    size = 3.0,
    color = "black",
    vjust = 0.5
  ) +
  facet_wrap(
    ~ project,
    ncol = 1
  ) +
  scale_fill_manual(
    values = sensitivity_colors,
    breaks = c("Higher-NPV case", "Lower-NPV case")
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = scales::label_number(accuracy = 1),
    limits = c(x_min, x_max),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  labs(
    x = "Mean NPV under sensitivity scenario (thousand CAD/ha)",
    y = NULL,
    fill = NULL,
    caption = paste0(
      "Bar labels show scenario level | P(NPV > 0). Dashed vertical lines show baseline mean NPV.\n",
      "Higher-NPV cases: +15% price/yield, -15% cost/freight, or 3% discount rate.\n",
      "lower-NPV cases: -15% price/yield, +15% cost/freight, or 10% discount rate.\n",
      baseline_caption
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.key.width = grid::unit(0.75, "cm"),
    legend.key.height = grid::unit(0.45, "cm"),
    legend.text = element_text(size = 10.5),
    
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12.5),
    
    axis.text = element_text(color = "black", size = 10.5),
    axis.title.x = element_text(size = 12),
    axis.line = element_line(color = "black", linewidth = 0.55),
    axis.ticks = element_line(color = "black", linewidth = 0.45),
    
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      hjust = 0,
      size = 8.8,
      color = "grey25",
      lineheight = 1.1
    ),
    
    plot.margin = margin(8, 40, 8, 8)
  ) +
  coord_cartesian(clip = "off")

print(p)

# ============================================================
# 5. Save figure
# ============================================================

dir.create(
  dirname(output_path),
  showWarnings = FALSE,
  recursive = TRUE
)

ggsave(
  filename = output_path,
  plot = p,
  width = 8.8,
  height = 7.4,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = output_tiff_path,
  plot = p,
  width = 8.8,
  height = 7.4,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)