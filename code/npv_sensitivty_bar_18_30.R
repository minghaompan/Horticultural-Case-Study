# ============================================================
# Mean NPV sensitivity bar chart
# 18-year and 30-year peat extraction projects
#
# Higher and lower settings refer to the parameter value,
# not to the resulting NPV.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# ============================================================
# 1. Settings
# ============================================================

onsite_EF_for_NPV <- 1.4

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

output_png_path <- file.path(
  "figures",
  "npv_sensitivity_bar_18_30_parameter_settings.png"
)


project_levels <- c(
  "18-year project",
  "30-year project"
)

driver_levels <- c(
  "Peat price",
  "Operating cost",
  "Freight cost",
  "Discount rate",
  "Peat yield"
)

case_levels <- c(
  "Higher parameter setting",
  "Lower parameter setting"
)

case_colors <- c(
  "Higher parameter setting" = "#0072B2",
  "Lower parameter setting"  = "#D55E00"
)

# Higher and lower refer to the numerical parameter value.
scenario_lookup <- tibble::tribble(
  ~scenario,              ~driver,           ~scenario_level, ~sensitivity_case,
  "Price +15%",           "Peat price",       "+15%",          "Higher parameter setting",
  "Price -15%",           "Peat price",       "-15%",          "Lower parameter setting",
  "Operating cost +15%",  "Operating cost",   "+15%",          "Higher parameter setting",
  "Operating cost -15%",  "Operating cost",   "-15%",          "Lower parameter setting",
  "Freight +15%",         "Freight cost",     "+15%",          "Higher parameter setting",
  "Freight -15%",         "Freight cost",     "-15%",          "Lower parameter setting",
  "Discount rate 10%",    "Discount rate",    "10%",           "Higher parameter setting",
  "Discount rate 3%",     "Discount rate",    "3%",            "Lower parameter setting",
  "Yield +15%",           "Peat yield",       "+15%",          "Higher parameter setting",
  "Yield -15%",           "Peat yield",       "-15%",          "Lower parameter setting"
)

# ============================================================
# 2. Helper functions
# ============================================================

read_sensitivity_summary <- function(
    path,
    project_label,
    onsite_ef_value = 1.4
) {
  
  df <- utils::read.csv(
    path,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      project = project_label,
      scenario = trimws(scenario)
    )
  
  if ("EF_onsite_tC_ha_yr" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        EF_onsite_tC_ha_yr = as.numeric(EF_onsite_tC_ha_yr)
      ) %>%
      dplyr::filter(
        abs(EF_onsite_tC_ha_yr - onsite_ef_value) < 1e-8
      )
  }
  
  df %>%
    dplyr::distinct(
      project,
      scenario,
      .keep_all = TRUE
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
      strip.text = element_text(
        face = "bold",
        size = 12,
        lineheight = 1.08
      ),
      
      axis.text = element_text(
        color = "black",
        size = 10.5
      ),
      axis.title.x = element_text(size = 12),
      axis.line = element_blank(),
      axis.ticks = element_line(
        color = "grey45",
        linewidth = 0.40
      ),
      
      panel.border = element_rect(
        color = "grey35",
        fill = NA,
        linewidth = 0.70
      ),
      panel.grid.major.x = element_line(
        color = "grey80",
        linewidth = 0.45
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = grid::unit(0.8, "lines"),
      
      plot.caption = element_text(
        hjust = 0,
        size = 9.0,
        color = "grey25",
        lineheight = 1.1
      ),
      
      plot.margin = margin(
        t = 10,
        r = 42,
        b = 14,
        l = 10
      )
    )
}

# ============================================================
# 3. Load sensitivity results
# ============================================================

sensitivity_all <- dplyr::bind_rows(
  read_sensitivity_summary(
    sensitivity_18_path,
    "18-year project",
    onsite_EF_for_NPV
  ),
  read_sensitivity_summary(
    sensitivity_30_path,
    "30-year project",
    onsite_EF_for_NPV
  )
) %>%
  dplyr::mutate(
    project = factor(
      project,
      levels = project_levels
    ),
    mean_NPV_per_ha = as.numeric(mean_NPV_per_ha),
    p_NPV_positive = as.numeric(p_NPV_positive),
    p_NPV_positive = dplyr::if_else(
      p_NPV_positive > 1,
      p_NPV_positive / 100,
      p_NPV_positive
    )
  )

# Check that all required columns are present.
required_columns <- c(
  "project",
  "scenario",
  "mean_NPV_per_ha",
  "p_NPV_positive"
)

missing_columns <- setdiff(
  required_columns,
  names(sensitivity_all)
)

if (length(missing_columns) > 0) {
  stop(
    "The following required columns are missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ============================================================
# 4. Prepare baseline values
# ============================================================

baseline_values <- sensitivity_all %>%
  dplyr::filter(scenario == "Baseline") %>%
  dplyr::transmute(
    project,
    baseline_mean_NPV_per_ha = mean_NPV_per_ha,
    baseline_mean_NPV_thousand_ha = mean_NPV_per_ha / 1000,
    baseline_p_NPV_positive = p_NPV_positive,
    panel = paste0(
      as.character(project),
      "\nBaseline mean NPV = ",
      scales::dollar(
        mean_NPV_per_ha,
        accuracy = 1
      ),
      "/ha",
      "\nBaseline P(NPV > 0) = ",
      scales::percent(
        p_NPV_positive,
        accuracy = 0.1
      )
    )
  )

if (nrow(baseline_values) != length(project_levels)) {
  stop(
    "A single Baseline row must be available for each project."
  )
}

panel_levels <- baseline_values %>%
  dplyr::arrange(project) %>%
  dplyr::pull(panel)

# ============================================================
# 5. Check sensitivity scenario names
# ============================================================

unmatched_scenarios <- sensitivity_all %>%
  dplyr::filter(scenario != "Baseline") %>%
  dplyr::distinct(scenario) %>%
  dplyr::anti_join(
    scenario_lookup,
    by = "scenario"
  )

if (nrow(unmatched_scenarios) > 0) {
  stop(
    paste0(
      "The following scenarios do not match scenario_lookup: ",
      paste(unmatched_scenarios$scenario, collapse = ", ")
    )
  )
}

# ============================================================
# 6. Prepare plotting data
# ============================================================

plot_data <- sensitivity_all %>%
  dplyr::filter(scenario != "Baseline") %>%
  dplyr::left_join(
    baseline_values,
    by = "project"
  ) %>%
  dplyr::left_join(
    scenario_lookup,
    by = "scenario"
  ) %>%
  dplyr::mutate(
    mean_NPV_thousand_ha = mean_NPV_per_ha / 1000,
    
    # Bar labels show the probability that NPV is positive.
    label = scales::percent(
      p_NPV_positive,
      accuracy = 0.1
    ),
    
    # Place labels outside the ends of the bars.
    label_x = dplyr::if_else(
      mean_NPV_thousand_ha >= 0,
      mean_NPV_thousand_ha + 1.5,
      mean_NPV_thousand_ha - 1.5
    ),
    
    label_hjust = dplyr::if_else(
      mean_NPV_thousand_ha >= 0,
      0,
      1
    ),
    
    driver = factor(
      driver,
      levels = rev(driver_levels)
    ),
    
    sensitivity_case = factor(
      sensitivity_case,
      levels = case_levels
    ),
    
    panel = factor(
      panel,
      levels = panel_levels
    )
  )

# ============================================================
# 7. Set x-axis limits
# ============================================================

all_x_values <- c(
  plot_data$mean_NPV_thousand_ha,
  plot_data$label_x,
  baseline_values$baseline_mean_NPV_thousand_ha,
  0
)

x_min_raw <- min(all_x_values, na.rm = TRUE)
x_max_raw <- max(all_x_values, na.rm = TRUE)

x_range <- x_max_raw - x_min_raw

if (x_range == 0) {
  x_range <- 10
}

x_padding <- x_range * 0.10

x_min <- floor(
  (x_min_raw - x_padding) / 10
) * 10

x_max <- ceiling(
  (x_max_raw + x_padding) / 10
) * 10

x_breaks <- seq(
  x_min,
  x_max,
  by = 10
)

bar_position <- position_dodge2(
  width = 0.78,
  preserve = "single"
)

# ============================================================
# 8. Plot
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
    color = "grey60",
    linewidth = 0.55
  ) +
  geom_vline(
    data = baseline_values,
    aes(
      xintercept = baseline_mean_NPV_thousand_ha
    ),
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
    ~ panel,
    ncol = 1
  ) +
  scale_fill_manual(
    values = case_colors,
    breaks = case_levels,
    limits = case_levels,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = x_breaks,
    labels = scales::label_number(
      accuracy = 1
    ),
    limits = c(x_min, x_max),
    expand = expansion(
      mult = c(0.03, 0.03)
    )
  ) +
  labs(
    x = "Mean NPV under sensitivity scenario (thousand CAD/ha)",
    y = NULL,
    fill = NULL,
    caption = paste0(
      "Bar labels show P(NPV > 0). Dashed vertical lines show baseline mean NPV.\n",
      "Lower parameter setting: -15% for peat price, operating cost, freight cost, ",
      "and peat yield; 3% for the discount rate.\n",
      "Higher parameter setting: +15% for peat price, operating cost, freight cost, ",
      "and peat yield; 10% for the discount rate."
    )
  ) +
  common_plot_theme() +
  coord_cartesian(
    clip = "off"
  )

print(p)

# ============================================================
# 9. Save figure
# ============================================================

dir.create(
  dirname(output_png_path),
  showWarnings = FALSE,
  recursive = TRUE
)

ggsave(
  filename = output_png_path,
  plot = p,
  width = 8.8,
  height = 8.0,
  units = "in",
  dpi = 300,
  bg = "white"
)
