# ============================================================
# 06_Mean NPV sensitivity bar chart
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


# ------------------------------------------------------------
# Factor levels
# ------------------------------------------------------------

project_levels <- c(
  "18-year project",
  "30-year project"
)

driver_levels <- c(
  "Peat price",
  "Operating cost",
  "Discount rate",
  "Freight cost",
  "Peat yield"
)

case_levels <- c(
  "Higher parameter setting",
  "Lower parameter setting"
)


# ------------------------------------------------------------
# Colors
# ------------------------------------------------------------

case_colors <- c(
  "Higher parameter setting" = "#0072B2",
  "Lower parameter setting"  = "#D55E00"
)


# ============================================================
# 2. Scenario lookup
# ============================================================

scenario_lookup <- tibble::tribble(
  
  ~scenario,             ~driver,          ~scenario_level, ~sensitivity_case,
  
  "Price +15%",          "Peat price",      "+15%",          "Higher parameter setting",
  "Price -15%",          "Peat price",      "-15%",          "Lower parameter setting",
  
  "Operating cost +15%", "Operating cost",  "+15%",          "Higher parameter setting",
  "Operating cost -15%", "Operating cost",  "-15%",          "Lower parameter setting",
  
  "Freight +15%",        "Freight cost",    "+15%",          "Higher parameter setting",
  "Freight -15%",        "Freight cost",    "-15%",          "Lower parameter setting",
  
  "Discount rate 10%",   "Discount rate",   "10%",           "Higher parameter setting",
  "Discount rate 3%",    "Discount rate",   "3%",            "Lower parameter setting",
  
  "Yield +15%",          "Peat yield",      "+15%",          "Higher parameter setting",
  "Yield -15%",          "Peat yield",      "-15%",          "Lower parameter setting"
)


# ============================================================
# 3. Helper function
# ============================================================

read_sensitivity_summary <- function(
    path,
    project_label,
    onsite_ef_value = 1.4
) {
  
  if (!file.exists(path)) {
    stop(
      paste0(
        "File not found:\n",
        path
      )
    )
  }
  
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
        EF_onsite_tC_ha_yr =
          as.numeric(EF_onsite_tC_ha_yr)
      ) %>%
      dplyr::filter(
        abs(
          EF_onsite_tC_ha_yr -
            onsite_ef_value
        ) < 1e-8
      )
  }
  
  df %>%
    dplyr::distinct(
      project,
      scenario,
      .keep_all = TRUE
    )
}


# ============================================================
# 4. Load sensitivity results
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
    
    mean_NPV_per_ha =
      as.numeric(mean_NPV_per_ha),
    
    p_NPV_positive =
      as.numeric(p_NPV_positive),
    
    # Convert 99.2 to 0.992 if stored as percentage
    p_NPV_positive =
      dplyr::if_else(
        p_NPV_positive > 1,
        p_NPV_positive / 100,
        p_NPV_positive
      )
  )


# ============================================================
# 5. Check columns
# ============================================================

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
    paste0(
      "Missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 6. Baseline values
# ============================================================

baseline_values <- sensitivity_all %>%
  dplyr::filter(
    scenario == "Baseline"
  ) %>%
  dplyr::transmute(
    
    project,
    
    baseline_mean_NPV_per_ha =
      mean_NPV_per_ha,
    
    baseline_mean_NPV_thousand_ha =
      mean_NPV_per_ha / 1000,
    
    baseline_p_NPV_positive =
      p_NPV_positive,
    
    # Compact panel title
    panel = paste0(
      
      as.character(project),
      
      "\nBaseline mean NPV = ",
      
      scales::number(
        mean_NPV_per_ha / 1000,
        accuracy = 0.1
      ),
      
      "k/ha",
      
      "  |  P(NPV > 0) = ",
      
      scales::percent(
        p_NPV_positive,
        accuracy = 0.1
      )
    )
  )


if (nrow(baseline_values) != 2) {
  
  stop(
    "A single Baseline row must be available for each project."
  )
}


panel_levels <- baseline_values %>%
  dplyr::arrange(project) %>%
  dplyr::pull(panel)


# ============================================================
# 7. Check scenario names
# ============================================================

unmatched_scenarios <- sensitivity_all %>%
  dplyr::filter(
    scenario != "Baseline"
  ) %>%
  dplyr::distinct(
    scenario
  ) %>%
  dplyr::anti_join(
    scenario_lookup,
    by = "scenario"
  )


if (nrow(unmatched_scenarios) > 0) {
  
  stop(
    paste0(
      "Scenarios not matched in scenario_lookup: ",
      paste(
        unmatched_scenarios$scenario,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 8. Prepare plotting data
# ============================================================

plot_data <- sensitivity_all %>%
  
  dplyr::filter(
    scenario != "Baseline"
  ) %>%
  
  dplyr::left_join(
    baseline_values,
    by = "project"
  ) %>%
  
  dplyr::left_join(
    scenario_lookup,
    by = "scenario"
  ) %>%
  
  dplyr::mutate(
    
    # Mean NPV in $k/ha
    mean_NPV_thousand_ha =
      mean_NPV_per_ha / 1000,
    
    
    # --------------------------------------------------------
    # Short bar labels
    # --------------------------------------------------------
    
    label = scales::percent(
      p_NPV_positive,
      accuracy = 0.1
    ),
    
    
    # --------------------------------------------------------
    # Put labels just beyond bar ends
    # --------------------------------------------------------
    
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
    
    
    # --------------------------------------------------------
    # Factor order
    # --------------------------------------------------------
    
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
# 9. Common x-axis
# ============================================================

all_x_values <- c(
  plot_data$mean_NPV_thousand_ha,
  plot_data$label_x,
  baseline_values$baseline_mean_NPV_thousand_ha,
  0
)

x_min_raw <- min(
  all_x_values,
  na.rm = TRUE
)

x_max_raw <- max(
  all_x_values,
  na.rm = TRUE
)

x_range <- x_max_raw - x_min_raw

if (x_range <= 0) {
  x_range <- 10
}


# Give enough room for labels
x_padding_left  <- x_range * 0.07
x_padding_right <- x_range * 0.10


x_min <- floor(
  (x_min_raw - x_padding_left) / 10
) * 10

x_max <- ceiling(
  (x_max_raw + x_padding_right) / 10
) * 10


x_breaks <- seq(
  x_min,
  x_max,
  by = 20
)


# ============================================================
# 10. Bar position
# ============================================================

bar_position <- position_dodge2(
  width = 0.76,
  preserve = "single",
  padding = 0.10
)


# ============================================================
# 11. Plot
# ============================================================

p <- ggplot(
  plot_data,
  aes(
    x = mean_NPV_thousand_ha,
    y = driver,
    group = sensitivity_case
  )
) +
  
  # ----------------------------------------------------------
# Zero reference
# ----------------------------------------------------------

geom_vline(
  xintercept = 0,
  color = "grey60",
  linewidth = 0.50
) +
  
  
  # ----------------------------------------------------------
# Baseline mean NPV
# ----------------------------------------------------------

geom_vline(
  data = baseline_values,
  aes(
    xintercept =
      baseline_mean_NPV_thousand_ha
  ),
  inherit.aes = FALSE,
  color = "grey20",
  linewidth = 0.80,
  linetype = "dashed"
) +
  
  
  # ----------------------------------------------------------
# Bars
# ----------------------------------------------------------

geom_col(
  aes(
    fill = sensitivity_case
  ),
  width = 0.60,
  orientation = "y",
  position = bar_position
) +
  
  
  # ----------------------------------------------------------
# Probability labels
# ----------------------------------------------------------

geom_text(
  aes(
    x = label_x,
    label = label,
    hjust = label_hjust
  ),
  position = bar_position,
  size = 3.35,
  color = "grey15",
  vjust = 0.5
) +
  
  
  # ----------------------------------------------------------
# Panels
# ----------------------------------------------------------

facet_wrap(
  ~ panel,
  ncol = 2
) +
  
  
  # ----------------------------------------------------------
# Fill colors
# ----------------------------------------------------------

scale_fill_manual(
  values = case_colors,
  breaks = case_levels,
  limits = case_levels,
  drop = FALSE
) +
  
  
  # ----------------------------------------------------------
# X-axis
# ----------------------------------------------------------

scale_x_continuous(
  breaks = x_breaks,
  labels = scales::label_number(
    accuracy = 1
  ),
  limits = c(
    x_min,
    x_max
  ),
  expand = expansion(
    mult = c(
      0.01,
      0.01
    )
  )
) +
  
  
  # ----------------------------------------------------------
# Labels
# ----------------------------------------------------------

labs(
  x = "Mean NPV under sensitivity scenario ($k/ha)",
  y = NULL,
  fill = NULL,
  
  caption = paste0(
    "Bar labels show P(NPV > 0); dashed vertical lines show baseline mean NPV.\n",
    "Lower setting: −15% for peat price, operating cost, freight cost, and peat yield; ",
    "discount rate = 3%.\n",
    "Higher setting: +15% for peat price, operating cost, freight cost, and peat yield; ",
    "discount rate = 10%."
  )
) +
  
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_classic(
  base_size = 12
) +
  
  theme(
    
    # Legend
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    
    legend.key = element_blank(),
    
    legend.key.width =
      grid::unit(
        0.85,
        "cm"
      ),
    
    legend.key.height =
      grid::unit(
        0.45,
        "cm"
      ),
    
    legend.text = element_text(
      size = 10.5
    ),
    
    
    # Subtitle
    plot.subtitle = element_text(
      size = 10,
      color = "grey30",
      hjust = 0.5,
      margin = margin(
        b = 10
      )
    ),
    
    
    # Panel headings
    strip.background = element_rect(
      fill = "grey97",
      color = NA
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11.5,
      lineheight = 1.10,
      margin = margin(
        t = 7,
        r = 5,
        b = 7,
        l = 5
      )
    ),
    
    
    # Axes
    axis.text.x = element_text(
      color = "black",
      size = 9.5
    ),
    
    axis.text.y = element_text(
      color = "black",
      size = 10.5
    ),
    
    axis.title.x = element_text(
      size = 11.5,
      margin = margin(
        t = 8
      )
    ),
    
    axis.line = element_blank(),
    
    axis.ticks = element_blank(),
    
    
    # Panel borders and grids
    panel.border = element_rect(
      color = "grey45",
      fill = NA,
      linewidth = 0.65
    ),
    
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.40
    ),
    
    panel.grid.major.y =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    panel.spacing.x =
      grid::unit(
        1.0,
        "lines"
      ),
    
    
    # Caption
    plot.caption = element_text(
      hjust = 0,
      size = 9.0,
      color = "grey30",
      lineheight = 1.15,
      margin = margin(
        t = 10
      )
    ),
    
    
    # Margins
    plot.margin = margin(
      t = 8,
      r = 24,
      b = 10,
      l = 12
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  )


# ============================================================
# 12. Display
# ============================================================

print(p)


# ============================================================
# 13. Save
# ============================================================

dir.create(
  dirname(output_png_path),
  showWarnings = FALSE,
  recursive = TRUE
)

ggsave(
  filename = output_png_path,
  plot = p,
  width = 13.0,
  height = 6.2,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat(
  "\n--- Figure saved ---\n",
  output_png_path,
  "\n"
)