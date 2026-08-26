
#Using income data

rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

setwd("C:/Users/rhianna.sookhy/OneDrive - The Health Foundation/Shortcuts/Analysis - 11-CAT/1. Work programme/Healthy Life Expectancy - strategy launch/Phase 2")



lemsoa <- read_excel(
  "Raw data/hslemsoa.xlsx",
  sheet = "1",
  skip = 6
)

hslemsoa <- read_excel(
  "Raw data/hslemsoa.xlsx",
  sheet = "2",
  skip = 6
)



MSOA_Region_2021 <- read_csv(
  "Raw data/MOSa_Region_2021.csv"
) %>%
  select(
    MSOA21CD,
    RGN22CD,
    RGN22NM
  ) %>%
  distinct(
    MSOA21CD,
    .keep_all = TRUE
  )



le_2021 <- lemsoa %>%
  filter(
    # Country == "England",
    `Area type` == "MSOA",
    Sex %in% c("Male", "Female")
  ) %>%
  select(
    Period,
    Country,
    `Area type`,
    `Area code`,
    `Area name`,
    Sex,
    `Sex code`,
    LE,
    LCI,
    UCI
  ) %>%
  rename(
    MSOA21CD = `Area code`,
    MSOA21NM = `Area name`,
    LE_2021 = LE,
    LE_2021_LCI = LCI,
    LE_2021_UCI = UCI
  )



hle_2021 <- hslemsoa %>%
  filter(
    # Country == "England",
    `Area type` == "MSOA",
    Sex %in% c("Male", "Female")
  ) %>%
  select(
    Period,
    Country,
    `Area type`,
    `Area code`,
    `Area name`,
    Sex,
    `Sex code`,
    HLE,
    LCI,
    UCI,
    `Proportion (%)`
  ) %>%
  rename(
    MSOA21CD = `Area code`,
    MSOA21NM = `Area name`,
    HLE_2021 = HLE,
    HLE_2021_LCI = LCI,
    HLE_2021_UCI = UCI
  )


# Check number of observations

nrow(le_2021)
nrow(hle_2021)



health_2021 <- le_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    Sex,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI
  ) %>%
  left_join(
    hle_2021 %>%
      select(
        MSOA21CD,
        Sex,
        HLE_2021,
        HLE_2021_LCI,
        HLE_2021_UCI
      ),
    by = c(
      "MSOA21CD",
      "Sex"
    )
  ) %>%
  left_join(
    MSOA_Region_2021,
    by = "MSOA21CD"
  )




##import income data (Will average 3 most recent years)
library(readxl)

msoa_income_2023 <- read_excel("Raw data/msoa_income_estimate_2023.xlsx", 
                                   skip = 3,
                                   sheet = "Net income after housing costs")

msoa_income_2020 <- read_excel("Raw data/msoa_income_estimate_2020.xlsx", 
                               skip = 3,
                               sheet = "Net income after housing costs")

msoa_income_2018 <- read_excel("Raw data/msoa_income_estimate_2018.xls", 
                               skip = 3,
                               sheet = "Net income after housing costs")


income_2023 <- msoa_income_2023 %>%
  select(
    `MSOA code`,
    `MSOA name`,
    `Local authority code`,
    `Local authority name`,
    `Region code`,
    `Region name`,
    `Disposable (net) annual income after housing costs (£)`,
    `Upper confidence limit (£)`,
    `Lower confidence limit (£)`,
    `Confidence interval (£)`
  ) %>%
  rename(
    income_2023 = `Disposable (net) annual income after housing costs (£)`,
    upper_ci_2023 = `Upper confidence limit (£)`,
    lower_ci_2023 = `Lower confidence limit (£)`,
    confidence_interval_2023 = `Confidence interval (£)`
  )

income_2020 <- msoa_income_2020 %>%
  select(
    `MSOA code`,
    `Net annual income after housing costs (£)`,
    `Upper confidence limit (£)`,
    `Lower confidence limit (£)`,
    `Confidence interval (£)`
  ) %>%
  rename(
    income_2020 = `Net annual income after housing costs (£)`,
    upper_ci_2020 = `Upper confidence limit (£)`,
    lower_ci_2020 = `Lower confidence limit (£)`,
    confidence_interval_2020 = `Confidence interval (£)`
  )

income_2018 <- msoa_income_2018 %>%
  select(
    `MSOA code`,
    `Net annual income after housing costs (£)`,
    `Upper confidence limit (£)`,
    `Lower confidence limit (£)`,
    `Confidence interval (£)`
  ) %>%
  rename(
    income_2018 = `Net annual income after housing costs (£)`,
    upper_ci_2018 = `Upper confidence limit (£)`,
    lower_ci_2018 = `Lower confidence limit (£)`,
    confidence_interval_2018 = `Confidence interval (£)`
  )


# -----------------------------
# Join the three years
# -----------------------------

msoa_income_combined <- income_2023 %>%
  inner_join(income_2020, by = "MSOA code") %>%
  inner_join(income_2018, by = "MSOA code")


# -----------------------------
# Calculate 3-year average
# -----------------------------

msoa_income_combined <- msoa_income_combined %>%
  mutate(
    income_average_3_years = rowMeans(
      across(c(income_2018, income_2020, income_2023)),
      na.rm = FALSE
    ),
    
    confidence_interval_average_3_years = rowMeans(
      across(c(
        confidence_interval_2018,
        confidence_interval_2020,
        confidence_interval_2023
      )),
      na.rm = FALSE
    )
  )


library(dplyr)

regional_income_summary <- msoa_income_combined %>%
  group_by(`Region code`, `Region name`) %>%
  summarise(
    
    # CI summary statistics
    CI_min = min(
      confidence_interval_average_3_years,
      na.rm = TRUE
    ),
    
    CI_max = max(
      confidence_interval_average_3_years,
      na.rm = TRUE
    ),
    
    CI_median = median(
      confidence_interval_average_3_years,
      na.rm = TRUE
    ),
    
    CI_mean = mean(
      confidence_interval_average_3_years,
      na.rm = TRUE
    ),
    
    # Number of MSOAs
    n_MSOA = sum(
      !is.na(confidence_interval_average_3_years)
    ),
    
    # CI > £20,000
    CI_over_20k_n = sum(
      confidence_interval_average_3_years > 20000,
      na.rm = TRUE
    ),
    
    CI_over_20k_prop = mean(
      confidence_interval_average_3_years > 20000,
      na.rm = TRUE
    ),
    
    # CI £15,000–£20,000
    CI_15k_20k_n = sum(
      confidence_interval_average_3_years >= 15000 &
        confidence_interval_average_3_years <= 20000,
      na.rm = TRUE
    ),
    
    CI_15k_20k_prop = mean(
      confidence_interval_average_3_years >= 15000 &
        confidence_interval_average_3_years <= 20000,
      na.rm = TRUE
    ),
    
    # CI £10,000–£15,000
    CI_10k_15k_n = sum(
      confidence_interval_average_3_years >= 10000 &
        confidence_interval_average_3_years < 15000,
      na.rm = TRUE
    ),
    
    CI_10k_15k_prop = mean(
      confidence_interval_average_3_years >= 10000 &
        confidence_interval_average_3_years < 15000,
      na.rm = TRUE
    ),
    
    # CI £5,000–£10,000
    CI_5k_10k_n = sum(
      confidence_interval_average_3_years >= 5000 &
        confidence_interval_average_3_years < 10000,
      na.rm = TRUE
    ),
    
    CI_5k_10k_prop = mean(
      confidence_interval_average_3_years >= 5000 &
        confidence_interval_average_3_years < 10000,
      na.rm = TRUE
    ),
    
    # CI < £5,000
    CI_under_5k_n = sum(
      confidence_interval_average_3_years < 5000,
      na.rm = TRUE
    ),
    
    CI_under_5k_prop = mean(
      confidence_interval_average_3_years < 5000,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# Create plotting dataset with both sexes combined

# Keep only MSOA code and 3-year average income
income_average <- msoa_income_combined %>%
  select(
    `MSOA code`,
    income_average_3_years
  ) %>%
  rename(
    MSOA21CD = `MSOA code`
  )


# Join average income to health data
health_2021 <- health_2021 %>%
  left_join(
    income_average,
    by = "MSOA21CD"
  )


# Create the analysis dataset
health_income_long <- health_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    RGN22NM,
    Sex,
    income_average_3_years,
    LE_2021,
    HLE_2021
  ) %>%
  drop_na()

# Function for Marmot curve plots

plot_income_curve <- function(data, outcome, y_label, title_text) {
  
  ggplot(
    data,
    aes(
      x = income_average_3_years,
      y = .data[[outcome]]
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    labs(
      x = "Disposable annual income after housing costs (£)",
      y = y_label,
      title = title_text
    ) +
    
    theme_minimal()
}


# Life expectancy vs income

LE_income_plot <- plot_income_curve(
  health_income_long,
  "LE_2021",
  "Life expectancy (years)",
  "Life expectancy vs income (2023)"
)


# Healthy life expectancy vs income

HLE_income_plot <- plot_income_curve(
  health_income_long,
  "HLE_2021",
  "Healthy life expectancy (years)",
  "Healthy life expectancy vs income (2023)"
)


# Display plots

LE_income_plot
HLE_income_plot

#Create income percentiles

# Create MSOA-level income percentile

income_percentiles <- health_2021 %>%
  select(
    MSOA21CD,
    income_average_3_years
  ) %>%
  distinct() %>%
  arrange(income_average_3_years) %>%
  mutate(
    income_percentile = 1 + 99 * (row_number() - 1) / (n() - 1)
  )


# Add the same percentile to Male and Female observations

income_percentile_data <- health_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    RGN22NM,
    Sex,
    income_average_3_years,
    
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI
  ) %>%
  drop_na(
    income_average_3_years,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI
  ) %>%
  left_join(
    income_percentiles %>%
      select(MSOA21CD, income_percentile),
    by = "MSOA21CD"
  )

#Plotting function

plot_income_percentile <- function(data, outcome, y_label, title_text) {
  
  ggplot(
    data,
    aes(
      x = income_percentile,
      y = .data[[outcome]]
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(1,100)
    ) +
    
    labs(
      x = "Income percentile (1 = lowest income, 100 = highest income)",
      y = y_label,
      title = title_text
    ) +
    
    theme_minimal()
}


# Overall percentile plots

LE_income_percentile_plot <- plot_income_percentile(
  income_percentile_data,
  "LE_2021",
  "Life expectancy (years)",
  "Life expectancy by income percentile (Avg 2018/20/23 income)"
)


HLE_income_percentile_plot <- plot_income_percentile(
  income_percentile_data,
  "HLE_2021",
  "Healthy life expectancy (years)",
  "Healthy life expectancy by income percentile (Avg 2018/20/23 income)"
)


LE_income_percentile_plot
HLE_income_percentile_plot

library(ggplot2)
library(dplyr)


marmot_plot_sex_measure <- function(data,
                                    measure_name,
                                    sex_name,
                                    title_text = NULL) {
  
  
  if (measure_name == "Life expectancy") {
    
    estimate_var <- "LE_2021"
    lower_var    <- "LE_2021_LCI"
    upper_var    <- "LE_2021_UCI"
    
  } else if (measure_name == "Healthy life expectancy") {
    
    estimate_var <- "HLE_2021"
    lower_var    <- "HLE_2021_LCI"
    upper_var    <- "HLE_2021_UCI"
    
  } else {
    
    stop(
      "measure_name must be 'Life expectancy' or 'Healthy life expectancy'"
    )
  }
  
  
  
  plot_data <- data %>%
    filter(
      Sex == sex_name,
      !is.na(income_percentile),
      !is.na(.data[[estimate_var]]),
      !is.na(.data[[lower_var]]),
      !is.na(.data[[upper_var]])
    )
  
  

  
  if (is.null(title_text)) {
    title_text <- paste(measure_name, "-", sex_name)
  }
  

  
  p <- ggplot(
    plot_data,
    aes(
      x = income_percentile,
      y = .data[[estimate_var]]
    )
  ) +

  geom_point(
    alpha = 0.25,
    size = 0.9,
    colour = "black"
  ) +
    

  
  geom_smooth(
    aes(
      y = .data[[estimate_var]]
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    linewidth = 1.3,
    colour = "black"
  ) +

  
  geom_smooth(
    aes(
      y = .data[[lower_var]],
      colour = "Lower 95% CI"
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    linetype = "dashed",
    linewidth = 1.1
  ) +
    

  
  geom_smooth(
    aes(
      y = .data[[upper_var]],
      colour = "Upper 95% CI"
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    linetype = "dashed",
    linewidth = 1.1
  ) +
    

  
  scale_colour_manual(
    values = c(
      "Lower 95% CI" = "#0072B2",
      "Upper 95% CI" = "#D55E00"
    )
  ) +
    

  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(10, 100, 10),
    minor_breaks = seq(1, 100, 1)
  ) +
    

  
  labs(
    x = "Income percentile\n(1 = lowest income, 100 = highest income)",
    y = "Years",
    colour = "95% confidence interval",
    title = title_text
  ) +
    
    
    theme_minimal()
  
  
  return(p)
}



LE_male_plot <- marmot_plot_sex_measure(
  data = income_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Male",
  title_text = "Life expectancy by income percentile (net income after housing cost) - Male"
)


LE_female_plot <- marmot_plot_sex_measure(
  data = income_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Female",
  title_text = "Life expectancy by income percentile (net income after housing cost) - Female"
)




HLE_male_plot <- marmot_plot_sex_measure(
  data = income_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  title_text = "Healthy life expectancy by income percentile (net income after housing cost) - Male"
)




HLE_female_plot <- marmot_plot_sex_measure(
  data = income_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  title_text = "Healthy life expectancy by income percentile (net income after housing cost) - Female"
)


# Display
LE_male_plot
LE_female_plot

HLE_male_plot
HLE_female_plot




## All regions data 

library(tidyverse)


# Regional Marmot plot function BY SEX


marmot_plot_by_region_sex <- function(data,
                                      measure_name,
                                      sex_name,
                                      title_text) {
  
  # ----------------------------------------------------------
  # Select the appropriate outcome variable
  # ----------------------------------------------------------
  
  y_variable <- case_when(
    measure_name == "Life expectancy" ~ "LE_2021",
    measure_name == "Healthy life expectancy" ~ "HLE_2021",
    TRUE ~ NA_character_
  )
  
  if (is.na(y_variable)) {
    stop(
      "measure_name must be 'Life expectancy' or 'Healthy life expectancy'"
    )
  }
  
  
  # ----------------------------------------------------------
  # Filter data
  # ----------------------------------------------------------
  
  plot_data <- data %>%
    filter(
      Sex == sex_name,
      !is.na(RGN22NM),
      !is.na(income_percentile),
      !is.na(.data[[y_variable]])
    )
  
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  ggplot(
    plot_data,
    aes(
      x = income_percentile,
      y = .data[[y_variable]],
      colour = RGN22NM
    )
  ) +
    
    # --------------------------------------------------------
  # MSOA points
  # --------------------------------------------------------
  
  geom_point(
    alpha = 0.25,
    size = 0.85
  ) +
    
    
    # --------------------------------------------------------
  # Regional LOESS curves
  # --------------------------------------------------------
  
  geom_smooth(
    aes(
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.2
  ) +
    
    
    # --------------------------------------------------------
  # X-axis
  # --------------------------------------------------------
  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(10, 100, 10),
    expand = c(0, 0)
  ) +
    
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    x = "Income percentile\n(1 = lowest income, 100 = highest income)",
    y = "Years",
    colour = "Region",
    title = title_text
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}



regional_LE_male_plot <- marmot_plot_by_region_sex(
  income_percentile_data,
  "Life expectancy",
  "Male",
  "Life expectancy by income percentile and region - Male"
)



regional_LE_female_plot <- marmot_plot_by_region_sex(
  income_percentile_data,
  "Life expectancy",
  "Female",
  "Life expectancy by income percentile and region - Female"
)



regional_HLE_male_plot <- marmot_plot_by_region_sex(
  income_percentile_data,
  "Healthy life expectancy",
  "Male",
  "Healthy life expectancy by income percentile and region - Male"
)




regional_HLE_female_plot <- marmot_plot_by_region_sex(
  income_percentile_data,
  "Healthy life expectancy",
  "Female",
  "Healthy life expectancy by income percentile and region - Female"
)

#Display

regional_LE_male_plot

regional_LE_female_plot

regional_HLE_male_plot

regional_HLE_female_plot


# By regional income estimate percentiles

library(tidyverse)

regional_income_percentiles <- health_2021 %>%
  select(
    MSOA21CD,
    RGN22NM,
    income_average_3_years
  ) %>%
  distinct() %>%
  drop_na(
    RGN22NM,
    income_average_3_years
  ) %>%
  
  group_by(RGN22NM) %>%
  
  arrange(
    income_average_3_years,
    .by_group = TRUE
  ) %>%
  
  mutate(
    regional_income_percentile =
      1 + 99 * (row_number() - 1) / (n() - 1)
  ) %>%
  
  ungroup()


regional_percentile_data <- health_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    RGN22NM,
    Sex,
    
    income_average_3_years,
    
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI
  ) %>%
  
  drop_na(
    income_average_3_years,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI
  ) %>%
  
  left_join(
    regional_income_percentiles %>%
      select(
        MSOA21CD,
        regional_income_percentile
      ),
    by = "MSOA21CD"
  )



# ============================================================
# REGIONAL MARMOT PLOT FUNCTION BY SEX


marmot_plot_regional_percentile <- function(data,
                                            measure_name,
                                            sex_name,
                                            title_text) {
  
  # ----------------------------------------------------------
  # Select outcome variable
  # ----------------------------------------------------------
  
  y_variable <- case_when(
    measure_name == "Life expectancy" ~ "LE_2021",
    measure_name == "Healthy life expectancy" ~ "HLE_2021",
    TRUE ~ NA_character_
  )
  
  if (is.na(y_variable)) {
    stop(
      "measure_name must be 'Life expectancy' or 'Healthy life expectancy'"
    )
  }
  
  
  # ----------------------------------------------------------
  # Filter data
  # ----------------------------------------------------------
  
  plot_data <- data %>%
    filter(
      Sex == sex_name,
      !is.na(RGN22NM),
      !is.na(regional_income_percentile),
      !is.na(.data[[y_variable]])
    )
  
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  ggplot(
    plot_data,
    aes(
      x = regional_income_percentile,
      y = .data[[y_variable]],
      colour = RGN22NM
    )
  ) +
    
    # --------------------------------------------------------
  # MSOA points
  # --------------------------------------------------------
  
  geom_point(
    alpha = 0.25,
    size = 0.85
  ) +
    
    
    # --------------------------------------------------------
  # Regional LOESS curves
  # --------------------------------------------------------
  
  geom_smooth(
    aes(
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.2
  ) +
    
    
    # --------------------------------------------------------
  # X-axis
  # --------------------------------------------------------
  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(10, 100, 10),
    expand = c(0, 0)
  ) +
    
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    x = "Regional income percentile\n(1 = lowest income, 100 = highest income within region)",
    y = "Years",
    colour = "Region",
    title = title_text
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}





regional_LE_male_plot <- marmot_plot_regional_percentile(
  data = regional_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Male",
  title_text = "Life expectancy by regional income percentile - Male"
)


regional_LE_female_plot <- marmot_plot_regional_percentile(
  data = regional_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Female",
  title_text = "Life expectancy by regional income percentile - Female"
)

regional_HLE_male_plot <- marmot_plot_regional_percentile(
  data = regional_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  title_text = "Healthy life expectancy by regional income percentile - Male"
)

regional_HLE_female_plot <- marmot_plot_regional_percentile(
  data = regional_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  title_text = "Healthy life expectancy by regional income percentile - Female"
)


regional_LE_male_plot
regional_LE_female_plot

regional_HLE_male_plot
regional_HLE_female_plot





##regional comparison

library(tidyverse)

# ============================================================
# MARMOT PLOT FUNCTION
# Region comparison + sex + measure + percentile type
# ============================================================

marmot_plot_region_comparison_ci <- function(
    data,
    measure_name,
    sex_name,
    region_1,
    region_2,
    percentile_variable,
    title_text
) {
  
  # ----------------------------------------------------------
  # Select outcome and CI variables
  # ----------------------------------------------------------
  
  if (measure_name == "Life expectancy") {
    
    estimate_var <- "LE_2021"
    lower_var    <- "LE_2021_LCI"
    upper_var    <- "LE_2021_UCI"
    
  } else if (measure_name == "Healthy life expectancy") {
    
    estimate_var <- "HLE_2021"
    lower_var    <- "HLE_2021_LCI"
    upper_var    <- "HLE_2021_UCI"
    
  } else {
    
    stop(
      "measure_name must be 'Life expectancy' or 'Healthy life expectancy'"
    )
  }
  
  
  # ----------------------------------------------------------
  # Check percentile variable
  # ----------------------------------------------------------
  
  if (!percentile_variable %in% names(data)) {
    stop(
      paste(
        "The percentile variable",
        percentile_variable,
        "does not exist in the data."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Filter to the two regions and selected sex
  # ----------------------------------------------------------
  
  plot_data <- data %>%
    filter(
      RGN22NM %in% c(region_1, region_2),
      Sex == sex_name,
      !is.na(.data[[percentile_variable]]),
      !is.na(.data[[estimate_var]]),
      !is.na(.data[[lower_var]]),
      !is.na(.data[[upper_var]])
    )
  
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  ggplot(
    plot_data,
    aes(
      x = .data[[percentile_variable]]
    )
  ) +
    
    # ========================================================
  # MAIN ESTIMATE POINTS
  # ========================================================
  
  geom_point(
    aes(
      y = .data[[estimate_var]],
      colour = RGN22NM
    ),
    alpha = 0.25,
    size = 0.8
  ) +
    
    
    # ========================================================
  # MAIN ESTIMATE — SOLID LOESS
  # ========================================================
  
  geom_smooth(
    aes(
      y = .data[[estimate_var]],
      colour = RGN22NM,
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.4,
    linetype = "solid"
  ) +
    
    
    # ========================================================
  # LOWER CI — DASHED LOESS
  # ========================================================
  
  geom_smooth(
    aes(
      y = .data[[lower_var]],
      colour = RGN22NM,
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.1,
    linetype = "dashed"
  ) +
    
    
    # ========================================================
  # UPPER CI — DASHED LOESS
  # ========================================================
  
  geom_smooth(
    aes(
      y = .data[[upper_var]],
      colour = RGN22NM,
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.1,
    linetype = "dashed"
  ) +
    
    
    # ========================================================
  # COLOURS
  # ========================================================
  
  scale_colour_manual(
    values = setNames(
      c("#D55E00", "#0072B2"),
      c(region_1, region_2)
    )
  ) +
    
    
    # ========================================================
  # X AXIS
  # ========================================================
  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(10, 100, 10),
    expand = c(0, 0)
  ) +
    
    
    # ========================================================
  # LABELS
  # ========================================================
  
  labs(
    x = ifelse(
      percentile_variable == "income_percentile",
      "National income percentile\n(1 = lowest income, 100 = highest income)",
      "Regional income percentile\n(1 = lowest income, 100 = highest income within region)"
    ),
    y = "Years",
    colour = "Region",
    title = title_text
  ) +
    
    
    # ========================================================
  # THEME
  # ========================================================
  
  theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}



LE_London_NE_male_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Male",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Life expectancy by income percentile: London vs North East - Male"
)

LE_London_NE_male_national


LE_London_NE_female_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Female",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Life expectancy by income percentile: London vs North East - Female"
)

HLE_London_NE_male_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Healthy life expectancy by income percentile: London vs North East - Male"
)


HLE_London_NE_female_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Healthy life expectancy by income percentile: London vs North East - Female"
)

LE_SE_NE_male_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Male",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Life expectancy by income percentile: South East vs North East - Male"
)


LE_SE_NE_female_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Female",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Life expectancy by income percentile: South East vs North East - Female"
)


HLE_SE_NE_male_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Healthy life expectancy by income percentile: South East vs North East - Male"
)


HLE_SE_NE_female_national <- marmot_plot_region_comparison_ci(
  data = income_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "income_percentile",
  title_text = "Healthy life expectancy by income percentile: South East vs North East - Female"
)


LE_London_NE_male_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Male",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Life expectancy by regional income percentile: London vs North East - Male"
)

LE_London_NE_female_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Female",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Life expectancy by regional income percentile: London vs North East - Female"
)


HLE_London_NE_male_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Healthy life expectancy by regional income percentile: London vs North East - Male"
)

HLE_London_NE_female_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  region_1 = "London",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Healthy life expectancy by regional income percentile: London vs North East - Female"
)


LE_SE_NE_male_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Male",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Life expectancy by regional income percentile: South East vs North East - Male"
)


LE_SE_NE_female_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Life expectancy",
  sex_name = "Female",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Life expectancy by regional income percentile: South East vs North East - Female"
)


HLE_SE_NE_male_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Healthy life expectancy by regional income percentile: South East vs North East - Male"
)


HLE_SE_NE_female_regional <- marmot_plot_region_comparison_ci(
  data = regional_percentile_data,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  region_1 = "South East",
  region_2 = "North East",
  percentile_variable = "regional_income_percentile",
  title_text = "Healthy life expectancy by regional income percentile: South East vs North East - Female"
)



LE_London_NE_male_national
LE_London_NE_female_national
HLE_London_NE_male_national
HLE_London_NE_female_national

LE_SE_NE_male_national
LE_SE_NE_female_national
HLE_SE_NE_male_national
HLE_SE_NE_female_national




LE_London_NE_male_regional
LE_London_NE_female_regional
HLE_London_NE_male_regional
HLE_London_NE_female_regional

LE_SE_NE_male_regional
LE_SE_NE_female_regional
HLE_SE_NE_male_regional
HLE_SE_NE_female_regional





















# CREATE MSOA LEVEL DATASET WITH INCOME, LE, HLE AND REGION


health_income <- health_2021 %>%
  left_join(
    income_2023,
    by = "MSOA21CD"
  ) %>%
  select(
    MSOA21CD,
    MSOA21NM,
    RGN22NM,
    Sex,
    income_2023,
    LE_2021,
    HLE_2021
  ) %>%
  drop_na()


# ============================================================
# CREATE MSOA AVERAGE 

health_income_msoa <- health_income %>%
  select(
    MSOA21CD,
    MSOA21NM,
    RGN22NM,
    Sex,
    income_2023,
    LE_2021,
    HLE_2021
  ) %>%
  drop_na()


# ============================================================
# RAW INCOME VS HEALTH PLOTS (BEFORE PERCENTILES)


# Plotting function

plot_raw_income <- function(data, outcome, y_label, title_text) {
  
  ggplot(
    data,
    aes(
      x = income_2023,
      y = .data[[outcome]]
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    labs(
      x = "Disposable annual income after housing costs (£)",
      y = y_label,
      title = title_text
    ) +
    
    theme_minimal()
}




LE_income_raw_plot <- plot_raw_income(
  health_income_msoa,
  "LE_2021",
  "Life expectancy (years)",
  "Life expectancy vs income (2023 income)"
)


HLE_income_raw_plot <- plot_raw_income(
  health_income_msoa,
  "HLE_2021",
  "Healthy life expectancy (years)",
  "Healthy life expectancy vs income (2023 income)"
)


LE_income_raw_plot
HLE_income_raw_plot

#Raw curves

health_income_region_raw <- health_income_msoa %>%
  filter(
    RGN22NM %in% c(
      "London",
      "North East"
    )
  )


plot_raw_income_region <- function(data, outcome, y_label, title_text) {
  
  ggplot(
    data,
    aes(
      x = income_2023,
      y = .data[[outcome]],
      colour = RGN22NM
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    labs(
      x = "Disposable annual income after housing costs (£)",
      y = y_label,
      colour = "Region",
      title = title_text
    ) +
    
    theme_minimal()
}


LE_income_raw_region_plot <- plot_raw_income_region(
  health_income_region_raw,
  "LE_2021",
  "Life expectancy (years)",
  "Life expectancy vs income: London vs North East"
)


HLE_income_raw_region_plot <- plot_raw_income_region(
  health_income_region_raw,
  "HLE_2021",
  "Healthy life expectancy (years)",
  "Healthy life expectancy vs income: London vs North East"
)


LE_income_raw_region_plot
HLE_income_raw_region_plot

# NATIONAL INCOME PERCENTILE
# 1 = lowest income MSOA
# 100 = highest income MSOA


health_income_msoa <- health_income_msoa %>%
  arrange(income_2023) %>%
  mutate(
    income_percentile_national =
      1 + 99 * (row_number() - 1) / (n() - 1)
  )



# REGIONAL INCOME PERCENTILE
# 1 = lowest income in region
# 100 = highest income in region


health_income_msoa <- health_income_msoa %>%
  group_by(RGN22NM) %>%
  arrange(income_2023, .by_group = TRUE) %>%
  mutate(
    income_percentile_region =
      1 + 99 * (row_number() - 1) / (n() - 1)
  ) %>%
  ungroup()

plot_percentile <- function(data, x_variable, outcome, y_label, title_text) {
  
  ggplot(
    data,
    aes(
      x = .data[[x_variable]],
      y = .data[[outcome]]
    )
  ) +
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    scale_x_continuous(
      limits = c(1,100)
    ) +
    labs(
      x = "Income percentile (1 = lowest, 100 = highest)",
      y = y_label,
      title = title_text
    ) +
    theme_minimal()
}


# National LE

LE_national_percentile <- plot_percentile(
  health_income_msoa,
  "income_percentile_national",
  "LE_2021",
  "Life expectancy (years)",
  "Life expectancy by national income percentile"
)


# National HLE

HLE_national_percentile <- plot_percentile(
  health_income_msoa,
  "income_percentile_national",
  "HLE_2021",
  "Healthy life expectancy (years)",
  "Healthy life expectancy by national income percentile"
)


LE_national_percentile
HLE_national_percentile


region_compare <- health_income_msoa %>%
  filter(
    RGN22NM %in% c(
      "London",
      "North East"
    )
  )


plot_region_percentile <- function(data, outcome, title_text) {
  
  ggplot(
    data,
    aes(
      x = income_percentile_region,
      y = .data[[outcome]],
      colour = RGN22NM
    )
  ) +
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    scale_x_continuous(
      limits = c(1,100)
    ) +
    labs(
      x = "Regional income percentile",
      y = ifelse(
        outcome == "LE_2021",
        "Life expectancy (years)",
        "Healthy life expectancy (years)"
      ),
      colour = "Region",
      title = title_text
    ) +
    theme_minimal()
}


LE_region_percentile <- plot_region_percentile(
  region_compare,
  "LE_2021",
  "Life expectancy by regional income percentile: London vs North East"
)


HLE_region_percentile <- plot_region_percentile(
  region_compare,
  "HLE_2021",
  "Healthy life expectancy by regional income percentile: London vs North East"
)


LE_region_percentile
HLE_region_percentile



# ============================================================
# LONDON VS NORTH EAST USING NATIONAL INCOME PERCENTILE
# ============================================================

region_compare_national <- health_income_msoa %>%
  filter(
    RGN22NM %in% c(
      "London",
      "North East"
    )
  )


plot_national_percentile_region <- function(data, outcome, title_text) {
  
  ggplot(
    data,
    aes(
      x = income_percentile_national,
      y = .data[[outcome]],
      colour = RGN22NM
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(1,100)
    ) +
    
    labs(
      x = "National income percentile (1 = lowest income, 100 = highest)",
      y = ifelse(
        outcome == "LE_2021",
        "Life expectancy (years)",
        "Healthy life expectancy (years)"
      ),
      colour = "Region",
      title = title_text
    ) +
    
    theme_minimal()
}


# Life expectancy

LE_national_percentile_region <- plot_national_percentile_region(
  region_compare_national,
  "LE_2021",
  "Life expectancy by national income percentile: London vs North East"
)


# Healthy life expectancy

HLE_national_percentile_region <- plot_national_percentile_region(
  region_compare_national,
  "HLE_2021",
  "Healthy life expectancy by national income percentile: London vs North East"
)


LE_national_percentile_region
HLE_national_percentile_region
