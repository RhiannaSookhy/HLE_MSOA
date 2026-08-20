#Date 28/07/2026

#Code to create marmot curve for 2021 HLE data 


rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA")

#Needs updating

# ============================================================
# 2. LOAD 2021 HLE / LE DATA
# ============================================================

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


# ============================================================
# 3. LOAD REGION DATA
# ============================================================

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


# ============================================================
# 4. PREPARE 2021 LIFE EXPECTANCY DATA
# ============================================================

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


# ============================================================
# 5. PREPARE 2021 HEALTHY LIFE EXPECTANCY DATA
# ============================================================

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


# ============================================================
# 6. CREATE HEALTH_2021 + REGION
# ============================================================

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

#imd
IMD_2025<- read_csv("Working files/imd_2025_final_msoa.csv")

# Join datasets


marmot_data <- health_2021 %>%
  left_join(
    IMD_2025,
    by = "MSOA21CD"
  )


#Removing
marmot_data_NA <- marmot_data %>%
  filter(if_any(everything(), is.na))

marmot_data <- marmot_data %>%
  filter(!if_any(everything(), is.na))

marmot_data %>%
  summarise(unique_msoas = n_distinct(MSOA21CD))

#Create percentiles based on income score; one for each msoa 
marmot_data <-
  marmot_data %>%
  arrange(desc(income_average_score)) %>%
  mutate(
    deprivation_percentile = 1 + 99 * (row_number() - 1) / (n() - 1)
  )


#Plotting foramt
# Create long format for plotting
marmot_long <- marmot_data %>%
  mutate(
    measure = "Life expectancy",
    years = LE_2021,
    lower = LE_2021_LCI,
    upper = LE_2021_UCI
  ) %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM.x,
    Sex,
    deprivation_percentile,
    measure,
    years,
    lower,
    upper
  ) %>%
  bind_rows(
    marmot_data %>%
      mutate(
        measure = "Healthy life expectancy",
        years = HLE_2021,
        lower = HLE_2021_LCI,
        upper = HLE_2021_UCI
      ) %>%
      select(
        RGN22NM,
        MSOA21CD,
        MSOA21NM.x,
        Sex,
        deprivation_percentile,
        measure,
        years,
        lower,
        upper
      )
  ) %>%
  drop_na(years)

#Can comment this be removing msoa with CLS > 20 
marmot_long %>%
  filter((upper - lower) > 20)

marmot_long <- marmot_long %>%
  filter((upper - lower) <= 20)

# Plot function
marmot_plot_overall <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = deprivation_percentile,
        y = years
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
      limits = c(1,100),
      breaks = seq(0,100,10)
    ) +
    
    labs(
      x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}

overall_LE_plot <- marmot_plot_overall(
  marmot_long,
  "Life expectancy",
  "Life expectancy by income deprivation"
)


overall_HLE_plot <- marmot_plot_overall(
  marmot_long,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation"
)

overall_LE_plot

overall_HLE_plot

marmot_plot_by_sex <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = deprivation_percentile,
        y = years,
        colour = Sex
      )
    ) +
    
    geom_point(alpha = 0.15, size = 0.7) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(0,100,10)
    ) +
    
    labs(
      x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "Sex",
      title = title_text
    ) +
    
    theme_minimal()
}

LE_by_sex_plot <- marmot_plot_by_sex(
  marmot_long,
  "Life expectancy",
  "Life expectancy by income deprivation and sex"
)

HLE_by_sex_plot <- marmot_plot_by_sex(
  marmot_long,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation and sex"
)



LE_by_sex_plot

HLE_by_sex_plot







library(dplyr)
library(ggplot2)
library(patchwork)

# ============================================================
# Function: scatter plot by sex and measure
# Includes:
#   - MSOA estimate points
#   - LOESS curve for estimate
#   - LOESS curves for lower and upper 95% CI
# ============================================================

marmot_plot_sex_measure <- function(data, measure_name, sex_name,
                                    title_text = NULL,
                                    show_ci_points = FALSE) {
  
  plot_data <- data %>%
    filter(
      measure == measure_name,
      Sex == sex_name
    )
  
  if (is.null(title_text)) {
    title_text <- paste(measure_name, "-", sex_name)
  }
  
  p <- ggplot(
    plot_data,
    aes(
      x = deprivation_percentile,
      y = years
    )
  ) +
    
    # --------------------------------------------------------
  # MSOA estimate points
  # --------------------------------------------------------
  geom_point(
    alpha = 0.25,
    size = 0.9,
    colour = "black"
  ) +
    
    # --------------------------------------------------------
  # LOESS curve for the actual estimate
  # --------------------------------------------------------
  geom_smooth(
    aes(y = years),
    method = "loess",
    span = 1,
    se = FALSE,
    linewidth = 1.3,
    colour = "black"
  ) +
    
    # --------------------------------------------------------
  # Lower 95% CI LOESS curve
  # --------------------------------------------------------
  geom_smooth(
    aes(
      y = lower,
      colour = "Lower 95% CI"
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    linetype = "dashed",
    linewidth = 1.3
  ) +
    
    # --------------------------------------------------------
  # Upper 95% CI LOESS curve
  # --------------------------------------------------------
  geom_smooth(
    aes(
      y = upper,
      colour = "Upper 95% CI"
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    linetype = "dashed",
    linewidth = 1.3
  )
  
  # ----------------------------------------------------------
  # Optional: add lower and upper CI points
  # ----------------------------------------------------------
  
  if (show_ci_points) {
    
    p <- p +
      geom_point(
        aes(
          y = lower,
          colour = "Lower 95% CI"
        ),
        alpha = 0.12,
        size = 0.6
      ) +
      
      geom_point(
        aes(
          y = upper,
          colour = "Upper 95% CI"
        ),
        alpha = 0.12,
        size = 0.6
      )
  }
  
  # ----------------------------------------------------------
  # Formatting
  # ----------------------------------------------------------
  
  p +
    scale_colour_manual(
      values = c(
        "Lower 95% CI" = "#0072B2",
        "Upper 95% CI" = "#D55E00"
      )
    ) +
    
    scale_x_continuous(
      limits = c(1, 100),
      breaks = seq(0, 100, 10)
    ) +
    
    labs(
      x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "95% confidence interval",
      title = title_text
    ) +
    
    theme_minimal()
}


# ============================================================
# 1. LIFE EXPECTANCY — MALE
# ============================================================

LE_male_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Life expectancy",
  sex_name = "Male",
  title_text = "Life expectancy by income deprivation - Male",
  show_ci_points = FALSE
)


# ============================================================
# 2. LIFE EXPECTANCY — FEMALE
# ============================================================

LE_female_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Life expectancy",
  sex_name = "Female",
  title_text = "Life expectancy by income deprivation - Female",
  show_ci_points = FALSE
)


# ============================================================
# 3. HEALTHY LIFE EXPECTANCY — MALE
# ============================================================

HLE_male_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  title_text = "Healthy life expectancy by income deprivation - Male",
  show_ci_points = FALSE
)


# ============================================================
# 4. HEALTHY LIFE EXPECTANCY — FEMALE
# ============================================================

HLE_female_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  title_text = "Healthy life expectancy by income deprivation - Female",
  show_ci_points = FALSE
)


# ============================================================
# DISPLAY INDIVIDUAL PLOTS
# ============================================================

LE_male_plot
LE_female_plot

HLE_male_plot
HLE_female_plot


# ============================================================
# DISPLAY ALL FOUR TOGETHER
# ============================================================

(LE_male_plot | LE_female_plot) /
  (HLE_male_plot | HLE_female_plot)



# ============================================================
# PACKAGES
# ============================================================

library(dplyr)
library(ggplot2)
library(patchwork)


# ============================================================
# FUNCTION
# ============================================================

marmot_plot_sex_measure <- function(data, measure_name, sex_name,
                                    title_text = NULL) {
  
  # Filter to the requested measure and sex
  plot_data <- data %>%
    filter(
      measure == measure_name,
      Sex == sex_name
    )
  
  
  # Default title if none is supplied
  if (is.null(title_text)) {
    title_text <- paste(
      measure_name,
      "by income deprivation -",
      sex_name
    )
  }
  
  
  # ==========================================================
  # PLOT
  # ==========================================================
  
  ggplot(
    plot_data,
    aes(
      x = deprivation_percentile
    )
  ) +
    
    # --------------------------------------------------------
  # Main MSOA estimate points
  # --------------------------------------------------------
  
  geom_point(
    aes(
      y = years
    ),
    colour = "black",
    alpha = 0.5,
    size = 0.9
  ) +
    
    
    # --------------------------------------------------------
  # Lower 95% CI points
  # --------------------------------------------------------
  
  geom_point(
    aes(
      y = lower,
      colour = "Lower 95% CI"
    ),
    alpha = 0.25,
    size = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # Upper 95% CI points
  # --------------------------------------------------------
  
  geom_point(
    aes(
      y = upper,
      colour = "Upper 95% CI"
    ),
    alpha = 0.25,
    size = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # LOESS curve for the main estimate
  # --------------------------------------------------------
  
  geom_smooth(
    aes(
      y = years
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    colour = "black",
    linewidth = 1.3
  ) +
    
    
    # --------------------------------------------------------
  # LOESS curve for lower 95% CI
  # --------------------------------------------------------
  
  geom_smooth(
    aes(
      y = lower,
      colour = "Lower 95% CI"
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    linetype = "dashed",
    linewidth = 1.3
  ) +
    
    
    # --------------------------------------------------------
  # LOESS curve for upper 95% CI
  # --------------------------------------------------------
  
  geom_smooth(
    aes(
      y = upper,
      colour = "Upper 95% CI"
    ),
    method = "loess",
    span = 1,
    se = FALSE,
    linetype = "dashed",
    linewidth = 1.3
  ) +
    
    
    # --------------------------------------------------------
  # Colours
  # --------------------------------------------------------
  
  scale_colour_manual(
    values = c(
      "Lower 95% CI" = "#0072B2",
      "Upper 95% CI" = "#D55E00"
    )
  ) +
    
    
    # --------------------------------------------------------
  # X-axis
  # --------------------------------------------------------
  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(0, 100, 10)
  ) +
    
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
    y = "Years",
    colour = "95% confidence interval",
    title = title_text
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_minimal() +
    
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        face = "bold"
      )
    )
}


# ============================================================
# LIFE EXPECTANCY — MALE
# ============================================================

LE_male_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Life expectancy",
  sex_name = "Male",
  title_text = "Life expectancy by income deprivation - Male"
)


# ============================================================
# LIFE EXPECTANCY — FEMALE
# ============================================================

LE_female_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Life expectancy",
  sex_name = "Female",
  title_text = "Life expectancy by income deprivation - Female"
)


# ============================================================
# HEALTHY LIFE EXPECTANCY — MALE
# ============================================================

HLE_male_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Healthy life expectancy",
  sex_name = "Male",
  title_text = "Healthy life expectancy by income deprivation - Male"
)


# ============================================================
# HEALTHY LIFE EXPECTANCY — FEMALE
# ============================================================

HLE_female_plot <- marmot_plot_sex_measure(
  data = marmot_long,
  measure_name = "Healthy life expectancy",
  sex_name = "Female",
  title_text = "Healthy life expectancy by income deprivation - Female"
)


# ============================================================
# VIEW INDIVIDUAL PLOTS
# ============================================================

LE_male_plot

LE_female_plot

HLE_male_plot

HLE_female_plot


# ============================================================
# PUT ALL FOUR PLOTS TOGETHER
# ============================================================

(LE_male_plot | LE_female_plot) /
  (HLE_male_plot | HLE_female_plot)




# 
# 
# ##Descriptive statistics 
# 
# marmot_summary <- marmot_data %>%
#   mutate(
#     deprivation_group = case_when(
#       deprivation_percentile <= 10 ~ "Most deprived 10%",
#       deprivation_percentile >= 90 ~ "Least deprived 10%",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   filter(!is.na(deprivation_group))
# 
# 
# descriptive_stats <- bind_rows(
#   
#   marmot_summary %>%
#     group_by(deprivation_group) %>%
#     summarise(
#       Sex = "Male",
#       Measure = "Life expectancy",
#       Mean = mean(le_male, na.rm = TRUE),
#       SD = sd(le_male, na.rm = TRUE),
#       Min = min(le_male, na.rm = TRUE),
#       Max = max(le_male, na.rm = TRUE),
#       Range = Max - Min,
#       n = n(),
#       .groups = "drop"
#     ),
#   
#   marmot_summary %>%
#     group_by(deprivation_group) %>%
#     summarise(
#       Sex = "Male",
#       Measure = "Healthy life expectancy",
#       Mean = mean(hle_male, na.rm = TRUE),
#       SD = sd(hle_male, na.rm = TRUE),
#       Min = min(hle_male, na.rm = TRUE),
#       Max = max(hle_male, na.rm = TRUE),
#       Range = Max - Min,
#       n = n(),
#       .groups = "drop"
#     ),
#   
#   marmot_summary %>%
#     group_by(deprivation_group) %>%
#     summarise(
#       Sex = "Female",
#       Measure = "Life expectancy",
#       Mean = mean(le_female, na.rm = TRUE),
#       SD = sd(le_female, na.rm = TRUE),
#       Min = min(le_female, na.rm = TRUE),
#       Max = max(le_female, na.rm = TRUE),
#       Range = Max - Min,
#       n = n(),
#       .groups = "drop"
#     ),
#   
#   marmot_summary %>%
#     group_by(deprivation_group) %>%
#     summarise(
#       Sex = "Female",
#       Measure = "Healthy life expectancy",
#       Mean = mean(hle_female, na.rm = TRUE),
#       SD = sd(hle_female, na.rm = TRUE),
#       Min = min(hle_female, na.rm = TRUE),
#       Max = max(hle_female, na.rm = TRUE),
#       Range = Max - Min,
#       n = n(),
#       .groups = "drop"
#     )
#   
# )
# 
# descriptive_stats
# 
# 
# highest_msoas <- bind_rows(
#   
#   marmot_data %>%
#     slice_max(le_male, n = 10) %>%
#     transmute(
#       Sex = "Male",
#       Measure = "Life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = le_male
#     ),
#   
#   marmot_data %>%
#     slice_max(hle_male, n = 10) %>%
#     transmute(
#       Sex = "Male",
#       Measure = "Healthy life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = hle_male
#     ),
#   
#   marmot_data %>%
#     slice_max(le_female, n = 10) %>%
#     transmute(
#       Sex = "Female",
#       Measure = "Life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = le_female
#     ),
#   
#   marmot_data %>%
#     slice_max(hle_female, n = 10) %>%
#     transmute(
#       Sex = "Female",
#       Measure = "Healthy life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = hle_female
#     )
#   
# )
# 
# highest_msoas
# 
# lowest_msoas <- bind_rows(
#   
#   marmot_data %>%
#     slice_min(le_male, n = 10) %>%
#     transmute(
#       Sex = "Male",
#       Measure = "Life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = le_male
#     ),
#   
#   marmot_data %>%
#     slice_min(hle_male, n = 10) %>%
#     transmute(
#       Sex = "Male",
#       Measure = "Healthy life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = hle_male
#     ),
#   
#   marmot_data %>%
#     slice_min(le_female, n = 10) %>%
#     transmute(
#       Sex = "Female",
#       Measure = "Life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = le_female
#     ),
#   
#   marmot_data %>%
#     slice_min(hle_female, n = 10) %>%
#     transmute(
#       Sex = "Female",
#       Measure = "Healthy life expectancy",
#       MSOA = msoa_name,
#       Code = msoa21,
#       Value = hle_female
#     )
#   
# )
# 
# lowest_msoas
# 
# # Top vs bottom 10% deprivation difference
# 
# top_bottom_difference <- marmot_data %>%
#   mutate(
#     deprivation_group = case_when(
#       deprivation_percentile <= 10 ~ "Bottom 10% (most deprived)",
#       deprivation_percentile >= 90 ~ "Top 10% (least deprived)",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   filter(!is.na(deprivation_group)) %>%
#   summarise(
#     
#     Male_LE_difference = mean(le_male[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
#       mean(le_male[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE),
#     
#     Male_HLE_difference = mean(hle_male[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
#       mean(hle_male[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE),
#     
#     Female_LE_difference = mean(le_female[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
#       mean(le_female[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE),
#     
#     Female_HLE_difference = mean(hle_female[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
#       mean(hle_female[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE)
#     
#   )
# 
# top_bottom_difference


regional_descriptive_stats <- marmot_long %>%
  group_by(
    RGN22NM,
    Sex,
    measure
  ) %>%
  summarise(
    n = n(),
    Mean_years = mean(years, na.rm = TRUE),
    SD = sd(years, na.rm = TRUE),
    Min = min(years, na.rm = TRUE),
    Max = max(years, na.rm = TRUE),
    Range = Max - Min,
    .groups = "drop"
  )

regional_descriptive_stats

ggplot(
  regional_descriptive_stats,
  aes(
    x = reorder(RGN22NM, Mean_years),
    y = Mean_years,
    fill = Sex
  )
) +
  geom_col(
    position = "dodge"
  ) +
  facet_wrap(~ measure) +
  coord_flip() +
  labs(
    x = "Region",
    y = "Mean years",
    fill = "Sex",
    title = "Average life expectancy and healthy life expectancy by region"
  ) +
  theme_minimal()






library(tidyverse)


# ============================================================
# Regional Marmot plot function
# ============================================================

marmot_plot_by_region <- function(data,
                                  measure_name,
                                  title_text) {
  
  plot_data <- data %>%
    filter(
      measure == measure_name,
      !is.na(RGN22NM),
      !is.na(deprivation_percentile),
      !is.na(years)
    )
  
  
  ggplot(
    plot_data,
    aes(
      x = deprivation_percentile,
      y = years,
      colour = RGN22NM
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.7
    ) +
    
    geom_smooth(
      aes(group = RGN22NM),
      method = "loess",
      span = 0.8,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(0,100,10),
      expand = c(0,0)
    ) +
    
    labs(
      x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "Region",
      title = title_text
    ) +
    
    theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}


# ============================================================
# Regional Life Expectancy
# ============================================================

regional_LE_plot <- marmot_plot_by_region(
  marmot_long,
  "Life expectancy",
  "Life expectancy by income deprivation and region"
)

regional_LE_plot


# ============================================================
# Regional Healthy Life Expectancy
# ============================================================

regional_HLE_plot <- marmot_plot_by_region(
  marmot_long,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation and region"
)

regional_HLE_plot




##By sex

library(tidyverse)

# ============================================================
# Regional Marmot plot function BY SEX
# ============================================================

marmot_plot_by_region_sex <- function(data,
                                      measure_name,
                                      sex_name,
                                      title_text) {
  
  plot_data <- data %>%
    filter(
      measure == measure_name,
      Sex == sex_name,
      !is.na(RGN22NM),
      !is.na(deprivation_percentile),
      !is.na(years)
    )
  
  ggplot(
    plot_data,
    aes(
      x = deprivation_percentile,
      y = years,
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
    aes(group = RGN22NM),
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
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
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


# ============================================================
# 1. LIFE EXPECTANCY — MALE
# ============================================================

regional_LE_male_plot <- marmot_plot_by_region_sex(
  marmot_long,
  "Life expectancy",
  "Male",
  "Life expectancy by income deprivation and region - Male"
)


# ============================================================
# 2. LIFE EXPECTANCY — FEMALE
# ============================================================

regional_LE_female_plot <- marmot_plot_by_region_sex(
  marmot_long,
  "Life expectancy",
  "Female",
  "Life expectancy by income deprivation and region - Female"
)


# ============================================================
# 3. HEALTHY LIFE EXPECTANCY — MALE
# ============================================================

regional_HLE_male_plot <- marmot_plot_by_region_sex(
  marmot_long,
  "Healthy life expectancy",
  "Male",
  "Healthy life expectancy by income deprivation and region - Male"
)


# ============================================================
# 4. HEALTHY LIFE EXPECTANCY — FEMALE
# ============================================================

regional_HLE_female_plot <- marmot_plot_by_region_sex(
  marmot_long,
  "Healthy life expectancy",
  "Female",
  "Healthy life expectancy by income deprivation and region - Female"
)


# ============================================================
# VIEW INDIVIDUAL PLOTS
# ============================================================

regional_LE_male_plot

regional_LE_female_plot

regional_HLE_male_plot

regional_HLE_female_plot


library(tidyverse)
library(patchwork)


# ============================================================
# Regional Marmot plot function


marmot_plot_by_region_sex_ci <- function(data,
                                         measure_name,
                                         sex_name,
                                         title_text) {
  
  plot_data <- data %>%
    filter(
      measure == measure_name,
      Sex == sex_name,
      !is.na(RGN22NM),
      !is.na(deprivation_percentile),
      !is.na(years),
      !is.na(lower),
      !is.na(upper)
    )
  
  ggplot(
    plot_data,
    aes(x = deprivation_percentile)
  ) +
    
    # ========================================================
  # MAIN ESTIMATE POINTS
  # Dark regional colours
  # ========================================================
  
  geom_point(
    aes(
      y = years,
      colour = RGN22NM
    ),
    alpha = 0.25,
    size = 0.8
  ) +
    
    # ========================================================
  # LOWER CI POINTS
  # Same colour as lower CI curves
  # ========================================================
  
  # geom_point(
  #   aes(
  #     y = lower,
  #     colour = paste0(RGN22NM, " - Lower CI")
  #   ),
  #   alpha = 0.25,
  #   size = 0.7
  # ) +
  #   
  #   # ========================================================
  # # UPPER CI POINTS
  # # Same colour as upper CI curves
  # # ========================================================
  # 
  # geom_point(
  #   aes(
  #     y = upper,
  #     colour = paste0(RGN22NM, " - Upper CI")
  #   ),
  #   alpha = 0.25,
  #   size = 0.7
  # ) +
    
    # ========================================================
  # MAIN ESTIMATE — SOLID
  # ========================================================
  
  geom_smooth(
    aes(
      y = years,
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
  # LOWER CI — DASHED
  # ========================================================
  
  geom_smooth(
    aes(
      y = lower,
      colour = paste0(RGN22NM, " - Lower CI"),
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
    
    # ========================================================
  # UPPER CI — DASHED
  # ========================================================
  
  geom_smooth(
    aes(
      y = upper,
      colour = paste0(RGN22NM, " - Upper CI"),
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
    
    # ========================================================
  # COLOUR PALETTE
  #
  # Dark = estimates
  # Light = corresponding CI
  # ========================================================
  
  scale_colour_manual(
    values = c(
      # London
      "London" = "red",
      "London - Lower CI" = "red",
      "London - Upper CI" = "red",
      
      # North East
      "North East" = "darkblue",
      "North East - Lower CI" = "darkblue",
      "North East - Upper CI" = "darkblue"
    )
  ) +
    
    # ========================================================
  # X AXIS
  # ========================================================
  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
    
    # ========================================================
  # LABELS
  # ========================================================
  
  labs(
    x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
    y = "Years",
    colour = "Region / estimate",
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
# ============================================================
# SELECT LONDON AND NORTH EAST
# ============================================================

london_northeast <- marmot_long %>%
  filter(
    RGN22NM %in% c(
      "London",
      "North East"
    )
  )


# ============================================================
# 1. LIFE EXPECTANCY — MALE
# ============================================================

LE_london_northeast_male <- marmot_plot_by_region_sex_ci(
  london_northeast,
  "Life expectancy",
  "Male",
  "Life expectancy by income deprivation: London vs North East - Male"
)


# ============================================================
# 2. LIFE EXPECTANCY — FEMALE
# ============================================================

LE_london_northeast_female <- marmot_plot_by_region_sex_ci(
  london_northeast,
  "Life expectancy",
  "Female",
  "Life expectancy by income deprivation: London vs North East - Female"
)


# ============================================================
# 3. HEALTHY LIFE EXPECTANCY — MALE
# ============================================================

HLE_london_northeast_male <- marmot_plot_by_region_sex_ci(
  london_northeast,
  "Healthy life expectancy",
  "Male",
  "Healthy life expectancy by income deprivation: London vs North East - Male"
)


# ============================================================
# 4. HEALTHY LIFE EXPECTANCY — FEMALE
# ============================================================

HLE_london_northeast_female <- marmot_plot_by_region_sex_ci(
  london_northeast,
  "Healthy life expectancy",
  "Female",
  "Healthy life expectancy by income deprivation: London vs North East - Female"
)


# ============================================================
# VIEW INDIVIDUAL PLOTS
# ============================================================

LE_london_northeast_male

LE_london_northeast_female

HLE_london_northeast_male

HLE_london_northeast_female


# ============================================================
# FOUR-PLOT 2 x 2 LAYOUT
# ============================================================

(
  LE_london_northeast_male |
    LE_london_northeast_female
) /
  (
    HLE_london_northeast_male |
      HLE_london_northeast_female
  )






# ============================================================
# Regional summary statistics
# ============================================================

regional_summary <- marmot_long %>%
  group_by(
    RGN22NM,
    measure
  ) %>%
  summarise(
    Mean = mean(years, na.rm = TRUE),
    SD = sd(years, na.rm = TRUE),
    Minimum = min(years, na.rm = TRUE),
    Maximum = max(years, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

regional_summary



# ============================================================
# Regional deprivation gap
# ============================================================

regional_gap <- marmot_long %>%
  
  mutate(
    deprivation_group = case_when(
      deprivation_percentile <= 10 ~ "Most deprived",
      deprivation_percentile >= 90 ~ "Least deprived",
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(!is.na(deprivation_group)) %>%
  
  group_by(
    RGN22NM,
    measure,
    deprivation_group
  ) %>%
  
  summarise(
    Mean = mean(years, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  pivot_wider(
    names_from = deprivation_group,
    values_from = Mean
  ) %>%
  
  mutate(
    Gap = `Least deprived` - `Most deprived`
  ) %>%
  
  arrange(desc(Gap))


regional_gap


##Confidence intervals ................Of England only 

# ============================================================
# CONFIDENCE INTERVAL WIDTH ANALYSIS
# Separate results for:
#   1. Life expectancy
#   2. Healthy life expectancy
# ============================================================

library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1. Calculate CI width
# ------------------------------------------------------------

marmot_long <- marmot_long %>%
  mutate(
    ci_width = upper - lower
  )


# ============================================================
# 2. SUMMARY BY SEX AND MEASURE
#    Mean, minimum and maximum CI width
# ============================================================

# Life expectancy
ci_summary_LE <- marmot_long %>%
  filter(measure == "Life expectancy") %>%
  group_by(Sex) %>%
  summarise(
    n = n(),
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    min_ci_width = min(ci_width, na.rm = TRUE),
    max_ci_width = max(ci_width, na.rm = TRUE),
    .groups = "drop"
  )

# Healthy life expectancy
ci_summary_HLE <- marmot_long %>%
  filter(measure == "Healthy life expectancy") %>%
  group_by(Sex) %>%
  summarise(
    n = n(),
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    min_ci_width = min(ci_width, na.rm = TRUE),
    max_ci_width = max(ci_width, na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================
# 3. CI WIDTH BANDS
#    Raw numbers AND percentages
#
#    Bands:
#       >10
#       7–10
#       5–7
#       3–5
#       2–3
#       1–2
#       <1
# ============================================================

marmot_long_bands <- marmot_long %>%
  mutate(
    ci_width_band = case_when(
      ci_width > 10 ~ ">10",
      ci_width >= 7 & ci_width <= 10 ~ "7–10",
      ci_width >= 5 & ci_width < 7 ~ "5–7",
      ci_width >= 3 & ci_width < 5 ~ "3–5",
      ci_width >= 2 & ci_width < 3 ~ "2–3",
      ci_width >= 1 & ci_width < 2 ~ "1–2",
      ci_width < 1 ~ "<1",
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# Life expectancy - CI width bands
# ------------------------------------------------------------

ci_bands_LE <- marmot_long_bands %>%
  filter(measure == "Life expectancy") %>%
  group_by(Sex, ci_width_band) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Sex) %>%
  mutate(
    total_n = sum(n),
    percentage = (n / total_n) * 100
  ) %>%
  ungroup() %>%
  mutate(
    percentage = round(percentage, 1)
  ) %>%
  arrange(
    Sex,
    factor(
      ci_width_band,
      levels = c(">10", "7–10", "5–7", "3–5", "2–3", "1–2", "<1")
    )
  )


# ------------------------------------------------------------
# Healthy life expectancy - CI width bands
# ------------------------------------------------------------

ci_bands_HLE <- marmot_long_bands %>%
  filter(measure == "Healthy life expectancy") %>%
  group_by(Sex, ci_width_band) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Sex) %>%
  mutate(
    total_n = sum(n),
    percentage = (n / total_n) * 100
  ) %>%
  ungroup() %>%
  mutate(
    percentage = round(percentage, 1)
  ) %>%
  arrange(
    Sex,
    factor(
      ci_width_band,
      levels = c(">10", "7–10", "5–7", "3–5", "2–3", "1–2", "<1")
    )
  )


# ============================================================
# 4. TOP 20 WIDEST CIs
#    Separate top 20 for LE and HLE
# ============================================================

# ------------------------------------------------------------
# Life expectancy - top 20
# ------------------------------------------------------------

top_20_ci_LE <- marmot_long %>%
  filter(measure == "Life expectancy") %>%
  arrange(desc(ci_width)) %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM.x,
    Sex,
    measure,
    years,
    lower,
    upper,
    ci_width
  ) %>%
  slice_head(n = 40)


# ------------------------------------------------------------
# Healthy life expectancy - top 20
# ------------------------------------------------------------

top_20_ci_HLE <- marmot_long %>%
  filter(measure == "Healthy life expectancy") %>%
  arrange(desc(ci_width)) %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM.x,
    Sex,
    measure,
    years,
    lower,
    upper,
    ci_width
  ) %>%
  slice_head(n = 20)


# ============================================================
# 5. CI WIDTH BY REGION
#    Separate for LE and HLE
#
#    Gives:
#       n
#       mean CI width
#       minimum CI width
#       maximum CI width
# ============================================================

# ------------------------------------------------------------
# Life expectancy - regional CI width
# ------------------------------------------------------------

ci_width_region_LE <- marmot_long %>%
  filter(measure == "Life expectancy") %>%
  group_by(RGN22NM, Sex) %>%
  summarise(
    n = n(),
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    min_ci_width = min(ci_width, na.rm = TRUE),
    max_ci_width = max(ci_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ci_width))


# ------------------------------------------------------------
# Healthy life expectancy - regional CI width
# ------------------------------------------------------------

ci_width_region_HLE <- marmot_long %>%
  filter(measure == "Healthy life expectancy") %>%
  group_by(RGN22NM, Sex) %>%
  summarise(
    n = n(),
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    min_ci_width = min(ci_width, na.rm = TRUE),
    max_ci_width = max(ci_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ci_width))


# ============================================================
# 6. OPTIONAL: REGIONAL SUMMARY WITHOUT SEX
#    If you also want one overall regional figure for each
#    measure, ignoring sex.
# ============================================================

# Life expectancy
ci_width_region_LE_overall <- marmot_long %>%
  filter(measure == "Life expectancy") %>%
  group_by(RGN22NM) %>%
  summarise(
    n = n(),
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    min_ci_width = min(ci_width, na.rm = TRUE),
    max_ci_width = max(ci_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ci_width))


# Healthy life expectancy
ci_width_region_HLE_overall <- marmot_long %>%
  filter(measure == "Healthy life expectancy") %>%
  group_by(RGN22NM) %>%
  summarise(
    n = n(),
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    min_ci_width = min(ci_width, na.rm = TRUE),
    max_ci_width = max(ci_width, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ci_width))


# ============================================================
# 7. VIEW THE RESULTS
# ============================================================

# Mean / min / max
ci_summary_LE
ci_summary_HLE

# CI width bands: count + percentage
ci_bands_LE
ci_bands_HLE

# Top 20 largest CI widths
top_20_ci_LE
top_20_ci_HLE

# Regional CI widths by sex
ci_width_region_LE
ci_width_region_HLE

# Regional CI widths overall
ci_width_region_LE_overall
ci_width_region_HLE_overall
##




library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. Create MSOA-level deprivation percentile
#    This gives each MSOA ONE deprivation percentile,
#    regardless of sex.
# ============================================================

msoa_deprivation <- marmot_data %>%
  select(
    MSOA21CD,
    MSOA21NM.x,
    income_average_score
  ) %>%
  distinct(MSOA21CD, .keep_all = TRUE) %>%
  arrange(desc(income_average_score)) %>%  # highest score = most deprived
  mutate(
    deprivation_percentile = 1 +
      99 * (row_number() - 1) / (n() - 1)
  )


# ============================================================
# 2. Add deprivation percentile to marmot_long
# ============================================================

marmot_long_plot <- marmot_long %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM.x,
    Sex,
    measure,
    years,
    lower,
    upper
  ) %>%
  left_join(
    msoa_deprivation %>%
      select(
        MSOA21CD,
        deprivation_percentile
      ),
    by = "MSOA21CD"
  ) %>%
  mutate(
    ci_width = upper - lower
  )


# ============================================================
# 3. LIFE EXPECTANCY
# ============================================================

plot_LE <- marmot_long_plot %>%
  filter(measure == "Life expectancy") %>%
  ggplot(
    aes(
      x = deprivation_percentile,
      y = years
    )
  ) +
  
  # 95% CI
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0,
    alpha = 0.20
  ) +
  
  # MSOA estimate
  geom_point(
    alpha = 0.35,
    size = 1.2
  ) +
  
  # Male and Female panels
  facet_wrap(~ Sex) +
  
  labs(
    title = "Life expectancy by income deprivation",
    subtitle = "MSOA-level estimates with 95% confidence intervals",
    x = "Income deprivation percentile",
    y = "Life expectancy (years)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

plot_LE


# ============================================================
# 4. HEALTHY LIFE EXPECTANCY
# ============================================================

plot_HLE <- marmot_long_plot %>%
  filter(measure == "Healthy life expectancy") %>%
  ggplot(
    aes(
      x = deprivation_percentile,
      y = years
    )
  ) +
  
  # 95% CI
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0,
    alpha = 0.20
  ) +
  
  # MSOA estimate
  geom_point(
    alpha = 0.35,
    size = 1.2
  ) +
  
  # Male and Female panels
  facet_wrap(~ Sex) +
  
  labs(
    title = "Healthy life expectancy by income deprivation",
    subtitle = "MSOA-level estimates with 95% confidence intervals",
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

plot_HLE




library(tidyverse)
library(future)
library(future.apply)

# ============================================================
# 1. Set up parallel processing
# ============================================================

cores <- parallel::detectCores() - 1

plan(
  multisession,
  workers = cores
)


# ============================================================
# 2. Select the data
# ============================================================

simulation_data <- marmot_long %>%
  filter(
    sex == "Male",
    measure == "Healthy life expectancy"
  ) %>%
  arrange(deprivation_percentile)


# ============================================================
# 3. Number of simulations
# ============================================================

n_sim <- 20000


# ============================================================
# 4. Prediction grid
# ============================================================

prediction_grid <- data.frame(
  deprivation_percentile = seq(1, 100, length.out = 100)
)


# ============================================================
# 5. Parallel Monte Carlo simulations
# ============================================================

set.seed(123)

all_curves <- future_lapply(
  
  1:n_sim,
  
  function(i) {
    
    # Randomly select one value between lower and upper CI
    simulated_years <- runif(
      nrow(simulation_data),
      min = simulation_data$lower,
      max = simulation_data$upper
    )
    
    # Fit LOESS Marmot curve
    fit <- loess(
      simulated_years ~ deprivation_percentile,
      data = simulation_data,
      span = 0.8
    )
    
    # Predict curve
    prediction <- predict(
      fit,
      newdata = prediction_grid
    )
    
    # Return this simulation
    data.frame(
      simulation = i,
      deprivation_percentile =
        prediction_grid$deprivation_percentile,
      years = prediction
    )
  },
  
  future.seed = TRUE
  
)


# ============================================================
# 6. Combine simulations
# ============================================================

all_curves <- bind_rows(all_curves)


# ============================================================
# 7. Plot all curves
# ============================================================

ggplot(
  all_curves,
  aes(
    x = deprivation_percentile,
    y = years,
    group = simulation
  )
) +
  
  geom_line(
    alpha = 0.01,
    linewidth = 0.3
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Monte Carlo Marmot Curves",
    subtitle = paste0(
      format(n_sim, big.mark = ","),
      " simulations using random values between each MSOA's lower and upper CI"
    ),
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 8. Shut down parallel workers when finished
# ============================================================

plan(sequential)


ggplot(
  all_curves,
  aes(
    x = deprivation_percentile,
    y = years,
    group = simulation,
    colour = factor(simulation)
  )
) +
  
  geom_line(
    alpha = 0.05,
    linewidth = 0.3
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Monte Carlo Marmot Curves",
    subtitle = paste0(
      format(n_sim, big.mark = ","),
      " simulated curves"
    ),
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)",
    colour = "Simulation"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )




























marmot_data <- marmot_data %>%
  mutate(
    income_decile = 21 - ntile(income_average_score, 10)
  )


income_decile_stats <- marmot_data %>%
  group_by(
    income_decile
  ) %>%
  summarise(
    n = n(),
    
    Male_LE_mean = mean(le_male, na.rm = TRUE),
    Male_HLE_mean = mean(hle_male, na.rm = TRUE),
    
    Female_LE_mean = mean(le_female, na.rm = TRUE),
    Female_HLE_mean = mean(hle_female, na.rm = TRUE),
    
    Male_LE_SD = sd(le_male, na.rm = TRUE),
    Male_HLE_SD = sd(hle_male, na.rm = TRUE),
    
    Female_LE_SD = sd(le_female, na.rm = TRUE),
    Female_HLE_SD = sd(hle_female, na.rm = TRUE),
    
    .groups = "drop"
  )

income_decile_stats


income_decile_long <- income_decile_stats %>%
  pivot_longer(
    cols = c(
      Male_LE_mean,
      Male_HLE_mean,
      Female_LE_mean,
      Female_HLE_mean
    ),
    names_to = "Measure",
    values_to = "Years"
  )


ggplot(
  income_decile_long,
  aes(
    x = income_decile,
    y = Years,
    colour = Measure
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:10,
    labels = paste0("D",1:10)
  ) +
  labs(
    x = "Income deprivation decile\n(1 = most deprived, 10 = least deprived)",
    y = "Years",
    colour = "",
    title = "Life expectancy and healthy life expectancy by income deprivation decile"
  ) +
  theme_minimal()


regional_decile_stats <- marmot_data %>%
  group_by(
    Region,
    income_decile
  ) %>%
  summarise(
    Male_HLE = mean(hle_male, na.rm = TRUE),
    Female_HLE = mean(hle_female, na.rm = TRUE),
    Male_LE = mean(le_male, na.rm = TRUE),
    Female_LE = mean(le_female, na.rm = TRUE),
    .groups = "drop"
  )


regional_deprivation_gap <- regional_decile_stats %>%
  group_by(Region) %>%
  summarise(
    Male_HLE_gap = max(Male_HLE) - min(Male_HLE),
    Female_HLE_gap = max(Female_HLE) - min(Female_HLE),
    Male_LE_gap = max(Male_LE) - min(Male_LE),
    Female_LE_gap = max(Female_LE) - min(Female_LE),
    .groups = "drop"
  )

ggplot(
  regional_deprivation_gap,
  aes(
    x = reorder(Region, Male_HLE_gap),
    y = Male_HLE_gap
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Region",
    y = "Healthy life expectancy gap (years)",
    title = "Difference in male healthy life expectancy between least and most deprived areas",
    subtitle = "Least deprived decile minus most deprived decile"
  ) +
  theme_minimal()


















#New analysis based on 4/8/26

library(dplyr)

marmot_data <- marmot_data %>%
  group_by(RGN22NM) %>%
  mutate(
    regional_deprivation_percentile =
      percent_rank(-income_average_score) * 99 + 1
  ) %>%
  ungroup()


library(ggplot2)
library(dplyr)
library(rlang)

marmot_plot_by_region <- function(data,
                                  outcome,
                                  lower,
                                  upper,
                                  title_text){
  
  ggplot(
    data,
    aes(
      x = regional_deprivation_percentile,
      y = {{ outcome }},
      colour = RGN22NM,
      fill = RGN22NM
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    
    geom_smooth(
      method = "loess",
      se = TRUE,
      span = 0.8,
      linewidth = 1.3,
      alpha = 0.20
    ) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(0,100,10),
      expand = c(0,0)
    ) +
    
    labs(
      x = "Income deprivation percentile within region\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "Region",
      fill = "Region",
      title = title_text
    ) +
    
    theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

london_northeast <- marmot_data %>%
  filter(
    RGN22NM %in% c(
      "London",
      "North East"
    )
  )


# Life expectancy

LE_london_northeast <-
  marmot_plot_by_region(
    london_northeast,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    "Life expectancy by regional deprivation percentile"
  )

LE_london_northeast


# Healthy life expectancy

HLE_london_northeast <- marmot_plot_by_region(
  london_northeast,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation: London vs North East"
)

HLE_london_northeast


##Using the pub;ishes CIs

library(dplyr)
library(ggplot2)

#---------------------------------------------------------
# 1. Create deprivation percentile within each region
#---------------------------------------------------------

marmot_data <- marmot_data %>%
  group_by(RGN22NM) %>%
  mutate(
    regional_deprivation_percentile =
      percent_rank(-income_average_score) * 99 + 1
  ) %>%
  ungroup()

#---------------------------------------------------------
# 2. Keep London and North East
#---------------------------------------------------------

london_northeast <- marmot_data %>%
  filter(
    RGN22NM %in% c("London", "North East")
  )

#---------------------------------------------------------
# 3. Generic plotting function
#---------------------------------------------------------

marmot_plot_by_region <- function(data,
                                  estimate,
                                  lower,
                                  upper,
                                  title_text,
                                  bin_width = 5){
  
  estimate <- rlang::ensym(estimate)
  lower <- rlang::ensym(lower)
  upper <- rlang::ensym(upper)
  
  plot_data <- data %>%
    mutate(
      percentile_bin =
        floor(regional_deprivation_percentile / bin_width) * bin_width
    ) %>%
    group_by(RGN22NM, percentile_bin) %>%
    summarise(
      estimate = mean(!!estimate, na.rm = TRUE),
      lower = mean(!!lower, na.rm = TRUE),
      upper = mean(!!upper, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  ggplot(
    plot_data,
    aes(
      x = percentile_bin,
      y = estimate,
      colour = RGN22NM,
      fill = RGN22NM
    )
  ) +
    
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      alpha = 0.20,
      colour = NA
    ) +
    
    geom_line(
      linewidth = 1.4
    ) +
    
    geom_point(
      size = 2
    ) +
    
    scale_x_continuous(
      limits = c(0,100),
      breaks = seq(0,100,10),
      expand = c(0,0)
    ) +
    
    labs(
      x = "Income deprivation percentile within region\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "Region",
      fill = "Region",
      title = title_text
    ) +
    
    theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

#---------------------------------------------------------
# 4. Life expectancy
#---------------------------------------------------------

LE_london_northeast <-
  marmot_plot_by_region(
    london_northeast,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    "Life expectancy by regional deprivation percentile"
  )

LE_london_northeast

#---------------------------------------------------------
# 5. Healthy life expectancy
#---------------------------------------------------------

HLE_london_northeast <-
  marmot_plot_by_region(
    london_northeast,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI,
    "Healthy life expectancy by regional deprivation percentile"
  )

HLE_london_northeast





library(dplyr)
library(ggplot2)

# ============================================================
# 1. CREATE REGIONAL IMD DEPRIVATION PERCENTILE
# ============================================================

marmot_data <- marmot_data %>%
  group_by(RGN22NM) %>%
  mutate(
    regional_deprivation_percentile =
      percent_rank(-income_average_score) * 99 + 1
  ) %>%
  ungroup()


# ============================================================
# 2. SELECT LONDON AND NORTH EAST
# ============================================================

london_northeast <- marmot_data %>%
  filter(
    RGN22NM %in% c(
      "London",
      "North East"
    )
  )


# ============================================================
# 3. FUNCTION TO PLOT BY REGION AND SEX
# ============================================================

library(dplyr)
library(ggplot2)


# ============================================================
# 1. CREATE REGIONAL IMD DEPRIVATION PERCENTILE
# ============================================================

marmot_data <- marmot_data %>%
  group_by(RGN22NM) %>%
  mutate(
    regional_deprivation_percentile =
      percent_rank(-income_average_score) * 99 + 1
  ) %>%
  ungroup()


# ============================================================
# 2. SELECT LONDON AND NORTH EAST
# ============================================================

london_northeast <- marmot_data %>%
  filter(
    RGN22NM %in% c(
      "South East",
      "North East"
    )
  )


# ============================================================
# 3. FUNCTION TO PLOT ESTIMATE + PUBLISHED CI
#    BY REGION AND SEX
# ============================================================

marmot_plot_by_region_sex_ci <- function(
    data,
    estimate,
    lower,
    upper,
    sex_name,
    title_text) {
  
  # Convert column names supplied to the function
  estimate <- rlang::ensym(estimate)
  lower <- rlang::ensym(lower)
  upper <- rlang::ensym(upper)
  
  # ----------------------------------------------------------
  # Filter for sex and remove missing values
  # ----------------------------------------------------------
  
  plot_data <- data %>%
    filter(
      Sex == sex_name,
      !is.na(RGN22NM),
      !is.na(regional_deprivation_percentile),
      !is.na(!!estimate),
      !is.na(!!lower),
      !is.na(!!upper)
    )
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  ggplot(
    plot_data,
    aes(x = regional_deprivation_percentile)
  ) +
    
    # ========================================================
  # MAIN ESTIMATE POINTS
  # ========================================================
  
  geom_point(
    aes(
      y = !!estimate,
      colour = RGN22NM
    ),
    alpha = 0.25,
    size = 0.8
  ) +
    
    # ========================================================
  # MAIN ESTIMATE — SOLID
  # ========================================================
  
  geom_smooth(
    aes(
      y = !!estimate,
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
  # LOWER CI — DASHED
  # ========================================================
  
  geom_smooth(
    aes(
      y = !!lower,
      colour = RGN22NM,
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.0,
    linetype = "dashed"
  ) +
    
    # ========================================================
  # UPPER CI — DASHED
  # ========================================================
  
  geom_smooth(
    aes(
      y = !!upper,
      colour = RGN22NM,
      group = RGN22NM
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.0,
    linetype = "dashed"
  ) +
    
    # ========================================================
  # COLOUR PALETTE
  # ========================================================
  
  scale_colour_manual(
    values = c(
      "South East" = "red",
      "North East" = "darkblue"
    )
  ) +
    
    # ========================================================
  # X AXIS
  # ========================================================
  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
    
    # ========================================================
  # LABELS
  # ========================================================
  
  labs(
    x = paste0(
      "Regional IMD deprivation percentile\n",
      "(1 = most deprived, 100 = least deprived)"
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


# ============================================================
# 4. LIFE EXPECTANCY — MALE
# ============================================================

LE_london_northeast_male <-
  marmot_plot_by_region_sex_ci(
    london_northeast,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    "Male",
    "Life expectancy by regional IMD deprivation percentile: South East vs North East - Male"
  )

LE_london_northeast_male


# ============================================================
# 5. LIFE EXPECTANCY — FEMALE
# ============================================================

LE_london_northeast_female <-
  marmot_plot_by_region_sex_ci(
    london_northeast,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    "Female",
    "Life expectancy by regional IMD deprivation percentile: South East vs North East - Female"
  )

LE_london_northeast_female


# ============================================================
# 6. HEALTHY LIFE EXPECTANCY — MALE
# ============================================================

HLE_london_northeast_male <-
  marmot_plot_by_region_sex_ci(
    london_northeast,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI,
    "Male",
    "Healthy life expectancy by regional IMD deprivation percentile: South East vs North East - Male"
  )

HLE_london_northeast_male


# ============================================================
# 7. HEALTHY LIFE EXPECTANCY — FEMALE
# ============================================================

HLE_london_northeast_female <-
  marmot_plot_by_region_sex_ci(
    london_northeast,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI,
    "Female",
    "Healthy life expectancy by regional IMD deprivation percentile: South East vs North East - Female"
  )

HLE_london_northeast_female



