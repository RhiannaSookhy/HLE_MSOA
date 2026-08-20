
#Date 28/07/2026

#Code to create marmot curve for 2011 HLE data 


rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA")


hle_males <- read_excel(
  "Raw data/hlereferencetable2_tcm77-417533 (2).xls",
  sheet = "MSOA_males",
  skip = 7
) %>%
  transmute(
    Region,
    msoa11 = `MSOA Codes`,
    msoa_name = `MSOA Names`,
    
    hle_male = `HLE (years)`,
    hle_male_lcl = `Lower 95 % confidence interval...6`,
    hle_male_ucl = `Upper 95 % confidence interval...7`,
    
    le_male = `LE (Years)`,
    le_male_lcl = `Lower 95 % confidence interval...9`,
    le_male_ucl = `Upper 95 % confidence interval...10`
  )

hle_females <- read_excel(
  "Raw data/hlereferencetable2_tcm77-417533 (2).xls",
  sheet = "MSOA_females",
  skip = 7
) %>%
  transmute(
    msoa11 = `MSOA Codes`,
    
    hle_female = `HLE (years)`,
    hle_female_lcl = `Lower 95 % confidence interval...6`,
    hle_female_ucl = `Upper 95 % confidence interval...7`,
    
    le_female = `LE (Years)`,
    le_female_lcl = `Lower 95 % confidence interval...9`,
    le_female_ucl = `Upper 95 % confidence interval...10`
  )



#imd
IMD_2015<- read_csv("Working files/imd_2015_final_msoa.csv")

# Join datasets

marmot_data <- IMD_2015 %>% 
  rename( msoa11 = MSOA11CD, msoa_name = MSOA11NM ) %>% 
  left_join( hle_males %>% 
               select(-msoa_name), by = "msoa11" ) %>% 
  left_join(hle_females, by = "msoa11")


#Removing
marmot_data_NA <- marmot_data %>%
  filter(if_any(everything(), is.na))

marmot_data <- marmot_data %>%
  filter(!if_any(everything(), is.na))


#Create percentiles based on income score; one for each msoa 
marmot_data <-
  marmot_data %>%
  arrange(desc(income_average_score)) %>%
  mutate(
    deprivation_percentile = 1 + 99 * (row_number() - 1) / (n() - 1)
  )


#Plotting foramt
marmot_long <-
  bind_rows(
    
    marmot_data %>%
      transmute(
        Region,
        msoa11,
        msoa_name,
        deprivation_percentile,
        sex = "Male",
        measure = "Healthy life expectancy",
        years = hle_male,
        lower = hle_male_lcl,
        upper = hle_male_ucl
      ),
    
    marmot_data %>%
      transmute(
        Region,
        msoa11,
        msoa_name,
        deprivation_percentile,
        sex = "Male",
        measure = "Life expectancy",
        years = le_male,
        lower = le_male_lcl,
        upper = le_male_ucl
      ),
    
    marmot_data %>%
      transmute(
        Region,
        msoa11,
        msoa_name,
        deprivation_percentile,
        sex = "Female",
        measure = "Healthy life expectancy",
        years = hle_female,
        lower = hle_female_lcl,
        upper = hle_female_ucl
      ),
    
    marmot_data %>%
      transmute(
        Region,
        msoa11,
        msoa_name,
        deprivation_percentile,
        sex = "Female",
        measure = "Life expectancy",
        years = le_female,
        lower = le_female_lcl,
        upper = le_female_ucl
      )
  ) %>%
  drop_na(years)


# Plot function


marmot_plot_single_curve <- function(data,
                                     measure_name,
                                     title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(aes(deprivation_percentile, years)) +
    
    geom_point(alpha = 0.25, size = 1.2) +
    
    geom_smooth(
      method = "loess",
      span = 0.8,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(1, 100),
      breaks = seq(0, 100, 10)
    ) +
    
    labs(
      x = "Population percentile of income deprivation\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}


overall_LE_plot <- marmot_plot_single_curve(
  marmot_long,
  "Life expectancy",
  "Life expectancy by income deprivation"
)


overall_HLE_plot <- marmot_plot_single_curve(
  marmot_long,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation"
)


overall_LE_plot

overall_HLE_plot

# Marmot plot - split by sex


marmot_plot_by_sex <- function(data,
                               measure_name,
                               title_text){
  
  plot_data <- data %>%
    filter(
      measure == measure_name
    )
  
  
  ggplot(
    plot_data,
    aes(
      x = deprivation_percentile,
      y = years,
      colour = sex
    )
  ) +
    
    geom_point(
      alpha = 0.15,
      size = 0.7
    ) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(0,100,10),
      expand = c(0,0)
    ) +
    
    labs(
      x = "Income deprivation rank percentile\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "Sex",
      title = title_text
    ) +
    
    theme_minimal(
      base_size = 13
    ) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
}


# Create sex-specific plots


# Life expectancy by sex

LE_by_sex_plot <- marmot_plot_by_sex(
  
  marmot_long,
  "Life expectancy",
  "Life expectancy by income deprivation and sex"
  
)



# Healthy life expectancy by sex

HLE_by_sex_plot <- marmot_plot_by_sex(
  
  marmot_long,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation and sex"
  
)



# Display plots

LE_by_sex_plot

HLE_by_sex_plot




##Descriptive statistics 

marmot_summary <- marmot_data %>%
  mutate(
    deprivation_group = case_when(
      deprivation_percentile <= 10 ~ "Most deprived 10%",
      deprivation_percentile >= 90 ~ "Least deprived 10%",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(deprivation_group))


descriptive_stats <- bind_rows(
  
  marmot_summary %>%
    group_by(deprivation_group) %>%
    summarise(
      Sex = "Male",
      Measure = "Life expectancy",
      Mean = mean(le_male, na.rm = TRUE),
      SD = sd(le_male, na.rm = TRUE),
      Min = min(le_male, na.rm = TRUE),
      Max = max(le_male, na.rm = TRUE),
      Range = Max - Min,
      n = n(),
      .groups = "drop"
    ),
  
  marmot_summary %>%
    group_by(deprivation_group) %>%
    summarise(
      Sex = "Male",
      Measure = "Healthy life expectancy",
      Mean = mean(hle_male, na.rm = TRUE),
      SD = sd(hle_male, na.rm = TRUE),
      Min = min(hle_male, na.rm = TRUE),
      Max = max(hle_male, na.rm = TRUE),
      Range = Max - Min,
      n = n(),
      .groups = "drop"
    ),
  
  marmot_summary %>%
    group_by(deprivation_group) %>%
    summarise(
      Sex = "Female",
      Measure = "Life expectancy",
      Mean = mean(le_female, na.rm = TRUE),
      SD = sd(le_female, na.rm = TRUE),
      Min = min(le_female, na.rm = TRUE),
      Max = max(le_female, na.rm = TRUE),
      Range = Max - Min,
      n = n(),
      .groups = "drop"
    ),
  
  marmot_summary %>%
    group_by(deprivation_group) %>%
    summarise(
      Sex = "Female",
      Measure = "Healthy life expectancy",
      Mean = mean(hle_female, na.rm = TRUE),
      SD = sd(hle_female, na.rm = TRUE),
      Min = min(hle_female, na.rm = TRUE),
      Max = max(hle_female, na.rm = TRUE),
      Range = Max - Min,
      n = n(),
      .groups = "drop"
    )
  
)

descriptive_stats


highest_msoas <- bind_rows(
  
  marmot_data %>%
    slice_max(le_male, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = le_male
    ),
  
  marmot_data %>%
    slice_max(hle_male, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_male
    ),
  
  marmot_data %>%
    slice_max(le_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = le_female
    ),
  
  marmot_data %>%
    slice_max(hle_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_female
    )
  
)

highest_msoas

lowest_msoas <- bind_rows(
  
  marmot_data %>%
    slice_min(le_male, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = le_male
    ),
  
  marmot_data %>%
    slice_min(hle_male, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_male
    ),
  
  marmot_data %>%
    slice_min(le_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = le_female
    ),
  
  marmot_data %>%
    slice_min(hle_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_female
    )
  
)

lowest_msoas

# Top vs bottom 10% deprivation difference

top_bottom_difference <- marmot_data %>%
  mutate(
    deprivation_group = case_when(
      deprivation_percentile <= 10 ~ "Bottom 10% (most deprived)",
      deprivation_percentile >= 90 ~ "Top 10% (least deprived)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(deprivation_group)) %>%
  summarise(
    
    Male_LE_difference = mean(le_male[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
      mean(le_male[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE),
    
    Male_HLE_difference = mean(hle_male[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
      mean(hle_male[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE),
    
    Female_LE_difference = mean(le_female[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
      mean(le_female[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE),
    
    Female_HLE_difference = mean(hle_female[deprivation_group == "Top 10% (least deprived)"], na.rm = TRUE) -
      mean(hle_female[deprivation_group == "Bottom 10% (most deprived)"], na.rm = TRUE)
    
  )

top_bottom_difference


regional_descriptive_stats <- marmot_long %>%
  group_by(
    Region,
    sex,
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
    x = reorder(Region, Mean_years),
    y = Mean_years,
    fill = sex
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



##

marmot_data <- marmot_data %>%
  mutate(
    income_decile = 11 - ntile(income_average_score, 10)
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



##cls==================================================================================================================================


hle_ci_summary <-
  
  marmot_long %>%
  filter(measure == "Healthy life expectancy") %>%
  mutate(
    ci_width = upper - lower
  ) %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    mean_ci = mean(ci_width),
    median_ci = median(ci_width),
    min_ci = min(ci_width),
    max_ci = max(ci_width)
  )

hle_ci_summary


hle_large_ci <- marmot_long %>%
  filter(measure == "Healthy life expectancy") %>%
  mutate(
    ci_width = upper - lower
  ) %>%
  arrange(desc(ci_width)) %>%
  select(
    Region,
    msoa11,
    msoa_name,
    deprivation_percentile,
    sex,
    years,
    lower,
    upper,
    ci_width
  )

hle_large_ci



#Monte carlo simulations
#############################################################
# Parallel Monte Carlo Marmot Curve Simulation
#############################################################

library(future)
library(future.apply)
library(dplyr)
library(ggplot2)


#############################################################
# Set parallel workers
#############################################################

cores <- parallel::detectCores() - 1

plan(
  multisession,
  workers = cores
)


#############################################################
# Monte Carlo simulation function
#############################################################

simulate_marmot_curve_parallel <- function(data,
                                           n_sim = 10000,
                                           span = 0.8){
  
  
  # Convert CI into standard error
  
  data <- data %>%
    mutate(
      se = (upper - lower) / 3.92
    )
  
  
  # Prediction points
  
  prediction_grid <- data.frame(
    deprivation_percentile = seq(1,100,1)
  )
  
  
  ###########################################################
  # Parallel simulations
  ###########################################################
  
  simulations <- future_lapply(
    
    1:n_sim,
    
    function(i){
      
      # Generate possible HLE values
      
      simulated_data <- data %>%
        mutate(
          simulated_years =
            rnorm(
              n(),
              mean = years,
              sd = se
            )
        )
      
      
      # Fit Marmot curve
      
      fit <- loess(
        simulated_years ~ deprivation_percentile,
        data = simulated_data,
        span = span
      )
      
      
      # Predict curve
      
      prediction <- predict(
        fit,
        prediction_grid
      )
      
      
      data.frame(
        simulation = i,
        deprivation_percentile =
          prediction_grid$deprivation_percentile,
        years = prediction
      )
      
    },
    
    future.seed = TRUE
    
  )
  
  
  bind_rows(simulations)
  
}



#############################################################
# Select curve to simulate
# Change these two lines for other curves
#############################################################

simulation_data <- marmot_long %>%
  filter(
    sex == "Male",
    measure == "Healthy life expectancy"
  )


library(tidyverse)
library(splines)

# -----------------------------------------------------------------------------
# 1. Setup Grid & Projection Matrix
# -----------------------------------------------------------------------------
n_sims <- 10000
grid_points <- 100

# Fixed prediction grid across percentiles (1 to 100)
grid_x <- seq(
  min(simulation_data$deprivation_percentile, na.rm = TRUE),
  max(simulation_data$deprivation_percentile, na.rm = TRUE),
  length.out = grid_points
)

# Natural spline basis (df = 4 captures the Marmot curve curvature perfectly)
X_obs  <- model.matrix(~ ns(deprivation_percentile, df = 4), data = simulation_data)
X_grid <- model.matrix(~ ns(grid_x, df = 4), data = tibble(deprivation_percentile = grid_x))

# Calculate projection matrix M (100 x N_msoa)
M <- X_grid %*% solve(crossprod(X_obs)) %*% t(X_obs)

# -----------------------------------------------------------------------------
# 2. Vectorized Monte Carlo Simulation
# -----------------------------------------------------------------------------
set.seed(42)
n_msoa <- nrow(simulation_data)
se_vec <- (simulation_data$upper - simulation_data$lower) / 3.92

# Draw a matrix of size (N_msoa x 10,000) in one shot
Y_sims <- matrix(
  rnorm(n_msoa * n_sims, mean = simulation_data$years, sd = se_vec),
  nrow = n_msoa,
  ncol = n_sims
)

# INSTANT MATRIX MULTIPLY: Yields a (100 x 10,000) matrix of predicted curves
Pred_matrix <- M %*% Y_sims

# -----------------------------------------------------------------------------
# 3. Reshape & Plot
# -----------------------------------------------------------------------------
# 100 grid points * 10,000 sims = 1,000,000 rows (very lightweight for ggplot)
sim_df <- as.data.frame(Pred_matrix) %>%
  mutate(x = grid_x) %>%
  pivot_longer(-x, names_to = "sim_id", values_to = "y")

ggplot(sim_df, aes(x = x, y = y, group = sim_id)) +
  # High transparency (alpha = 0.008) allows 10,000 lines to form a smooth density cloud
  geom_line(alpha = 0.008, color = "black", linewidth = 0.2) +
  theme_minimal() +
  labs(
    title = "Monte Carlo uncertainty around Marmot curve",
    subtitle = paste(format(n_sims, big.mark = ","), "parallel simulations using published 95% confidence intervals"),
    x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
    y = "Healthy life expectancy (years)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )












library(tidyverse)

# ============================================================
# Prepare Marmot data
# ============================================================

plot_data <- marmot_long %>%
  filter(
    sex == "Male",
    measure == "Healthy life expectancy"
  ) %>%
  arrange(deprivation_percentile)


# ============================================================
# GRAPH 1
# Lower CI and Upper CI as separate scatter plots
# ============================================================

ggplot(plot_data, aes(x = deprivation_percentile)) +
  
  # Lower CI observations
  geom_point(
    aes(y = lower),
    colour = "blue",
    alpha = 0.35,
    size = 1.2
  ) +
  
  # Upper CI observations
  geom_point(
    aes(y = upper),
    colour = "red",
    alpha = 0.35,
    size = 1.2
  ) +
  
  # Smooth curve through lower CI values
  geom_smooth(
    aes(y = lower),
    method = "loess",
    span = 0.8,
    se = FALSE,
    colour = "blue",
    linewidth = 1.3
  ) +
  
  # Smooth curve through upper CI values
  geom_smooth(
    aes(y = upper),
    method = "loess",
    span = 0.8,
    se = FALSE,
    colour = "red",
    linewidth = 1.3
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Upper and Lower 95% Confidence Limits",
    subtitle = "Healthy life expectancy — Male",
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# ============================================================
# GRAPH 2
# HLE estimate with confidence intervals
# ============================================================

ggplot(
  plot_data,
  aes(
    x = deprivation_percentile,
    y = years
  )
) +
  
  # Confidence interval for each MSOA
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.20,
    width = 0
  ) +
  
  # Actual HLE estimates
  geom_point(
    alpha = 0.35,
    size = 1.2
  ) +
  
  # Marmot curve through HLE estimates
  geom_smooth(
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.4
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Healthy Life Expectancy by Deprivation",
    subtitle = "Male MSOAs with 95% confidence intervals",
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )





#table

# ============================================================
# TABLE: NUMBER OF MSOAs BY SEX AND HLE 95% CI WIDTH
# ============================================================

library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1. Calculate CI width and create groups
# ------------------------------------------------------------

ci_table <- marmot_long %>%
  filter(
    measure == "Healthy life expectancy"
  ) %>%
  mutate(
    
    # Width of the 95% CI
    ci_width = upper - lower,
    
    # CI width categories
    ci_group = case_when(
      ci_width > 10 ~ ">10 years",
      ci_width >= 7 & ci_width <= 10 ~ "7–10 years",
      ci_width >= 5 & ci_width < 7 ~ "5–<7 years",
      ci_width >= 3 & ci_width < 5 ~ "3–<5 years",
      ci_width < 3 ~ "<3 years",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(ci_group)
  )


# ------------------------------------------------------------
# 2. Count MSOAs by sex and CI group
# ------------------------------------------------------------

ci_table_counts <- ci_table %>%
  count(
    sex,
    ci_group
  ) %>%
  complete(
    sex,
    ci_group = c(
      ">10 years",
      "7–10 years",
      "5–<7 years",
      "3–<5 years",
      "<3 years"
    ),
    fill = list(n = 0)
  ) %>%
  mutate(
    ci_group = factor(
      ci_group,
      levels = c(
        ">10 years",
        "7–10 years",
        "5–<7 years",
        "3–<5 years",
        "<3 years"
      )
    )
  ) %>%
  arrange(
    sex,
    ci_group
  )


# View table
ci_table_counts


# ------------------------------------------------------------
# 3. Put Male and Female into columns
# ------------------------------------------------------------

ci_table_wide <- ci_table_counts %>%
  select(
    sex,
    ci_group,
    n
  ) %>%
  pivot_wider(
    names_from = sex,
    values_from = n,
    values_fill = 0
  ) %>%
  rename(
    `CI width` = ci_group
  )


# View final table
ci_table_wide


# ============================================================
# 4. Add totals
# ============================================================

ci_table_final <- ci_table_counts %>%
  select(
    sex,
    ci_group,
    n
  ) %>%
  pivot_wider(
    names_from = sex,
    values_from = n,
    values_fill = 0
  ) %>%
  rename(
    `CI width` = ci_group
  ) %>%
  mutate(
    Total = rowSums(
      across(
        where(is.numeric)
      )
    )
  )


# Add total row
ci_table_final <- bind_rows(
  
  ci_table_final,
  
  ci_table_final %>%
    summarise(
      `CI width` = "Total",
      across(
        where(is.numeric),
        sum
      )
    )
)


# Display final table
ci_table_final


# ============================================================
# 5. Optional: add percentages within sex
# ============================================================

ci_table_percentages <- ci_table_counts %>%
  group_by(sex) %>%
  mutate(
    percentage = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    percentage = round(percentage, 1)
  )


ci_table_percentages



library(tidyverse)

# ============================================================
# 1. Select the data
# ============================================================

plot_data <- marmot_long %>%
  filter(
#    sex == "Male",
    measure == "Healthy life expectancy"
  ) %>%
  mutate(
    ci_width = upper - lower,
    ci_group = if_else(
      ci_width < 3,
      "CI < 3 years",
      "CI >= 3 years"
    )
  )


# ============================================================
# 2. Count how many MSOAs have CI < 3 years
# ============================================================

narrow_ci <- plot_data %>%
  filter(ci_width < 3)

wide_ci <- plot_data %>%
  filter(ci_width >= 3)

cat(
  "MSOAs with CI < 3 years:",
  nrow(narrow_ci),
  "\n"
)

cat(
  "MSOAs with CI >= 3 years:",
  nrow(wide_ci),
  "\n"
)

cat(
  "Total MSOAs:",
  nrow(plot_data),
  "\n"
)


# ============================================================
# 3. Optional: percentage in each group
# ============================================================

plot_data %>%
  count(ci_group) %>%
  mutate(
    percentage = 100 * n / sum(n)
  )


# ============================================================
# 4. Plot MSOAs with CI < 3 years
# ============================================================

ggplot(
  narrow_ci,
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
    alpha = 0.35
  ) +
  
  # HLE estimate
  geom_point(
    size = 1.4,
    alpha = 0.55
  ) +
  
  # Marmot curve
  geom_smooth(
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Marmot Curve: MSOAs with CI < 3 Years",
    subtitle = paste0(
      "Male healthy life expectancy — ",
      nrow(narrow_ci),
      " MSOAs"
    ),
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 5. Plot MSOAs with CI >= 3 years
# ============================================================

ggplot(
  wide_ci,
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
    alpha = 0.35
  ) +
  
  # HLE estimate
  geom_point(
    size = 1.4,
    alpha = 0.55
  ) +
  
  # Marmot curve
  geom_smooth(
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Marmot Curve: MSOAs with CI >= 3 Years",
    subtitle = paste0(
      "Male healthy life expectancy — ",
      nrow(wide_ci),
      " MSOAs"
    ),
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 6. Alternative: show BOTH groups on one graph
# ============================================================

ggplot(
  plot_data,
  aes(
    x = deprivation_percentile,
    y = years,
    colour = ci_group
  )
) +
  
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0,
    alpha = 0.25
  ) +
  
  geom_point(
    alpha = 0.5,
    size = 1.3
  ) +
  
  geom_smooth(
    aes(group = ci_group),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  theme_minimal() +
  
  labs(
    title = "Healthy Life Expectancy by CI Width",
    subtitle = "Comparison of MSOAs with confidence intervals below and above 3 years",
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)",
    colour = "Confidence interval"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )













library(tidyverse)

# ============================================================
# 1. Select the data
# ============================================================

simulation_data <- marmot_long %>%
  filter(
    sex == "Male",
    measure == "Healthy life expectancy"
  ) %>%
  arrange(deprivation_percentile)


# ============================================================
# 2. Number of simulations
# ============================================================

n_sim <- 10000


# ============================================================
# 3. Run Monte Carlo simulations
# ============================================================

set.seed(123)

all_curves <- vector("list", n_sim)

for (i in 1:n_sim) {
  
  # Randomly select one value between lower and upper
  simulated_data <- simulation_data %>%
    mutate(
      simulated_years = runif(
        n(),
        min = lower,
        max = upper
      )
    )
  
  # Fit Marmot curve to simulated values
  fit <- loess(
    simulated_years ~ deprivation_percentile,
    data = simulated_data,
    span = 0.8
  )
  
  # Predict curve at 100 percentile points
  prediction_grid <- data.frame(
    deprivation_percentile = seq(1, 100, length.out = 100)
  )
  
  prediction <- predict(
    fit,
    newdata = prediction_grid
  )
  
  # Store curve
  all_curves[[i]] <- data.frame(
    simulation = i,
    deprivation_percentile =
      prediction_grid$deprivation_percentile,
    years = prediction
  )
}


# ============================================================
# 4. Combine all simulations
# ============================================================

all_curves <- bind_rows(all_curves)


# ============================================================
# 5. Plot all possible curves
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
      n_sim,
      " simulations using random values between each MSOA's lower and upper CI"
    ),
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )




#parall version

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
  slice_head(n = 20)


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


