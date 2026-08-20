
# ============================================================
# MSOA MARMOT CURVES
# 2021 HLE / LE BY INCOME DEPRIVATION
#
# Date: 30/07/2026
# ============================================================

rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)


setwd(
  "~/Analysis and Modelling general/2011-2021 HLE by MSOA"
)


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


#For regional data
MOSa_Region_2021 <- read_csv("Raw data/MOSa_Region_2021.csv")

# ============================================================
# 3. PREPARE 2021 LIFE EXPECTANCY DATA
# ============================================================

le_2021 <- lemsoa %>%
  filter(
    Country == "England",
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
# 4. PREPARE 2021 HEALTHY LIFE EXPECTANCY DATA
# ============================================================

hle_2021 <- hslemsoa %>%
  filter(
    Country == "England",
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
# 5. COMBINE 2021 LE + HLE
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
  )


# ============================================================
# 6. LOAD IMD 2025
# ============================================================

IMD_2025 <- read_csv(
  "Working files/imd_2025_final_msoa.csv"
)


glimpse(IMD_2025)


# ============================================================
# 7. LOAD MSOA -> REGION LOOKUP
# ============================================================
#
# This file contains the relationship between each MSOA
# and its region.
#
# We only need:
#
# MSOA21CD = MSOA code
# RGN22CD  = region code
# RGN22NM  = region name

# ============================================================
# 8. ADD REGION TO MSOA DATA
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
# 9. JOIN HEALTH DATA TO IMD + REGION
# ============================================================

marmot_data <- IMD_2025 %>%
  left_join(
    health_2021,
    by = "MSOA21CD"
  ) %>%
  left_join(
    MSOA_Region_2021,
    by = "MSOA21CD"
  ) %>%
  rename(
    MSOA21NM = MSOA21NM.x
  )


# Check


glimpse(marmot_data)


marmot_data %>%
  summarise(
    n = n(),
    
    missing_HLE = sum(is.na(HLE_2021)),
    missing_LE = sum(is.na(LE_2021)),
    
    missing_HLE_LCI =
      sum(is.na(HLE_2021_LCI)),
    
    missing_HLE_UCI =
      sum(is.na(HLE_2021_UCI)),
    
    missing_LE_LCI =
      sum(is.na(LE_2021_LCI)),
    
    missing_LE_UCI =
      sum(is.na(LE_2021_UCI)),
    
    missing_region_code =
      sum(is.na(RGN22CD)),
    
    missing_region_name =
      sum(is.na(RGN22NM))
  )


# Check regional coverage

marmot_data %>%
  count(
    RGN22CD,
    RGN22NM,
    sort = TRUE
  )


# ============================================================
# 10. IDENTIFY INCOMPLETE MSOAs
# ============================================================

marmot_data_NA <- marmot_data %>%
  filter(
    if_any(
      c(
        HLE_2021,
        HLE_2021_LCI,
        HLE_2021_UCI,
        LE_2021,
        LE_2021_LCI,
        LE_2021_UCI,
        income_average_score,
        RGN22CD,
        RGN22NM
      ),
      is.na
    )
  )


nrow(marmot_data_NA)


# ============================================================
# 11. KEEP COMPLETE CASES
# ============================================================

marmot_data <- marmot_data %>%
  filter(
    if_all(
      c(
        HLE_2021,
        HLE_2021_LCI,
        HLE_2021_UCI,
        LE_2021,
        LE_2021_LCI,
        LE_2021_UCI,
        income_average_score,
        RGN22CD,
        RGN22NM
      ),
      ~ !is.na(.)
    )
  )


nrow(marmot_data)


```r
# ============================================================
# CREATE MSOA-LEVEL DEPRIVATION PERCENTILE
# ============================================================
#
# Higher income_average_score = MORE income deprivation
#
# Therefore:
#   1   = most deprived
#   100 = least deprived
#
# The ranking is calculated ONCE per MSOA,
# not separately for males and females.
# ============================================================


# First create one row per MSOA

msoa_deprivation <- marmot_data %>%
  select(
    MSOA21CD,
    income_average_score
  ) %>%
  distinct(MSOA21CD, .keep_all = TRUE) %>%
  arrange(desc(income_average_score)) %>%
  mutate(
    deprivation_percentile =
      1 + 99 * (row_number() - 1) / (n() - 1)
  )


# Check the ranking

msoa_deprivation %>%
  summarise(
    n_msoa = n(),
    minimum_percentile = min(deprivation_percentile),
    maximum_percentile = max(deprivation_percentile)
  )


# Check the most deprived MSOAs

msoa_deprivation %>%
  arrange(deprivation_percentile) %>%
  select(
    MSOA21CD,
    income_average_score,
    deprivation_percentile
  ) %>%
  head(10)


# Check the least deprived MSOAs

msoa_deprivation %>%
  arrange(desc(deprivation_percentile)) %>%
  select(
    MSOA21CD,
    income_average_score,
    deprivation_percentile
  ) %>%
  head(10)


# ============================================================
# ADD THE MSOA-LEVEL PERCENTILE BACK TO THE MAIN DATA
# ============================================================

marmot_data <- marmot_data %>%
  select(
    -deprivation_percentile
  ) %>%
  left_join(
    msoa_deprivation %>%
      select(
        MSOA21CD,
        deprivation_percentile
      ),
    by = "MSOA21CD"
  )


# Check

marmot_data %>%
  summarise(
    n = n(),
    missing_percentile =
      sum(is.na(deprivation_percentile)),
    min_percentile =
      min(deprivation_percentile, na.rm = TRUE),
    max_percentile =
      max(deprivation_percentile, na.rm = TRUE)
  )
```



# ============================================================
# 13. PREPARE LONG DATA FOR MARMOT PLOTS
# ============================================================

marmot_long <- bind_rows(
  
  # ----------------------------------------------------------
  # Male HLE
  # ----------------------------------------------------------
  
  marmot_data %>%
    filter(Sex == "Male") %>%
    transmute(
      RGN22CD,
      RGN22NM,
      MSOA21CD,
      MSOA21NM,
      deprivation_percentile,
      sex = "Male",
      measure = "Healthy life expectancy",
      years = HLE_2021,
      lower = HLE_2021_LCI,
      upper = HLE_2021_UCI
    ),
  
  # ----------------------------------------------------------
  # Male LE
  # ----------------------------------------------------------
  
  marmot_data %>%
    filter(Sex == "Male") %>%
    transmute(
      RGN22CD,
      RGN22NM,
      MSOA21CD,
      MSOA21NM,
      deprivation_percentile,
      sex = "Male",
      measure = "Life expectancy",
      years = LE_2021,
      lower = LE_2021_LCI,
      upper = LE_2021_UCI
    ),
  
  # ----------------------------------------------------------
  # Female HLE
  # ----------------------------------------------------------
  
  marmot_data %>%
    filter(Sex == "Female") %>%
    transmute(
      RGN22CD,
      RGN22NM,
      MSOA21CD,
      MSOA21NM,
      deprivation_percentile,
      sex = "Female",
      measure = "Healthy life expectancy",
      years = HLE_2021,
      lower = HLE_2021_LCI,
      upper = HLE_2021_UCI
    ),
  
  # ----------------------------------------------------------
  # Female LE
  # ----------------------------------------------------------
  
  marmot_data %>%
    filter(Sex == "Female") %>%
    transmute(
      RGN22CD,
      RGN22NM,
      MSOA21CD,
      MSOA21NM,
      deprivation_percentile,
      sex = "Female",
      measure = "Life expectancy",
      years = LE_2021,
      lower = LE_2021_LCI,
      upper = HLE_2021_UCI
    )
  
) %>%
  drop_na(
    years,
    lower,
    upper,
    deprivation_percentile
  )

# ============================================================
# 14. CHECK THE RESULTING LONG DATA
# ============================================================

glimpse(marmot_long)

nrow(marmot_long)


# Check that each MSOA has a region

marmot_long %>%
  select(
    MSOA21CD,
    MSOA21NM,
    RGN22CD,
    RGN22NM
  ) %>%
  distinct() %>%
  head(20)



# Check the resulting long data

glimpse(marmot_long)

nrow(marmot_long)


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
      colour = "sex",
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




# ============================================================
# DESCRIPTIVE STATISTICS
# Based on YOUR marmot_data / marmot_long structure
# ============================================================

library(tidyverse)


# ============================================================
# 1. DESCRIPTIVE STATISTICS: MOST VS LEAST DEPRIVED 10%
# ============================================================

marmot_summary <- marmot_long %>%
  mutate(
    deprivation_group = case_when(
      deprivation_percentile <= 10 ~ "Most deprived 10%",
      deprivation_percentile >= 90 ~ "Least deprived 10%",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(deprivation_group))


descriptive_stats <- marmot_summary %>%
  group_by(
    deprivation_group,
    sex,
    measure
  ) %>%
  summarise(
    Mean = mean(years, na.rm = TRUE),
    SD = sd(years, na.rm = TRUE),
    Min = min(years, na.rm = TRUE),
    Max = max(years, na.rm = TRUE),
    Range = Max - Min,
    n = n(),
    .groups = "drop"
  )


descriptive_stats


# ============================================================
# 2. HIGHEST 10 MSOAs
# ============================================================

highest_msoas <- marmot_long %>%
  group_by(
    sex,
    measure
  ) %>%
  slice_max(
    order_by = years,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  transmute(
    Sex = sex,
    Measure = measure,
    MSOA = MSOA21NM,
    Code = MSOA21CD,
    Value = years
  )


highest_msoas


# ============================================================
# 3. LOWEST 10 MSOAs
# ============================================================

lowest_msoas <- marmot_long %>%
  group_by(
    sex,
    measure
  ) %>%
  slice_min(
    order_by = years,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  transmute(
    Sex = sex,
    Measure = measure,
    MSOA = MSOA21NM,
    Code = MSOA21CD,
    Value = years
  )


lowest_msoas


# ============================================================
# 4. TOP VS BOTTOM 10% DEPRIVATION DIFFERENCE
# ============================================================

top_bottom_difference <- marmot_long %>%
  mutate(
    deprivation_group = case_when(
      deprivation_percentile <= 10 ~ "Most deprived 10%",
      deprivation_percentile >= 90 ~ "Least deprived 10%",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(deprivation_group)) %>%
  group_by(
    sex,
    measure
  ) %>%
  summarise(
    most_deprived_mean =
      mean(
        years[deprivation_group == "Most deprived 10%"],
        na.rm = TRUE
      ),
    
    least_deprived_mean =
      mean(
        years[deprivation_group == "Least deprived 10%"],
        na.rm = TRUE
      ),
    
    difference =
      least_deprived_mean - most_deprived_mean,
    
    .groups = "drop"
  )


top_bottom_difference


# ============================================================
# 5. REGIONAL DESCRIPTIVE STATISTICS
# ============================================================

regional_descriptive_stats <- marmot_long %>%
  group_by(
    RGN22NM,
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
  ) %>%
  rename(
    Region = RGN22NM
  )


regional_descriptive_stats


# ============================================================
# 6. REGIONAL BAR CHART
# ============================================================

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
  facet_wrap(
    ~ measure
  ) +
  coord_flip() +
  labs(
    x = "Region",
    y = "Mean years",
    fill = "Sex",
    title = "Average life expectancy and healthy life expectancy by region"
  ) +
  theme_minimal()


# ============================================================
# 7. CREATE INCOME DEPRIVATION DECILES
# ============================================================
#
# Your deprivation_percentile already has:
#
# 1   = most deprived
# 100 = least deprived
#
# Therefore we can create deciles directly from it.
# ============================================================

marmot_data <- marmot_data %>%
  mutate(
    income_decile = ntile(
      deprivation_percentile,
      10
    )
  )


# Check
marmot_data %>%
  count(income_decile)


# ============================================================
# 8. INCOME DECILE STATISTICS
# ============================================================

income_decile_stats <- marmot_long %>%
  mutate(
    income_decile = ntile(
      deprivation_percentile,
      10
    )
  ) %>%
  group_by(
    income_decile,
    sex,
    measure
  ) %>%
  summarise(
    n = n(),
    Mean_years = mean(years, na.rm = TRUE),
    SD = sd(years, na.rm = TRUE),
    .groups = "drop"
  )


income_decile_stats


# ============================================================
# 9. INCOME DECILE PLOT
# ============================================================

ggplot(
  income_decile_stats,
  aes(
    x = income_decile,
    y = Mean_years,
    colour = interaction(sex, measure),
    group = interaction(sex, measure)
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = 1:10,
    labels = paste0("D", 1:10)
  ) +
  labs(
    x = "Income deprivation decile\n(1 = most deprived, 10 = least deprived)",
    y = "Mean years",
    colour = "",
    title = "Life expectancy and healthy life expectancy by income deprivation decile"
  ) +
  theme_minimal()


# ============================================================
# 10. REGIONAL DEPRIVATION STATISTICS
# ============================================================

regional_decile_stats <- marmot_long %>%
  mutate(
    income_decile = ntile(
      deprivation_percentile,
      10
    )
  ) %>%
  group_by(
    RGN22NM,
    income_decile,
    sex,
    measure
  ) %>%
  summarise(
    Mean_years = mean(years, na.rm = TRUE),
    .groups = "drop"
  )


regional_decile_stats


# ============================================================
# 11. REGIONAL DEPRIVATION GAPS
# ============================================================

regional_deprivation_gap <- regional_decile_stats %>%
  group_by(
    RGN22NM,
    sex,
    measure
  ) %>%
  summarise(
    Most_deprived =
      Mean_years[income_decile == 1] %>%
      mean(na.rm = TRUE),
    
    Least_deprived =
      Mean_years[income_decile == 10] %>%
      mean(na.rm = TRUE),
    
    deprivation_gap =
      Least_deprived - Most_deprived,
    
    .groups = "drop"
  ) %>%
  rename(
    Region = RGN22NM
  )


regional_deprivation_gap


# ============================================================
# 12. REGIONAL HLE GAP PLOT
# ============================================================

ggplot(
  regional_deprivation_gap %>%
    filter(
      measure == "Healthy life expectancy"
    ),
  aes(
    x = reorder(Region, deprivation_gap),
    y = deprivation_gap,
    fill = sex
  )
) +
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  labs(
    x = "Region",
    y = "Healthy life expectancy gap (years)",
    fill = "Sex",
    title = "Healthy life expectancy gap between most and least deprived areas",
    subtitle = "Least deprived decile minus most deprived decile"
  ) +
  theme_minimal()


# ============================================================
# 13. CONFIDENCE INTERVAL WIDTH
# ============================================================

hle_ci_summary <- marmot_long %>%
  filter(
    measure == "Healthy life expectancy"
  ) %>%
  mutate(
    ci_width = upper - lower
  ) %>%
  group_by(
    sex
  ) %>%
  summarise(
    n = n(),
    mean_ci = mean(ci_width, na.rm = TRUE),
    median_ci = median(ci_width, na.rm = TRUE),
    min_ci = min(ci_width, na.rm = TRUE),
    max_ci = max(ci_width, na.rm = TRUE),
    .groups = "drop"
  )


hle_ci_summary


# ============================================================
# 14. MSOAs WITH THE LARGEST CI
# ============================================================

hle_large_ci <- marmot_long %>%
  filter(
    measure == "Healthy life expectancy"
  ) %>%
  mutate(
    ci_width = upper - lower
  ) %>%
  arrange(
    desc(ci_width)
  ) %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM,
    deprivation_percentile,
    sex,
    years,
    lower,
    upper,
    ci_width
  )


hle_large_ci


# ============================================================
# 15. CI WIDTH TABLE BY SEX
# ============================================================

ci_table <- marmot_long %>%
  filter(
    measure == "Healthy life expectancy"
  ) %>%
  mutate(
    ci_width = upper - lower,
    
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


ci_table_counts


# ============================================================
# 16. WIDE CI TABLE
# ============================================================

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


ci_table_wide


# ============================================================
# 17. CI TABLE WITH TOTALS
# ============================================================

ci_table_final <- ci_table_wide %>%
  mutate(
    Total = rowSums(
      across(
        where(is.numeric)
      )
    )
  )


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


ci_table_final


# ============================================================
# 18. CI PERCENTAGES WITHIN SEX
# ============================================================

ci_table_percentages <- ci_table_counts %>%
  group_by(
    sex
  ) %>%
  mutate(
    percentage = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    percentage = round(
      percentage,
      1
    )
  )


ci_table_percentages


# ============================================================
# 19. MALE HLE: LOWER VS UPPER CI
# ============================================================

plot_data <- marmot_long %>%
  filter(
    sex == "Male",
    measure == "Healthy life expectancy"
  ) %>%
  arrange(
    deprivation_percentile
  )


ggplot(
  plot_data,
  aes(
    x = deprivation_percentile
  )
) +
  
  # Lower confidence limit
  geom_point(
    aes(y = lower, colour = "Lower"),
    alpha = 0.35,
    size = 1.2
  ) +
  
  # Upper confidence limit
  geom_point(
    aes(y = upper, colour = "Upper"),
    alpha = 0.35,
    size = 1.2
  ) +
  
  # Lower LOESS
  geom_smooth(
    aes(y = lower, colour = "Lower"),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  
  # Upper LOESS
  geom_smooth(
    aes(y = upper, colour = "Upper"),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  
  scale_colour_manual(
    values = c(
      "Lower" = "steelblue",
      "Upper" = "firebrick"
    )
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Upper and Lower 95% Confidence Limits",
    subtitle = "Healthy life expectancy — Male",
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)",
    colour = "Confidence limit"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
`



# ============================================================
# 20. MALE HLE WITH CONFIDENCE INTERVALS
# ============================================================

ggplot(
  plot_data,
  aes(
    x = deprivation_percentile,
    y = years
  )
) +
  
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.20,
    width = 0
  ) +
  
  geom_point(
    alpha = 0.35,
    size = 1.2
  ) +
  
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


# ============================================================
# 21. CI < 3 YEARS VS CI >= 3 YEARS
# ============================================================

plot_data <- marmot_long %>%
  filter(
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


narrow_ci <- plot_data %>%
  filter(
    ci_width < 3
  )


wide_ci <- plot_data %>%
  filter(
    ci_width >= 3
  )


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
  "Total observations:",
  nrow(plot_data),
  "\n"
)


# ============================================================
# 22. PLOT CI < 3 YEARS
# ============================================================

ggplot(
  narrow_ci,
  aes(
    x = deprivation_percentile,
    y = years
  )
) +
  
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0,
    alpha = 0.35
  ) +
  
  geom_point(
    size = 1.4,
    alpha = 0.55
  ) +
  
  geom_smooth(
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  
  facet_wrap(
    ~ sex
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Marmot Curve: MSOAs with CI < 3 Years",
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 23. PLOT CI >= 3 YEARS
# ============================================================

ggplot(
  wide_ci,
  aes(
    x = deprivation_percentile,
    y = years
  )
) +
  
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0,
    alpha = 0.35
  ) +
  
  geom_point(
    size = 1.4,
    alpha = 0.55
  ) +
  
  geom_smooth(
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  
  facet_wrap(
    ~ sex
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Marmot Curve: MSOAs with CI >= 3 Years",
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 24. BOTH CI GROUPS ON ONE GRAPH
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
    aes(
      group = ci_group
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.3
  ) +
  
  facet_wrap(
    ~ sex
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





# ============================================================
 MONTE CARLO SIMULATION - random / normal disribution - check
# ============================================================
#

# ============================================================

library(future)
library(future.apply)


# ------------------------------------------------------------
# Select curve
# ------------------------------------------------------------

simulation_data <- marmot_long %>%
  filter(
    sex == "Male",
    measure == "Healthy life expectancy"
  ) %>%
  arrange(
    deprivation_percentile
  )


# ------------------------------------------------------------
# Number of simulations
# ------------------------------------------------------------

n_sim <- 10000


# ------------------------------------------------------------
# Prediction grid
# ------------------------------------------------------------

prediction_grid <- data.frame(
  deprivation_percentile = seq(
    1,
    100,
    length.out = 100
  )
)


# ------------------------------------------------------------
# Parallel processing
# ------------------------------------------------------------

cores <- max(
  1,
  parallel::detectCores() - 1
)

plan(
  multisession,
  workers = cores
)


# ------------------------------------------------------------
# Monte Carlo simulations
# ------------------------------------------------------------

set.seed(123)

all_curves <- future_lapply(
  
  1:n_sim,
  
  function(i) {
    
    # Simulate from CI
    simulated_years <- runif(
      nrow(simulation_data),
      min = simulation_data$lower,
      max = simulation_data$upper
    )
    
    # Fit LOESS
    fit <- loess(
      simulated_years ~ deprivation_percentile,
      data = simulation_data,
      span = 0.8
    )
    
    # Predict
    prediction <- predict(
      fit,
      newdata = prediction_grid
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


# ------------------------------------------------------------
# Combine
# ------------------------------------------------------------

all_curves <- bind_rows(
  all_curves
)


# ------------------------------------------------------------
# Plot Monte Carlo curves
# ------------------------------------------------------------

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
      format(
        n_sim,
        big.mark = ","
      ),
      " simulations using values sampled within published 95% confidence intervals"
    ),
    x = "Income deprivation percentile",
    y = "Healthy life expectancy (years)"
  ) +
  
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# ------------------------------------------------------------
# Shut down parallel workers
# ------------------------------------------------------------

plan(
  sequential
)











#Email

library(dplyr)

library(dplyr)

hle <- marmot_long %>%
  filter(measure == "Healthy life expectancy")

# Calculate the range separately for males and females
hle %>%
  group_by(sex) %>%
  summarise(
    lowest_HLE = min(years, na.rm = TRUE),
    highest_HLE = max(years, na.rm = TRUE),
    gap = highest_HLE - lowest_HLE,
    .groups = "drop"
  )


hle %>%
  group_by(sex) %>%
  filter(
    years == min(years, na.rm = TRUE) |
      years == max(years, na.rm = TRUE)
  ) %>%
  select(
    sex,
    RGN22NM,
    MSOA21NM,
    deprivation_percentile,
    years
  ) %>%
  arrange(sex, years)


top_10_by_sex <- hle %>%
  group_by(sex) %>%
  arrange(desc(years)) %>%
  slice_head(n = 10) %>%
  select(
    sex,
    RGN22NM,
    MSOA21NM,
    deprivation_percentile,
    years
  )

top_10_by_sex

bottom_10_by_sex <- hle %>%
  group_by(sex) %>%
  arrange(years) %>%
  slice_head(n = 10) %>%
  select(
    sex,
    RGN22NM,
    MSOA21NM,
    deprivation_percentile,
    years
  )

bottom_10_by_sex




hle %>%
  group_by(sex) %>%
  summarise(
    minimum_HLE = min(years, na.rm = TRUE),
    maximum_HLE = max(years, na.rm = TRUE),
    .groups = "drop"
  )


sex_gap <- hle %>%
  group_by(sex) %>%
  summarise(
    top_10_mean = mean(
      sort(years, decreasing = TRUE)[1:10],
      na.rm = TRUE
    ),
    bottom_10_mean = mean(
      sort(years, decreasing = FALSE)[1:10],
      na.rm = TRUE
    ),
    gap = top_10_mean - bottom_10_mean,
    .groups = "drop"
  )

sex_gap
