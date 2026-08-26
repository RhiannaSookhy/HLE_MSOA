
# ============================================================
# MSOA HEALTHY LIFE EXPECTANCY 2011 -> 2021
# All Comparison HLE plots are here (although not methodologically sound due to ONS chnages)
#
# Date: 30/07/2026
# ============================================================


rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

setwd("C:/Users/rhianna.sookhy/OneDrive - The Health Foundation/Shortcuts/Analysis - 11-CAT/1. Work programme/Healthy Life Expectancy - strategy launch/Phase 2")


#Load exisiting files
msoa_data <- read_csv(
  "Working files/MSOA_2011_HLE_IMD.csv"
)


# Inspect structure
glimpse(msoa_data)


#New data

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
    LE_2021 = LE
  )

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
    HLE_2021 = HLE
  )


nrow(le_2021)
nrow(hle_2021)



health_2021 <- le_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    Sex,
    LE_2021
  ) %>%
  left_join(
    hle_2021 %>%
      select(
        MSOA21CD,
        Sex,
        HLE_2021
      ),
    by = c("MSOA21CD", "Sex")
  )




#checks


# Check missing health measures
health_2021 %>%
  summarise(
    missing_HLE = sum(is.na(HLE_2021)),
    missing_LE  = sum(is.na(LE_2021))
  )



health_2021 %>%
  summarise(
    n = n(),
    missing_LE = sum(is.na(LE_2021)),
    missing_HLE = sum(is.na(HLE_2021))
  )


health_2021_wide <- health_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    Sex,
    LE_2021,
    HLE_2021
  ) %>%
  pivot_wider(
    names_from = Sex,
    values_from = c(LE_2021, HLE_2021),
    names_glue = "{.value}_{ifelse(Sex == 'Male', 'M', 'F')}"
  )

#6856 MSOA's 

#Merge

msoa_data <- msoa_data %>%
  left_join(
    health_2021_wide %>%
      select(
        MSOA21CD,
        HLE_2021_M,
        LE_2021_M,
        HLE_2021_F,
        LE_2021_F
      ),
    by = "MSOA21CD"
  )



#Check key variables

required_variables <- c(
  "MSOA21CD",
  "HLE_2011_M",
  "LE_2011_M",
  "HLE_2011_F",
  "LE_2011_F",
  "HLE_2021_M",
  "LE_2021_M",
  "HLE_2021_F",
  "LE_2021_F",
  "income_average_score_2015",
  "income_average_score_2025"
)


missing_summary <- msoa_data %>%
  summarise(
    across(
      all_of(required_variables),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    percentage_missing =
      100 * n_missing / nrow(msoa_data)
  )


missing_summary


#Missing rows

msoa_missing <- msoa_data %>%
  filter(
    if_any(
      all_of(required_variables),
      is.na
    )
  )


# View these before deciding whether to exclude
msoa_missing


# Number of incomplete rows
nrow(msoa_missing)


###COME BACK TOO!


#tHIS IS COMPLETE CASES ONLY

analysis_data <- msoa_data %>%
  filter(
    if_all(
      all_of(required_variables),
      ~ !is.na(.)
    )
  )


# ============================================================
# CALCULATE INCOME DEPRIVATION PERCENTILES
# ============================================================
#
# Higher income score = more deprived.
#
# We rank each year independently.
#
# percentile 1   = most deprived
# percentile 100 = least deprived
#
# 2015 score is used for 2011 health.
# 2025 score is used for 2021 health.
# ============================================================

analysis_data <- analysis_data %>%
  mutate(
    
    income_percentile_2015 =
      100 - (percent_rank(income_average_score_2015) * 99),
    
    income_percentile_2025 =
      100 - (percent_rank(income_average_score_2025) * 99)
    
  )


# 
# analysis_data <- analysis_data %>%
#   mutate(
#     income_percentile_2015 =
#       round(income_percentile_2015, 2),
#     
#     income_percentile_2025 =
#       round(income_percentile_2025, 2)
#   )




analysis_data <- analysis_data %>%
  mutate(
    
    # Male
    change_HLE_M =
      HLE_2021_M - HLE_2011_M,
    
    change_LE_M =
      LE_2021_M - LE_2011_M,
    
    
    # Female
    change_HLE_F =
      HLE_2021_F - HLE_2011_F,
    
    change_LE_F =
      LE_2021_F - LE_2011_F,
    
    
    # Change in income deprivation score
    change_income_score =
      income_average_score_2025 -
      income_average_score_2015,
    
    
    # Change in income deprivation percentile
    change_income_percentile =
      income_percentile_2025 -
      income_percentile_2015
  )




national_change_summary <- analysis_data %>%
  summarise(
    
    n = n(),
    
    # Male HLE
    mean_HLE_change_M = mean(change_HLE_M),
    median_HLE_change_M = median(change_HLE_M),
    sd_HLE_change_M = sd(change_HLE_M),
    min_HLE_change_M = min(change_HLE_M),
    max_HLE_change_M = max(change_HLE_M),
    
    # Female HLE
    mean_HLE_change_F = mean(change_HLE_F),
    median_HLE_change_F = median(change_HLE_F),
    sd_HLE_change_F = sd(change_HLE_F),
    min_HLE_change_F = min(change_HLE_F),
    max_HLE_change_F = max(change_HLE_F),
    
    # Male LE
    mean_LE_change_M = mean(change_LE_M),
    median_LE_change_M = median(change_LE_M),
    sd_LE_change_M = sd(change_LE_M),
    min_LE_change_M = min(change_LE_M),
    max_LE_change_M = max(change_LE_M),
    
    # Female LE
    mean_LE_change_F = mean(change_LE_F),
    median_LE_change_F = median(change_LE_F),
    sd_LE_change_F = sd(change_LE_F),
    min_LE_change_F = min(change_LE_F),
    max_LE_change_F = max(change_LE_F),
    
    # Income deprivation
    mean_income_percentile_change =
      mean(change_income_percentile),
    
    median_income_percentile_change =
      median(change_income_percentile)
  )


national_change_summary


#Regional summary

regional_change_summary <- analysis_data %>%
  group_by(Region) %>%
  summarise(
    
    n = n(),
    
    # Male HLE
    mean_HLE_change_M =
      mean(change_HLE_M),
    
    median_HLE_change_M =
      median(change_HLE_M),
    
    # Female HLE
    mean_HLE_change_F =
      mean(change_HLE_F),
    
    median_HLE_change_F =
      median(change_HLE_F),
    
    # Male LE
    mean_LE_change_M =
      mean(change_LE_M),
    
    median_LE_change_M =
      median(change_LE_M),
    
    # Female LE
    mean_LE_change_F =
      mean(change_LE_F),
    
    median_LE_change_F =
      median(change_LE_F),
    
    # Income deprivation
    mean_income_percentile_change =
      mean(change_income_percentile),
    
    median_income_percentile_change =
      median(change_income_percentile),
    
    .groups = "drop"
  ) %>%
  arrange(mean_HLE_change_M)


regional_change_summary




ggplot(
  regional_change_summary,
  aes(
    x = reorder(Region, mean_HLE_change_F),
    y = mean_HLE_change_F
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    x = "Region",
    y = "Mean change in male HLE (years)",
    title = "Change in male healthy life expectancy by region",
    subtitle = "2021 minus 2011"
  ) +
  
  theme_minimal()


#Small and large chnages

largest_HLE_increases_M <- analysis_data %>%
  slice_max(
    change_HLE_M,
    n = 20
  ) %>%
  select(
    Region,
    MSOA21NM,
    MSOA21CD,
    HLE_2011_M,
    HLE_2021_M,
    change_HLE_M,
    income_percentile_2015,
    income_percentile_2025
  )


smallest_HLE_changes_M <- analysis_data %>%
  slice_min(
    change_HLE_M,
    n = 20
  ) %>%
  select(
    Region,
    MSOA21NM,
    MSOA21CD,
    HLE_2011_M,
    HLE_2021_M,
    change_HLE_M,
    income_percentile_2015,
    income_percentile_2025
  )


largest_HLE_increases_M
smallest_HLE_changes_M




change_rankings <- bind_rows(
  
  analysis_data %>%
    slice_max(change_HLE_M, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Healthy life expectancy",
      Direction = "Largest increase",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_HLE_M
    ),
  
  analysis_data %>%
    slice_min(change_HLE_M, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Healthy life expectancy",
      Direction = "Smallest increase / largest decrease",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_HLE_M
    ),
  
  analysis_data %>%
    slice_max(change_HLE_F, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Healthy life expectancy",
      Direction = "Largest increase",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_HLE_F
    ),
  
  analysis_data %>%
    slice_min(change_HLE_F, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Healthy life expectancy",
      Direction = "Smallest increase / largest decrease",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_HLE_F
    ),
  
  analysis_data %>%
    slice_max(change_LE_M, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Life expectancy",
      Direction = "Largest increase",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_LE_M
    ),
  
  analysis_data %>%
    slice_min(change_LE_M, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Life expectancy",
      Direction = "Smallest increase / largest decrease",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_LE_M
    ),
  
  analysis_data %>%
    slice_max(change_LE_F, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Life expectancy",
      Direction = "Largest increase",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_LE_F
    ),
  
  analysis_data %>%
    slice_min(change_LE_F, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Life expectancy",
      Direction = "Smallest increase / largest decrease",
      Region,
      MSOA = MSOA21NM,
      MSOA21CD,
      Change = change_LE_F
    )
)


change_rankings

#Prep for plotting

male_HLE_plot_data <- analysis_data %>%
  select(
    Region,
    MSOA21CD,
    MSOA21NM,
    income_percentile_2015,
    income_percentile_2025,
    HLE_2011_M,
    HLE_2021_M
  )


female_HLE_plot_data <- analysis_data %>%
  select(
    Region,
    MSOA21CD,
    MSOA21NM,
    income_percentile_2015,
    income_percentile_2025,
    HLE_2011_F,
    HLE_2021_F
  )


male_LE_plot_data <- analysis_data %>%
  select(
    Region,
    MSOA21CD,
    MSOA21NM,
    income_percentile_2015,
    income_percentile_2025,
    LE_2011_M,
    LE_2021_M
  )


female_LE_plot_data <- analysis_data %>%
  select(
    Region,
    MSOA21CD,
    MSOA21NM,
    income_percentile_2015,
    income_percentile_2025,
    LE_2011_F,
    LE_2021_F
  )


#Plotting function

marmot_2011_2021_plot <- function(
    data,
    x_old,
    y_old,
    x_new,
    y_new,
    title_text,
    old_label,
    new_label
) {
  
  old_data <- data %>%
    transmute(
      Region,
      deprivation_percentile = {{ x_old }},
      years = {{ y_old }},
      period = old_label
    )
  
  new_data <- data %>%
    transmute(
      Region,
      deprivation_percentile = {{ x_new }},
      years = {{ y_new }},
      period = new_label
    )
  
  
  plot_data <- bind_rows(
    old_data,
    new_data
  )
  
  
  ggplot(
    plot_data,
    aes(
      x = deprivation_percentile,
      y = years,
      colour = period
    )
  ) +
    
    # MSOA points
    geom_point(
      alpha = 0.25,
      size = 0.8
    ) +
    
    # Separate Marmot curve for each period
    geom_smooth(
      aes(
        group = period
      ),
      method = "loess",
      span = 0.8,
      se = FALSE,
      linewidth = 1.3
    ) +
    
    scale_x_continuous(
      limits = c(1, 100),
      breaks = seq(0, 100, 10),
      expand = c(0, 0)
    ) +
    
    scale_colour_manual(
      values = c(
        "2011" = "#2C7BB6",
        "2021" = "#D7191C"
      )
    ) +
    
    labs(
      x = "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "Year",
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


# plotting main figures

male_HLE_2011_2021 <- marmot_2011_2021_plot(
  male_HLE_plot_data,
  income_percentile_2015,
  HLE_2011_M,
  income_percentile_2025,
  HLE_2021_M,
  "Male healthy life expectancy: 2011 vs 2021",
  "2011",
  "2021"
)

male_HLE_2011_2021


# -----------------------------
# Female HLE
# -----------------------------

female_HLE_2011_2021 <- marmot_2011_2021_plot(
  female_HLE_plot_data,
  income_percentile_2015,
  HLE_2011_F,
  income_percentile_2025,
  HLE_2021_F,
  "Female healthy life expectancy: 2011 vs 2021",
  "2011",
  "2021"
)

female_HLE_2011_2021


# -----------------------------
# Male LE
# -----------------------------

male_LE_2011_2021 <- marmot_2011_2021_plot(
  male_LE_plot_data,
  income_percentile_2015,
  LE_2011_M,
  income_percentile_2025,
  LE_2021_M,
  "Male life expectancy: 2011 vs 2021",
  "2011",
  "2021"
)

male_LE_2011_2021


# -----------------------------
# Female LE
# -----------------------------

female_LE_2011_2021 <- marmot_2011_2021_plot(
  female_LE_plot_data,
  income_percentile_2015,
  LE_2011_F,
  income_percentile_2025,
  LE_2021_F,
  "Female life expectancy: 2011 vs 2021",
  "2011",
  "2021"
)

female_LE_2011_2021


#can save analysis data set
# write_csv(
#   analysis_data,
#   "MSOA_2011_2021_HLE_LE_IMD_analysis.csv"
# )




# write_csv(
#   regional_change_summary,
#   "regional_change_summary_2011_2021.csv"
# )
# 
# write_csv(
#   national_change_summary,
#   "national_change_summary_2011_2021.csv"
# )
# 
# write_csv(
#   change_rankings,
#   "MSOA_change_rankings_2011_2021.csv"
# )
# 
# write_csv(
#   missing_summary,
#   "missing_data_summary_2011_2021.csv"
# )




#Sense checking



national_msoa_averages <- analysis_data %>%
  summarise(
    
    n_MSOA = n(),
    
    # 2011
    HLE_2011_M = mean(HLE_2011_M, na.rm = TRUE),
    HLE_2011_F = mean(HLE_2011_F, na.rm = TRUE),
    LE_2011_M  = mean(LE_2011_M,  na.rm = TRUE),
    LE_2011_F  = mean(LE_2011_F,  na.rm = TRUE),
    
    # 2021
    HLE_2021_M = mean(HLE_2021_M, na.rm = TRUE),
    HLE_2021_F = mean(HLE_2021_F, na.rm = TRUE),
    LE_2021_M  = mean(LE_2021_M,  na.rm = TRUE),
    LE_2021_F  = mean(LE_2021_F,  na.rm = TRUE)
    
  )


national_msoa_averages


# ============================================================
# 2. CREATE DEPRIVATION GROUPS
# ============================================================
#
# IMPORTANT:
# This assumes:
#
# 1 = most deprived
# 100 = least deprived
#
# Therefore:
# bottom 10% = percentiles 1-10
# top 10%    = percentiles 91-100
#
# ------------------------------------------------------------

analysis_data <- analysis_data %>%
  mutate(
    
    deprivation_group_2015 = case_when(
      income_percentile_2015 <= 10 ~ "Bottom 10%",
      income_percentile_2015 >= 90 ~ "Top 10%",
      TRUE ~ "Middle 80%"
    ),
    
    deprivation_group_2025 = case_when(
      income_percentile_2025 <= 10 ~ "Bottom 10%",
      income_percentile_2025 >= 90 ~ "Top 10%",
      TRUE ~ "Middle 80%"
    )
    
  )


# ============================================================
# 3. 2011 HEALTH BY 2015 DEPRIVATION
# ============================================================

national_2011_deprivation <- analysis_data %>%
  filter(
    deprivation_group_2015 %in% c(
      "Bottom 10%",
      "Top 10%"
    )
  ) %>%
  group_by(
    deprivation_group_2015
  ) %>%
  summarise(
    
    n_MSOA = n(),
    
    mean_HLE_M = mean(HLE_2011_M, na.rm = TRUE),
    mean_HLE_F = mean(HLE_2011_F, na.rm = TRUE),
    
    mean_LE_M = mean(LE_2011_M, na.rm = TRUE),
    mean_LE_F = mean(LE_2011_F, na.rm = TRUE),
    
    .groups = "drop"
    
  )


national_2011_deprivation


# ============================================================
# 4. 2021 HEALTH BY 2025 DEPRIVATION
# ============================================================

national_2021_deprivation <- analysis_data %>%
  filter(
    deprivation_group_2025 %in% c(
      "Bottom 10%",
      "Top 10%"
    )
  ) %>%
  group_by(
    deprivation_group_2025
  ) %>%
  summarise(
    
    n_MSOA = n(),
    
    mean_HLE_M = mean(HLE_2021_M, na.rm = TRUE),
    mean_HLE_F = mean(HLE_2021_F, na.rm = TRUE),
    
    mean_LE_M = mean(LE_2021_M, na.rm = TRUE),
    mean_LE_F = mean(LE_2021_F, na.rm = TRUE),
    
    .groups = "drop"
    
  )


national_2021_deprivation








# ============================================================



# 26.13 REGIONAL CURVES FOR ALL ENGLAND REGIONS

regional_marmot_plot <- function(
    data,
    measure_2011,
    measure_2021,
    title_text,
    y_label
) {
  
  plot_2011 <- data %>%
    transmute(
      Region,

      deprivation_percentile =
        income_percentile_2015,
      
      value =
        {{ measure_2011 }},
      
      Year =
        "2011"
    )

  
  plot_2021 <- data %>%
    transmute(
      Region,
      

      deprivation_percentile =
        income_percentile_2025,
      
      value =
        {{ measure_2021 }},
      
      Year =
        "2021"
    )

  
  plot_data <- bind_rows(
    plot_2011,
    plot_2021
  ) %>%
    filter(
      !is.na(deprivation_percentile),
      !is.na(value)
    ) %>%
    mutate(
      Region_Year =
        paste(
          Region,
          Year,
          sep = " - "
        )
    )
  
  ggplot(
    plot_data,
    aes(
      x = deprivation_percentile,
      y = value,
      colour = Year
    )
  ) +

  geom_smooth(
    aes(
      group = interaction(
        Region,
        Year
      )
    ),
    method = "loess",
    span = 0.8,
    se = FALSE,
    linewidth = 1.1
  ) +
    
    facet_wrap(
      ~ Region
    ) +
    
    scale_x_continuous(
      limits = c(
        1,
        100
      ),
      breaks = c(
        1,
        25,
        50,
        75,
        100
      ),
      expand = c(
        0,
        0
      )
    ) +
    
    labs(
      x =
        "Income deprivation percentile\n(1 = most deprived, 100 = least deprived)",
      
      y =
        y_label,
      
      colour =
        "Year",
      
      title =
        title_text,
      
      subtitle =
        "Regional Marmot curves using existing deprivation percentiles"
    ) +
    
    theme_minimal(
      base_size = 12
    ) +
    
    theme(
      panel.grid.minor =
        element_blank(),
      
      plot.title =
        element_text(
          face = "bold"
        ),
      
      legend.position =
        "top"
    )

  
}

# MALE LE - ALL REGIONS

regional_male_LE <-
  regional_marmot_plot(
    

    analysis_data,
    
    LE_2011_M,
    LE_2021_M,
    
    "Male life expectancy by deprivation: 2011 vs 2021",
    
    "Life expectancy (years)"

    
  )

regional_male_LE

# FEMALE LE - ALL REGIONS

regional_female_LE <-
  regional_marmot_plot(
    

    analysis_data,
    
    LE_2011_F,
    LE_2021_F,
    
    "Female life expectancy by deprivation: 2011 vs 2021",
    
    "Life expectancy (years)"

    
  )

regional_female_LE

###Pulling out LE comparisons ============================================



LE_change_rankings <- bind_rows(
  
  analysis_data %>%
    slice_max(change_LE_M, n = 20) %>%
    mutate(
      Sex = "Male",
      Direction = "Largest increase"
    ),
  
  analysis_data %>%
    slice_min(change_LE_M, n = 20) %>%
    mutate(
      Sex = "Male",
      Direction = "Largest decrease"
    ),
  
  analysis_data %>%
    slice_max(change_LE_F, n = 20) %>%
    mutate(
      Sex = "Female",
      Direction = "Largest increase"
    ),
  
  analysis_data %>%
    slice_min(change_LE_F, n = 20) %>%
    mutate(
      Sex = "Female",
      Direction = "Largest decrease"
    )
  
) %>%
  
  select(
    
    Sex,
    Direction,
    Region,
    MSOA21CD,
    MSOA21NM,
    
    LE_2011_M,
    LE_2021_M,
    change_LE_M,
    
    LE_2011_F,
    LE_2021_F,
    change_LE_F,
    
    income_percentile_2015,
    income_percentile_2025
  )

LE_change_rankings



# TOP 10% VS BOTTOM 10%


analysis_data <- analysis_data %>%
  mutate(
    
    deprivation_group_2015 = case_when(
      income_percentile_2015 <= 10 ~ "Bottom 10%",
      income_percentile_2015 >= 90 ~ "Top 10%",
      TRUE ~ "Middle 80%"
    ),
    
    deprivation_group_2025 = case_when(
      income_percentile_2025 <= 10 ~ "Bottom 10%",
      income_percentile_2025 >= 90 ~ "Top 10%",
      TRUE ~ "Middle 80%"
    )
    
  )#

LE_gap <- analysis_data %>%
  
  filter(
    deprivation_group_2025 %in%
      c("Bottom 10%", "Top 10%")
  ) %>%
  
  group_by(
    deprivation_group_2025
  ) %>%
  
  summarise(
    
    Male_2021 =
      mean(LE_2021_M),
    
    Female_2021 =
      mean(LE_2021_F),
    
    .groups="drop"
    
  )

LE_gap



LE_gap_summary <- LE_gap %>%
  
  summarise(
    
    Male_gap =
      Male_2021[
        deprivation_group_2025=="Top 10%"
      ] -
      
      Male_2021[
        deprivation_group_2025=="Bottom 10%"
      ],
    
    Female_gap =
      Female_2021[
        deprivation_group_2025=="Top 10%"
      ] -
      
      Female_2021[
        deprivation_group_2025=="Bottom 10%"
      ]
    
  )

LE_gap_summary


##
# ============================================================
# INEQUALITY GAP OVER TIME
# 2011 LE by 2015 deprivation
# 2021 LE by 2025 deprivation


# -----------------------------
# 2011 GAP (using 2015 IMD)
# -----------------------------

LE_gap_2011 <- analysis_data %>%
  mutate(
    deprivation_group_2015 = case_when(
      income_percentile_2015 <= 10 ~ "Bottom 10%",
      income_percentile_2015 >= 90 ~ "Top 10%",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(deprivation_group_2015)) %>%
  group_by(deprivation_group_2015) %>%
  summarise(
    Male_LE_2011 = mean(LE_2011_M, na.rm = TRUE),
    Female_LE_2011 = mean(LE_2011_F, na.rm = TRUE),
    .groups = "drop"
  )


# -----------------------------
# 2021 GAP (using 2025 IMD)
# -----------------------------

LE_gap_2021 <- analysis_data %>%
  mutate(
    deprivation_group_2025 = case_when(
      income_percentile_2025 <= 10 ~ "Bottom 10%",
      income_percentile_2025 >= 90 ~ "Top 10%",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(deprivation_group_2025)) %>%
  group_by(deprivation_group_2025) %>%
  summarise(
    Male_LE_2021 = mean(LE_2021_M, na.rm = TRUE),
    Female_LE_2021 = mean(LE_2021_F, na.rm = TRUE),
    .groups = "drop"
  )


#Combine Results

LE_gap_time <- LE_gap_2011 %>%
  rename(Group = deprivation_group_2015) %>%
  left_join(
    LE_gap_2021 %>%
      rename(Group = deprivation_group_2025),
    by = "Group"
  )

LE_gap_time


#Calculate gaps

LE_gap_summary <- tibble(
  
  Male_gap_2011 =
    LE_gap_time$Male_LE_2011[
      LE_gap_time$Group == "Top 10%"
    ] -
    LE_gap_time$Male_LE_2011[
      LE_gap_time$Group == "Bottom 10%"
    ],
  
  Male_gap_2021 =
    LE_gap_time$Male_LE_2021[
      LE_gap_time$Group == "Top 10%"
    ] -
    LE_gap_time$Male_LE_2021[
      LE_gap_time$Group == "Bottom 10%"
    ],
  
  Female_gap_2011 =
    LE_gap_time$Female_LE_2011[
      LE_gap_time$Group == "Top 10%"
    ] -
    LE_gap_time$Female_LE_2011[
      LE_gap_time$Group == "Bottom 10%"
    ],
  
  Female_gap_2021 =
    LE_gap_time$Female_LE_2021[
      LE_gap_time$Group == "Top 10%"
    ] -
    LE_gap_time$Female_LE_2021[
      LE_gap_time$Group == "Bottom 10%"
    ]
  
) %>%
  mutate(
    
    Male_gap_change =
      Male_gap_2021 - Male_gap_2011,
    
    Female_gap_change =
      Female_gap_2021 - Female_gap_2011
    
  )

LE_gap_summary %>%
  mutate(
    across(
      everything(),
      ~ round(.x, 2)
    )
  )

