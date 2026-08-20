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
      Value = le_male,
      Region
    ),
  
  marmot_data %>%
    slice_max(hle_male, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_male,
      Region
    ),
  
  marmot_data %>%
    slice_max(le_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = le_female,
      Region
    ),
  
  marmot_data %>%
    slice_max(hle_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_female,
      Region
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
      Value = le_male,
      Region
    ),
  
  marmot_data %>%
    slice_min(hle_male, n = 10) %>%
    transmute(
      Sex = "Male",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_male,
      Region
    ),
  
  marmot_data %>%
    slice_min(le_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = le_female,
      Region
    ),
  
  marmot_data %>%
    slice_min(hle_female, n = 10) %>%
    transmute(
      Sex = "Female",
      Measure = "Healthy life expectancy",
      MSOA = msoa_name,
      Code = msoa11,
      Value = hle_female,
      Region
    )
  
)

lowest_msoas

#Top v bottom difference 

# Average of top 10 vs bottom 10 MSOAs for each measure
# Difference = average of top 10 - average of bottom 10

top_bottom_difference <- bind_rows(
  
  # Male Life Expectancy
  tibble(
    Sex = "Male",
    Measure = "Life expectancy",
    Top_10_average = mean(highest_msoas$Value[
      highest_msoas$Sex == "Male" &
        highest_msoas$Measure == "Life expectancy"
    ], na.rm = TRUE),
    Bottom_10_average = mean(lowest_msoas$Value[
      lowest_msoas$Sex == "Male" &
        lowest_msoas$Measure == "Life expectancy"
    ], na.rm = TRUE)
  ),
  
  # Male Healthy Life Expectancy
  tibble(
    Sex = "Male",
    Measure = "Healthy life expectancy",
    Top_10_average = mean(highest_msoas$Value[
      highest_msoas$Sex == "Male" &
        highest_msoas$Measure == "Healthy life expectancy"
    ], na.rm = TRUE),
    Bottom_10_average = mean(lowest_msoas$Value[
      lowest_msoas$Sex == "Male" &
        lowest_msoas$Measure == "Healthy life expectancy"
    ], na.rm = TRUE)
  ),
  
  # Female Life Expectancy
  tibble(
    Sex = "Female",
    Measure = "Life expectancy",
    Top_10_average = mean(highest_msoas$Value[
      highest_msoas$Sex == "Female" &
        highest_msoas$Measure == "Life expectancy"
    ], na.rm = TRUE),
    Bottom_10_average = mean(lowest_msoas$Value[
      lowest_msoas$Sex == "Female" &
        lowest_msoas$Measure == "Life expectancy"
    ], na.rm = TRUE)
  ),
  
  # Female Healthy Life Expectancy
  tibble(
    Sex = "Female",
    Measure = "Healthy life expectancy",
    Top_10_average = mean(highest_msoas$Value[
      highest_msoas$Sex == "Female" &
        highest_msoas$Measure == "Healthy life expectancy"
    ], na.rm = TRUE),
    Bottom_10_average = mean(lowest_msoas$Value[
      lowest_msoas$Sex == "Female" &
        lowest_msoas$Measure == "Healthy life expectancy"
    ], na.rm = TRUE)
  )
) %>%
  mutate(
    Difference = Top_10_average - Bottom_10_average
  )

top_bottom_difference


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



#




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











# ============================================================
# REGIONAL MARMOT PLOTS
# ============================================================

library(tidyverse)


# ============================================================
# 1. Regional Marmot plot function
# ============================================================

marmot_plot_by_region <- function(data,
                                  measure_name,
                                  title_text) {
  
  plot_data <- data %>%
    filter(
      measure == measure_name
    ) %>%
    filter(
      !is.na(Region),
      !is.na(deprivation_percentile),
      !is.na(years)
    )
  
  
  ggplot(
    plot_data,
    aes(
      x = deprivation_percentile,
      y = years,
      colour = Region
    )
  ) +
    
    # MSOA observations
    geom_point(
      alpha = 0.25,
      size = 0.7
    ) +
    
    # Separate LOESS curve for each region
    geom_smooth(
      aes(
        group = Region
      ),
      method = "loess",
      span = 0.8,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    scale_x_continuous(
      limits = c(1, 100),
      breaks = seq(0, 100, 10),
      expand = c(0, 0)
    ) +
    
    labs(
      x = "Population percentile of income deprivation\n(1 = most deprived, 100 = least deprived)",
      y = "Years",
      colour = "Region",
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


# ============================================================
# 2. Regional Life Expectancy
# ============================================================

regional_LE_plot <- marmot_plot_by_region(
  marmot_long,
  "Life expectancy",
  "Life expectancy by income deprivation and region"
)

regional_LE_plot


# ============================================================
# 3. Regional Healthy Life Expectancy
# ============================================================

regional_HLE_plot <- marmot_plot_by_region(
  marmot_long,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation and region"
)

regional_HLE_plot


# ============================================================
# 4. LONDON + NORTH EAST ONLY
# ============================================================

london_northeast <- marmot_long %>%
  filter(
    Region %in% c(
      "London",
      "North East"
    )
  )


# ============================================================
# 5. London + North East Life Expectancy
# ============================================================

LE_london_northeast <- marmot_plot_by_region(
  london_northeast,
  "Life expectancy",
  "Life expectancy by income deprivation: London vs North East"
)

LE_london_northeast


# ============================================================
# 6. London + North East Healthy Life Expectancy
# ============================================================

HLE_london_northeast <- marmot_plot_by_region(
  london_northeast,
  "Healthy life expectancy",
  "Healthy life expectancy by income deprivation: London vs North East"
)

HLE_london_northeast




##

regional_summary <- marmot_long %>%
  group_by(
    Region,
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
    Region,
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




london_ne_summary <- london_northeast %>%
  group_by(
    Region,
    measure
  ) %>%
  summarise(
    Mean = mean(years, na.rm = TRUE),
    Median = median(years, na.rm = TRUE),
    SD = sd(years, na.rm = TRUE),
    .groups = "drop"
  )

london_ne_summary








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
  group_by(sex) %>%
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
  group_by(sex) %>%
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
  group_by(sex, ci_width_band) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(sex) %>%
  mutate(
    total_n = sum(n),
    percentage = (n / total_n) * 100
  ) %>%
  ungroup() %>%
  mutate(
    percentage = round(percentage, 1)
  ) %>%
  arrange(
    sex,
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
  group_by(sex, ci_width_band) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(sex) %>%
  mutate(
    total_n = sum(n),
    percentage = (n / total_n) * 100
  ) %>%
  ungroup() %>%
  mutate(
    percentage = round(percentage, 1)
  ) %>%
  arrange(
    sex,
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
    Region,
    msoa11,
    msoa_name,
    sex,
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
    Region,
    msoa11,
    msoa_name,
    sex,
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
  group_by(Region, sex) %>%
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
  group_by(Region, sex) %>%
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

