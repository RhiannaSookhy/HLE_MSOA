# ============================================================
# Purpose of script
#
# This script examines changes in life expectancy across MSOAs.
#
# It compares male and female life expectancy changes.
#
# It identifies MSOAs where life expectancy:
#   - increased
#   - decreased
#   - changed for both males and females
#
# It examines these changes by:
#   - English region
#   - IMD 2015 deprivation decile
#   - IMD 2025 deprivation decile
#   - IMD 2025 deprivation quintile
#
# The script calculates:
#   - number of MSOAs affected
#   - percentage of MSOAs affected
#   - changes above specific thresholds
#   - results for males and females separately
#   - results where both sexes increased or decreased
#
#============================================================

#Clear environment and load libraries 
rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)


#SW
setwd("C:/Users/rhianna.sookhy/OneDrive - The Health Foundation/Shortcuts/Analysis - 11-CAT/1. Work programme/Healthy Life Expectancy - strategy launch/Phase 2")



#Load in data (this has been made from 1.IMD calculations and 2.HLE_IMDdata set (which were prepped to be suitable for MSOA21 data))

msoa_data <- read_csv(
  "Working files/MSOA_2011_HLE_IMD.csv"
)

# Keep region and variables needed for LE analysis
msoa_data <- msoa_data %>%
  select(
    MSOA21CD,
    MSOA21NM,
    Region,
    # HLE_2011_M,
    # HLE_2011_M_LCL,
    # HLE_2011_M_UCL,
    LE_2011_M,
    LE_2011_M_LCL,
    LE_2011_M_UCL,
    # HLE_2011_F,
    # HLE_2011_F_LCL,
    # HLE_2011_F_UCL,
    LE_2011_F,
    LE_2011_F_LCL,
    LE_2011_F_UCL,
    imd_average_score_2015,
    imd_average_score_2025
  )

#Adding Quintile and Deciles by the IMD avg score, anaysis uses IMD2025 but IMD2015 also availabe in case needed. 
msoa_data <- msoa_data %>%
  mutate(
    # Decile:
    # 1 = most deprived
    # 10 = least deprived
    imd_decile_2015 = ntile(
      desc(imd_average_score_2015),
      10
    ),
    
    # Quintile:
    # 1 = most deprived
    # 5 = least deprived
    imd_quintile_2015 = ntile(
      desc(imd_average_score_2015),
      5
    ),
    
    # Rank:
    # 1 = most deprived
    # Higher rank = less deprived
    imd_rank_2015 = min_rank(
      desc(imd_average_score_2015)
    ),
    #Same as above but now for IMD2025
    imd_decile_2025 = ntile(
      desc(imd_average_score_2025),
      10
    ),
    
    imd_quintile_2025 = ntile(
      desc(imd_average_score_2025),
      5
    ),
    
    imd_rank_2025 = min_rank(
      desc(imd_average_score_2025)
    )
  )

#Checking the data

# Number of MSOAs in each region
msoa_data %>%
  count(Region, name = "n_MSOAs") %>%
  arrange(desc(n_MSOAs))


# Missing values in each variable
msoa_data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    percentage_missing = 100 * n_missing / nrow(msoa_data)
  ) %>%
  arrange(desc(n_missing))

#This is as expected from ONS not available LE for some MSOAs

#Loading in most recent data

lemsoa <- read_excel(
  "Raw data/hslemsoa.xlsx",
  sheet = "1",
  skip = 6
)

le_2021 <- lemsoa %>%
  filter(
    Country == "England",
    `Area type` == "MSOA",
    Sex %in% c("Male", "Female")
  ) %>%
  select(
    `Area code`,
    `Area name`,
    Sex,
    LE,
    LCI,
    UCI
  ) %>%
  rename(
    MSOA21CD = `Area code`,
    MSOA21NM = `Area name`,
    LE_2021 = LE,
    LE_2021_LCL = LCI,
    LE_2021_UCL = UCI
  )

#Changing format so easier to join
le_2021_wide <- le_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    Sex,
    LE_2021,
    LE_2021_LCL,
    LE_2021_UCL
  ) %>%
  pivot_wider(
    names_from = Sex,
    values_from = c(
      LE_2021,
      LE_2021_LCL,
      LE_2021_UCL
    ),
    names_glue = "{.value}_{ifelse(Sex == 'Male', 'M', 'F')}"
  ) %>%
  rename(
    LE_2021_M_LCL = LE_2021_LCL_M,
    LE_2021_M_UCL = LE_2021_UCL_M,
    LE_2021_F_LCL = LE_2021_LCL_F,
    LE_2021_F_UCL = LE_2021_UCL_F
  )

# Merging onto 2009-13 data and IMD data
msoa_data <- msoa_data %>%
  left_join(
    le_2021_wide %>%
      select(
        MSOA21CD,
        
        # Male
        LE_2021_M,
        LE_2021_M_LCL,
        LE_2021_M_UCL,
        
        # Female
        LE_2021_F,
        LE_2021_F_LCL,
        LE_2021_F_UCL
      ),
    by = "MSOA21CD"
  )


# Variables required for the analysis
required_variables <- c(
  "MSOA21CD",
  "MSOA21NM",
  "Region",
  "LE_2021_M",
  "LE_2021_M_LCL",
  "LE_2021_M_UCL",
  "LE_2021_F",
  "LE_2021_F_LCL",
  "LE_2021_F_UCL",
  "LE_2011_M",
  "LE_2011_M_LCL",
  "LE_2011_M_UCL",
  "LE_2011_F",
  "LE_2011_F_LCL",
  "LE_2011_F_UCL",
  "imd_average_score_2015",
  "imd_average_score_2025"
)

#Rows with missing variables
rows_with_missing <- msoa_data %>%
  filter(
    if_any(
      all_of(required_variables),
      is.na
    )
  ) %>%
  select(
    MSOA21CD,
    MSOA21NM,
    Region,
    all_of(required_variables)
  )

rows_with_missing

#Removing rows with missing data
msoa_data_complete <- msoa_data %>%
  filter(
    if_all(
      all_of(required_variables),
      ~ !is.na(.)
    )
  )


# Check number of MSOAs remaining
nrow(msoa_data_complete)


#Creating LE change variables 
msoa_data_complete <- msoa_data_complete %>%
  mutate(
    LE_change_M = LE_2021_M - LE_2011_M,
    LE_change_F = LE_2021_F - LE_2011_F
  )


#Both Male and Female LE Decreasing overall
both_decreased <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(LE_change_F)
  ) %>%
  summarise(
    total_msoas = n(),
    n_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    percentage_decreased = 100 * n_decreased / total_msoas
  )

both_decreased

both_increased <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(LE_change_F)
  ) %>%
  summarise(
    total_msoas = n(),
    n_increased = sum(
      LE_change_M > 0 &
        LE_change_F > 0
    ),
    percentage_increased = 100 * n_increased / total_msoas
  )

both_increased


#Both male and Female LE decreasing by regions
both_decreased_within_region <- msoa_data_complete %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    n_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0,
      na.rm = TRUE
    ),
    percentage_decreased = 100 * n_decreased / total_msoas
  ) %>%
  arrange(desc(percentage_decreased))

both_decreased_within_region

# Sense checking what does this look like by 0.5/1 not 0
both_decreased_within_region <- msoa_data_complete %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    
    # Less than 0
    n_decreased_0 = sum(
      LE_change_M < 0 & LE_change_F < 0,
      na.rm = TRUE
    ),
    pct_decreased_0 = 100 * n_decreased_0 / total_msoas,
    
    # More than 0.5 decrease
    n_decreased_0.5 = sum(
      LE_change_M < -0.5 & LE_change_F < -0.5,
      na.rm = TRUE
    ),
    pct_decreased_0.5 = 100 * n_decreased_0.5 / total_msoas,
    
    # More than 1 decrease
    n_decreased_1 = sum(
      LE_change_M < -1 & LE_change_F < -1,
      na.rm = TRUE
    ),
    pct_decreased_1 = 100 * n_decreased_1 / total_msoas,
    
    # More than 1.5 decrease
    n_decreased_1.5 = sum(
      LE_change_M < -1.5 & LE_change_F < -1.5,
      na.rm = TRUE
    ),
    pct_decreased_1.5 = 100 * n_decreased_1.5 / total_msoas,
    
    # More than 2 decrease
    n_decreased_2 = sum(
      LE_change_M < -2 & LE_change_F < -2,
      na.rm = TRUE
    ),
    pct_decreased_2 = 100 * n_decreased_2 / total_msoas,
    
    # More than 2.5 decrease
    n_decreased_2.5 = sum(
      LE_change_M < -2.5 & LE_change_F < -2.5,
      na.rm = TRUE
    ),
    pct_decreased_2.5 = 100 * n_decreased_2.5 / total_msoas,
    
    # More than 3 decrease
    n_decreased_3 = sum(
      LE_change_M < -3 & LE_change_F < -3,
      na.rm = TRUE
    ),
    pct_decreased_3 = 100 * n_decreased_3 / total_msoas
  ) %>%
  arrange(desc(pct_decreased_0))

both_decreased_within_region

#For Females only

female_decreased_within_region <- msoa_data_complete %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    
    n_decreased_0 = sum(LE_change_F < 0, na.rm = TRUE),
    pct_decreased_0 = 100 * n_decreased_0 / total_msoas,
    
    n_decreased_0.5 = sum(LE_change_F < -0.5, na.rm = TRUE),
    pct_decreased_0.5 = 100 * n_decreased_0.5 / total_msoas,
    
    n_decreased_1 = sum(LE_change_F < -1, na.rm = TRUE),
    pct_decreased_1 = 100 * n_decreased_1 / total_msoas,
    
    n_decreased_1.5 = sum(LE_change_F < -1.5, na.rm = TRUE),
    pct_decreased_1.5 = 100 * n_decreased_1.5 / total_msoas,
    
    n_decreased_2 = sum(LE_change_F < -2, na.rm = TRUE),
    pct_decreased_2 = 100 * n_decreased_2 / total_msoas,
    
    n_decreased_2.5 = sum(LE_change_F < -2.5, na.rm = TRUE),
    pct_decreased_2.5 = 100 * n_decreased_2.5 / total_msoas,
    
    n_decreased_3 = sum(LE_change_F < -3, na.rm = TRUE),
    pct_decreased_3 = 100 * n_decreased_3 / total_msoas
  ) %>%
  arrange(desc(pct_decreased_0))

female_decreased_within_region


#Now males 
male_decreased_within_region <- msoa_data_complete %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    
    n_decreased_0 = sum(LE_change_M < 0, na.rm = TRUE),
    pct_decreased_0 = 100 * n_decreased_0 / total_msoas,
    
    n_decreased_0.5 = sum(LE_change_M < -0.5, na.rm = TRUE),
    pct_decreased_0.5 = 100 * n_decreased_0.5 / total_msoas,
    
    n_decreased_1 = sum(LE_change_M < -1, na.rm = TRUE),
    pct_decreased_1 = 100 * n_decreased_1 / total_msoas,
    
    n_decreased_1.5 = sum(LE_change_M < -1.5, na.rm = TRUE),
    pct_decreased_1.5 = 100 * n_decreased_1.5 / total_msoas,
    
    n_decreased_2 = sum(LE_change_M < -2, na.rm = TRUE),
    pct_decreased_2 = 100 * n_decreased_2 / total_msoas,
    
    n_decreased_2.5 = sum(LE_change_M < -2.5, na.rm = TRUE),
    pct_decreased_2.5 = 100 * n_decreased_2.5 / total_msoas,
    
    n_decreased_3 = sum(LE_change_M < -3, na.rm = TRUE),
    pct_decreased_3 = 100 * n_decreased_3 / total_msoas
  ) %>%
  arrange(desc(pct_decreased_0))

male_decreased_within_region


#Increase
both_increased_within_region <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(LE_change_F)
  ) %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    n_increased = sum(
      LE_change_M > 0 &
        LE_change_F > 0
    ),
    percentage_increased = 100 * n_increased / total_msoas
  ) %>%
  arrange(desc(percentage_increased))

both_increased_within_region

#with varaition for senstivity
both_increased_within_region <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(LE_change_F)
  ) %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    
    n_increased_0 = sum(
      LE_change_M > 0 & LE_change_F > 0
    ),
    pct_increased_0 = 100 * n_increased_0 / total_msoas,
    
    n_increased_0.5 = sum(
      LE_change_M > 0.5 & LE_change_F > 0.5
    ),
    pct_increased_0.5 = 100 * n_increased_0.5 / total_msoas,
    
    n_increased_1 = sum(
      LE_change_M > 1 & LE_change_F > 1
    ),
    pct_increased_1 = 100 * n_increased_1 / total_msoas,
    
    n_increased_1.5 = sum(
      LE_change_M > 1.5 & LE_change_F > 1.5
    ),
    pct_increased_1.5 = 100 * n_increased_1.5 / total_msoas,
    
    n_increased_2 = sum(
      LE_change_M > 2 & LE_change_F > 2
    ),
    pct_increased_2 = 100 * n_increased_2 / total_msoas,
    
    n_increased_2.5 = sum(
      LE_change_M > 2.5 & LE_change_F > 2.5
    ),
    pct_increased_2.5 = 100 * n_increased_2.5 / total_msoas,
    
    n_increased_3 = sum(
      LE_change_M > 3 & LE_change_F > 3
    ),
    pct_increased_3 = 100 * n_increased_3 / total_msoas
  ) %>%
  arrange(desc(pct_increased_0))

both_increased_within_region

#Female only
female_increased_within_region <- msoa_data_complete %>%
  filter(!is.na(LE_change_F)) %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    
    n_increased_0 = sum(LE_change_F > 0),
    pct_increased_0 = 100 * n_increased_0 / total_msoas,
    
    n_increased_0.5 = sum(LE_change_F > 0.5),
    pct_increased_0.5 = 100 * n_increased_0.5 / total_msoas,
    
    n_increased_1 = sum(LE_change_F > 1),
    pct_increased_1 = 100 * n_increased_1 / total_msoas,
    
    n_increased_1.5 = sum(LE_change_F > 1.5),
    pct_increased_1.5 = 100 * n_increased_1.5 / total_msoas,
    
    n_increased_2 = sum(LE_change_F > 2),
    pct_increased_2 = 100 * n_increased_2 / total_msoas,
    
    n_increased_2.5 = sum(LE_change_F > 2.5),
    pct_increased_2.5 = 100 * n_increased_2.5 / total_msoas,
    
    n_increased_3 = sum(LE_change_F > 3),
    pct_increased_3 = 100 * n_increased_3 / total_msoas
  ) %>%
  arrange(desc(pct_increased_0))

female_increased_within_region

#Males only
male_increased_within_region <- msoa_data_complete %>%
  filter(!is.na(LE_change_M)) %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    
    n_increased_0 = sum(LE_change_M > 0),
    pct_increased_0 = 100 * n_increased_0 / total_msoas,
    
    n_increased_0.5 = sum(LE_change_M > 0.5),
    pct_increased_0.5 = 100 * n_increased_0.5 / total_msoas,
    
    n_increased_1 = sum(LE_change_M > 1),
    pct_increased_1 = 100 * n_increased_1 / total_msoas,
    
    n_increased_1.5 = sum(LE_change_M > 1.5),
    pct_increased_1.5 = 100 * n_increased_1.5 / total_msoas,
    
    n_increased_2 = sum(LE_change_M > 2),
    pct_increased_2 = 100 * n_increased_2 / total_msoas,
    
    n_increased_2.5 = sum(LE_change_M > 2.5),
    pct_increased_2.5 = 100 * n_increased_2.5 / total_msoas,
    
    n_increased_3 = sum(LE_change_M > 3),
    pct_increased_3 = 100 * n_increased_3 / total_msoas
  ) %>%
  arrange(desc(pct_increased_0))

male_increased_within_region

##By IMD

both_le_by_imd_2015 <- msoa_data_complete %>%
  group_by(imd_decile_2015) %>%
  summarise(
    denominator = n(),
    
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    
    proportion_both_decreased =
      100 * n_both_decreased / denominator,
    
    n_both_increased = sum(
      LE_change_M > 0 &
        LE_change_F > 0
    ),
    
    proportion_both_increased =
      100 * n_both_increased / denominator,
    
    .groups = "drop"
  ) %>%
  arrange(imd_decile_2015)

both_le_by_imd_2015


## add in region  to the IMD

# Number of MSOAs in each 2025 IMD decile by region (Sense check)
msoa_by_region_decile_2025 <- msoa_data_complete %>%
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n_MSOAs = n(),
    .groups = "drop"
  ) %>%
  arrange(Region, imd_decile_2025)

msoa_by_region_decile_2025

both_le_by_region_decile_2025 <- msoa_data_complete %>%
  
  # Only look at most and least deprived deciles
  filter(
    imd_decile_2025 %in% c(1, 10)
  ) %>%
  
  group_by(
    imd_decile_2025,
    Region
  ) %>%
  
  summarise(
    
    # Total number of MSOAs in that region + decile
    denominator = n(),
    
    # Number where BOTH male and female LE decreased
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    
    # Percentage of MSOAs in that region + decile
    # where both LE decreased
    proportion_both_decreased =
      round(
        100 * n_both_decreased / denominator,
        2
      ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    imd_decile_2025,
    desc(proportion_both_decreased)
  )


both_le_by_region_decile_2025


#For all deciles 
both_sex_declined_by_region_decile <- msoa_data_complete %>%
  filter(
    LE_change_M < 0 &
      LE_change_F < 0
  ) %>%
  group_by(
    Region,
    imd_decile_2025
  ) %>%
  summarise(
    n_MSOAs_both_declined = n(),
    .groups = "drop"
  ) %>%
  arrange(
    Region,
    imd_decile_2025
  )

both_sex_declined_by_region_decile


##For plotting
both_decreased_d1_vs_other <- msoa_data_complete %>%
  group_by(Region) %>%
  summarise(
    total_msoas = n(),
    
    # Both male and female life expectancy decreased
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0,
      na.rm = TRUE
    ),
    
    # Of these, how many are in IMD Q1 (most deprived)
    n_q1_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0 &
        imd_decile_2025 == 1,
      na.rm = TRUE
    ),
    
    # Remaining MSOAs where both decreased
    n_other_both_decreased = n_both_decreased - n_q1_both_decreased,
    
    # Percentages based on ALL MSOAs in the region
    pct_d1_both_decreased = 100 * n_q1_both_decreased / total_msoas,
    
    pct_other_both_decreased = 100 * n_other_both_decreased / total_msoas,
    
    pct_overall_both_decreased = 100 * n_both_decreased / total_msoas
  ) %>%
  arrange(desc(pct_overall_both_decreased))

both_decreased_d1_vs_other


#If I wanted every Deciles
both_decreased_by_region_decile <- msoa_data_complete %>%
  group_by(Region, imd_decile_2025) %>%
  summarise(
    total_msoas_decile = n(),
    
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  group_by(Region) %>%
  mutate(
    total_msoas_region = sum(total_msoas_decile),
    
    pct_both_decreased = 100 *
      n_both_decreased /
      total_msoas_region
  ) %>%
  ungroup() %>%
  arrange(
    Region,
    imd_decile_2025
  )

both_decreased_by_region_decile


plot_data_all_deciles <- both_decreased_by_region_decile %>%
  mutate(
    decile = paste0("D", imd_decile_2025)
  )

ggplot(
  plot_data_all_deciles,
  aes(
    x = reorder(
      Region,
      -pct_both_decreased,
      FUN = sum
    ),
    y = pct_both_decreased,
    fill = decile
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Percentage of all MSOAs in region",
    fill = "IMD Decile",
    title = "MSOAs where both male and female life expectancy decreased"
  ) +
  theme_minimal()



#By each sex for sense check .....
# BOTH SEXES - DECREASED
# D1 vs All Other (D2-D10), plus individual deciles

both_sexes_decreased_wide <- msoa_data_complete %>%
  
  # Number of MSOAs where BOTH males and females decreased,
  # within each region and IMD decile
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  # Add total number of MSOAs in each region
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  
  # Percentage of ALL MSOAs in the region
  mutate(
    percentage = 100 * n_decreased / total_msoas
  ) %>%
  
  # Calculate D2-D10 combined = "All Other"
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n_decreased[imd_decile_2025 >= 2],
      na.rm = TRUE
    ),
    
    All_other_percentage = 
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  
  # Keep variables needed
  select(
    Region,
    imd_decile_2025,
    n_decreased,
    percentage,
    All_other_n,
    All_other_percentage
  ) %>%
  
  # Put D1-D10 into separate columns
  pivot_wider(
    names_from = imd_decile_2025,
    values_from = c(n_decreased, percentage),
    names_glue = "D{imd_decile_2025}_{.value}"
  ) %>%
  
  # Keep All Other columns
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_percentage = first(All_other_percentage)
  ) %>%
  ungroup() %>%
  
  # Add overall regional total
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        total_msoas = n(),
        
        overall_n = sum(
          LE_change_M < 0 &
            LE_change_F < 0,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      mutate(
        overall_percentage =
          100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  
  arrange(desc(overall_percentage))


both_sexes_decreased_wide


#For females# FEMALES - DECREASED
# D1 vs All Other (D2-D10), plus individual deciles

females_decreased_wide <- msoa_data_complete %>%
  
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n_decreased = sum(
      LE_change_F < 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  
  mutate(
    percentage = 100 * n_decreased / total_msoas
  ) %>%
  
  # D2-D10 combined
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n_decreased[imd_decile_2025 >= 2],
      na.rm = TRUE
    ),
    
    All_other_percentage =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  
  select(
    Region,
    imd_decile_2025,
    n_decreased,
    percentage,
    All_other_n,
    All_other_percentage
  ) %>%
  
  pivot_wider(
    names_from = imd_decile_2025,
    values_from = c(n_decreased, percentage),
    names_glue = "D{imd_decile_2025}_{.value}"
  ) %>%
  
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_percentage = first(All_other_percentage)
  ) %>%
  ungroup() %>%
  
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        total_msoas = n(),
        
        overall_n = sum(
          LE_change_F < 0,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      mutate(
        overall_percentage =
          100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  
  arrange(desc(overall_percentage))


females_decreased_wide

#For males
# MALES - DECREASED
# D1 vs All Other (D2-D10), plus individual deciles

males_decreased_wide <- msoa_data_complete %>%
  
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n_decreased = sum(
      LE_change_M < 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  
  mutate(
    percentage = 100 * n_decreased / total_msoas
  ) %>%
  
  # D2-D10 combined
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n_decreased[imd_decile_2025 >= 2],
      na.rm = TRUE
    ),
    
    All_other_percentage =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  
  select(
    Region,
    imd_decile_2025,
    n_decreased,
    percentage,
    All_other_n,
    All_other_percentage
  ) %>%
  
  pivot_wider(
    names_from = imd_decile_2025,
    values_from = c(n_decreased, percentage),
    names_glue = "D{imd_decile_2025}_{.value}"
  ) %>%
  
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_percentage = first(All_other_percentage)
  ) %>%
  ungroup() %>%
  
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        total_msoas = n(),
        
        overall_n = sum(
          LE_change_M < 0,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      mutate(
        overall_percentage =
          100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  
  arrange(desc(overall_percentage))


males_decreased_wide


#combining for flourish
# Add Sex label to each existing dataset
both_sexes_decreased_deciles_flourish <- 
  both_sexes_decreased_wide %>%
  mutate(Sex = "Both sexes")

females_decreased_deciles_flourish <- 
  females_decreased_wide %>%
  mutate(Sex = "Female")

males_decreased_deciles_flourish <- 
  males_decreased_wide %>%
  mutate(Sex = "Male")


# Combine into one table
decreased_deciles_flourish <- bind_rows(
  both_sexes_decreased_deciles_flourish,
  females_decreased_deciles_flourish,
  males_decreased_deciles_flourish
) %>%
  
  select(
    Sex,
    Region,
    
    # D1-D10 individually
    D1_n_decreased,
    D1_percentage,
    
    D2_n_decreased,
    D2_percentage,
    
    D3_n_decreased,
    D3_percentage,
    
    D4_n_decreased,
    D4_percentage,
    
    D5_n_decreased,
    D5_percentage,
    
    D6_n_decreased,
    D6_percentage,
    
    D7_n_decreased,
    D7_percentage,
    
    D8_n_decreased,
    D8_percentage,
    
    D9_n_decreased,
    D9_percentage,
    
    D10_n_decreased,
    D10_percentage,
    
    # D2-D10 combined
    All_other_n,
    All_other_percentage,
    
    # Overall
    total_msoas,
    overall_n,
    overall_percentage
  ) %>%
  
  arrange(
    Sex,
    desc(overall_percentage)
  )


decreased_deciles_flourish



##By quintile==========================================================

both_le_by_imd_quintile_2025 <- msoa_data_complete %>%
  group_by(imd_quintile_2025) %>%
  summarise(
    
    # Total number of MSOAs in each quintile
    denominator = n(),
    
    # Number where BOTH male and female LE decreased
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    
    # Percentage where BOTH male and female LE decreased
    proportion_both_decreased =
      round(
        100 * n_both_decreased / denominator,
        2
      ),
    
    # Number where BOTH male and female LE increased
    n_both_increased = sum(
      LE_change_M > 0 &
        LE_change_F > 0
    ),
    
    # Percentage where BOTH male and female LE increased
    proportion_both_increased =
      round(
        100 * n_both_increased / denominator,
        2
      ),
    
    .groups = "drop"
  ) %>%
  arrange(imd_quintile_2025)


both_le_by_imd_quintile_2025



#Sense check: number of MSOA in each region for each Quintile
msoa_by_region_quintile_2025 <- msoa_data_complete %>%
  group_by(
    Region,
    imd_quintile_2025
  ) %>%
  summarise(
    n_MSOAs = n(),
    .groups = "drop"
  ) %>%
  arrange(
    Region,
    imd_quintile_2025
  )


msoa_by_region_quintile_2025

both_le_by_region_quintile_1_vs_5 <- msoa_data_complete %>%
  
  # Only look at Quintile 1 and Quintile 5
  filter(
    imd_quintile_2025 %in% c(1, 5)
  ) %>%
  
  group_by(
    imd_quintile_2025,
    Region
  ) %>%
  
  summarise(
    
    # Total number of MSOAs
    denominator = n(),
    
    # Number where BOTH male and female LE decreased
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    
    # Percentage where BOTH male and female LE decreased
    proportion_both_decreased =
      round(
        100 * n_both_decreased / denominator,
        2
      ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    imd_quintile_2025,
    Region
  )


both_le_by_region_quintile_1_vs_5



# ============================================================

# Flourish charts - QUINTILES
# BOTH SEXES - DECREASED


both_sexes_decreased_quintile_wide <- msoa_data_complete %>%
  
  # Count MSOAs where BOTH males and females decreased
  # within each region and quintile
  group_by(Region, imd_quintile_2025) %>%
  summarise(
    n_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  # Total number of MSOAs in each region
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  
  # Percentage of ALL MSOAs in the region
  mutate(
    percentage = 100 * n_decreased / total_msoas
  ) %>%
  
  # Calculate Q2-Q5 combined = All Other
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n_decreased[imd_quintile_2025 >= 2],
      na.rm = TRUE
    ),
    
    All_other_percentage =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  
  # Keep required variables
  select(
    Region,
    imd_quintile_2025,
    n_decreased,
    percentage,
    All_other_n,
    All_other_percentage
  ) %>%
  
  # Put Q1-Q5 into separate columns
  tidyr::pivot_wider(
    names_from = imd_quintile_2025,
    values_from = c(n_decreased, percentage),
    names_glue = "Q{imd_quintile_2025}_{.value}"
  ) %>%
  
  # Keep All Other
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_percentage = first(All_other_percentage)
  ) %>%
  ungroup() %>%
  
  # Calculate overall regional result
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        total_msoas = n(),
        
        overall_n = sum(
          LE_change_M < 0 &
            LE_change_F < 0,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      mutate(
        overall_percentage =
          100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  
  select(
    Region,
    
    # Q1-Q5 individually
    Q1_n_decreased,
    Q1_percentage,
    
    Q2_n_decreased,
    Q2_percentage,
    
    Q3_n_decreased,
    Q3_percentage,
    
    Q4_n_decreased,
    Q4_percentage,
    
    Q5_n_decreased,
    Q5_percentage,
    
    # Q2-Q5 combined
    All_other_n,
    All_other_percentage,
    
    # Overall
    overall_n,
    overall_percentage
  ) %>%
  
  arrange(desc(overall_percentage))


both_sexes_decreased_quintile_wide


# females - DECREASED

females_decreased_quintile_wide <- msoa_data_complete %>%
  
  group_by(Region, imd_quintile_2025) %>%
  summarise(
    n_decreased = sum(
      LE_change_F < 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  
  mutate(
    percentage = 100 * n_decreased / total_msoas
  ) %>%
  
  # Q2-Q5 combined = All Other
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n_decreased[imd_quintile_2025 >= 2],
      na.rm = TRUE
    ),
    
    All_other_percentage =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  
  select(
    Region,
    imd_quintile_2025,
    n_decreased,
    percentage,
    All_other_n,
    All_other_percentage
  ) %>%
  
  tidyr::pivot_wider(
    names_from = imd_quintile_2025,
    values_from = c(n_decreased, percentage),
    names_glue = "Q{imd_quintile_2025}_{.value}"
  ) %>%
  
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_percentage = first(All_other_percentage)
  ) %>%
  ungroup() %>%
  
  # Overall
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        total_msoas = n(),
        
        overall_n = sum(
          LE_change_F < 0,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      mutate(
        overall_percentage =
          100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  
  select(
    Region,
    
    # Q1-Q5 individually
    Q1_n_decreased,
    Q1_percentage,
    
    Q2_n_decreased,
    Q2_percentage,
    
    Q3_n_decreased,
    Q3_percentage,
    
    Q4_n_decreased,
    Q4_percentage,
    
    Q5_n_decreased,
    Q5_percentage,
    
    # Q2-Q5 combined
    All_other_n,
    All_other_percentage,
    
    # Overall
    overall_n,
    overall_percentage
  ) %>%
  
  arrange(desc(overall_percentage))


females_decreased_quintile_wide


# males - DECREASED

males_decreased_quintile_wide <- msoa_data_complete %>%
  
  group_by(Region, imd_quintile_2025) %>%
  summarise(
    n_decreased = sum(
      LE_change_M < 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  
  mutate(
    percentage = 100 * n_decreased / total_msoas
  ) %>%
  
  # Q2-Q5 combined = All Other
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n_decreased[imd_quintile_2025 >= 2],
      na.rm = TRUE
    ),
    
    All_other_percentage =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  
  select(
    Region,
    imd_quintile_2025,
    n_decreased,
    percentage,
    All_other_n,
    All_other_percentage
  ) %>%
  
  tidyr::pivot_wider(
    names_from = imd_quintile_2025,
    values_from = c(n_decreased, percentage),
    names_glue = "Q{imd_quintile_2025}_{.value}"
  ) %>%
  
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_percentage = first(All_other_percentage)
  ) %>%
  ungroup() %>%
  
  # Overall
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        total_msoas = n(),
        
        overall_n = sum(
          LE_change_M < 0,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      mutate(
        overall_percentage =
          100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  
  select(
    Region,
    
    # Q1-Q5 individually
    Q1_n_decreased,
    Q1_percentage,
    
    Q2_n_decreased,
    Q2_percentage,
    
    Q3_n_decreased,
    Q3_percentage,
    
    Q4_n_decreased,
    Q4_percentage,
    
    Q5_n_decreased,
    Q5_percentage,
    
    # Q2-Q5 combined
    All_other_n,
    All_other_percentage,
    
    # Overall
    overall_n,
    overall_percentage
  ) %>%
  
  arrange(desc(overall_percentage))


males_decreased_quintile_wide



# Combining

both_sexes_decreased_quintile_table <- 
  both_sexes_decreased_quintile_wide %>%
  mutate(
    Sex = "Both sexes"
  )


#females
females_decreased_quintile_table <- 
  females_decreased_quintile_wide %>%
  mutate(
    Sex = "Female"
  )


#males

males_decreased_quintile_table <- 
  males_decreased_quintile_wide %>%
  mutate(
    Sex = "Male"
  )


#combining

decreased_quintiles_all <- bind_rows(
  both_sexes_decreased_quintile_table,
  females_decreased_quintile_table,
  males_decreased_quintile_table
) %>%
  
  select(
    Sex,
    Region,
    
    # Q1
    Q1_n_decreased,
    Q1_percentage,
    
    # Q2
    Q2_n_decreased,
    Q2_percentage,
    
    # Q3
    Q3_n_decreased,
    Q3_percentage,
    
    # Q4
    Q4_n_decreased,
    Q4_percentage,
    
    # Q5
    Q5_n_decreased,
    Q5_percentage,
    
    # Q2-Q5 combined
    All_other_n,
    All_other_percentage,
    
    # Overall
    overall_n,
    overall_percentage
  ) %>%
  
  arrange(
    Sex,
    desc(overall_percentage)
  )


decreased_quintiles_all


###All of the above but increased
# All Other = D1-D9 (i.e. everything except the least deprived decile, D10)

both_increased_deciles <- msoa_data_complete %>%
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n = sum(
      LE_change_M > 0 &
        LE_change_F > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  mutate(
    pct = 100 * n / total_msoas
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n[imd_decile_2025 <= 9],
      na.rm = TRUE
    ),
    All_other_pct =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = imd_decile_2025,
    values_from = c(n, pct),
    names_glue = "D{imd_decile_2025}_{.value}"
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_pct = first(All_other_pct)
  ) %>%
  ungroup() %>%
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        overall_n = sum(
          LE_change_M > 0 &
            LE_change_F > 0,
          na.rm = TRUE
        ),
        total_msoas = n(),
        .groups = "drop"
      ) %>%
      mutate(
        overall_pct = 100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  select(
    Region,
    D1_n, D1_pct,
    D2_n, D2_pct,
    D3_n, D3_pct,
    D4_n, D4_pct,
    D5_n, D5_pct,
    D6_n, D6_pct,
    D7_n, D7_pct,
    D8_n, D8_pct,
    D9_n, D9_pct,
    D10_n, D10_pct,
    All_other_n,
    All_other_pct,
    overall_n,
    overall_pct
  )

both_increased_deciles


# Females
females_increased_deciles <- msoa_data_complete %>%
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n = sum(
      LE_change_F > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  mutate(
    pct = 100 * n / total_msoas
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n[imd_decile_2025 <= 9],
      na.rm = TRUE
    ),
    All_other_pct =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = imd_decile_2025,
    values_from = c(n, pct),
    names_glue = "D{imd_decile_2025}_{.value}"
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_pct = first(All_other_pct)
  ) %>%
  ungroup() %>%
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        overall_n = sum(
          LE_change_F > 0,
          na.rm = TRUE
        ),
        total_msoas = n(),
        .groups = "drop"
      ) %>%
      mutate(
        overall_pct = 100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  select(
    Region,
    D1_n, D1_pct,
    D2_n, D2_pct,
    D3_n, D3_pct,
    D4_n, D4_pct,
    D5_n, D5_pct,
    D6_n, D6_pct,
    D7_n, D7_pct,
    D8_n, D8_pct,
    D9_n, D9_pct,
    D10_n, D10_pct,
    All_other_n,
    All_other_pct,
    overall_n,
    overall_pct
  )

females_increased_deciles


# Males
males_increased_deciles <- msoa_data_complete %>%
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n = sum(
      LE_change_M > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  mutate(
    pct = 100 * n / total_msoas
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n[imd_decile_2025 <= 9],
      na.rm = TRUE
    ),
    All_other_pct =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = imd_decile_2025,
    values_from = c(n, pct),
    names_glue = "D{imd_decile_2025}_{.value}"
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_pct = first(All_other_pct)
  ) %>%
  ungroup() %>%
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        overall_n = sum(
          LE_change_M > 0,
          na.rm = TRUE
        ),
        total_msoas = n(),
        .groups = "drop"
      ) %>%
      mutate(
        overall_pct = 100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  select(
    Region,
    D1_n, D1_pct,
    D2_n, D2_pct,
    D3_n, D3_pct,
    D4_n, D4_pct,
    D5_n, D5_pct,
    D6_n, D6_pct,
    D7_n, D7_pct,
    D8_n, D8_pct,
    D9_n, D9_pct,
    D10_n, D10_pct,
    All_other_n,
    All_other_pct,
    overall_n,
    overall_pct
  )

males_increased_deciles


##combining


combined_increased_deciles <- bind_rows(
  
  both_increased_deciles %>%
    mutate(Sex = "Both sexes"),
  
  females_increased_deciles %>%
    mutate(Sex = "Female"),
  
  males_increased_deciles %>%
    mutate(Sex = "Male")
  
) %>%
  
  select(
    Sex,
    Region,
    
    # D1-D10
    D1_n,
    D1_pct,
    
    D2_n,
    D2_pct,
    
    D3_n,
    D3_pct,
    
    D4_n,
    D4_pct,
    
    D5_n,
    D5_pct,
    
    D6_n,
    D6_pct,
    
    D7_n,
    D7_pct,
    
    D8_n,
    D8_pct,
    
    D9_n,
    D9_pct,
    
    D10_n,
    D10_pct,
    
    # D2-D10 combined
    All_other_n,
    All_other_pct,
    
    # Overall
    overall_n,
    overall_pct
  ) %>%
  
  arrange(
    factor(
      Sex,
      levels = c("Both sexes", "Female", "Male")
    ),
    desc(overall_pct)
  )


# View combined table
combined_increased_deciles

#Now quintiles

# Quintiles

both_increased_quintiles <- msoa_data_complete %>%
  group_by(Region, imd_quintile_2025) %>%
  summarise(
    n = sum(
      LE_change_M > 0 &
        LE_change_F > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  mutate(
    pct = 100 * n / total_msoas
  ) %>%
  # Q1-Q4 combined = All Other
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n[imd_quintile_2025 <= 4],
      na.rm = TRUE
    ),
    All_other_pct =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = imd_quintile_2025,
    values_from = c(n, pct),
    names_glue = "Q{imd_quintile_2025}_{.value}"
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_pct = first(All_other_pct)
  ) %>%
  ungroup() %>%
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        overall_n = sum(
          LE_change_M > 0 &
            LE_change_F > 0,
          na.rm = TRUE
        ),
        total_msoas = n(),
        .groups = "drop"
      ) %>%
      mutate(
        overall_pct = 100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  select(
    Region,
    Q1_n, Q1_pct,
    Q2_n, Q2_pct,
    Q3_n, Q3_pct,
    Q4_n, Q4_pct,
    Q5_n, Q5_pct,
    All_other_n,
    All_other_pct,
    overall_n,
    overall_pct
  )

both_increased_quintiles


# Females
females_increased_quintiles <- msoa_data_complete %>%
  group_by(Region, imd_quintile_2025) %>%
  summarise(
    n = sum(
      LE_change_F > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  mutate(
    pct = 100 * n / total_msoas
  ) %>%
  # Q1-Q4 combined = All Other
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n[imd_quintile_2025 <= 4],
      na.rm = TRUE
    ),
    All_other_pct =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = imd_quintile_2025,
    values_from = c(n, pct),
    names_glue = "Q{imd_quintile_2025}_{.value}"
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_pct = first(All_other_pct)
  ) %>%
  ungroup() %>%
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        overall_n = sum(
          LE_change_F > 0,
          na.rm = TRUE
        ),
        total_msoas = n(),
        .groups = "drop"
      ) %>%
      mutate(
        overall_pct = 100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  select(
    Region,
    Q1_n, Q1_pct,
    Q2_n, Q2_pct,
    Q3_n, Q3_pct,
    Q4_n, Q4_pct,
    Q5_n, Q5_pct,
    All_other_n,
    All_other_pct,
    overall_n,
    overall_pct
  )

females_increased_quintiles


# Males
males_increased_quintiles <- msoa_data_complete %>%
  group_by(Region, imd_quintile_2025) %>%
  summarise(
    n = sum(
      LE_change_M > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    msoa_data_complete %>%
      count(Region, name = "total_msoas"),
    by = "Region"
  ) %>%
  mutate(
    pct = 100 * n / total_msoas
  ) %>%
  # Q1-Q4 combined = All Other
  group_by(Region) %>%
  mutate(
    All_other_n = sum(
      n[imd_quintile_2025 <= 4],
      na.rm = TRUE
    ),
    All_other_pct =
      100 * All_other_n / first(total_msoas)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = imd_quintile_2025,
    values_from = c(n, pct),
    names_glue = "Q{imd_quintile_2025}_{.value}"
  ) %>%
  group_by(Region) %>%
  mutate(
    All_other_n = first(All_other_n),
    All_other_pct = first(All_other_pct)
  ) %>%
  ungroup() %>%
  left_join(
    msoa_data_complete %>%
      group_by(Region) %>%
      summarise(
        overall_n = sum(
          LE_change_M > 0,
          na.rm = TRUE
        ),
        total_msoas = n(),
        .groups = "drop"
      ) %>%
      mutate(
        overall_pct = 100 * overall_n / total_msoas
      ),
    by = "Region"
  ) %>%
  select(
    Region,
    Q1_n, Q1_pct,
    Q2_n, Q2_pct,
    Q3_n, Q3_pct,
    Q4_n, Q4_pct,
    Q5_n, Q5_pct,
    All_other_n,
    All_other_pct,
    overall_n,
    overall_pct
  )

males_increased_quintiles


#COmbining

increased_quintiles_combined <- bind_rows(
  
  both_increased_quintiles %>%
    mutate(Sex = "Both sexes"),
  
  females_increased_quintiles %>%
    mutate(Sex = "Female"),
  
  males_increased_quintiles %>%
    mutate(Sex = "Male")
  
) %>%
  
  # Put Sex first, then Region
  select(
    Sex,
    Region,
    
    # Q1-Q5 individually
    Q1_n,
    Q1_pct,
    
    Q2_n,
    Q2_pct,
    
    Q3_n,
    Q3_pct,
    
    Q4_n,
    Q4_pct,
    
    Q5_n,
    Q5_pct,
    
    # Q2-Q5 combined
    All_other_n,
    All_other_pct,
    
    # Overall
    overall_n,
    overall_pct
  ) %>%
  
  # Optional: order by Sex and then overall percentage
  arrange(
    factor(
      Sex,
      levels = c("Both sexes", "Female", "Male")
    ),
    desc(overall_pct)
  )


# View combined table
increased_quintiles_combined



##Violin plot - mimic fig 3 from publication in hle progresss chat



#install.packages("ggbeeswarm")

library(dplyr)
library(ggplot2)
library(ggbeeswarm)

# Prepare data
plot_males <- msoa_data_complete %>%
  filter(!is.na(LE_change_M)) %>%
  mutate(
    LE_index = LE_change_M,
    direction = case_when(
      LE_change_M > 0 ~ "Increased",
      LE_change_M < 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  )

# Order regions by median change
region_order <- plot_males %>%
  group_by(Region) %>%
  summarise(
    median_change = median(LE_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_change)) %>%
  pull(Region)

plot_males <- plot_males %>%
  mutate(
    Region = factor(Region, levels = region_order)
  )

# Plot
ggplot(
  plot_males,
  aes(
    x = LE_index,
    y = Region,
    colour = direction
  )
) +
  
  # Optional very light distribution behind the dots
  geom_violin(
    aes(group = Region),
    fill = "grey90",
    colour = NA,
    alpha = 0.5,
    width = 0.8,
    orientation = "y"
  ) +
  
  # One dot = one MSOA
  geom_quasirandom(
    size = 2.5,
    alpha = 0.85,
    width = 0.25
  ) +
  
  # No-change reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  
  labs(
    x = "Life expectancy change",
    y = NULL,
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )




plot_females <- msoa_data_complete %>%
  filter(!is.na(LE_change_F)) %>%
  mutate(
    LE_index = LE_change_F,
    direction = case_when(
      LE_change_F > 0 ~ "Increased",
      LE_change_F < 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  )

region_order <- plot_females %>%
  group_by(Region) %>%
  summarise(
    median_change = median(LE_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_change)) %>%
  pull(Region)

plot_females <- plot_females %>%
  mutate(
    Region = factor(Region, levels = region_order)
  )

ggplot(
  plot_females,
  aes(
    x = LE_index,
    y = Region,
    colour = direction
  )
) +
  geom_violin(
    aes(group = Region),
    fill = "grey90",
    colour = NA,
    alpha = 0.5,
    width = 0.8,
    orientation = "y"
  ) +
  geom_quasirandom(
    size = 2.5,
    alpha = 0.85,
    width = 0.25
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  labs(
    x = "Life expectancy change",
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )


##Editing these to add median/ more box plot like 

#males
plot_males <- msoa_data_complete %>%
  filter(!is.na(LE_change_M)) %>%
  mutate(
    LE_index = LE_change_M,
    direction = case_when(
      LE_change_M > 0 ~ "Increased",
      LE_change_M < 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  )

# Order regions by median change
region_order <- plot_males %>%
  group_by(Region) %>%
  summarise(
    median_change = median(LE_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_change)) %>%
  pull(Region)

plot_males <- plot_males %>%
  mutate(
    Region = factor(Region, levels = region_order)
  )


# Plot
ggplot(
  plot_males,
  aes(
    x = LE_index,
    y = Region,
    colour = direction
  )
) +
  
  # Distribution
  geom_violin(
    aes(group = Region),
    fill = "grey90",
    colour = NA,
    alpha = 0.5,
    width = 0.8,
    orientation = "y"
  ) +
  
  # One dot = one MSOA
  geom_quasirandom(
    size = 2.5,
    alpha = 0.70,
    width = 0.25
  ) +
  
  # Boxplot showing IQR + median
  geom_boxplot(
    aes(group = Region),
    width = 0.20,
    fill = "white",
    colour = "black",
    alpha = 0.9,
    outlier.shape = NA,
    orientation = "y"
  ) +
  
  # No-change reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  
  labs(
    x = "Life expectancy change",
    y = NULL,
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )


#females

plot_females <- msoa_data_complete %>%
  filter(!is.na(LE_change_F)) %>%
  mutate(
    LE_index = LE_change_F,
    direction = case_when(
      LE_change_F > 0 ~ "Increased",
      LE_change_F < 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  )

# Order regions by median change
region_order <- plot_females %>%
  group_by(Region) %>%
  summarise(
    median_change = median(LE_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_change)) %>%
  pull(Region)

plot_females <- plot_females %>%
  mutate(
    Region = factor(Region, levels = region_order)
  )


# Plot
ggplot(
  plot_females,
  aes(
    x = LE_index,
    y = Region,
    colour = direction
  )
) +
  
  # Distribution
  geom_violin(
    aes(group = Region),
    fill = "grey90",
    colour = NA,
    alpha = 0.5,
    width = 0.8,
    orientation = "y"
  ) +
  
  # One dot = one MSOA
  geom_quasirandom(
    size = 2.5,
    alpha = 0.70,
    width = 0.25
  ) +
  
  # Boxplot showing IQR + median
  geom_boxplot(
    aes(group = Region),
    width = 0.20,
    fill = "white",
    colour = "black",
    alpha = 0.9,
    outlier.shape = NA,
    orientation = "y"
  ) +
  
  # No-change reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  
  labs(
    x = "Life expectancy change",
    y = NULL,
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )

# Prepare male + female data in long format to try and plot both of them

plot_both <- msoa_data_complete %>%
  select(Region, LE_change_M, LE_change_F) %>%
  pivot_longer(
    cols = c(LE_change_M, LE_change_F),
    names_to = "Sex",
    values_to = "LE_change"
  ) %>%
  mutate(
    Sex = case_when(
      Sex == "LE_change_M" ~ "Male",
      Sex == "LE_change_F" ~ "Female"
    ),
    
    # Convert change into index
    LE_index = LE_change,
    
    # Direction of change
    direction = case_when(
      LE_change > 0 ~ "Increased",
      LE_change < 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  ) %>%
  filter(!is.na(LE_change))



# Order regions by median change across males + females

region_order <- plot_both %>%
  group_by(Region) %>%
  summarise(
    median_change = median(LE_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_change) %>%
  pull(Region)

plot_both <- plot_both %>%
  mutate(
    Region = factor(Region, levels = region_order)
  )


# Plot males + females together

ggplot(
  plot_both,
  aes(
    x = LE_index,
    y = Region,
    colour = direction,
    shape = Sex
  )
) +
  
  # Distribution behind the dots
  geom_violin(
    aes(
      group = Region
    ),
    fill = "grey90",
    colour = NA,
    alpha = 0.5,
    width = 0.8,
    orientation = "y"
  ) +
  
  # One dot = one MSOA
  # Dodge separates males and females slightly
  geom_quasirandom(
    size = 2.5,
    alpha = 0.75,
    width = 0.25,
    dodge.width = 0.35
  ) +
  
  # No-change reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  
  # Colours for direction
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  
  # Different shapes for male/female
  scale_shape_manual(
    values = c(
      "Male" = 16,
      "Female" = 17
    )
  ) +
  
  labs(
    x = "Life expectancy change",
    y = NULL,
    colour = NULL,
    shape = "Sex"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )




##Instead of region looking at IMD

## Instead of region, looking at IMD deciles

plot_females_deciles <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_F),
    !is.na(imd_decile_2025)
  ) %>%
  mutate(
    direction = case_when(
      LE_change_F > 0 ~ "Increased",
      LE_change_F < 0 ~ "Decreased",
      TRUE ~ "No change"
    ),
    
    Decile = factor(
      imd_decile_2025,
      levels = 1:10,
      labels = paste0("D", 1:10)
    )
  )

# Arrange IMD deciles by median change
decile_order_female <- plot_females_deciles %>%
  group_by(Decile) %>%
  summarise(
    median_change = median(LE_change_F, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_change) %>%       # most decreased first
  pull(Decile)

# Reverse because ggplot puts first level at the bottom
plot_females_deciles <- plot_females_deciles %>%
  mutate(
    Decile = factor(
      Decile,
      levels = rev(decile_order_female)
    )
  )

ggplot(
  plot_females_deciles,
  aes(
    x = LE_change_F,
    y = Decile,
    colour = direction
  )
) +
  geom_quasirandom(
    size = 2.5,
    alpha = 0.8,
    width = 0.25
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  labs(
    x = "Change in female life expectancy",
    y = "IMD decile",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )


plot_males_deciles <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(imd_decile_2025)
  ) %>%
  mutate(
    direction = case_when(
      LE_change_M > 0 ~ "Increased",
      LE_change_M < 0 ~ "Decreased",
      TRUE ~ "No change"
    ),
    
    Decile = factor(
      imd_decile_2025,
      levels = 1:10,
      labels = paste0("D", 1:10)
    )
  )

# Arrange IMD deciles by median change
decile_order_male <- plot_males_deciles %>%
  group_by(Decile) %>%
  summarise(
    median_change = median(LE_change_M, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(median_change) %>%       # most decreased first
  pull(Decile)

# Reverse because ggplot puts first level at the bottom
plot_males_deciles <- plot_males_deciles %>%
  mutate(
    Decile = factor(
      Decile,
      levels = rev(decile_order_male)
    )
  )

ggplot(
  plot_males_deciles,
  aes(
    x = LE_change_M,
    y = Decile,
    colour = direction
  )
) +
  geom_quasirandom(
    size = 2.5,
    alpha = 0.8,
    width = 0.25
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  labs(
    x = "Change in male life expectancy",
    y = "IMD decile",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )


#For Quintiles

plot_females_quintiles <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_F),
    !is.na(imd_quintile_2025)
  ) %>%
  mutate(
    direction = case_when(
      LE_change_F > 0 ~ "Increased",
      LE_change_F < 0 ~ "Decreased",
      TRUE ~ "No change"
    ),
    
    Quintile = factor(
      imd_quintile_2025,
      levels = 1:5,
      labels = paste0("Q", 1:5)
    )
  )

ggplot(
  plot_females_quintiles,
  aes(
    x = LE_change_F,
    y = Quintile,
    colour = direction
  )
) +
  geom_quasirandom(
    size = 2.5,
    alpha = 0.8,
    width = 0.25
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  scale_y_discrete(limits = rev(paste0("Q", 1:5))) +
  labs(
    x = "Change in female life expectancy",
    y = "IMD quintile",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )




plot_males_quintiles <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(imd_quintile_2025)
  ) %>%
  mutate(
    direction = case_when(
      LE_change_M > 0 ~ "Increased",
      LE_change_M < 0 ~ "Decreased",
      TRUE ~ "No change"
    ),
    
    Quintile = factor(
      imd_quintile_2025,
      levels = 1:5,
      labels = paste0("Q", 1:5)
    )
  )

ggplot(
  plot_males_quintiles,
  aes(
    x = LE_change_M,
    y = Quintile,
    colour = direction
  )
) +
  geom_quasirandom(
    size = 2.5,
    alpha = 0.8,
    width = 0.25
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  scale_y_discrete(limits = rev(paste0("Q", 1:5))) +
  labs(
    x = "Change in male life expectancy",
    y = "IMD quintile",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )




##Adding box plots here


plot_females_quintiles <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_F),
    !is.na(imd_quintile_2025)
  ) %>%
  mutate(
    direction = case_when(
      LE_change_F > 0 ~ "Increased",
      LE_change_F < 0 ~ "Decreased",
      TRUE ~ "No change"
    ),
    
    Quintile = factor(
      imd_quintile_2025,
      levels = 1:5,
      labels = paste0("Q", 1:5)
    )
  )

ggplot(
  plot_females_quintiles,
  aes(
    x = LE_change_F,
    y = Quintile,
    colour = direction
  )
) +
  
  # Individual MSOAs
  geom_quasirandom(
    size = 2.5,
    alpha = 0.70,
    width = 0.25
  ) +
  
  # Boxplot showing IQR + median
  geom_boxplot(
    aes(group = Quintile),
    width = 0.20,
    fill = "white",
    colour = "black",
    alpha = 0.9,
    outlier.shape = NA,
    orientation = "y"
  ) +
  
  # No-change reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  
  scale_y_discrete(
    limits = rev(paste0("Q", 1:5))
  ) +
  
  labs(
    x = "Change in female life expectancy",
    y = "IMD quintile",
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )


plot_males_quintiles <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(imd_quintile_2025)
  ) %>%
  mutate(
    direction = case_when(
      LE_change_M > 0 ~ "Increased",
      LE_change_M < 0 ~ "Decreased",
      TRUE ~ "No change"
    ),
    
    Quintile = factor(
      imd_quintile_2025,
      levels = 1:5,
      labels = paste0("Q", 1:5)
    )
  )

ggplot(
  plot_males_quintiles,
  aes(
    x = LE_change_M,
    y = Quintile,
    colour = direction
  )
) +
  
  # Individual MSOAs
  geom_quasirandom(
    size = 2.5,
    alpha = 0.70,
    width = 0.25
  ) +
  
  # Boxplot showing IQR + median
  geom_boxplot(
    aes(group = Quintile),
    width = 0.20,
    fill = "white",
    colour = "black",
    alpha = 0.9,
    outlier.shape = NA,
    orientation = "y"
  ) +
  
  # No-change reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.7
  ) +
  
  scale_colour_manual(
    values = c(
      "Decreased" = "#D7304F",
      "Increased" = "#4EA3C8",
      "No change" = "grey50"
    )
  ) +
  
  scale_y_discrete(
    limits = rev(paste0("Q", 1:5))
  ) +
  
  labs(
    x = "Change in male life expectancy",
    y = "IMD quintile",
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(face = "bold")
  )





##Data behind the box plots on the powerpoint


female_msoa <- msoa_data_complete %>%
  filter(!is.na(LE_change_F)) %>%
  mutate(
    direction = case_when(
      LE_change_F > 0 ~ "Increased",
      LE_change_F < 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  ) %>%
  select(
    MSOA = MSOA21CD,
    MSOA_name = MSOA21NM,
    Region,
    LE_change = LE_change_F,
    direction
  )

# write_csv(
#   female_msoa,
#   "female_MSOA_level_data.csv"
# )




male_msoa <- msoa_data_complete %>%
  filter(!is.na(LE_change_M)) %>%
  mutate(
    direction = case_when(
      LE_change_M > 0 ~ "Increased",
      LE_change_M < 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  ) %>%
  select(
    MSOA = MSOA21CD,
    MSOA_name = MSOA21NM,
    Region,
    LE_change = LE_change_M,
    direction
  )

# write_csv(
#   male_msoa,
#   "male_MSOA_level_data.csv"
# )


female_region_summary <- female_msoa %>%
  group_by(Region) %>%
  summarise(
    n_MSOA = n(),
    median = median(LE_change, na.rm = TRUE),
    Q1 = quantile(LE_change, 0.25, na.rm = TRUE),
    Q3 = quantile(LE_change, 0.75, na.rm = TRUE),
    IQR = IQR(LE_change, na.rm = TRUE),
    min = min(LE_change, na.rm = TRUE),
    max = max(LE_change, na.rm = TRUE),
    .groups = "drop"
  )

# write_csv(
#   female_region_summary,
#   "female_regional_summary.csv"
# )




male_region_summary <- male_msoa %>%
  group_by(Region) %>%
  summarise(
    n_MSOA = n(),
    median = median(LE_change, na.rm = TRUE),
    Q1 = quantile(LE_change, 0.25, na.rm = TRUE),
    Q3 = quantile(LE_change, 0.75, na.rm = TRUE),
    IQR = IQR(LE_change, na.rm = TRUE),
    min = min(LE_change, na.rm = TRUE),
    max = max(LE_change, na.rm = TRUE),
    .groups = "drop"
  )

# write_csv(
#   male_region_summary,
#   "male_regional_summary.csv"
# )




female_quintile_summary <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_F),
    !is.na(imd_quintile_2025)
  ) %>%
  group_by(imd_quintile_2025) %>%
  summarise(
    n_MSOA = n(),
    median = median(LE_change_F, na.rm = TRUE),
    Q1 = quantile(LE_change_F, 0.25, na.rm = TRUE),
    Q3 = quantile(LE_change_F, 0.75, na.rm = TRUE),
    IQR = IQR(LE_change_F, na.rm = TRUE),
    min = min(LE_change_F, na.rm = TRUE),
    max = max(LE_change_F, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Quintile = paste0("Q", imd_quintile_2025)
  ) %>%
  select(
    Quintile,
    n_MSOA,
    median,
    Q1,
    Q3,
    IQR,
    min,
    max
  )

# write_csv(
#   female_quintile_summary,
#   "female_IMD_quintile_summary.csv"
# )



male_quintile_summary <- msoa_data_complete %>%
  filter(
    !is.na(LE_change_M),
    !is.na(imd_quintile_2025)
  ) %>%
  group_by(imd_quintile_2025) %>%
  summarise(
    n_MSOA = n(),
    median = median(LE_change_M, na.rm = TRUE),
    Q1 = quantile(LE_change_M, 0.25, na.rm = TRUE),
    Q3 = quantile(LE_change_M, 0.75, na.rm = TRUE),
    IQR = IQR(LE_change_M, na.rm = TRUE),
    min = min(LE_change_M, na.rm = TRUE),
    max = max(LE_change_M, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Quintile = paste0("Q", imd_quintile_2025)
  ) %>%
  select(
    Quintile,
    n_MSOA,
    median,
    Q1,
    Q3,
    IQR,
    min,
    max
  )

# write_csv(
#   male_quintile_summary,
#   "male_IMD_quintile_summary.csv"
# )
# 
# 
# cat(
#   "\nExport complete. Files created:\n",
#   "1. female_MSOA_level_data.csv\n",
#   "2. male_MSOA_level_data.csv\n",
#   "3. female_regional_summary.csv\n",
#   "4. male_regional_summary.csv\n",
#   "5. female_IMD_quintile_summary.csv\n",
#   "6. male_IMD_quintile_summary.csv\n"
# )





























##checking Confidence intervals ==============================================


both_le_by_region_ci <- msoa_data_complete %>%
  
  # Only consider MSOAs where both sexes declined
  # based on the observed point estimates
  filter(
    LE_2021_M < LE_2011_M,
    LE_2021_F < LE_2011_F
  ) %>%
  
  mutate(
    
    # Maximum possible change within the CIs
    male_max_change =
      LE_2021_M_UCL - LE_2011_M_LCL,
    
    female_max_change =
      LE_2021_F_UCL - LE_2011_F_LCL,
    
    # Both sexes necessarily declined
    both_robust_decline =
      male_max_change < 0 &
      female_max_change < 0
  ) %>%
  
  group_by(Region) %>%
  
  summarise(
    
    # All MSOAs where both sexes observed a decline
    msoas_both_observed_decline = n(),
    
    # MSOAs where BOTH sexes still necessarily declined
    # even under the most favourable CI scenario
    msoas_both_robust_decline =
      sum(both_robust_decline),
    
    # Percentage of observed both-sex declines that
    # remain robust
    percentage_both_robust_decline =
      round(
        100 *
          msoas_both_robust_decline /
          msoas_both_observed_decline,
        1
      ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    desc(percentage_both_robust_decline)
  )


both_le_by_region_ci


#,maullay checking the MSOAs

# ============================================================
# MSOAs WITH ROBUST BOTH-SEX LE DECLINE
#
# Both sexes declined based on the observed estimates AND
# the decline cannot disappear anywhere within the 95% CIs.


msoa_both_declines_robust <- msoa_data_complete %>%
  
  mutate(
    
    # Observed changes
    LE_change_M = LE_2021_M - LE_2011_M,
    LE_change_F = LE_2021_F - LE_2011_F,
    
    # Most favourable possible change within the CIs
    male_max_change =
      LE_2021_M_UCL - LE_2011_M_LCL,
    
    female_max_change =
      LE_2021_F_UCL - LE_2011_F_LCL
    
  ) %>%
  
  filter(
    
    # Both sexes declined based on point estimates
    LE_change_M < 0,
    LE_change_F < 0,
    
    # Both declines remain negative across the CIs
    male_max_change < 0,
    female_max_change < 0
    
  ) %>%
  
  select(
    
    # MSOA
    MSOA21CD,
    MSOA21NM,
    Region,
    
    # IMD
    imd_average_score_2015,
    imd_decile_2015,
    imd_quintile_2015,
    imd_average_score_2025,
    imd_decile_2025,
    imd_quintile_2025,
    
    # ----------------------------
    # MALE
    # ----------------------------
    
    LE_2011_M,
    LE_2011_M_LCL,
    LE_2011_M_UCL,
    
    LE_2021_M,
    LE_2021_M_LCL,
    LE_2021_M_UCL,
    
    LE_change_M,
    male_max_change,
    
    # ----------------------------
    # FEMALE
    # ----------------------------
    
    LE_2011_F,
    LE_2011_F_LCL,
    LE_2011_F_UCL,
    
    LE_2021_F,
    LE_2021_F_LCL,
    LE_2021_F_UCL,
    
    LE_change_F,
    female_max_change
    
  ) %>%
  
  arrange(
    Region,
    MSOA21CD
  )


# View the dataset
msoa_both_declines_robust



#Then the same but for imd


both_le_by_imd_2025_ci <- msoa_data_complete %>%
  
  mutate(
    
    # Observed changes
    LE_change_M = LE_2021_M - LE_2011_M,
    LE_change_F = LE_2021_F - LE_2011_F,
    
    # Maximum possible change within the CIs
    male_max_change =
      LE_2021_M_UCL - LE_2011_M_LCL,
    
    female_max_change =
      LE_2021_F_UCL - LE_2011_F_LCL,
    
    # Both sexes still declined even at the most
    # favourable ends of their confidence intervals
    both_robust_decline =
      male_max_change < 0 &
      female_max_change < 0
    
  ) %>%
  
  group_by(imd_quintile_2025) %>%
  
  summarise(
    
    # Total MSOAs in the quintile
    denominator = n(),
    
    # Both sexes declined based on observed estimates
    n_both_decreased =
      sum(
        LE_change_M < 0 &
          LE_change_F < 0
      ),
    
    proportion_both_decreased =
      round(
        100 * n_both_decreased / denominator,
        2
      ),
    
    # Of the observed both-sex declines,
    # how many remain robust when considering the CIs?
    n_both_decreased_robust =
      sum(
        LE_change_M < 0 &
          LE_change_F < 0 &
          both_robust_decline
      ),
    
    proportion_both_decreased_robust =
      round(
        100 *
          n_both_decreased_robust /
          n_both_decreased,
        2
      ),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(imd_quintile_2025)


both_le_by_imd_2025_ci
