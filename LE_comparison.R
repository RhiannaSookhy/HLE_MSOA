

#LE comparison by region and IMD =================================================================


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
setwd(
  "~/Analysis and Modelling general/2011-2021 HLE by MSOA"
)


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


both_le_by_imd_2025 <- msoa_data_complete %>%
  group_by(imd_decile_2025) %>%
  summarise(
    denominator = n(),
    
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    
    proportion_both_decreased =
      round(100 * n_both_decreased / denominator, 2),
    
    n_both_increased = sum(
      LE_change_M > 0 &
        LE_change_F > 0
    ),
    
    proportion_both_increased =
      round(100 * n_both_increased / denominator, 2),
    
    .groups = "drop"
  ) %>%
  arrange(imd_decile_2025)

both_le_by_imd_2025



## add in region 
# ============================================================
# BOTH MALE AND FEMALE LE DECREASED
# BY REGION WITHIN IMD DECILES
#
# Decile 1  = most deprived
# Decile 10 = least deprived
#
# Denominator = ALL MSOAs in that region and decile
# Numerator   = MSOAs where BOTH male and female LE decreased
# ============================================================

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



# Number of MSOAs in each 2025 IMD decile by region
msoa_by_region_decile_2025 <- msoa_data_complete %>%
  group_by(Region, imd_decile_2025) %>%
  summarise(
    n_MSOAs = n(),
    .groups = "drop"
  ) %>%
  arrange(Region, imd_decile_2025)

msoa_by_region_decile_2025



# ============================================================
# ============================================================
# MSOAs WHERE BOTH MALE AND FEMALE LE DECLINED
# BY REGION AND 2025 IMD DECILE
# ============================================================

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



##By quintile
# ============================================================
# 2025 IMD QUINTILE ANALYSIS
#
# Quintile 1 = most deprived
# Quintile 5 = least deprived
# ============================================================


# ============================================================
# BOTH MALE AND FEMALE LE DECREASED
# BY 2025 IMD QUINTILE
# ============================================================

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



# ============================================================
# BOTH MALE AND FEMALE LE DECREASED
# BY REGION WITHIN 2025 IMD QUINTILES
#
# Quintile 1 = most deprived
# Quintile 5 = least deprived
#
# Denominator = ALL MSOAs in that region and quintile
# Numerator   = MSOAs where BOTH male and female LE decreased
# ============================================================

both_le_by_region_quintile_2025 <- msoa_data_complete %>%
  
  group_by(
    imd_quintile_2025,
    Region
  ) %>%
  
  summarise(
    
    # Total number of MSOAs in that region + quintile
    denominator = n(),
    
    # Number where BOTH male and female LE decreased
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    
    # Percentage of MSOAs where both LE decreased
    proportion_both_decreased =
      round(
        100 * n_both_decreased / denominator,
        2
      ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    imd_quintile_2025,
    desc(proportion_both_decreased)
  )


both_le_by_region_quintile_2025


#checking this with previous code

both_le_by_imd_2025 <- msoa_data_complete %>%
  group_by(imd_quintile_2025) %>%
  summarise(
    denominator = n(),
    
    n_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0
    ),
    
    proportion_both_decreased =
      round(100 * n_both_decreased / denominator, 2),
    
    n_both_increased = sum(
      LE_change_M > 0 &
        LE_change_F > 0
    ),
    
    proportion_both_increased =
      round(100 * n_both_increased / denominator, 2),
    
    .groups = "drop"
  ) %>%
  arrange(imd_quintile_2025)

both_le_by_imd_2025



# ============================================================
# NUMBER OF MSOAs IN EACH 2025 IMD QUINTILE BY REGION
# ============================================================

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



# ============================================================
# MSOAs WHERE BOTH MALE AND FEMALE LE DECLINED
# BY REGION AND 2025 IMD QUINTILE
# ============================================================

both_sex_declined_by_region_quintile <- msoa_data_complete %>%
  
  filter(
    LE_change_M < 0 &
      LE_change_F < 0
  ) %>%
  
  group_by(
    Region,
    imd_quintile_2025
  ) %>%
  
  summarise(
    n_MSOAs_both_declined = n(),
    .groups = "drop"
  ) %>%
  
  arrange(
    Region,
    imd_quintile_2025
  )


both_sex_declined_by_region_quintile





# ============================================================
# QUICK LOOK:
# BOTH MALE AND FEMALE LE DECLINED
# 2025 IMD QUINTILE 1 vs QUINTILE 5
#
# Quintile 1 = most deprived
# Quintile 5 = least deprived
# ============================================================

both_le_quintile_1_vs_5 <- msoa_data_complete %>%
  
  filter(
    imd_quintile_2025 %in% c(1, 5)
  ) %>%
  
  group_by(
    imd_quintile_2025
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


both_le_quintile_1_vs_5


# ============================================================
# BOTH MALE AND FEMALE LE DECREASED
# BY REGION: 2025 IMD QUINTILE 1 vs QUINTILE 5
#
# Quintile 1 = most deprived
# Quintile 5 = least deprived
#
# Denominator = ALL MSOAs in that region + quintile
# Numerator   = MSOAs where BOTH male and female LE decreased
# ============================================================

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



##

# ============================================================
# PROPORTION OF 2025 IMD QUINTILE 1 MSOAs BY REGION
#
# Quintile 1 = most deprived
#
# Shows what proportion of all Quintile 1 MSOAs
# come from each region
# ============================================================

quintile_1_by_region <- msoa_data_complete %>%
  
  # Keep only the most deprived quintile
  filter(
    imd_quintile_2025 == 1
  ) %>%
  
  group_by(Region) %>%
  
  summarise(
    
    # Number of Quintile 1 MSOAs in the region
    n_MSOAs = n(),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    # Proportion of ALL Quintile 1 MSOAs
    proportion = round(
      100 * n_MSOAs / sum(n_MSOAs),
      2
    )
  ) %>%
  
  arrange(
    desc(proportion)
  )


quintile_1_by_region





# ============================================================
# BOTH MALE AND FEMALE LE DECREASED
# NORTH EAST BY 2025 IMD QUINTILE
#
# Quintile 1 = most deprived
# Quintile 5 = least deprived
#
# Denominator = ALL North East MSOAs in each quintile
# Numerator   = MSOAs where BOTH male and female LE decreased
# ============================================================

north_east_both_le_by_quintile <- msoa_data_complete %>%
  
  filter(
    Region == "London"
  ) %>%
  
  group_by(
    imd_quintile_2025
  ) %>%
  
  summarise(
    
    # Total North East MSOAs in the quintile
    denominator = n(),
    
    # MSOAs where BOTH male and female LE decreased
    numerator_both_decreased = sum(
      LE_change_M < 0 &
        LE_change_F < 0,
      na.rm = TRUE
    ),
    
    # Percentage
    proportion_both_decreased = round(
      100 * numerator_both_decreased / denominator,
      2
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(imd_quintile_2025)


north_east_both_le_by_quintile






##checking Confidence intervals ==============================================
# ============================================================
# ROBUST BOTH-SEX LE DECLINE BY REGION
#
# Question:
# Of MSOAs where both male and female LE declined based on
# the observed estimates, how many would STILL have declined
# in both sexes even if the estimates could take ANY value
# within their 95% confidence intervals?
#
# Most favourable scenario for NO decline:
#
#   2021 UCL - 2011 LCL
#
# If this remains < 0, the decline cannot disappear.
# ============================================================

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
#
# Robust decline criterion:
#
#   Male:   2021 UCL - 2011 LCL < 0
#   Female: 2021 UCL - 2011 LCL < 0
# ============================================================

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
# ============================================================
# BOTH-SEX LE DECLINE BY 2025 IMD QUINTILE
#
# Robust decline:
# 2021 UCL - 2011 LCL < 0 for BOTH males and females
# ============================================================

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
