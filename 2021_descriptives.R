
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


# ============================================================
# 7. CHECK HEALTH DATA
# ============================================================

health_2021 %>%
  count(Sex)

health_2021 %>%
  count(MSOA21CD) %>%
  count(n)



# ------------------------------------------------------------
# LE: TOP 10 BY SEX
# ------------------------------------------------------------

top10_LE <- health_2021 %>%
  filter(!is.na(LE_2021)) %>%
  group_by(Sex) %>%
  arrange(desc(LE_2021)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(
    Sex,
    MSOA21CD,
    MSOA21NM,
    RGN22CD,
    RGN22NM,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI
  )


# ------------------------------------------------------------
# LE: BOTTOM 10 BY SEX
# ------------------------------------------------------------

bottom10_LE <- health_2021 %>%
  filter(!is.na(LE_2021)) %>%
  group_by(Sex) %>%
  arrange(LE_2021) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(
    Sex,
    MSOA21CD,
    MSOA21NM,
    RGN22CD,
    RGN22NM,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI
  )


# ------------------------------------------------------------
# HLE: TOP 10 BY SEX
# ------------------------------------------------------------

top10_HLE <- health_2021 %>%
  filter(!is.na(HLE_2021)) %>%
  group_by(Sex) %>%
  arrange(desc(HLE_2021)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(
    Sex,
    MSOA21CD,
    MSOA21NM,
    RGN22CD,
    RGN22NM,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI
  )


# ------------------------------------------------------------
# HLE: BOTTOM 10 BY SEX
# ------------------------------------------------------------

bottom10_HLE <- health_2021 %>%
  filter(!is.na(HLE_2021)) %>%
  group_by(Sex) %>%
  arrange(HLE_2021) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(
    Sex,
    MSOA21CD,
    MSOA21NM,
    RGN22CD,
    RGN22NM,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI
  )


# ============================================================
# 9. DIFFERENCE BETWEEN HIGHEST AND LOWEST
# ============================================================

LE_high_low <- health_2021 %>%
  filter(!is.na(LE_2021)) %>%
  group_by(Sex) %>%
  summarise(
    Highest_LE = max(LE_2021),
    Lowest_LE = min(LE_2021),
    Difference_highest_lowest_LE = Highest_LE - Lowest_LE,
    .groups = "drop"
  )


HLE_high_low <- health_2021 %>%
  filter(!is.na(HLE_2021)) %>%
  group_by(Sex) %>%
  summarise(
    Highest_HLE = max(HLE_2021),
    Lowest_HLE = min(HLE_2021),
    Difference_highest_lowest_HLE = Highest_HLE - Lowest_HLE,
    .groups = "drop"
  )


##

# ============================================================
# 9. DIFFERENCE BETWEEN HIGHEST AND LOWEST
#    BY SEX
# ============================================================

LE_high_low <- health_2021 %>%
  filter(!is.na(LE_2021)) %>%
  group_by(Sex) %>%
  summarise(
    Highest_LE = max(LE_2021, na.rm = TRUE),
    Lowest_LE = min(LE_2021, na.rm = TRUE),
    Difference_highest_lowest_LE =
      Highest_LE - Lowest_LE,
    .groups = "drop"
  )


HLE_high_low <- health_2021 %>%
  filter(!is.na(HLE_2021)) %>%
  group_by(Sex) %>%
  summarise(
    Highest_HLE = max(HLE_2021, na.rm = TRUE),
    Lowest_HLE = min(HLE_2021, na.rm = TRUE),
    Difference_highest_lowest_HLE =
      Highest_HLE - Lowest_HLE,
    .groups = "drop"
  )


# ============================================================
# 10. DIFFERENCE BETWEEN AVERAGE TOP 10 AND BOTTOM 10
#     BY SEX
# ============================================================


# ------------------------------------------------------------
# LE
# ------------------------------------------------------------

LE_top_bottom_difference <- health_2021 %>%
  filter(!is.na(LE_2021)) %>%
  group_by(Sex) %>%
  arrange(desc(LE_2021)) %>%
  mutate(
    rank = row_number()
  ) %>%
  summarise(
    
    Average_top10_LE =
      mean(
        LE_2021[rank <= 10],
        na.rm = TRUE
      ),
    
    Average_bottom10_LE =
      mean(
        LE_2021[rank > n() - 10],
        na.rm = TRUE
      ),
    
    Difference_top10_bottom10_LE =
      Average_top10_LE -
      Average_bottom10_LE,
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# HLE
# ------------------------------------------------------------

HLE_top_bottom_difference <- health_2021 %>%
  filter(!is.na(HLE_2021)) %>%
  group_by(Sex) %>%
  arrange(desc(HLE_2021)) %>%
  mutate(
    rank = row_number()
  ) %>%
  summarise(
    
    Average_top10_HLE =
      mean(
        HLE_2021[rank <= 10],
        na.rm = TRUE
      ),
    
    Average_bottom10_HLE =
      mean(
        HLE_2021[rank > n() - 10],
        na.rm = TRUE
      ),
    
    Difference_top10_bottom10_HLE =
      Average_top10_HLE -
      Average_bottom10_HLE,
    
    .groups = "drop"
  )


# ============================================================
# 11. COMBINED SUMMARY BY SEX
# ============================================================

summary_LE <- LE_high_low %>%
  left_join(
    LE_top_bottom_difference,
    by = "Sex"
  )


summary_HLE <- HLE_high_low %>%
  left_join(
    HLE_top_bottom_difference,
    by = "Sex"
  )


# ============================================================
# 12. ROUND ONLY FINAL DISPLAYED RESULTS
# ============================================================

summary_LE_display <- summary_LE %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )


summary_HLE_display <- summary_HLE %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )


# ============================================================
# 13. VIEW RESULTS
# ============================================================

summary_LE_display

summary_HLE_display



# ============================================================
# 9. OVERALL DIFFERENCE BETWEEN TOP 10 AND BOTTOM 10
#    Averaging across male + female
# ============================================================

# Create one row per MSOA
# and calculate the average HLE / LE across both sexes

health_msoa_overall <- health_2021 %>%
  group_by(
    MSOA21CD,
    MSOA21NM,
    RGN22CD,
    RGN22NM
  ) %>%
  summarise(
    
    Average_LE = mean(
      LE_2021,
      na.rm = TRUE
    ),
    
    Average_HLE = mean(
      HLE_2021,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# LE: TOP 10 AND BOTTOM 10
# ------------------------------------------------------------

LE_top10_overall <- health_msoa_overall %>%
  filter(!is.na(Average_LE)) %>%
  arrange(desc(Average_LE)) %>%
  slice_head(n = 10)


LE_bottom10_overall <- health_msoa_overall %>%
  filter(!is.na(Average_LE)) %>%
  arrange(Average_LE) %>%
  slice_head(n = 10)


# ------------------------------------------------------------
# HLE: TOP 10 AND BOTTOM 10
# ------------------------------------------------------------

HLE_top10_overall <- health_msoa_overall %>%
  filter(!is.na(Average_HLE)) %>%
  arrange(desc(Average_HLE)) %>%
  slice_head(n = 10)


HLE_bottom10_overall <- health_msoa_overall %>%
  filter(!is.na(Average_HLE)) %>%
  arrange(Average_HLE) %>%
  slice_head(n = 10)


# ------------------------------------------------------------
# OVERALL LE GAP
# ------------------------------------------------------------

Average_top10_LE_overall <- mean(
  LE_top10_overall$Average_LE,
  na.rm = TRUE
)

Average_bottom10_LE_overall <- mean(
  LE_bottom10_overall$Average_LE,
  na.rm = TRUE
)

Difference_top10_bottom10_LE_overall <-
  Average_top10_LE_overall -
  Average_bottom10_LE_overall


# ------------------------------------------------------------
# OVERALL HLE GAP
# ------------------------------------------------------------

Average_top10_HLE_overall <- mean(
  HLE_top10_overall$Average_HLE,
  na.rm = TRUE
)

Average_bottom10_HLE_overall <- mean(
  HLE_bottom10_overall$Average_HLE,
  na.rm = TRUE
)

Difference_top10_bottom10_HLE_overall <-
  Average_top10_HLE_overall -
  Average_bottom10_HLE_overall


# ============================================================
# 9A. OVERALL SUMMARY
# ============================================================

overall_summary <- tibble(
  
  Average_top10_LE =
    Average_top10_LE_overall,
  
  Average_bottom10_LE =
    Average_bottom10_LE_overall,
  
  Difference_top10_bottom10_LE =
    Difference_top10_bottom10_LE_overall,
  
  Average_top10_HLE =
    Average_top10_HLE_overall,
  
  Average_bottom10_HLE =
    Average_bottom10_HLE_overall,
  
  Difference_top10_bottom10_HLE =
    Difference_top10_bottom10_HLE_overall
)


# View overall results

overall_summary



































# ============================================================
# 8. LOAD IMD DATA
# ============================================================

IMD_2025 <- read_csv(
  "Working files/imd_2025_final_msoa.csv"
)


# ============================================================
# 9. JOIN IMD + HEALTH
# ============================================================

marmot_data <- IMD_2025 %>%
  left_join(
    health_2021,
    by = "MSOA21CD"
  )
```









































































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
 #   Country == "England",
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
#    Country == "England",
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




library(dplyr)
library(readr)

# ============================================================
# 1. CREATE HEALTH_2021
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
# 2. CHECK HEALTH DATA
# ============================================================

health_2021 %>%
  count(Sex)

health_2021 %>%
  count(MSOA21CD) %>%
  count(n)


# ============================================================
# 5. LOAD IMD AND REGION DATA
# ============================================================

IMD_2025 <- read_csv(
  "Working files/imd_2025_final_msoa.csv"
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


# ============================================================
# 6. JOIN IMD + HEALTH + REGION
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






























#  CHECK REGIONAL COVERAGE


marmot_data %>%
  count(
    RGN22CD,
    RGN22NM,
    sort = TRUE
  )


#remove mising values

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


# ============================================================
# 9. CREATE ONE ROW PER MSOA FOR IMD RANKING
# ============================================================

msoa_deprivation <- marmot_data %>%
  select(
    MSOA21CD,
    income_average_score
  ) %>%
  distinct(
    MSOA21CD,
    .keep_all = TRUE
  ) %>%
  arrange(
    desc(income_average_score)
  ) %>%
  mutate(
    deprivation_percentile =
      1 + 99 * (row_number() - 1) / (n() - 1)
  )


# ============================================================
# 10. ADD PERCENTILE BACK TO MAIN DATA
# ============================================================

marmot_data <- marmot_data %>%
  select(
    -any_of("deprivation_percentile")
  ) %>%
  left_join(
    msoa_deprivation %>%
      select(
        MSOA21CD,
        deprivation_percentile
      ),
    by = "MSOA21CD"
  )


# ============================================================
# 11. CHECK FINAL DATA
# ============================================================

marmot_data %>%
  summarise(
    n_rows = n(),
    n_msoa = n_distinct(MSOA21CD),
    missing_percentile =
      sum(is.na(deprivation_percentile)),
    min_percentile =
      min(deprivation_percentile, na.rm = TRUE),
    max_percentile =
      max(deprivation_percentile, na.rm = TRUE)
  )







# ============================================================
# TOP 10% AND BOTTOM 10% OF MSOAs BY HLE
# ============================================================

# First make ONE row per MSOA
# with male, female and both-sex HLE

hle_msoa <- health_2021 %>%
  group_by(
    MSOA21CD,
    MSOA21NM
  ) %>%
  summarise(
    
    male_HLE = mean(
      HLE_2021[Sex == "Male"],
      na.rm = TRUE
    ),
    
    female_HLE = mean(
      HLE_2021[Sex == "Female"],
      na.rm = TRUE
    ),
    
    # Average of male and female HLE
    both_HLE = mean(
      HLE_2021,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ============================================================
# ADD REGION + DEPRIVATION INFORMATION
# ============================================================

hle_msoa <- hle_msoa %>%
  
  left_join(
    MSOA_Region_2021,
    by = "MSOA21CD"
  ) %>%
  
  left_join(
    msoa_deprivation %>%
      select(
        MSOA21CD,
        income_average_score,
        deprivation_percentile
      ),
    by = "MSOA21CD"
  )


# ============================================================
# CHECK
# ============================================================

hle_msoa %>%
  summarise(
    n_MSOAs = n(),
    missing_region =
      sum(is.na(RGN22NM)),
    missing_deprivation =
      sum(is.na(deprivation_percentile))
  )


# ============================================================
# DEFINE TOP AND BOTTOM 10%
# ============================================================

n_msoa <- nrow(hle_msoa)

top_n <- ceiling(
  n_msoa * 0.10
)

bottom_n <- ceiling(
  n_msoa * 0.10
)


# ============================================================
# TOP 10%
# ============================================================

top10pct <- hle_msoa %>%
  arrange(
    desc(both_HLE)
  ) %>%
  slice_head(
    n = top_n
  ) %>%
  mutate(
    group = "Top 10%"
  )


# ============================================================
# BOTTOM 10%
# ============================================================

bottom10pct <- hle_msoa %>%
  arrange(
    both_HLE
  ) %>%
  slice_head(
    n = bottom_n
  ) %>%
  mutate(
    group = "Bottom 10%"
  )


# ============================================================
# COMBINE TOP + BOTTOM
# ============================================================

top_bottom_10pct <- bind_rows(
  top10pct,
  bottom10pct
)


# ============================================================
# LOOK AT TOP 10%
# ============================================================

top10pct %>%
  arrange(
    desc(both_HLE)
  ) %>%
  select(
    MSOA21CD,
    MSOA21NM,
    both_HLE,
    male_HLE,
    female_HLE,
    RGN22NM,
    income_average_score,
    deprivation_percentile
  )


# ============================================================
# LOOK AT BOTTOM 10%
# ============================================================

bottom10pct %>%
  arrange(
    both_HLE
  ) %>%
  select(
    MSOA21CD,
    MSOA21NM,
    both_HLE,
    male_HLE,
    female_HLE,
    RGN22NM,
    income_average_score,
    deprivation_percentile
  )


# ============================================================
# REGION COUNTS
#
# Number of MSOAs from each region in TOP and BOTTOM 10%
# ============================================================

region_counts <- top_bottom_10pct %>%
  count(
    group,
    RGN22NM,
    name = "n_MSOAs"
  ) %>%
  arrange(
    group,
    desc(n_MSOAs)
  )

region_counts


# ============================================================
# REGION PERCENTAGES
#
# Percentage of TOP 10% and BOTTOM 10%
# coming from each region
# ============================================================

region_percentages <- top_bottom_10pct %>%
  count(
    group,
    RGN22NM,
    name = "n_MSOAs"
  ) %>%
  group_by(
    group
  ) %>%
  mutate(
    percentage =
      100 * n_MSOAs / sum(n_MSOAs)
  ) %>%
  ungroup() %>%
  arrange(
    group,
    desc(percentage)
  )

region_percentages



region_summary <- top_bottom_10pct %>%
  group_by(
    group,
    RGN22NM
  ) %>%
  summarise(
    
    n_MSOAs = n(),
    
    percentage =
      100 * n_MSOAs /
      nrow(
        filter(
          top_bottom_10pct,
          group == first(group)
        )
      ),
    
    mean_HLE =
      mean(
        both_HLE,
        na.rm = TRUE
      ),
    
    mean_deprivation_percentile =
      mean(
        deprivation_percentile,
        na.rm = TRUE
      ),
    
    median_deprivation_percentile =
      median(
        deprivation_percentile,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  arrange(
    group,
    desc(percentage)
  )

region_summary





# ============================================================
# GAP BETWEEN TOP 10 AND BOTTOM 10 MSOAs
# Average male + female HLE
# ============================================================

# Create one row per MSOA
# and calculate average male/female HLE

hle_msoa <- health_2021 %>%
  group_by(
    MSOA21CD,
    MSOA21NM
  ) %>%
  summarise(
    
    male_HLE = mean(
      HLE_2021[Sex == "Male"],
      na.rm = TRUE
    ),
    
    female_HLE = mean(
      HLE_2021[Sex == "Female"],
      na.rm = TRUE
    ),
    
    # Simple average of male and female HLE
    both_HLE = mean(
      HLE_2021,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ============================================================
# TOP 10 MSOAs
# ============================================================

top10_MSOAs <- hle_msoa %>%
  arrange(
    desc(both_HLE)
  ) %>%
  slice_head(
    n = 10
  )


# ============================================================
# BOTTOM 10 MSOAs
# ============================================================

bottom10_MSOAs <- hle_msoa %>%
  arrange(
    both_HLE
  ) %>%
  slice_head(
    n = 10
  )


# ============================================================
# MEAN HLE FOR TOP AND BOTTOM 10
# ============================================================

top10_mean_HLE <- mean(
  top10_MSOAs$both_HLE,
  na.rm = TRUE
)

bottom10_mean_HLE <- mean(
  bottom10_MSOAs$both_HLE,
  na.rm = TRUE
)


# ============================================================
# CALCULATE GAP
# ============================================================

HLE_gap <- top10_mean_HLE -
  bottom10_mean_HLE


# ============================================================
# RESULTS
# ============================================================

top_bottom_10_results <- tibble(
  
  top_10_mean_HLE =
    top10_mean_HLE,
  
  bottom_10_mean_HLE =
    bottom10_mean_HLE,
  
  gap_years =
    HLE_gap,
  
  claimed_gap =
    27.4,
  
  difference_from_claim =
    HLE_gap - 27.4
  
)

top_bottom_10_results

