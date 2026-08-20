# ============================================================
# MSOA LIFE EXPECTANCY / HEALTHY LIFE EXPECTANCY
# + FINGERTIPS HEALTH DRIVERS
# ============================================================

rm(list = ls())


# ============================================================
# 1. PACKAGES
# ============================================================

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fingertipsR)
library(purrr)


# ============================================================
# 2. WORKING DIRECTORY
# ============================================================

setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA")


# ============================================================
# 3. LOAD LIFE EXPECTANCY DATA
# ============================================================

lemsoa <- read_excel(
  "Raw data/hslemsoa.xlsx",
  sheet = "1",
  skip = 6
)


# ============================================================
# 4. LOAD HEALTHY LIFE EXPECTANCY DATA
# ============================================================

hslemsoa <- read_excel(
  "Raw data/hslemsoa.xlsx",
  sheet = "2",
  skip = 6
)


# ============================================================
# 5. LOAD MSOA -> REGION LOOKUP
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
# 6. PREPARE LIFE EXPECTANCY
# ============================================================

le_2021 <- lemsoa %>%
  filter(
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
    LE_2021_LCI = LCI,
    LE_2021_UCI = UCI
  )


# ============================================================
# 7. PREPARE HEALTHY LIFE EXPECTANCY
# ============================================================

hle_2021 <- hslemsoa %>%
  filter(
    `Area type` == "MSOA",
    Sex %in% c("Male", "Female")
  ) %>%
  select(
    `Area code`,
    `Area name`,
    Sex,
    HLE,
    LCI,
    UCI
  ) %>%
  rename(
    MSOA21CD = `Area code`,
    MSOA21NM = `Area name`,
    HLE_2021 = HLE,
    HLE_2021_LCI = LCI,
    HLE_2021_UCI = UCI
  )


# ============================================================
# 8. PUT MALE + FEMALE LE/HLE ON ONE ROW PER MSOA
# ============================================================

health_outcomes <- le_2021 %>%
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
    by = c(
      "MSOA21CD",
      "Sex"
    )
  ) %>%
  pivot_wider(
    names_from = Sex,
    values_from = c(
      LE_2021,
      HLE_2021
    ),
    names_glue = "{.value}_{Sex}"
  )


# ============================================================
# 9. ADD REGION
# ============================================================

health_outcomes <- health_outcomes %>%
  left_join(
    MSOA_Region_2021,
    by = "MSOA21CD"
  )


# ============================================================
# 10. CHECK OUTCOMES
# ============================================================

glimpse(health_outcomes)

summary(health_outcomes$LE_2021_Male)
summary(health_outcomes$LE_2021_Female)

summary(health_outcomes$HLE_2021_Male)
summary(health_outcomes$HLE_2021_Female)



# ============================================================
# 11. DRIVER INDICATORS
# ============================================================

driver_ids <- c(
  
  # Population / demographics
  93084,
  93081,
  93226,
  93747,
  
  # Poverty / deprivation
  93275,
  93268,
  93094,
  93279,
  93280,
  
  # Employment
  93097,
  93098,
  
  # Childhood obesity
  93105,
  93106,
  93107,
  93108,
  
  # Birth
  93089,
  93092,
  
  # Children / young people admissions
  93115,
  93114,
  93219,
  93224,
  
  # Hospital admissions
  93227,
  93229,
  93231,
  93232,
  93233,
  
  # Cancer
  93234,
  93239,
  93235,
  93241,
  93236,
  
  # Alcohol
  93465,
  93240,
  
  # Other cancer
  93237,
  93238,
  
  # Life expectancy
  93283,
  
  # Mortality
  93250,
  93252,
  93253,
  93254,
  93255,
  93256,
  93257,
  93259,
  93260,
  93480
)


# ============================================================
# 12. DOWNLOAD FINGERTIPS MSOA DATA
# ============================================================

download_driver <- function(id) {
  
  message("Downloading: ", id)
  
  fingertips_data(
    IndicatorID = id,
    AreaTypeID = 3
  ) %>%
    filter(
      AreaType == "MSOA"
    )
}


driver_raw <- map_dfr(
  driver_ids,
  download_driver
)




# ============================================================
# 13. CLEAN DRIVER DATA
# ============================================================

driver_data <- driver_raw %>%
  select(
    IndicatorID,
    IndicatorName,
    AreaCode,
    AreaName,
    Sex,
    Age,
    Category,
    Timeperiod,
    TimeperiodSortable,
    Value
  ) %>%
  rename(
    MSOA21CD = AreaCode,
    MSOA21NM = AreaName,
    DriverValue = Value
  )

# ============================================================
# 14. GET LATEST DRIVER PERIOD
# ============================================================

driver_latest <- driver_data %>%
  group_by(
    IndicatorID,
    MSOA21CD
  ) %>%
  filter(
    TimeperiodSortable ==
      max(TimeperiodSortable, na.rm = TRUE)
  ) %>%
  ungroup()



# ============================================================
# 15. CHECK DRIVER DATA
# ============================================================

driver_latest %>%
  count(
    IndicatorID,
    IndicatorName
  )


driver_latest %>%
  filter(
    IndicatorID == 93105
  ) %>%
  head(20)


# ============================================================
# 16. JOIN DRIVERS TO LE / HLE
# ============================================================

analysis_data <- driver_latest %>%
  select(
    IndicatorID,
    IndicatorName,
    MSOA21CD,
    MSOA21NM,
    DriverValue,
    Timeperiod
  ) %>%
  left_join(
    health_outcomes,
    by = "MSOA21CD"
  )

# ============================================================
# 17. EXAMPLE: OBESITY VS LE/HLE
# ============================================================

obesity <- analysis_data %>%
  filter(
    IndicatorID == 93105
  )


ggplot(
  obesity,
  aes(
    x = DriverValue,
    y = LE_2021_Male
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Childhood obesity vs Male Life Expectancy",
    x = "Childhood obesity (%)",
    y = "Male life expectancy"
  ) +
  theme_minimal()


















##Full code

# ============================================================
# MSOA LIFE EXPECTANCY / HEALTHY LIFE EXPECTANCY
# + FINGERTIPS HEALTH DRIVERS
# ============================================================

rm(list = ls())

# ============================================================
# 1. PACKAGES
# ============================================================

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fingertipsR)
library(purrr)
library(stringr)

# ============================================================
# 2. WORKING DIRECTORY
# ============================================================

setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA")


# ============================================================
# 3. LOAD LE DATA
# ============================================================

lemsoa <- read_excel(
  "Raw data/hslemsoa.xlsx",
  sheet = "1",
  skip = 6
)


# ============================================================
# 4. LOAD HLE DATA
# ============================================================

hslemsoa <- read_excel(
  "Raw data/hslemsoa.xlsx",
  sheet = "2",
  skip = 6
)


# ============================================================
# 5. LOAD MSOA -> REGION LOOKUP
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
# 6. PREPARE LIFE EXPECTANCY
# ============================================================

le_2021 <- lemsoa %>%
  filter(
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
# 7. PREPARE HEALTHY LIFE EXPECTANCY
# ============================================================

hle_2021 <- hslemsoa %>%
  filter(
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
    UCI
  ) %>%
  rename(
    MSOA21CD = `Area code`,
    MSOA21NM = `Area name`,
    HLE_2021 = HLE,
    HLE_2021_LCI = LCI,
    HLE_2021_UCI = UCI
  )


# ============================================================
# 8. JOIN LE + HLE + REGION
# ============================================================

health_outcomes <- le_2021 %>%
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
# 9. DRIVER INDICATORS
# ============================================================

driver_ids <- c(
  
  # Population / demographics
  93084,
  93081,
  93226,
  93747,
  
  # Poverty / deprivation
  93275,
  93268,
  93094,
  93279,
  93280,
  
  # Employment
  93097,
  93098,
  
  # Childhood obesity
  93105,
  93106,
  93107,
  93108,
  
  # Birth
  93089,
  93092,
  
  # Children / young people
  93115,
  93114,
  93219,
  93224,
  
  # Hospital admissions
  93227,
  93229,
  93231,
  93232,
  93233,
  
  # Cancer
  93234,
  93239,
  93235,
  93241,
  93236,
  
  # Alcohol
  93465,
  93240,
  
  # Other cancer
  93237,
  93238,
  
  # Life expectancy
  93283,
  
  # Mortality
  93250,
  93252,
  93253,
  93254,
  93255,
  93256,
  93257,
  93259,
  93260,
  93480
)


# ============================================================
# 10. DOWNLOAD ALL DRIVER DATA
# ============================================================

download_driver <- function(indicator_id) {
  
  message("Downloading indicator: ", indicator_id)
  
  tryCatch({
    
    fingertips_data(
      IndicatorID = indicator_id,
      AreaTypeID = 3
    ) %>%
      filter(
        AreaType == "MSOA"
      )
    
  }, error = function(e) {
    
    message(
      "ERROR for indicator ",
      indicator_id,
      ": ",
      e$message
    )
    
    NULL
  })
}


driver_raw <- map_dfr(
  driver_ids,
  download_driver
)


# ============================================================
# 11. KEEP ONLY USEFUL DRIVER VARIABLES
# ============================================================

driver_data <- driver_raw %>%
  select(
    IndicatorID,
    IndicatorName,
    AreaCode,
    AreaName,
    AreaType,
    Sex,
    Age,
    Category,
    Timeperiod,
    TimeperiodSortable,
    Value,
    LowerCI95.0limit,
    UpperCI95.0limit,
    Count,
    Denominator
  ) %>%
  rename(
    MSOA21CD = AreaCode,
    MSOA21NM = AreaName,
    DriverValue = Value
  )


# ============================================================
# 12. ADD REGION
# ============================================================

driver_data <- driver_data %>%
  left_join(
    MSOA_Region_2021,
    by = "MSOA21CD"
  )


# ============================================================
# 13. GET LATEST PERIOD FOR EACH DRIVER / MSOA
#
# Important:
# Drivers often have different time periods.
# We therefore take the latest available period for
# each indicator and MSOA.
# ============================================================

latest_driver_period <- driver_data %>%
  
  filter(
    !is.na(DriverValue)
  ) %>%
  
  group_by(
    IndicatorID,
    MSOA21CD
  ) %>%
  
  filter(
    TimeperiodSortable ==
      max(
        TimeperiodSortable,
        na.rm = TRUE
      )
  ) %>%
  
  ungroup()


# ============================================================
# 14. REMOVE DRIVER SEX WHERE APPROPRIATE
#
# We want one driver value per MSOA.
#
# For indicators that have Male/Female rows, we keep the
# first non-missing value. For sex-specific outcome measures
# we use the LE/HLE sex instead.
# ============================================================

driver_data_msoa <- latest_driver_period %>%
  
  group_by(
    IndicatorID,
    MSOA21CD
  ) %>%
  
  summarise(
    IndicatorName = first(IndicatorName),
    DriverValue = first(
      DriverValue[!is.na(DriverValue)]
    ),
    DriverSex = paste(
      unique(
        na.omit(Sex)
      ),
      collapse = ", "
    ),
    Timeperiod = first(Timeperiod),
    .groups = "drop"
  )


# ============================================================
# 15. CREATE SEX-SPECIFIC OUTCOME DATA
# ============================================================

outcomes <- health_outcomes %>%
  
  select(
    MSOA21CD,
    MSOA21NM,
    Sex,
    RGN22CD,
    RGN22NM,
    LE_2021,
    HLE_2021
  )


# ============================================================
# 16. JOIN DRIVERS TO OUTCOMES
#
# This deliberately creates the same driver value against
# Male and Female outcomes.
#
# Example:
#
# Obesity in MSOA X
#       |
#       +---- Male LE
#       +---- Female LE
#       +---- Male HLE
#       +---- Female HLE
#
# This is what you want for sex-specific outcome plots.
# ============================================================

analysis_data <- driver_data_msoa %>%
  
  inner_join(
    outcomes,
    by = "MSOA21CD"
  )


# ============================================================
# 17. CHECK DATA
# ============================================================

glimpse(analysis_data)

analysis_data %>%
  summarise(
    rows = n(),
    indicators = n_distinct(IndicatorID),
    msoas = n_distinct(MSOA21CD)
  )

table(
  analysis_data$Sex
)


# ============================================================
# 18. FUNCTION TO CREATE THE FOUR PLOTS
# ============================================================

make_driver_plots <- function(
    data,
    indicator_id
) {
  
  d <- data %>%
    
    filter(
      IndicatorID == indicator_id
    )
  
  
  # Get indicator name
  
  indicator_name <- d %>%
    pull(IndicatorName) %>%
    first()
  
  
  # ----------------------------------------------------------
  # MALE LE
  # ----------------------------------------------------------
  
  p_male_le <- d %>%
    
    filter(
      Sex == "Male",
      !is.na(DriverValue),
      !is.na(LE_2021)
    ) %>%
    
    ggplot(
      aes(
        x = DriverValue,
        y = LE_2021
      )
    ) +
    
    geom_point(
      alpha = 0.35
    ) +
    
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    
    labs(
      title = paste(
        indicator_name,
        "vs Male Life Expectancy"
      ),
      x = indicator_name,
      y = "Male life expectancy"
    ) +
    
    theme_minimal()
  
  
  # ----------------------------------------------------------
  # FEMALE LE
  # ----------------------------------------------------------
  
  p_female_le <- d %>%
    
    filter(
      Sex == "Female",
      !is.na(DriverValue),
      !is.na(LE_2021)
    ) %>%
    
    ggplot(
      aes(
        x = DriverValue,
        y = LE_2021
      )
    ) +
    
    geom_point(
      alpha = 0.35
    ) +
    
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    
    labs(
      title = paste(
        indicator_name,
        "vs Female Life Expectancy"
      ),
      x = indicator_name,
      y = "Female life expectancy"
    ) +
    
    theme_minimal()
  
  
  # ----------------------------------------------------------
  # MALE HLE
  # ----------------------------------------------------------
  
  p_male_hle <- d %>%
    
    filter(
      Sex == "Male",
      !is.na(DriverValue),
      !is.na(HLE_2021)
    ) %>%
    
    ggplot(
      aes(
        x = DriverValue,
        y = HLE_2021
      )
    ) +
    
    geom_point(
      alpha = 0.35
    ) +
    
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    
    labs(
      title = paste(
        indicator_name,
        "vs Male Healthy Life Expectancy"
      ),
      x = indicator_name,
      y = "Male healthy life expectancy"
    ) +
    
    theme_minimal()
  
  
  # ----------------------------------------------------------
  # FEMALE HLE
  # ----------------------------------------------------------
  
  p_female_hle <- d %>%
    
    filter(
      Sex == "Female",
      !is.na(DriverValue),
      !is.na(HLE_2021)
    ) %>%
    
    ggplot(
      aes(
        x = DriverValue,
        y = HLE_2021
      )
    ) +
    
    geom_point(
      alpha = 0.35
    ) +
    
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    
    labs(
      title = paste(
        indicator_name,
        "vs Female Healthy Life Expectancy"
      ),
      x = indicator_name,
      y = "Female healthy life expectancy"
    ) +
    
    theme_minimal()
  
  
  # ----------------------------------------------------------
  # RETURN ALL FOUR PLOTS
  # ----------------------------------------------------------
  
  list(
    male_LE = p_male_le,
    female_LE = p_female_le,
    male_HLE = p_male_hle,
    female_HLE = p_female_hle
  )
}


# ============================================================
# 19. CREATE PLOTS FOR EVERY DRIVER
#
# Nothing is saved to disk.
# Everything is kept in the plots list.
# ============================================================

plots <- setNames(
  
  map(
    driver_ids,
    ~ make_driver_plots(
      analysis_data,
      .x
    )
  ),
  
  driver_ids
)


# ============================================================

#plot
# IMD vs Male LE

plots[["93275"]]$male_LE


# IMD vs Female LE

plots[["93275"]]$female_LE


# IMD vs Male HLE

plots[["93275"]]$male_HLE


# IMD vs Female HLE

plots[["93275"]]$female_HLE


# Obesity vs Male HLE

plots[["93227"]]$male_HLE


# Obesity vs Female HLE

plots[["93280"]]$female_HLE


# ============================================================
#See whats available

driver_dictionary <- driver_data_msoa %>%
  
  select(
    IndicatorID,
    IndicatorName
  ) %>%
  
  distinct() %>%
  
  arrange(
    IndicatorID
  )

print(
  driver_dictionary,
  n = 100
)


