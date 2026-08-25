#Date 28/07/2026

#Code to create data frame with HLE by MSOA and IMD to compare 2011 ->2021


rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(stringr)

#setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA")
setwd("C:/Users/rhianna.sookhy/OneDrive - The Health Foundation/Shortcuts/Analysis - 11-CAT/1. Work programme/Healthy Life Expectancy - strategy launch/Phase 2")

#load in data

msoa_lookup <- read_csv(
  "Raw data/MSOA_(2011)_to_MSOA_(2021).csv"
)

hle_males <- read_excel(
  "Raw data/hlereferencetable2_tcm77-417533 (2).xls",
  sheet = "MSOA_males",
  skip = 7
)

hle_females <- read_excel(
  "Raw data/hlereferencetable2_tcm77-417533 (2).xls",
  sheet = "MSOA_females",
  skip = 7
)

mid2011_msoa_males <- read_excel(
  "Raw data/mid2011msoaquinaryageestimates (2).xls",
  sheet = "Mid-2011 Males",
  skip = 3
)

mid2011_msoa_females <- read_excel(
  "Raw data/mid2011msoaquinaryageestimates (2).xls",
  sheet = "Mid-2011 Females",
  skip = 3
)




#Remove rows with X Complex boundaries 

msoa_lookup <- msoa_lookup %>%
  select(
    MSOA11CD,
    MSOA11NM,
    MSOA21CD,
    MSOA21NM,
    CHNGIND
  ) %>%
  filter(CHNGIND != "X")

#Prep 2011 HLE

hle_males_2011 <- hle_males %>%
  rename(
    MSOA11CD = `MSOA Codes`,
    HLE_2011_M = `HLE (years)`,
    HLE_2011_M_LCL = `Lower 95 % confidence interval...6`,
    HLE_2011_M_UCL = `Upper 95 % confidence interval...7`,
    LE_2011_M = `LE (Years)`,
    LE_2011_M_LCL = `Lower 95 % confidence interval...9`,
    LE_2011_M_UCL = `Upper 95 % confidence interval...10`
  ) %>%
  select(
    Region,
    MSOA11CD,
    starts_with("HLE"),
    starts_with("LE")
  )

#Just values - uncomment for cls

# hle_males_2011 <- hle_males %>%
#   rename(
#     MSOA11CD = `MSOA Codes`,
#     HLE_2011_M = `HLE (years)`,
#     LE_2011_M = `LE (Years)`
#   ) %>%
#   select(
#     Region,
#     MSOA11CD,
#     HLE_2011_M,
#     LE_2011_M
#   )

hle_females_2011 <- hle_females %>%
  rename(
    MSOA11CD = `MSOA Codes`,
    HLE_2011_F = `HLE (years)`,
    HLE_2011_F_LCL = `Lower 95 % confidence interval...6`,
    HLE_2011_F_UCL = `Upper 95 % confidence interval...7`,
    LE_2011_F = `LE (Years)`,
    LE_2011_F_LCL = `Lower 95 % confidence interval...9`,
    LE_2011_F_UCL = `Upper 95 % confidence interval...10`
  ) %>%
  select(
    MSOA11CD,
    starts_with("HLE"),
    starts_with("LE")
  )

# hle_females_2011 <- hle_females %>%
#   rename(
#     MSOA11CD = `MSOA Codes`,
#     HLE_2011_F = `HLE (years)`,
#     LE_2011_F = `LE (Years)`
#   ) %>%
#   select(
#     MSOA11CD,
#     HLE_2011_F,
#     LE_2011_F
#   )



# Prepare population datasets

pop_male <- mid2011_msoa_males %>%
  filter(str_detect(`Area Codes`, "^E020")) %>%
  transmute(
    MSOA11CD = `Area Codes`,
    Population2011_M = `All Ages`
  )

pop_female <- mid2011_msoa_females %>%
  filter(str_detect(`Area Codes`, "^E020")) %>%
  transmute(
    MSOA11CD = `Area Codes`,
    Population2011_F = `All Ages`
  )


#Don't need to do anything to split or unchanged
msoa_lookup_2011 <- msoa_lookup %>%
  filter(CHNGIND %in% c("U", "S")) %>%
  left_join(hle_males_2011, by = "MSOA11CD") %>%
  left_join(hle_females_2011, by = "MSOA11CD")

#This line removes all Welsh MSOAs
msoa_lookup_2011_removed <- msoa_lookup_2011 %>%
  filter(is.na(HLE_2011_M) & is.na(HLE_2011_F))

msoa_lookup_2011 <- msoa_lookup_2011 %>%
  filter(!(is.na(HLE_2011_M) & is.na(HLE_2011_F)))




#Need to weight merged HLEs

msoa_lookup_M <- msoa_lookup %>%
  filter(CHNGIND == "M") %>%
  left_join(pop_male, by = "MSOA11CD") %>%
  left_join(pop_female, by = "MSOA11CD") %>%
  left_join(hle_males_2011, by = "MSOA11CD") %>%
  left_join(hle_females_2011, by = "MSOA11CD")

msoa_lookup_M_removed <- msoa_lookup_M %>%
  filter(is.na(HLE_2011_M) & is.na(HLE_2011_F))

msoa_lookup_M <- msoa_lookup_M %>%
  filter(!(is.na(HLE_2011_M) & is.na(HLE_2011_F)))

# msoa_lookup_M_weighted <- msoa_lookup_M %>%
#   group_by(
#     MSOA21CD,
#     MSOA21NM,
#     CHNGIND
#   ) %>%
#   summarise(
#     
#     Region = first(Region),
#     
#     MSOA11CD = paste(unique(MSOA11CD), collapse = "; "),
#     MSOA11NM = paste(unique(MSOA11NM), collapse = "; "),
#     
#     HLE_2011_M = weighted.mean(HLE_2011_M, Population2011_M, na.rm = TRUE),
#     LE_2011_M  = weighted.mean(LE_2011_M, Population2011_M, na.rm = TRUE),
#     
#     HLE_2011_F = weighted.mean(HLE_2011_F, Population2011_F, na.rm = TRUE),
#     LE_2011_F  = weighted.mean(LE_2011_F, Population2011_F, na.rm = TRUE),
#     
#     .groups = "drop"
#     
#   )


msoa_lookup_M_weighted <- msoa_lookup_M %>%
  group_by(
    MSOA21CD,
    MSOA21NM,
    CHNGIND
  ) %>%
  summarise(
    
    Region = first(Region),
    
    MSOA11CD = paste(unique(MSOA11CD), collapse = "; "),
    MSOA11NM = paste(unique(MSOA11NM), collapse = "; "),
    
    # Male HLE
    HLE_2011_M = weighted.mean(HLE_2011_M, Population2011_M, na.rm = TRUE),
    HLE_2011_M_LCL = weighted.mean(HLE_2011_M_LCL, Population2011_M, na.rm = TRUE),
    HLE_2011_M_UCL = weighted.mean(HLE_2011_M_UCL, Population2011_M, na.rm = TRUE),
    
    # Male LE
    LE_2011_M = weighted.mean(LE_2011_M, Population2011_M, na.rm = TRUE),
    LE_2011_M_LCL = weighted.mean(LE_2011_M_LCL, Population2011_M, na.rm = TRUE),
    LE_2011_M_UCL = weighted.mean(LE_2011_M_UCL, Population2011_M, na.rm = TRUE),
    
    # Female HLE
    HLE_2011_F = weighted.mean(HLE_2011_F, Population2011_F, na.rm = TRUE),
    HLE_2011_F_LCL = weighted.mean(HLE_2011_F_LCL, Population2011_F, na.rm = TRUE),
    HLE_2011_F_UCL = weighted.mean(HLE_2011_F_UCL, Population2011_F, na.rm = TRUE),
    
    # Female LE
    LE_2011_F = weighted.mean(LE_2011_F, Population2011_F, na.rm = TRUE),
    LE_2011_F_LCL = weighted.mean(LE_2011_F_LCL, Population2011_F, na.rm = TRUE),
    LE_2011_F_UCL = weighted.mean(LE_2011_F_UCL, Population2011_F, na.rm = TRUE),
    
    .groups = "drop"
  )

# #Combine these
# msoa_lookup_final <- bind_rows(
#   msoa_lookup_2011,
#   msoa_lookup_M_weighted
# ) %>%
#   select(
#     Region,
#     MSOA11CD,
#     MSOA11NM,
#     MSOA21CD,
#     MSOA21NM,
#     CHNGIND,
#     HLE_2011_M,
#     LE_2011_M,
#     HLE_2011_F,
#     LE_2011_F
#   )


#Overwrite the region in Southwark as NA from input
# Combine these
# msoa_lookup_final <- bind_rows(
#   msoa_lookup_2011,
#   msoa_lookup_M_weighted
# ) %>%
#   mutate(
#     Region = if_else(
#       MSOA11NM == "Southwark 009",
#       "London",
#       Region
#     )
#   ) %>%
#   select(
#     Region,
#     MSOA11CD,
#     MSOA11NM,
#     MSOA21CD,
#     MSOA21NM,
#     CHNGIND,
#     HLE_2011_M,
#     LE_2011_M,
#     HLE_2011_F,
#     LE_2011_F
#   )


msoa_lookup_final <- bind_rows(
  msoa_lookup_2011,
  msoa_lookup_M_weighted
) %>%
  mutate(
    Region = if_else(
      MSOA11NM == "Southwark 009",
      "London",
      Region
    )
  ) %>%
  select(
    Region,
    MSOA11CD,
    MSOA11NM,
    MSOA21CD,
    MSOA21NM,
    CHNGIND,
    
    HLE_2011_M,
    HLE_2011_M_LCL,
    HLE_2011_M_UCL,
    
    LE_2011_M,
    LE_2011_M_LCL,
    LE_2011_M_UCL,
    
    HLE_2011_F,
    HLE_2011_F_LCL,
    HLE_2011_F_UCL,
    
    LE_2011_F,
    LE_2011_F_LCL,
    LE_2011_F_UCL
  )

##These are the HLE exclusions
msoa_lookup_final_missing <- msoa_lookup_final %>%
  filter(if_any(everything(), is.na))

# #Do before any analysis 
# msoa_lookup_final <- msoa_lookup_final %>%
#   filter(if_all(everything(), ~ !is.na(.)))

# write_csv(
# msoa_lookup_final,"MSOA_2011_HLE_skeleton.csv")



# #Can also remove MSOA with implausible life expectances ....
# 
# imd_2015 <- read_csv("Working files/imd_2015_final_merged.csv") %>%
#   select(-MSOA11NM) %>%
#   rename_with(
#     ~ paste0(.x, "_2015"),
#     -MSOA11CD
#   )
# 
# imd_2025 <- read_csv("Working files/imd_2025_final_msoa.csv") %>%
#   select(-MSOA21NM) %>%
#   rename_with(
#     ~ paste0(.x, "_2025"),
#     -MSOA21CD
#   )
# 
# msoa_lookup_final <- msoa_lookup_final %>%
#   left_join(imd_2015, by = "MSOA11CD") %>%
#   left_join(imd_2025, by = "MSOA21CD")
# 
# setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA/Working files")
# write_csv(
#   msoa_lookup_final,"MSOA_2011_HLE_IMD.csv")
# 
# 
# msoa_lookup_final_missing <- msoa_lookup_final %>%
#   filter(if_any(everything(), is.na))
# 
# ###

#Corrected

# Read IMD 2015
imd_2015 <- read_csv(
  "Working files/imd_2015_final_merged.csv",
  show_col_types = FALSE
) %>%
  rename_with(
    ~ paste0(.x, "_2015"),
    -MSOA11CD
  )

# Read IMD 2025

imd_2025 <- read_csv(
  "Working files/imd_2025_final_msoa.csv",
  show_col_types = FALSE
) %>%
  rename_with(
    ~ paste0(.x, "_2025"),
    -MSOA21CD
  )

# First join: 2015 IMD using MSOA11CD

msoa_lookup_final <- msoa_lookup_final %>%
  left_join(
    imd_2015,
    by = "MSOA11CD"
  )

# Second join: 2015 IMD using MSOA21CD
# This catches the merged MSOAs


imd_2015_by_2021 <- imd_2015 %>%
  rename(
    MSOA21CD = MSOA11CD
  ) %>%
  rename_with(
    ~ paste0(.x, "_fallback"),
    -MSOA21CD
  )


msoa_lookup_final <- msoa_lookup_final %>%
  left_join(
    imd_2015_by_2021,
    by = "MSOA21CD"
  )


# Fill missing 2015 values with the MSOA21 match

msoa_lookup_final <- msoa_lookup_final %>%
  mutate(
    across(
      ends_with("_2015"),
      ~ coalesce(
        .x,
        get(
          paste0(
            cur_column(),
            "_fallback"
          )
        )
      )
    )
  ) %>%
  select(
    -ends_with("_fallback")
  )

# Join 2025 IMD

msoa_lookup_final <- msoa_lookup_final %>%
  left_join(
    imd_2025,
    by = "MSOA21CD"
  )


# ---------------------------------------------------------
# Save/ recall for subsequent scripts 

setwd("C:/Users/rhianna.sookhy/OneDrive - The Health Foundation/Shortcuts/Analysis - 11-CAT/1. Work programme/Healthy Life Expectancy - strategy launch/Phase 2/Working files")

write_csv(
  msoa_lookup_final,
  "MSOA_2011_HLE_IMD.csv"
)



msoa_lookup_final_missing <- msoa_lookup_final %>%
  filter(if_any(everything(), is.na))

