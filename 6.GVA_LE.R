#Date 28/07/2026

#Code to create marmot curve for 2021 HLE data 


rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

# setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA")
setwd("C:/Users/rhianna.sookhy/OneDrive - The Health Foundation/Shortcuts/Analysis - 11-CAT/1. Work programme/Healthy Life Expectancy - strategy launch/Phase 2")

#Load new data

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




##import GVA data
library(readxl)
gva_msoa <- read_excel("Raw data/gva_msoa.xlsx",
                       skip = 1,
                       sheet = "Table 1")


# Keep only MSOA code and GVA values, rename year columns
gva_msoa_clean <- gva_msoa %>%
  select(`MSOA code`, `1998`:`2023`) %>%
  rename(MSOA21CD = `MSOA code`) %>%
  rename_with(
    ~ paste0("GVA_", .x),
    .cols = matches("^\\d{4}$")
  )

# Join to health data
health_2021 <- health_2021 %>%
  left_join(gva_msoa_clean, by = "MSOA21CD")


health_2021 <- health_2021 %>%
  mutate(
    GVA_2019_23 = rowMeans(
      select(., GVA_2019:GVA_2023),
      na.rm = TRUE
    )
  )

# Remove MSOAs with extreme GVA outliers (1.5 x IQR rule)

gva_limits <- quantile(health_2021$GVA_2019_23, probs = c(0.25, 0.75), na.rm = TRUE)

IQR_gva <- diff(gva_limits)

lower_limit <- gva_limits[1] - 1.5 * IQR_gva
upper_limit <- gva_limits[2] + 1.5 * IQR_gva

health_2021 <- health_2021 %>%
  filter(
    GVA_2019_23 >= lower_limit,
    GVA_2019_23 <= upper_limit
  )



health_long <- health_2021 %>%
  mutate(
    measure = "Life expectancy",
    years = LE_2021,
    lower = LE_2021_LCI,
    upper = LE_2021_UCI
  ) %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM,
    Sex,
    GVA_2019_23,
    measure,
    years,
    lower,
    upper
  ) %>%
  bind_rows(
    health_2021 %>%
      mutate(
        measure = "Healthy life expectancy",
        years = HLE_2021,
        lower = HLE_2021_LCI,
        upper = HLE_2021_UCI
      ) %>%
      select(
        RGN22NM,
        MSOA21CD,
        MSOA21NM,
        Sex,
        GVA_2019_23,
        measure,
        years,
        lower,
        upper
      )
  ) %>%
  drop_na(years, GVA_2019_23)


plot_overall <- function(data, measure_name, title_text) {
  
  data %>%
    filter(measure == measure_name) %>%
    ggplot(aes(
      x = GVA_2019_23,
      y = years
    )) +
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
    labs(
      x = "Mean GVA (2019–2023)",
      y = "Years",
      title = title_text
    ) +
    theme_minimal()
}

# Create plots
overall_LE_GVA_plot <- plot_overall(
  health_long,
  "Life expectancy",
  "Life expectancy vs mean GVA (2019–2023)"
)

overall_HLE_GVA_plot <- plot_overall(
  health_long,
  "Healthy life expectancy",
  "Healthy life expectancy vs mean GVA hour worked (2019–2023)"
)

# Display plots
overall_LE_GVA_plot
overall_HLE_GVA_plot

##Plot by region

gva_plot_by_region <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = GVA_2019_23,
        y = years
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
      linewidth = 1
    ) +
    
    scale_x_log10(
      labels = scales::label_number(big.mark = ",")
    ) +
    
    facet_wrap(~RGN22NM) +
    
    labs(
      x = "Mean GVA (2019–2023, log scale)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}


LE_GVA_by_region <- gva_plot_by_region(
  health_long,
  "Life expectancy",
  "Life expectancy by mean GVA and region"
)

HLE_GVA_by_region <- gva_plot_by_region(
  health_long,
  "Healthy life expectancy",
  "Healthy life expectancy by mean GVA and region"
)

LE_GVA_by_region
HLE_GVA_by_region




##Re doing this with a continous percentile

gva_percentiles <- health_2021 %>%
  select(
    MSOA21CD,
    RGN22NM,
    GVA_2019_23
  ) %>%
  distinct() %>%
  
  mutate(
    GVA_percentile_national = 1 + (percent_rank(GVA_2019_23) * 99)
  ) %>%
  
  group_by(RGN22NM) %>%
  mutate(
    GVA_percentile_region = 1 + (percent_rank(GVA_2019_23) * 99)
  ) %>%
  
  ungroup()


health_2021 <- health_2021 %>%
  select(
    -any_of(c(
      "GVA_percentile_national",
      "GVA_percentile_region"
    ))
  ) %>%
  left_join(
    gva_percentiles %>%
      select(
        MSOA21CD,
        GVA_percentile_national,
        GVA_percentile_region
      ),
    by = "MSOA21CD"
  )
summary(health_2021$GVA_percentile_national)

summary(health_2021$GVA_percentile_region)

health_long <- health_2021 %>%
  mutate(
    measure = "Life expectancy",
    years = LE_2021,
    lower = LE_2021_LCI,
    upper = LE_2021_UCI
  ) %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM,
    Sex,
    GVA_percentile_national,
    GVA_percentile_region,
    measure,
    years,
    lower,
    upper
  ) %>%
  
  bind_rows(
    health_2021 %>%
      mutate(
        measure = "Healthy life expectancy",
        years = HLE_2021,
        lower = HLE_2021_LCI,
        upper = HLE_2021_UCI
      ) %>%
      select(
        RGN22NM,
        MSOA21CD,
        MSOA21NM,
        Sex,
        GVA_percentile_national,
        GVA_percentile_region,
        measure,
        years,
        lower,
        upper
      )
  ) %>%
  drop_na(years)



gva_percentile_plot <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = GVA_percentile_national,
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
      breaks = seq(1,100,10)
    ) +
    
    labs(
      x = "National GVA percentile\n(1 = lowest, 100 = highest)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}

###With ci

gva_percentile_plot <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = GVA_percentile_national,
        y = years
      )
    ) +
    
    # Observations
    geom_point(
      alpha = 0.15,
      size = 0.8
    ) +
    
    # Main relationship
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    # # Lower confidence limit trend
    # geom_smooth(
    #   aes(y = lower),
    #   method = "loess",
    #   span = 1,
    #   se = FALSE,
    #   linetype = "dashed",
    #   linewidth = 0.8
    # ) +
    # 
    # # Upper confidence limit trend
    # geom_smooth(
    #   aes(y = upper),
    #   method = "loess",
    #   span = 1,
    #   se = FALSE,
    #   linetype = "dashed",
    #   linewidth = 0.8
    # ) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(0,100,10)
    ) +
    
    labs(
      x = "National GVA percentile\n(1 = lowest, 100 = highest)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}

LE_GVA_national <- gva_percentile_plot(
  health_long,
  "Life expectancy",
  "Life expectancy by national GVA percentile"
)

HLE_GVA_national <- gva_percentile_plot(
  health_long,
  "Healthy life expectancy",
  "Healthy life expectancy by national GVA percentile"
)

LE_GVA_national
HLE_GVA_national


gva_percentile_plot_by_sex <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = GVA_percentile_national,
        y = years,
        colour = Sex,
        group = Sex
      )
    ) +
    
    # Observations
    geom_point(
      alpha = 0.12,
      size = 0.7
    ) +
    
    # Smoothed relationship
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
      x = "National GVA percentile\n(1 = lowest, 100 = highest)",
      y = "Years",
      colour = "Sex",
      title = title_text
    ) +
    
    theme_minimal()
}


LE_GVA_national_sex <- gva_percentile_plot_by_sex(
  health_long,
  "Life expectancy",
  "Life expectancy by national GVA percentile and sex"
)

LE_GVA_national_sex

HLE_GVA_national_sex <- gva_percentile_plot_by_sex(
  health_long,
  "Healthy life expectancy",
  "Healthy life expectancy by national GVA percentile and sex"
)

HLE_GVA_national_sex

gva_region_percentile_plot <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = GVA_percentile_region,
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
      linewidth = 1.1
    ) +
    
    facet_wrap(~RGN22NM) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(1,100,25)
    ) +
    
    labs(
      x = "Regional GVA percentile\n(1 = lowest, 100 = highest)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}


LE_GVA_region <- gva_region_percentile_plot(
  health_long,
  "Life expectancy",
  "Life expectancy by regional GVA percentile"
)

HLE_GVA_region <- gva_region_percentile_plot(
  health_long,
  "Healthy life expectancy",
  "Healthy life expectancy by regional GVA percentile"
)

LE_GVA_region
HLE_GVA_region










##for london v north east

library(dplyr)
library(ggplot2)
library(rlang)



london_northeast_gva <- health_2021 %>%
  filter(
    RGN22NM %in% c("London", "North East")
  )



gva_region_plot <- function(data,
                            estimate,
                            lower,
                            upper,
                            title){
  
  estimate <- rlang::ensym(estimate)
  lower <- rlang::ensym(lower)
  upper <- rlang::ensym(upper)
  
  ggplot(data, aes(x = GVA_percentile_region)) +
    
    # Lower CI smoother
    geom_smooth(
      aes(
        y = !!lower,
        colour = RGN22NM
      ),
      method = "loess",
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    # Upper CI smoother
    geom_smooth(
      aes(
        y = !!upper,
        colour = RGN22NM
      ),
      method = "loess",
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    # Main estimate
    geom_smooth(
      aes(
        y = !!estimate,
        colour = RGN22NM
      ),
      method = "loess",
      se = FALSE,
      linewidth = 1.4
    ) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(0,100,10),
      expand = c(0,0)
    ) +
    
    labs(
      x = "Regional GVA percentile\n(1 = lowest GVA, 100 = highest GVA)",
      y = "Years",
      colour = "Region",
      title = title
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

LE_london_northeast_gva <-
  gva_region_plot(
    london_northeast_gva,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    "Life expectancy by regional GVA percentile"
  )

LE_london_northeast_gva

HLE_london_northeast_gva <-
  gva_region_plot(
    london_northeast_gva,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI,
    "Healthy life expectancy by regional GVA percentile"
  )

HLE_london_northeast_gva







five_regions_gva <- health_2021 %>%
  filter(
    RGN22NM %in% c(
      "London",
      "North East",
      "North West",
      "South West",
      "West Midlands"
    )
  )


gva_national_plot <- function(data,
                              estimate,
                              lower,
                              upper,
                              title){
  
  estimate <- rlang::ensym(estimate)
  lower <- rlang::ensym(lower)
  upper <- rlang::ensym(upper)
  
  ggplot(data, aes(x = GVA_percentile_national)) +
    
    # Lower published CI
    geom_smooth(
      aes(
        y = !!lower,
        colour = RGN22NM
      ),
      method = "loess",
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    # Upper published CI
    geom_smooth(
      aes(
        y = !!upper,
        colour = RGN22NM
      ),
      method = "loess",
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    # Estimate
    geom_smooth(
      aes(
        y = !!estimate,
        colour = RGN22NM
      ),
      method = "loess",
      se = FALSE,
      linewidth = 1.4
    ) +
    
    scale_x_continuous(
      limits = c(1,100),
      breaks = seq(0,100,10),
      expand = c(0,0)
    ) +
    
    labs(
      x = "National GVA percentile\n(1 = lowest GVA, 100 = highest GVA)",
      y = "Years",
      colour = "Region",
      title = title
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}


LE_five_regions_gva <-
  gva_national_plot(
    five_regions_gva,
    LE_2021,
    LE_2021_LCI,
    LE_2021_UCI,
    "Life expectancy by national GVA percentile"
  )

LE_five_regions_gva



HLE_five_regions_gva <-
  gva_national_plot(
    five_regions_gva,
    HLE_2021,
    HLE_2021_LCI,
    HLE_2021_UCI,
    "Healthy life expectancy by national GVA percentile"
  )

HLE_five_regions_gva





#Descriptives 
library(dplyr)

health_long %>%
  group_by(measure) %>%
  summarise(
    n = n(),
    mean_years = mean(years, na.rm = TRUE),
    sd_years = sd(years, na.rm = TRUE),
    min_years = min(years, na.rm = TRUE),
    max_years = max(years, na.rm = TRUE),
    mean_GVA_percentile = mean(GVA_percentile_national, na.rm = TRUE),
    sd_GVA_percentile = sd(GVA_percentile_national, na.rm = TRUE)
  )


health_long %>%
  mutate(
    GVA_group = case_when(
      GVA_percentile_national <= 20 ~ "Lowest 20%",
      GVA_percentile_national >= 80 ~ "Highest 20%",
      TRUE ~ "Middle 60%"
    )
  ) %>%
  group_by(measure, Sex, GVA_group) %>%
  summarise(
    mean_years = mean(years, na.rm = TRUE),
    sd_years = sd(years, na.rm = TRUE),
    n = n()
  )


health_long %>%
  filter(
    GVA_percentile_national <= 10 |
      GVA_percentile_national >= 90
  ) %>%
  group_by(measure, Sex) %>%
  summarise(
    low_GVA = mean(
      years[GVA_percentile_national <= 10],
      na.rm = TRUE
    ),
    high_GVA = mean(
      years[GVA_percentile_national >= 90],
      na.rm = TRUE
    ),
    difference = high_GVA - low_GVA
  )

library(dplyr)

#----------------------------------------------------------
# National GVA summary
#----------------------------------------------------------

health_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    GVA_2019_23
  ) %>%
  distinct() %>%
  summarise(
    n_MSOA = n(),
    mean_GVA = mean(GVA_2019_23, na.rm = TRUE),
    median_GVA = median(GVA_2019_23, na.rm = TRUE),
    sd_GVA = sd(GVA_2019_23, na.rm = TRUE),
    min_GVA = min(GVA_2019_23, na.rm = TRUE),
    p25_GVA = quantile(GVA_2019_23, 0.25, na.rm = TRUE),
    p75_GVA = quantile(GVA_2019_23, 0.75, na.rm = TRUE),
    max_GVA = max(GVA_2019_23, na.rm = TRUE)
  )

health_2021 %>%
  select(
    MSOA21CD,
    MSOA21NM,
    RGN22NM,
    GVA_2019_23
  ) %>%
  distinct() %>%
  
  group_by(RGN22NM) %>%
  
  summarise(
    n_MSOA = n(),
    mean_GVA = mean(GVA_2019_23, na.rm = TRUE),
    median_GVA = median(GVA_2019_23, na.rm = TRUE),
    sd_GVA = sd(GVA_2019_23, na.rm = TRUE),
    min_GVA = min(GVA_2019_23, na.rm = TRUE),
    p25_GVA = quantile(GVA_2019_23, 0.25, na.rm = TRUE),
    p75_GVA = quantile(GVA_2019_23, 0.75, na.rm = TRUE),
    max_GVA = max(GVA_2019_23, na.rm = TRUE)
  ) %>%
  
  arrange(desc(mean_GVA))



##Now diff x avis 
# Create early-period GVA average and change over time

health_2021 <- health_2021 %>%
  mutate(
    GVA_mean_2009_2013 = rowMeans(
      select(., GVA_2009:GVA_2013),
      na.rm = TRUE
    ),
    
    GVA_change_2009_2023 = GVA_2019_23 - GVA_mean_2009_2013
  )



health_long <- health_2021 %>%
  mutate(
    measure = "Life expectancy",
    years = LE_2021,
    lower = LE_2021_LCI,
    upper = LE_2021_UCI
  ) %>%
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM,
    Sex,
    GVA_2019_23,
    GVA_change_2009_2023,
    measure,
    years,
    lower,
    upper
  ) %>%
  
  bind_rows(
    health_2021 %>%
      mutate(
        measure = "Healthy life expectancy",
        years = HLE_2021,
        lower = HLE_2021_LCI,
        upper = HLE_2021_UCI
      ) %>%
      select(
        RGN22NM,
        MSOA21CD,
        MSOA21NM,
        Sex,
        GVA_2019_23,
        GVA_change_2009_2023,
        measure,
        years,
        lower,
        upper
      )
  ) %>%
  drop_na(years, GVA_change_2009_2023)


gva_change_plot <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = GVA_change_2009_2023,
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
      labels = scales::label_number(big.mark = ",")
    ) +
    
    labs(
      x = "Increase in mean GVA\n(2019–2023 compared with 2009–2013)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}

LE_GVA_growth <- gva_change_plot(
  health_long,
  "Life expectancy",
  "Life expectancy by GVA growth (2009–2023)"
)

LE_GVA_growth


HLE_GVA_growth <- gva_change_plot(
  health_long,
  "Healthy life expectancy",
  "Healthy life expectancy by GVA growth (2009–2023)"
)

HLE_GVA_growth



# National percentile of GVA growth


health_2021 <- health_2021 %>%
  mutate(
    GVA_mean_2009_2013 = rowMeans(
      select(., GVA_2009:GVA_2013),
      na.rm = TRUE
    ),
    
    GVA_growth_percent =
      ((GVA_2019_23 - GVA_mean_2009_2013) /
         GVA_mean_2009_2013) * 100
  )


# ============================================================
# 2. Create national growth percentile
# ============================================================

gva_growth_percentiles <- health_2021 %>%
  select(
    MSOA21CD,
    GVA_growth_percent
  ) %>%
  distinct() %>%
  
  mutate(
    GVA_growth_percentile_national =
      1 + (percent_rank(GVA_growth_percent) * 99)
  )


health_2021 <- health_2021 %>%
  left_join(
    gva_growth_percentiles,
    by = "MSOA21CD"
  )


# check distribution
summary(health_2021$GVA_growth_percent)
summary(health_2021$GVA_growth_percentile_national)


health_long_growth <- health_2021 %>%
  
  mutate(
    measure = "Life expectancy",
    years = LE_2021,
    lower = LE_2021_LCI,
    upper = LE_2021_UCI
  ) %>%
  
  select(
    RGN22NM,
    MSOA21CD,
    MSOA21NM,
    Sex,
    GVA_growth_percent,
    GVA_growth_percentile_national,
    measure,
    years,
    lower,
    upper
  ) %>%
  
  bind_rows(
    health_2021 %>%
      mutate(
        measure = "Healthy life expectancy",
        years = HLE_2021,
        lower = HLE_2021_LCI,
        upper = HLE_2021_UCI
      ) %>%
      select(
        RGN22NM,
        MSOA21CD,
        MSOA21NM,
        Sex,
        GVA_growth_percent,
        GVA_growth_percentile_national,
        measure,
        years,
        lower,
        upper
      )
  ) %>%
  
  drop_na(years, GVA_growth_percent)

gva_growth_percentile_plot <- function(data, measure_name, title_text){
  
  data %>%
    filter(measure == measure_name) %>%
    
    ggplot(
      aes(
        x = GVA_growth_percentile_national,
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
      breaks = seq(1,100,10)
    ) +
    
    labs(
      x = "National GVA growth percentile\n(1 = lowest growth, 100 = highest growth)",
      y = "Years",
      title = title_text
    ) +
    
    theme_minimal()
}


LE_growth_percentile <- gva_growth_percentile_plot(
  health_long_growth,
  "Life expectancy",
  "Life expectancy by national GVA growth percentile"
)


HLE_growth_percentile <- gva_growth_percentile_plot(
  health_long_growth,
  "Healthy life expectancy",
  "Healthy life expectancy by national GVA growth percentile"
)


LE_growth_percentile
HLE_growth_percentile





































###Growth LE vs GVA 


rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)


setwd(
  "~/Analysis and Modelling general/2011-2021 HLE by MSOA"
)


# ============================================================
# 2. LOAD EXISTING 2011 HLE + IMD SKELETON
# ============================================================

msoa_data <- read_csv(
  "Working files/MSOA_2011_HLE_IMD.csv"
)


# Inspect structure
glimpse(msoa_data)


# ============================================================
# 3. LOAD NEW 2021 HLE / LE DATA
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




# ============================================================
# 6. CHECK 2021 HLE FILES BEFORE MERGING
# ============================================================


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



# ============================================================
# 9. CHECK ALL REQUIRED VARIABLES FOR MISSING VALUES
# ============================================================

required_variables <- c(
  "MSOA21CD",
  "LE_2011_M",
  "LE_2011_F",
  "LE_2021_M",
  "LE_2021_F",
  "HLE_2011_M",
  "HLE_2011_F",
  "HLE_2021_M",
  "HLE_2021_F",
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


# ============================================================
# 10. IDENTIFY ROWS WITH ANY REQUIRED MISSING DATA
# ============================================================

msoa_missing <- msoa_data %>%
  filter(
    if_any(
      all_of(required_variables),
      is.na
    )
  )


# View these before deciding whether to exclude
msoa_missing


analysis_data <- msoa_data %>%
  filter(
    if_all(
      all_of(required_variables),
      ~ !is.na(.)
    )
  )


analysis_data <- msoa_data

%>%
  select(all_of(required_variables)) %>%
  filter(
    if_all(
      all_of(required_variables),
      ~ !is.na(.)
    )
  )


##GVA =======================

##import GVA data
library(readxl)
gva_msoa <- read_excel("Raw data/gva_msoa.xlsx",
                       skip = 1,
                       sheet = "Table 1")


# Keep only MSOA code and GVA values, rename year columns
gva_msoa_clean <- gva_msoa %>%
  select(`MSOA code`, `1998`:`2023`) %>%
  rename(MSOA21CD = `MSOA code`) %>%
  rename_with(
    ~ paste0("GVA_", .x),
    .cols = matches("^\\d{4}$")
  )


gva_msoa_clean <- gva_msoa %>%
  transmute(
    MSOA21CD = `MSOA code`,
    
    GVA_2009_13 = rowMeans(
      across(`2009`:`2013`),
      na.rm = TRUE
    ),
    
    GVA_2019_23 = rowMeans(
      across(`2019`:`2023`),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    GVA_change_2009_13_to_2019_23 = GVA_2019_23 - GVA_2009_13
  )


analysis_data <- analysis_data %>%
  left_join(gva_msoa_clean, by = "MSOA21CD")


analysis_data <- analysis_data %>%
  mutate(
    LE_change_M = LE_2021_M - LE_2011_M,
    LE_change_F = LE_2021_F - LE_2011_F
  )


# Remove extreme GVA change outliers

Q1 <- quantile(
  analysis_data$GVA_change_2009_13_to_2019_23,
  0.25,
  na.rm = TRUE
)

Q3 <- quantile(
  analysis_data$GVA_change_2009_13_to_2019_23,
  0.75,
  na.rm = TRUE
)

IQR_GVA <- IQR(
  analysis_data$GVA_change_2009_13_to_2019_23,
  na.rm = TRUE
)

analysis_data_plot <- analysis_data %>%
  filter(
    GVA_change_2009_13_to_2019_23 >= Q1 - 1.5 * IQR_GVA,
    GVA_change_2009_13_to_2019_23 <= Q3 + 1.5 * IQR_GVA
  )

plot_gva_change <- function(data, le_change, title_text) {
  
  ggplot(data, aes(
    x = GVA_change_2009_13_to_2019_23,
    y = .data[[le_change]]
  )) +
    
    geom_point(
      alpha = 0.2,
      size = 0.8
    ) +
    
    geom_smooth(
      method = "loess",
      span = 1,
      se = FALSE,
      linewidth = 1.2
    ) +
    
    labs(
      x = "Change in mean GVA (2019-23 minus 2009-13)",
      y = "Change in life expectancy (years)",
      title = title_text
    ) +
    
    theme_minimal()
}


LE_change_M_plot <- plot_gva_change(
  analysis_data_plot,
  "LE_change_M",
  "Male life expectancy change vs GVA change"
)

LE_change_F_plot <- plot_gva_change(
  analysis_data_plot,
  "LE_change_F",
  "Female life expectancy change vs GVA change"
)

LE_change_M_plot
LE_change_F_plot























# ============================================================
# GVA GROWTH VS CURRENT LIFE EXPECTANCY / HLE
# 2021 HEALTH OUTCOMES
# ============================================================




analysis_data_gva <- analysis_data %>%
  mutate(
    
    # Current 2021 life expectancy
    LE_current_M = LE_2021_M,
    LE_current_F = LE_2021_F,
    
    # Current 2021 healthy life expectancy
    HLE_current_M = HLE_2021_M,
    HLE_current_F = HLE_2021_F
    
  )




analysis_data_gva <- analysis_data_gva %>%
  filter(
    !is.na(GVA_change_2009_13_to_2019_23)
  )


#Remove outliers

Q1 <- quantile(
  analysis_data_gva$GVA_change_2009_13_to_2019_23,
  0.25,
  na.rm = TRUE
)

Q3 <- quantile(
  analysis_data_gva$GVA_change_2009_13_to_2019_23,
  0.75,
  na.rm = TRUE
)

IQR_GVA <- IQR(
  analysis_data_gva$GVA_change_2009_13_to_2019_23,
  na.rm = TRUE
)


analysis_data_gva_plot <- analysis_data_gva %>%
  filter(
    GVA_change_2009_13_to_2019_23 >= Q1 - 1.5 * IQR_GVA,
    GVA_change_2009_13_to_2019_23 <= Q3 + 1.5 * IQR_GVA
  )


# ============================================================
# 4. GENERAL PLOT FUNCTION
# ============================================================

plot_gva_vs_current <- function(
    data,
    outcome,
    sex_name,
    outcome_label,
    title_text
) {
  
  ggplot(
    data,
    aes(
      x = GVA_change_2009_13_to_2019_23,
      y = .data[[outcome]]
    )
  ) +
    
    # --------------------------------------------------------
  # MSOA points
  # --------------------------------------------------------
  
  geom_point(
    alpha = 0.2,
    size = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # LOESS curve
  # --------------------------------------------------------
  
  geom_smooth(
    method = "loess",
    span = 1,
    se = FALSE,
    linewidth = 1.2
  ) +
    
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    x = "Change in mean GVA\n(2019-23 minus 2009-13)",
    y = outcome_label,
    title = title_text
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}



LE_current_M_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "LE_current_M",
  sex_name = "Male",
  outcome_label = "Life expectancy in 2021 (years)",
  title_text = "Current male life expectancy vs GVA growth"
)




LE_current_F_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "LE_current_F",
  sex_name = "Female",
  outcome_label = "Life expectancy in 2021 (years)",
  title_text = "Current female life expectancy vs GVA growth"
)



HLE_current_M_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "HLE_current_M",
  sex_name = "Male",
  outcome_label = "Healthy life expectancy in 2021 (years)",
  title_text = "Current male healthy life expectancy vs GVA growth"
)



HLE_current_F_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "HLE_current_F",
  sex_name = "Female",
  outcome_label = "Healthy life expectancy in 2021 (years)",
  title_text = "Current female healthy life expectancy vs GVA growth"
)


#Display

LE_current_M_plot
LE_current_F_plot

HLE_current_M_plot
HLE_current_F_plot







# ============================================================
# GVA GROWTH VS CURRENT LE / HLE
# EXCLUDING LONDON



# EXCLUDE LONDON


analysis_data_gva <- analysis_data %>%
  filter(
    Region != "London"
  ) %>%
  mutate(
    
    # Current 2021 outcomes
    LE_current_M  = LE_2021_M,
    LE_current_F  = LE_2021_F,
    HLE_current_M = HLE_2021_M,
    HLE_current_F = HLE_2021_F
    
  ) %>%
  filter(
    !is.na(GVA_change_2009_13_to_2019_23)
  )



# REMOVE EXTREME GVA OUTLIERS


Q1 <- quantile(
  analysis_data_gva$GVA_change_2009_13_to_2019_23,
  0.25,
  na.rm = TRUE
)

Q3 <- quantile(
  analysis_data_gva$GVA_change_2009_13_to_2019_23,
  0.75,
  na.rm = TRUE
)

IQR_GVA <- IQR(
  analysis_data_gva$GVA_change_2009_13_to_2019_23,
  na.rm = TRUE
)


analysis_data_gva_plot <- analysis_data_gva %>%
  filter(
    GVA_change_2009_13_to_2019_23 >= Q1 - 1.5 * IQR_GVA,
    GVA_change_2009_13_to_2019_23 <= Q3 + 1.5 * IQR_GVA
  )


#Create plotting function

plot_gva_vs_current <- function(
    data,
    outcome,
    outcome_label,
    title_text
) {
  
  ggplot(
    data,
    aes(
      x = GVA_change_2009_13_to_2019_23,
      y = .data[[outcome]]
    )
  ) +
    
    # --------------------------------------------------------
  # MSOA points
  # --------------------------------------------------------
  
  geom_point(
    alpha = 0.2,
    size = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # LOESS curve
  # --------------------------------------------------------
  
  geom_smooth(
    method = "loess",
    span = 1,
    se = FALSE,
    linewidth = 1.2
  ) +
    
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    x = "Change in mean GVA\n(2019–23 minus 2009–13)",
    y = outcome_label,
    title = title_text
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}



LE_current_M_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "LE_current_M",
  outcome_label = "Life expectancy in 2021 (years)",
  title_text = "Current male life expectancy vs GVA growth\n(excluding London)"
)



LE_current_F_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "LE_current_F",
  outcome_label = "Life expectancy in 2021 (years)",
  title_text = "Current female life expectancy vs GVA growth\n(excluding London)"
)



HLE_current_M_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "HLE_current_M",
  outcome_label = "Healthy life expectancy in 2021 (years)",
  title_text = "Current male healthy life expectancy vs GVA growth\n(excluding London)"
)



HLE_current_F_plot <- plot_gva_vs_current(
  data = analysis_data_gva_plot,
  outcome = "HLE_current_F",
  outcome_label = "Healthy life expectancy in 2021 (years)",
  title_text = "Current female healthy life expectancy vs GVA growth\n(excluding London)"
)


#Displaying

LE_current_M_plot
LE_current_F_plot

HLE_current_M_plot
HLE_current_F_plot






#Growth percentile

analysis_data_gva <- analysis_data %>%
  filter(
    !is.na(GVA_change_2009_13_to_2019_23)
  ) %>%
  
  arrange(
    GVA_change_2009_13_to_2019_23
  ) %>%
  
  mutate(
    GVA_growth_percentile =
      1 + 99 * (row_number() - 1) / (n() - 1)
  )


#Removing london

analysis_data_gva_plot <- analysis_data_gva %>%
  filter(
    Region != "London"
  ) %>%
  mutate(
    
    LE_current_M  = LE_2021_M,
    LE_current_F  = LE_2021_F,
    
    HLE_current_M = HLE_2021_M,
    HLE_current_F = HLE_2021_F
    
  ) %>%
  
  filter(
    !is.na(LE_current_M),
    !is.na(LE_current_F),
    !is.na(HLE_current_M),
    !is.na(HLE_current_F)
  )

#Looking at growth

plot_gva_percentile_vs_current <- function(
    data,
    outcome,
    outcome_label,
    title_text
) {
  
  ggplot(
    data,
    aes(
      x = GVA_growth_percentile,
      y = .data[[outcome]]
    )
  ) +
    
    # --------------------------------------------------------
  # MSOA points
  # --------------------------------------------------------
  
  geom_point(
    alpha = 0.2,
    size = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # LOESS curve
  # --------------------------------------------------------
  
  geom_smooth(
    method = "loess",
    span = 1,
    se = FALSE,
    linewidth = 1.2
  ) +
    
    
    # --------------------------------------------------------
  # X axis
  # --------------------------------------------------------
  
  scale_x_continuous(
    limits = c(1, 100),
    breaks = seq(10, 100, 10),
    expand = c(0, 0)
  ) +
    
    
    # --------------------------------------------------------
  # Labels
  # --------------------------------------------------------
  
  labs(
    x = "GVA growth percentile\n(1 = lowest growth, 100 = highest growth)",
    y = outcome_label,
    title = title_text
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_minimal(base_size = 13) +
    
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}



LE_GVA_percentile_M_plot <- plot_gva_percentile_vs_current(
  data = analysis_data_gva_plot,
  outcome = "LE_current_M",
  outcome_label = "Life expectancy in 2021 (years)",
  title_text = "Current male life expectancy vs GVA growth percentile\n(excluding London)"
)

LE_GVA_percentile_F_plot <- plot_gva_percentile_vs_current(
  data = analysis_data_gva_plot,
  outcome = "LE_current_F",
  outcome_label = "Life expectancy in 2021 (years)",
  title_text = "Current female life expectancy vs GVA growth percentile\n(excluding London)"
)


HLE_GVA_percentile_M_plot <- plot_gva_percentile_vs_current(
  data = analysis_data_gva_plot,
  outcome = "HLE_current_M",
  outcome_label = "Healthy life expectancy in 2021 (years)",
  title_text = "Current male healthy life expectancy vs GVA growth percentile\n(excluding London)"
)


HLE_GVA_percentile_F_plot <- plot_gva_percentile_vs_current(
  data = analysis_data_gva_plot,
  outcome = "HLE_current_F",
  outcome_label = "Healthy life expectancy in 2021 (years)",
  title_text = "Current female healthy life expectancy vs GVA growth percentile\n(excluding London)"
)

LE_GVA_percentile_M_plot
LE_GVA_percentile_F_plot

HLE_GVA_percentile_M_plot
HLE_GVA_percentile_F_plot
