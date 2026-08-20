#Date 28/07/2026

#Author Rhianna Sookhy

#Weights IMD 2015 & 2025 based on mid 2012 and 2022 populations following ONS methodology for LAD

#########################


#TO QA Check:
#2025 matches this https://justknowledge.org.uk/blog/imd
# 2019 LAD macthes file 10 (https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015)
# 2025 LAD matches  file 10 (https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025)

rm(list = ls())

library(readr)
library(dplyr)
library(tibble)

#Edit depending
setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA")


#Creating functions
weighted_mean_pop <- function(x, w) {
  sum(x * w, na.rm = TRUE) /
    sum(w, na.rm = TRUE)
}

reverse_rank <- function(x) {
  max(x, na.rm = TRUE) + 1 - x
}


#Aggregation
aggregate_domains <- function(
    data,
    group_vars,
    pop_var,
    domains
) {
  
  result <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      population =
        sum(.data[[pop_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  for (i in seq_len(nrow(domains))) {
    
    domain <- domains$domain[i]
    
    score_var <- domains$score_var[i]
    
    rank_var <- domains$rank_var[i]
    
    tmp <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        
        !!paste0(domain, "_average_score") :=
          weighted_mean_pop(
            .data[[score_var]],
            .data[[pop_var]]
          ),
        
        !!paste0(domain, "_average_rank") :=
          weighted_mean_pop(
            .data[[rank_var]],
            .data[[pop_var]]
          ),
        
        .groups = "drop"
        
      )
    
    result <- left_join(
      result,
      tmp,
      by = group_vars
    )
  }
  
  result
}


#Add rankings
add_rankings <- function(data, domains) {
  
  for (i in seq_len(nrow(domains))) {
    
    domain <- domains$domain[i]
    
    score_col <- paste0(domain, "_average_score")
    
    rank_col <- paste0(domain, "_average_rank")
    
    data <- data %>%
      mutate(
        
        !!paste0(domain, "_score_rank") :=
          rank(
            -.data[[score_col]],
            ties.method = "min"
          ),
        
        !!paste0(domain, "_average_rank_rank") :=
          rank(
            -.data[[rank_col]],
            ties.method = "min"
          ),
        
        !!paste0(domain, "_score_decile") :=
          ntile(
            desc(.data[[score_col]]),
            10
          ),
        
        !!paste0(domain, "_rank_decile") :=
          ntile(
            desc(.data[[rank_col]]),
            10
          )
        
      )
    
  }
  
  # Reorder columns
  
  id_cols <- names(data)[1:3]
  
  domain_cols <- unlist(
    lapply(domains$domain, function(domain) {
      
      c(
        paste0(domain, "_average_rank"),
        paste0(domain, "_average_rank_rank"),
        paste0(domain, "_rank_decile"),
        
        paste0(domain, "_average_score"),
        paste0(domain, "_score_rank"),
        paste0(domain, "_score_decile")
      )
      
    })
  )
  
  data %>%
    select(
      all_of(id_cols),
      all_of(domain_cols)
    )
  
}


#Combine into one fucntion
process_imd <- function(
    imd_file,
    lookup_file,
    pop_var,
    lsoa_col,
    msoa_code,
    msoa_name,
    lad_code,
    lad_name,
    lookup_cols,
    domains,
    output_prefix
) {
  
  message("Reading files...")
  
  imd <- read_csv(imd_file, show_col_types = FALSE)
  
  lookup <- read_csv(lookup_file, show_col_types = FALSE)
  
  #Lookup file
  
  lookup_clean <- lookup %>%
    select(all_of(lookup_cols)) %>%
    distinct()
  
  imd <- imd %>%
    left_join(
      lookup_clean,
      by = setNames(
        lookup_cols[1],
        lsoa_col
      )
    )
  
  #raking order
  
  for (i in seq_len(nrow(domains))) {
    
    original_rank <- domains$original_rank[i]
    
    reversed_rank <- domains$rank_var[i]
    
    imd[[reversed_rank]] <-
      reverse_rank(
        imd[[original_rank]]
      )
  }
  
  #LAD
  
  imd_lad <- aggregate_domains(
    data = imd,
    group_vars = c(lad_code, lad_name),
    pop_var = pop_var,
    domains = domains
  ) %>%
    add_rankings(
      domains = domains
    )
  
  #MSOA
  
  imd_msoa <- aggregate_domains(
    data = imd,
    group_vars = c(msoa_code, msoa_name),
    pop_var = pop_var,
    domains = domains
  ) %>%
    add_rankings(
      domains = domains
    )
  
  #Save files/ comment out if don't want 
  
  # write_csv(
  #   imd_lad,
  #   paste0(output_prefix, "_LAD_weighted.csv")
  # )
  # 
  # write_csv(
  #   imd_msoa,
  #   paste0(output_prefix, "_MSOA_weighted.csv")
  # )
  
  list(
    msoa = imd_msoa,
    lad = imd_lad
  )

}

domains <- tribble(
  ~domain, ~score_var, ~original_rank, ~rank_var,
  
  "imd",
  "Index of Multiple Deprivation (IMD) Score",
  "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)",
  "imd_rank_rev",
  
  "income",
  "Income Score (rate)",
  "Income Rank (where 1 is most deprived)",
  "income_rank_rev",
  
  "employment",
  "Employment Score (rate)",
  "Employment Rank (where 1 is most deprived)",
  "employment_rank_rev",
  
  "education",
  "Education, Skills and Training Score",
  "Education, Skills and Training Rank (where 1 is most deprived)",
  "education_rank_rev",
  
  "health",
  "Health Deprivation and Disability Score",
  "Health Deprivation and Disability Rank (where 1 is most deprived)",
  "health_rank_rev",
  
  "crime",
  "Crime Score",
  "Crime Rank (where 1 is most deprived)",
  "crime_rank_rev",
  
  "barriers",
  "Barriers to Housing and Services Score",
  "Barriers to Housing and Services Rank (where 1 is most deprived)",
  "barriers_rank_rev",
  
  "living",
  "Living Environment Score",
  "Living Environment Rank (where 1 is most deprived)",
  "living_rank_rev"
)


imd_2015_results <- process_imd(
  
  imd_file =
    "Raw data/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv",
  
  lookup_file =
    "Raw data/oa_lsoa_msoa_lad_2011.csv",
  
  pop_var =
    "Total population: mid 2012 (excluding prisoners)",
  
  lsoa_col =
    "LSOA code (2011)",
  
  msoa_code =
    "MSOA11CD",
  
  msoa_name =
    "MSOA11NM",
  
  lad_code =
    "Local Authority District code (2013)",
  
  lad_name =
    "Local Authority District name (2013)",
  
  lookup_cols =
    c(
      "LSOA11CD",
      "LSOA11NM",
      "MSOA11CD",
      "MSOA11NM"
    ),
  
  domains =
    domains,
  
  output_prefix =
    "IMD_2015"
)

imd_2025_results <- process_imd(
  
  imd_file =
    "Raw data/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv",
  
  lookup_file =
    "Raw data/0a_lsoa_msoa_lad_2021.csv",
  
  pop_var =
    "Total population: mid 2022",
  
  lsoa_col =
    "LSOA code (2021)",
  
  msoa_code =
    "MSOA21CD",
  
  msoa_name =
    "MSOA21NM",
  
  lad_code =
    "LAD22CD",
  
  lad_name =
    "LAD22NM",
  
  lookup_cols =
    c(
      "LSOA21CD",
      "LSOA21NM",
      "MSOA21CD",
      "MSOA21NM",
      "LAD22CD",
      "LAD22NM"
    ),
  
  domains =
    domains,
  
  output_prefix =
    "IMD_2025"
)


imd_2015_msoa <- imd_2015_results$msoa
imd_2015_lad  <- imd_2015_results$lad

imd_2025_msoa <- imd_2025_results$msoa
imd_2025_lad  <- imd_2025_results$lad


#Now re-doing this for MSOA's that were merged in 2021 ====================================

#Load data
imd_2015 <- read_csv(
  "Raw data/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv",
  show_col_types = FALSE
)

msoa_lsoa_2011 <- read_csv(
"Raw data/oa_lsoa_msoa_lad_2011.csv"
) %>%
  clean_names() %>%
  select(
    msoa11_name = msoa11nm,
    msoa11 = msoa11cd,
    lsoa11 = lsoa11cd,
    lsoa11_name = lsoa11nm
  ) %>%
  distinct()

#Join
imd_2015_lsoa <- msoa_lsoa_2011 %>%
  left_join(
    imd_2015,
    by = c(
      "lsoa11" = "LSOA code (2011)"
    )
  ) %>%
  select(
    msoa11_name,
    msoa11,
    lsoa11,
    lsoa11_name,
    everything()
  )

#Only keeping merged MSOA's
msoa_lookup_M <- read_csv(
  "Raw data/MSOA_(2011)_to_MSOA_(2021)_to_Local_Authority_District_(2022)_Exact_Fit_Lookup_for_EW_(V2) (2).csv",
  show_col_types = FALSE
) %>%
  select(
    MSOA11CD,
    MSOA11NM,
    MSOA21CD,
    MSOA21NM,
    CHNGIND
  ) %>%
  filter(
    CHNGIND == "M"
  )

#join
imd_2015_merged <- imd_2015_lsoa %>%
  left_join(
    msoa_lookup_M,
    by = c(
      "msoa11" = "MSOA11CD"
    )
  ) %>%
  filter(
    !is.na(MSOA21CD)
  )


#Weighting function
weighted_mean_pop <- function(x, w) {
  
  sum(
    x * w,
    na.rm = TRUE
  ) /
    sum(
      w,
      na.rm = TRUE
    )
}

#Aggregation
imd_2015_MSOA21_merged <- imd_2015_merged %>%
  
  group_by(
    MSOA21CD,
    MSOA21NM
  ) %>%
  
  summarise(
    
    MSOA11CD =
      paste(
        unique(msoa11),
        collapse = "; "
      ),
    
    MSOA11NM =
      paste(
        unique(msoa11_name),
        collapse = "; "
      ),
    
    population =
      sum(
        `Total population: mid 2012 (excluding prisoners)`,
        na.rm = TRUE
      ),
    
    
    #Weighting
    
    imd_average_score =
      weighted_mean_pop(
        `Index of Multiple Deprivation (IMD) Score`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    imd_average_rank =
      weighted_mean_pop(
        `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    income_average_score =
      weighted_mean_pop(
        `Income Score (rate)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    income_average_rank =
      weighted_mean_pop(
        `Income Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    employment_average_score =
      weighted_mean_pop(
        `Employment Score (rate)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    employment_average_rank =
      weighted_mean_pop(
        `Employment Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    education_average_score =
      weighted_mean_pop(
        `Education, Skills and Training Score`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    education_average_rank =
      weighted_mean_pop(
        `Education, Skills and Training Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    health_average_score =
      weighted_mean_pop(
        `Health Deprivation and Disability Score`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    health_average_rank =
      weighted_mean_pop(
        `Health Deprivation and Disability Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    crime_average_score =
      weighted_mean_pop(
        `Crime Score`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    crime_average_rank =
      weighted_mean_pop(
        `Crime Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    barriers_average_score =
      weighted_mean_pop(
        `Barriers to Housing and Services Score`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    barriers_average_rank =
      weighted_mean_pop(
        `Barriers to Housing and Services Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    living_average_score =
      weighted_mean_pop(
        `Living Environment Score`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    living_average_rank =
      weighted_mean_pop(
        `Living Environment Rank (where 1 is most deprived)`,
        `Total population: mid 2012 (excluding prisoners)`
      ),
    
    
    .groups = "drop"
  )


#Adding ranking if needed later 
imd_2015_MSOA21_merged <- imd_2015_MSOA21_merged %>%
  mutate(
    
    # IMD
    imd_average_rank_rank =
      rank(-imd_average_rank, ties.method = "min"),
    imd_score_rank =
      rank(-imd_average_score, ties.method = "min"),
    imd_rank_decile =
      ntile(desc(imd_average_rank), 10),
    imd_score_decile =
      ntile(desc(imd_average_score), 10),
    
    
    # Income
    income_average_rank_rank =
      rank(-income_average_rank, ties.method = "min"),
    income_score_rank =
      rank(-income_average_score, ties.method = "min"),
    income_rank_decile =
      ntile(desc(income_average_rank), 10),
    income_score_decile =
      ntile(desc(income_average_score), 10),
    
    
    # Employment
    employment_average_rank_rank =
      rank(-employment_average_rank, ties.method = "min"),
    employment_score_rank =
      rank(-employment_average_score, ties.method = "min"),
    employment_rank_decile =
      ntile(desc(employment_average_rank), 10),
    employment_score_decile =
      ntile(desc(employment_average_score), 10),
    
    
    # Education
    education_average_rank_rank =
      rank(-education_average_rank, ties.method = "min"),
    education_score_rank =
      rank(-education_average_score, ties.method = "min"),
    education_rank_decile =
      ntile(desc(education_average_rank), 10),
    education_score_decile =
      ntile(desc(education_average_score), 10),
    
    
    # Health
    health_average_rank_rank =
      rank(-health_average_rank, ties.method = "min"),
    health_score_rank =
      rank(-health_average_score, ties.method = "min"),
    health_rank_decile =
      ntile(desc(health_average_rank), 10),
    health_score_decile =
      ntile(desc(health_average_score), 10),
    
    
    # Crime
    crime_average_rank_rank =
      rank(-crime_average_rank, ties.method = "min"),
    crime_score_rank =
      rank(-crime_average_score, ties.method = "min"),
    crime_rank_decile =
      ntile(desc(crime_average_rank), 10),
    crime_score_decile =
      ntile(desc(crime_average_score), 10),
    
    
    # Barriers
    barriers_average_rank_rank =
      rank(-barriers_average_rank, ties.method = "min"),
    barriers_score_rank =
      rank(-barriers_average_score, ties.method = "min"),
    barriers_rank_decile =
      ntile(desc(barriers_average_rank), 10),
    barriers_score_decile =
      ntile(desc(barriers_average_score), 10),
    
    
    # Living
    living_average_rank_rank =
      rank(-living_average_rank, ties.method = "min"),
    living_score_rank =
      rank(-living_average_score, ties.method = "min"),
    living_rank_decile =
      ntile(desc(living_average_rank), 10),
    living_score_decile =
      ntile(desc(living_average_score), 10)
  )


imd_2015_MSOA21_merged <- imd_2015_MSOA21_merged %>%
  rename_with(
    ~ paste0(.x, "_weighted"),
    .cols = -c(MSOA21CD, MSOA21NM, MSOA11CD, MSOA11NM)
  )








#Final data for 2015 IMD to join 2011 data
# MSOA11 codes that have merged into MSOA21
merged_msoa11 <- msoa_lookup_M %>%
  pull(MSOA11CD) %>%
  unique()
# Keep only unchanged MSOA11 areas


imd_2015_unmerged <- imd_2015_msoa  %>%
  filter(
    !MSOA11CD %in% merged_msoa11
  )

# Convert weighted MSOA21 output to same structure as IMD2015

imd_2015_MSOA21_replace <- imd_2015_MSOA21_merged %>%
  transmute(
    
    MSOA11CD = MSOA21CD,
    MSOA11NM = MSOA21NM,
    
    population =
      population_weighted,
    
    imd_average_rank =
      imd_average_rank_weighted,
    
    imd_average_rank_rank =
      imd_average_rank_rank_weighted,
    
    imd_rank_decile =
      imd_rank_decile_weighted,
    
    imd_average_score =
      imd_average_score_weighted,
    
    imd_score_rank =
      imd_score_rank_weighted,
    
    imd_score_decile =
      imd_score_decile_weighted,
    
    income_average_rank =
      income_average_rank_weighted,
    
    income_average_rank_rank =
      income_average_rank_rank_weighted,
    
    income_rank_decile =
      income_rank_decile_weighted,
    
    income_average_score =
      income_average_score_weighted,
    
    income_score_rank =
      income_score_rank_weighted,
    
    income_score_decile =
      income_score_decile_weighted,
    
    employment_average_rank =
      employment_average_rank_weighted,
    
    employment_average_rank_rank =
      employment_average_rank_rank_weighted,
    
    employment_rank_decile =
      employment_rank_decile_weighted,
    
    employment_average_score =
      employment_average_score_weighted,
    
    employment_score_rank =
      employment_score_rank_weighted,
    
    employment_score_decile =
      employment_score_decile_weighted,
    
    education_average_rank =
      education_average_rank_weighted,
    
    education_average_rank_rank =
      education_average_rank_rank_weighted,
    
    education_rank_decile =
      education_rank_decile_weighted,
    
    education_average_score =
      education_average_score_weighted,
    
    education_score_rank =
      education_score_rank_weighted,
    
    education_score_decile =
      education_score_decile_weighted,
    
    health_average_rank =
      health_average_rank_weighted,
    
    health_average_rank_rank =
      health_average_rank_rank_weighted,
    
    health_rank_decile =
      health_rank_decile_weighted,
    
    health_average_score =
      health_average_score_weighted,
    
    health_score_rank =
      health_score_rank_weighted,
    
    health_score_decile =
      health_score_decile_weighted,
    
    crime_average_rank =
      crime_average_rank_weighted,
    
    crime_average_rank_rank =
      crime_average_rank_rank_weighted,
    
    crime_rank_decile =
      crime_rank_decile_weighted,
    
    crime_average_score =
      crime_average_score_weighted,
    
    crime_score_rank =
      crime_score_rank_weighted,
    
    crime_score_decile =
      crime_score_decile_weighted,
    
    barriers_average_rank =
      barriers_average_rank_weighted,
    
    barriers_average_rank_rank =
      barriers_average_rank_rank_weighted,
    
    barriers_rank_decile =
      barriers_rank_decile_weighted,
    
    barriers_average_score =
      barriers_average_score_weighted,
    
    barriers_score_rank =
      barriers_score_rank_weighted,
    
    barriers_score_decile =
      barriers_score_decile_weighted,
    
    living_average_rank =
      living_average_rank_weighted,
    
    living_average_rank_rank =
      living_average_rank_rank_weighted,
    
    living_rank_decile =
      living_rank_decile_weighted,
    
    living_average_score =
      living_average_score_weighted,
    
    living_score_rank =
      living_score_rank_weighted,
    
    living_score_decile =
      living_score_decile_weighted
  )

# Final IMD2015 dataset with replacements
imd_2015_final_merged <- bind_rows(
  imd_2015_unmerged,
  imd_2015_MSOA21_replace
) %>%
  arrange(MSOA11CD)



#Saving useful files

# Keep only the score variables

keep_scores <- function(data) {
  
  data %>%
    select(
      
      # Geography
      matches("^(MSOA|LAD)"),
      
      # Keep only the average scores
      ends_with("average_score")
      
    )
  
}

# Weighted replacement MSOAs only

imd_2015_final_merged <- imd_2015_final_merged %>%
  keep_scores()

#Not merged 
imd_2015_final_msoa <- imd_2015_results$msoa %>%
  keep_scores()

# 2025 MSOA

imd_2025_final_msoa <- imd_2025_results$msoa %>%
  keep_scores()

# 2025 LAD

imd_2025_final_lad <- imd_2025_results$lad %>%
  keep_scores()

# 2015 LAD

imd_2015_final_lad <- imd_2015_results$lad %>%
  keep_scores()

# Save outputs
setwd("~/Analysis and Modelling general/2011-2021 HLE by MSOA/Working files")

write_csv(imd_2015_final_msoa,
          "imd_2015_final_msoa.csv")

write_csv(imd_2015_final_merged,
          "imd_2015_final_merged.csv")

write_csv(imd_2025_final_msoa,
          "imd_2025_final_msoa.csv")

write_csv(imd_2025_final_lad,
          "imd_2025_final_lad.csv")

write_csv(imd_2015_final_lad,
          "imd_2015_final_lad.csv")



