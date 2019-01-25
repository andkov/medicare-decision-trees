# this script imports the raw data described in this shared document 
# https://drive.google.com/file/d/10idMxy8eX8nTHr6wr2Q40x4XOP3Y5ck7/view
# and prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched-output/0-greeter.md"
# )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2"                 )
requireNamespace("readr"                   )
requireNamespace("tidyr"                   )
requireNamespace("dplyr"                   ) #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"                  ) #For asserting conditions meet expected patterns.

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data-unshared/raw/pt_with_locations_reduced.csv"

# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv(file = path_input)

# ---- tweak-data ------------------------------
ds <- ds0
# transform variable names
names(ds) <- gsub(" |-", "_", names(ds))
names(ds) <- gsub("#", "n", names(ds))
names(ds) <- gsub("-", "n", names(ds))
names(ds) <- gsub("\\(|\\)", "", names(ds))
names(ds) <- tolower(names(ds))

# readr::write_csv(as.data.frame(names(ds)), "./data-unshared/derived/rawnames.csv")
ds %>% dplyr::glimpse(50)  

# renames some of the variables 

ds <- ds %>% 
  dplyr::rename_(
    "average_hcc_risk_score"  = "average_hcc_risk_score_of_beneficiaries"
    ,"non_metropolitan_area"  = "non_metropolitan_area_or_missing_9_counties_missing"
    ,"mean_age_benef_county"  = "medicare_ffs_benef_average_age_fee_for_service_2014"
    ,"mean_hcc_benef_county"  = "medicare_ffs_bene_avg_hcc_score__fee_for_service_2014"
    ,"medicare_cost"          = "standardized_risk_adjusted_per_capita_medicare_costs"
    ,"total_medicare_payment" = "total_medicare_standardized_payment_amount"
  )

# ---- group-variables ---------------------------------------
practice <- c(
 "female" # Gender of the Physical Therapist                                               
 ,"dpt"# Reporting DPT degree                                              
 ,"number_of_hcpcs"# Number of HCPCS/CPT codes billed 
 #  HealthCare Common Procedure System/Current Procedural terminology
 ,"number_of_medicare_beneficiaries"  # Number of beneficiaries served
 ,"chrg_allowed_amt_ratio" # Charge to Medicare allowed amount ratio                         
 ,"medicare_stnd_amt_bene" # Average Medicare standardized payment amount per beneficiary                               
 ,"physical_agent_pct" # Proportion of physical agents 
 ,"proxy_for_n_of_new_patients"  # Number of new patients                        
 ,"average_age_of_beneficiaries" # Average age of beneficiaries                        
 # ,"average_hcc_risk_score_of_beneficiaries" # Average HCC risk score of beneficiaries              
 ,"average_hcc_risk_score"                    # Average HCC risk score of beneficiaries              
 
 ,"large_metro_area"                                     
 ,"mid_sized_metro_area"                                 
 ,"small_metro_area"                                     
 # ,"non_metropolitan_area_or_missing_9_counties_missing"  
 ,"non_metropolitan_area"  
 )

market <- c(
 "pcp_per_10k_pop_14" # Primary care physicians per 10,000 population, county level                                   
,"pt_per_10k_pop_09" # PTs per 10,000 population (2009), county level                                   
# ,"medicare_ffs_benef_average_age_fee_for_service_2014" # Average age of beneficiaries, county level 
,"mean_age_benef_county"                               # Average age of beneficiaries, county level 
,"pct_mdcr_ffs_benef_female_14" # Percent of female beneficiaries, county level                         
# ,"medicare_ffs_bene_avg_hcc_score__fee_for_service_2014" # Average HCC risk score of beneficiaries, county level
,"mean_hcc_benef_county"                                 # Average HCC risk score of beneficiaries, county level
,"pct_mdcr_benef_elig_medcaid_14" # Percent of beneficiaries eligible for Medicaid, county level                      
,"pct_mdcr_ff_benef_pop_14" # Beneficiaries as a share of total population, county level                           
 # ,"standardized_risk_adjusted_per_capita_medicare_costs" # Standardized Risk-Adjusted Per Capita Medicare Costs, county level 
 ,"medicare_cost"                                        # Standardized Risk-Adjusted Per Capita Medicare Costs, county level 
,"median_household_income__2014" # Median Household Income, county level                        
,"pct_65older_in_deep_poverty_14" # Percent of persons 65 or older in deep poverty, county level                       
,"pt_bene_ratio"  # PTs per 10,000 beneficiaries, county level                                      
)

outcome <- c(
 "number_of_services"   # Number of services performed                                
# ,"total_medicare_standardized_payment_amount" # Total Medicare standardized payment amount   
,"total_medicare_payment"                       # Total Medicare standardized payment amount   
)

unaccounted <- c("therapeutic_pct")
# sort the variables according to the level
# ds <- ds %>% dplyr::select(c(practice, market, outcome, unaccounted))
# store for possible tweaking in spreadsheets
# readr::write_csv(as.data.frame(names(ds1)), "./data-unshared/derived/rawnames.csv")



# ----- marginals --------------------
 
# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------
ds %>% 
  dplyr::select(c(practice, market, outcome)) %>% 
  saveRDS("./data-unshared/derived/dto-0-greeted.rds")




