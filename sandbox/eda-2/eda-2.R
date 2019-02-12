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
path_input <- "./data-unshared/derived/dto-0-greeted.rds"

practice_vars <- c(
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
  ,"mid_metro_area"                                 
  # ,"mid_sized_metro_area"                                 
  ,"small_metro_area"                                     
  # ,"non_metropolitan_area_or_missing_9_counties_missing"  
  ,"non_metro_area"  
)

market_vars <- c(
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
  ,"median_household_income_2014" # Median Household Income, county level                        
  ,"pct_65older_in_deep_poverty_14" # Percent of persons 65 or older in deep poverty, county level                       
  ,"pt_bene_ratio"  # PTs per 10,000 beneficiaries, county level                                      
)

outcome_vars <- c(
  "number_of_services"   # Number of services performed                                
  # ,"total_medicare_standardized_payment_amount" # Total Medicare standardized payment amount   
  ,"total_medicare_payment"                       # Total Medicare standardized payment amount   
)


library(dplyr)
# ---- load-data ---------------------------------------------------------------
ds0 <- readRDS(file = path_input)

# ---- tweak-data ------------------------------
ds <- ds0

ds %>% dplyr::glimpse(80)  

metro_area_hot_one_vars <-  c("large_metro_area","mid_metro_area","small_metro_area","non_metro_area")
ds1 <- ds %>% 
  tidyr::gather("key","value",metro_area_hot_one_vars ) %>% 
  dplyr::mutate(
    metro_area = gsub("(\\w+)_metro_area", "\\1", key)
  ) %>% 
  dplyr::select(-key, -value) 
  
 
ds1 %>% glimpse()


# ----- pairs --------------------

ds2 <- ds1 %>% 
  dplyr::select_(.dots = c("total_medicare_payment", setdiff(practice_vars, metro_area_hot_one_vars), "metro_area") )

g1 <- ds2 %>% 
  dplyr::mutate(
    female = as.factor(female)
    ,dpt = as.factor(dpt)
  ) %>% 
  dplyr::sample_frac(size = .01) %>% 
  GGally::ggpairs() 

quick_save <- function(g,name){
  ggplot2::ggsave(
    filename = paste0(name,".png"), 
    plot     = g,
    device   = png,
    path     = "./sandbox/eda-2/prints/1/", # female marital educ poor_healt
    width    = 1600,
    height   = 1200,
    # units = "cm",
    dpi      = 200,
    limitsize = FALSE
  )
}

g1 <- g1 + 
g1 %>% quick_save("practice")
  
# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------------
path_report_1 <- "./sandbox/eda-1/eda-1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}



