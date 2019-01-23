# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# This script reads two files: patient event table + location map. 
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("readr"  )
requireNamespace("tidyr"  )
requireNamespace("dplyr"  ) #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit" ) #For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
# source("./manipulation/object-glossary.R")   # object definitions
# source("./scripts/common-functions.R")        # reporting functions and quick views
# source("./scripts/graphing/graph-presets.R") # font and color conventions
source("./scripts/modeling/model-basic.R") # basic model inspection function
source("./scripts/modeling/model-best-subset.R") # best subset functions

# ---- load-globals ---------------------------------------------------------
path_input <- c("./data-unshared/derived/dto-0-greeted.rds")

practice <- c(
   "female" # Gender of the Physical Therapist                                               
  ,"dpt"# Reporting DPT degree                                              
  ,"number_of_hcpcs"# Number of HCPCS/CPT codes billed  #  HealthCare Common Procedure System/Current Procedural terminology
  ,"number_of_medicare_beneficiaries"  # Number of beneficiaries served
  ,"chrg_allowed_amt_ratio" # Charge to Medicare allowed amount ratio                         
  ,"medicare_stnd_amt_bene" # Average Medicare standardized payment amount per beneficiary                               
  ,"physical_agent_pct" # Proportion of physical agents 
  ,"proxy_for_n_of_new_patients"  # Number of new patients                        
  ,"average_age_of_beneficiaries" # Average age of beneficiaries                        
  ,"average_hcc_risk_score_of_beneficiaries" # Average HCC risk score of beneficiaries              
  ,"large_metro_area"                                     
  ,"mid_sized_metro_area"                                 
  ,"small_metro_area"                                     
  ,"non_metropolitan_area_or_missing_9_counties_missing"  
)

market <- c(
   "pcp_per_10k_pop_14" # Primary care physicians per 10,000 population, county level                                   
  ,"pt_per_10k_pop_09" # PTs per 10,000 population (2009), county level                                   
  ,"medicare_ffs_benef_average_age_fee_for_service_2014" # Average age of beneficiaries, county level 
  ,"pct_mdcr_ffs_benef_female_14" # Percent of female beneficiaries, county level                         
  ,"medicare_ffs_bene_avg_hcc_score__fee_for_service_2014" # Average HCC risk score of beneficiaries, county level
  ,"pct_mdcr_benef_elig_medcaid_14" # Percent of beneficiaries eligible for Medicaid, county level                      
  ,"pct_mdcr_ff_benef_pop_14" # Beneficiaries as a share of total population, county level                           
  ,"standardized_risk_adjusted_per_capita_medicare_costs" # Standardized Risk-Adjusted Per Capita Medicare Costs, county level 
  ,"median_household_income__2014" # Median Household Income, county level                        
  ,"pct_65older_in_deep_poverty_14" # Percent of persons 65 or older in deep poverty, county level                       
  ,"pt_bene_ratio"  # PTs per 10,000 beneficiaries, county level                                      
)
# ---- load-data -------------------------------------------------------------
ds <- readRDS(path_input)

# ---- inspect-data -------------------------------------------------------------
ds %>% dplyr::glimpse()

# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------

# Sonata form report structure
# ---- dev-a-0 ---------------------------------
# Define a general function
estimate_best_subset <- function(data, predictors, eq_formula, level, method, plotty, includeobjects){
  # eq_formula <- as.formula(paste0(local_stem, predictors))
  best_subset_local <- glmulti::glmulti(
    eq_formula
    ,data           = data
    ,level          = level           # 1 = No interaction considered
    ,method         = method            # Exhaustive approach
    ,crit           = "aicc"            # AIC as criteria
    ,confsetsize    = 100         # Keep 5 best models
    ,plotty = plotty, report = T  # No plot or interim reports
    ,fitfunction    = "glm"     # glm function
    ,family         = binomial       # binomial family for logistic regression family=binomial(link="logit")
    ,includeobjects = includeobjects
  )
}

outcome_var   <- "total_medicare_standardized_payment_amount ~ "
predictor_var <- "1 + 
female +
dpt +
number_of_hcpcs +
number_of_medicare_beneficiaries +
chrg_allowed_amt_ratio +
medicare_stnd_amt_bene +
physical_agent_pct +
proxy_for_n_of_new_patients +
average_age_of_beneficiaries +
average_hcc_risk_score_of_beneficiaries +
large_metro_area +
mid_sized_metro_area +
small_metro_area +
non_metropolitan_area_or_missing_9_counties_missing +
pcp_per_10k_pop_14 +
pt_per_10k_pop_09 +
medicare_ffs_benef_average_age_fee_for_service_2014 +
pct_mdcr_ffs_benef_female_14 +
medicare_ffs_bene_avg_hcc_score__fee_for_service_2014 +
pct_mdcr_benef_elig_medcaid_14 +
pct_mdcr_ff_benef_pop_14 +
standardized_risk_adjusted_per_capita_medicare_costs +
median_household_income__2014 +
pct_65older_in_deep_poverty_14 +
pt_bene_ratio 
"

(eq_formula <- as.formula(paste0(outcome_var, predictor_var)))

modelA <-  glm(eq_formula, data = ds, family = gaussian(link="identity"))

modelA %>%  basic_model_info()

modelA %>% make_result_table()
  
best_subset_local <- glmulti::glmulti(
  eq_formula
  ,data           = ds
  ,level          = 1           # 1 = No interaction considered
  ,method         = method            # Exhaustive approach
  ,crit           = "aicc"            # AIC as criteria
  ,confsetsize    = 100         # Keep 5 best models
  ,plotty = plotty, report = T  # No plot or interim reports
  ,fitfunction    = "glm"     # glm function
  ,family         = binomial       # binomial family for logistic regression family=binomial(link="logit")
  ,includeobjects = includeobjects
)

# ---- dev-a-1 ---------------------------------
# ---- dev-a-2 ---------------------------------
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- dev-b-0 ---------------------------------
# ---- dev-b-1 ---------------------------------
# ---- dev-b-2 ---------------------------------
# ---- dev-b-3 ---------------------------------
# ---- dev-b-4 ---------------------------------
# ---- dev-b-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

