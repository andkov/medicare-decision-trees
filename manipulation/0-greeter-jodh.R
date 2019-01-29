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
names(ds) <- gsub(" |-", "_", names(ds))
names(ds) <- gsub("#", "n", names(ds))
names(ds) <- gsub("-", "n", names(ds))
names(ds) <- gsub("\\(|\\)", "", names(ds))

ds %>% dplyr::glimpse(50)  

# ----- marginals --------------------
 
ds %>% TabularManifest::histogram_discrete("female"                                               )
ds %>% TabularManifest::histogram_discrete("dpt"                                                  )
ds %>% TabularManifest::histogram_continuous("Number_of_HCPCS"                                      )
ds %>% TabularManifest::histogram_continuous("Number_of_Medicare_Beneficiaries"                     )
ds %>% TabularManifest::histogram_continuous("chrg_allowed_amt_ratio"                               )
ds %>% TabularManifest::histogram_continuous("medicare_stnd_amt_bene"                               )
ds %>% TabularManifest::histogram_continuous("physical_agent_pct"                                   )
ds %>% TabularManifest::histogram_continuous("therapeutic_pct"                                      )
ds %>% TabularManifest::histogram_continuous("proxy_for_n_of_new_patients"                          )
ds %>% TabularManifest::histogram_continuous("Average_Age_of_Beneficiaries"                         )
ds %>% TabularManifest::histogram_continuous("Average_HCC_Risk_Score_of_Beneficiaries"              )
ds %>% TabularManifest::histogram_continuous("Mid_sized_metro_area"                                 )
ds %>% TabularManifest::histogram_continuous("Small_metro_area"                                     )
ds %>% TabularManifest::histogram_continuous("Non_metropolitan_area_or_missing_9_counties_missing"  )
ds %>% TabularManifest::histogram_continuous("Large_metro_area"                                     )
ds %>% TabularManifest::histogram_continuous("Standardized_Risk_Adjusted_Per_Capita_Medicare_Costs" )
ds %>% TabularManifest::histogram_continuous("pcp_per_10k_pop_14"                                   )
ds %>% TabularManifest::histogram_continuous("pt_per_10k_pop_09"                                    )
ds %>% TabularManifest::histogram_continuous("pct_mdcr_FF_benef_pop_14"                             )
ds %>% TabularManifest::histogram_continuous("Medicare_FFS_Benef_Average_Age_Fee_for_Service_2014"  )
ds %>% TabularManifest::histogram_continuous("pct_mdcr_FFS_Benef_Female_14"                         )
ds %>% TabularManifest::histogram_continuous("Medicare_FFS_Bene_Avg_HCC_Score__Fee_for_Service_2014")
ds %>% TabularManifest::histogram_continuous("pct_mdcr_Benef_Elig_Medcaid_14"                       )
ds %>% TabularManifest::histogram_continuous("Median_Household_Income__2014"                        )
ds %>% TabularManifest::histogram_continuous("pct_65older_in_Deep_Poverty_14"                       )
ds %>% TabularManifest::histogram_continuous("pt_bene_ratio"                                        )
ds %>% TabularManifest::histogram_continuous("Number_of_Services"                                   )
ds %>% TabularManifest::histogram_continuous("Total_Medicare_Standardized_Payment_Amount"           )

# ----- pairs -----------------------------------
selected_columns <- c(
"Total_Medicare_Standardized_Payment_Amount" 
,"Number_of_Services" 
,"proxy_for_n_of_new_patients" 
,"Number_of_Medicare_Beneficiaries"
,"chrg_allowed_amt_ratio"   
,"Number_of_HCPCS" 
)
ds %>% 
  dplyr::select(selected_columns) %>% 
   GGally::ggpairs()

# ---- save-to-disk ----------------------------





