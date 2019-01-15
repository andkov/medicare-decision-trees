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
variables_discrete   <- c(
  "female"
  ,"dpt"
  , "Small_metro_area"
  ,"Large_metro_area"
  ,"Mid_sized_metro_area"
  ,"Non_metropolitan_area_or_missing_9_counties_missing"
)
variables_continuous <- setdiff(names(ds), variables_discrete) 

for(variable_i in names(ds)){
  cat("\n##", variable_i,"\n")
  cat("\n")
  if(variable_i %in% variables_discrete){
    ds %>% TabularManifest::histogram_discrete(variable_i) %>% print()
  }else{
    ds %>% TabularManifest::histogram_continuous(variable_i) %>% print()
  }
  cat("\n")
}
  
  
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



