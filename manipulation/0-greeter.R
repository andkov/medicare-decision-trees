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
requireNamespace("readxl")
# ---- declare-globals ---------------------------------------------------------
path_input <- "./data-unshared/raw/pt_with_locations_reduced.csv"
path_input_meta <- "./data-public/raw/meta-2019-02-12.xlsx"
# ---- load-data ---------------------------------------------------------------
ds0 <-  readr::read_csv(file = path_input)
ds_meta <- readxl::read_excel(path_input_meta)

# ---- tweak-data ------------------------------
ds <- ds0
# transform variable names
names(ds) <- gsub(" |-", "_", names(ds))
names(ds) <- gsub("#", "n", names(ds))
names(ds) <- gsub("-", "n", names(ds))
names(ds) <- gsub("\\(|\\)", "", names(ds))
names(ds) <- gsub("__", "_", names(ds)) # double underscore into one
names(ds) <- tolower(names(ds))

# store the original variable names to be used in metadata 
# variable_names <- names(ds)
# readr::write_csv(as.data.frame(variable_names), "./data-unshared/derived/rawnames.csv")
ds %>% dplyr::glimpse(50)

ds_meta %>% dplyr::glimpse(50)

# renames some of the variables for easier handling
# d_names_old <- names(ds) %>% tibble::as.tibble()
# d_names_both <- d_names_old %>% 
#   dplyr::left_join(
#     ds_meta
#     ,by = c("value" = "name_old")
#   )
# v_names_new <- d_names_both %>% dplyr::select(name_short) %>% 
#   as.list() %>% unlist() %>% as.character()
# names(ds) <- v_names_new

# a more elegant way of doing the same thing
names(ds) <- 
  dplyr::left_join(
    names(ds) %>% tibble::as.tibble()
    , ds_meta
    , by = c("value" = "name_old")
  ) %>% 
  dplyr::select(name_short) %>% 
  as.list() %>% unlist() %>% as.character()

ds %>% dplyr::glimpse() 

# we have identified that m06_agents and m07_thera are just inverses of each other
# use only one during the analysis, they are perfectly correlated

# ---- save-to-disk ----------------------------

ds %>% 
  saveRDS("./data-unshared/derived/dto-0-greeted.rds")




