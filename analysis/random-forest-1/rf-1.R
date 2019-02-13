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
base::source("./scripts/graphing/graph-missing.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
# library(mmpipe)
library(ggplot2)
library(ggRandomForests)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
# requireNamespace("ggplot2"                 )
requireNamespace("readr"                   )
requireNamespace("tidyr"                   )
requireNamespace("dplyr"                   ) #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"                  ) #For asserting conditions meet expected patterns.

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data-unshared/derived/dto-0-greeted.rds"
path_input_meta <- "./data-public/raw/meta-2019-01-15.xlsx"
# ---- load-data ---------------------------------------------------------------
ds0 <- readRDS(file = path_input)
ds_meta <- readxl::read_excel(path_input_meta)


# ---- tweak-data ------------------------------
ds <- ds0

ds %>% dplyr::glimpse(50)  

# create a categorical variable for metro area for graphing
# metro_area_hot_one_vars <-  c("m01_large","m02_mid","m03_small","m04_nometro")
# ds1 <- ds %>% 
#   tidyr::gather("key","value",metro_area_hot_one_vars ) %>% 
#   dplyr::mutate(
#     m01_metro = gsub("m\\d+_(\\w+)", "\\1", key)
#   ) %>% 
#   dplyr::select(-key, -value) 
# ds1 %>% dplyr::glimpse()
# add the newly created variable back to the dataset
# ds2 <- dplyr::left_join(
#   ds
#   ,ds1
#   ) 
# ds2 %>% dplyr::glimpse()

sorted_names <- names(ds)

# ----- local-functions --------------------
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".png"), 
    plot     = g,
    device   = png,
    path     = "./analysis/random-forest-1/prints/1/", # female marital educ poor_healt
    # width    = width,
    # height   = height,
    # units = "cm",
    # dpi      = 50,
    limitsize = FALSE,
    ...
  )
}

# ---- define-utility-functions ---------------
outcome <- "n_services"
# outcome <- "total_payment"

(var_practice <- grep("p\\d+_\\w+",names(ds), value = TRUE))
(var_practice <- setdiff(var_practice, "p07_thera")) # because it's inverse of p06_agents
(var_market   <- grep("m\\d+_\\w+",names(ds), value = TRUE))
(var_market <- setdiff(var_market, "m01_metro")) # because we will use one-hot transforms

ds %>% dplyr::glimpse()
ds_rf1 <- ds %>% dplyr::select_(.dots = c("total_payment", var_practice, var_market)) %>% 
# ds_rf1 <- ds2 %>% dplyr::select_(.dots = c("total_payment", var_practice, var_market)) %>% 
  dplyr::sample_frac(.1) %>% 
  # dplyr::filter(!is.na(total_payment) ) %>%
  as.data.frame()
ds_rf1 %>% dplyr::glimpse()

ds_rf1 <- MASS::Boston
# ds_rf1 %>% dplyr::glimpse()

# ds %>% as.data.frame() %>% dplyr::colMeans(is.na(.))
# ds %>%   dplyr::summarise_all(dplyr::funs(100*mean(is.na(.)))) %>% t()

# rf_pt <- randomForestSRC::rfsrc(total_payment ~ ., data = ds_rf1)
rf_pt <- randomForestSRC::rfsrc(
  # formula    = total_payment ~ .
  formula    = medv ~ .
  ,data      = ds_rf1
  # ,ntree     = 1000
  # ,mtry      = 5
  # ,nodesize  = 5
  # ,splitrule = "mse"
  # ,na.action = "na.impute" # does not seem necessary as data has no missing data
  
)

# printing the model object
rf_pt %>% print()
# rf_pt %>% randomForestSRC::print.rfsrc()

# error estimates
gg_e <- ggRandomForests::gg_error(rf_pt)
gg_e %>% plot()
# ---- save-to-disk ----------------------------

# replicate the example
library(ggRandomForests)
library(randomForrestSRC)
rfsrc_Boston <- rfsrc(medv~., data=MASS::Boston)
rfsrc_Boston %>% print()
gg_e <- gg_error(rfsrc_Boston)
data(rfsrc_Boston)
plot(gg_e)

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



