# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# This script reads two files: patient event table + location map. 
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
requireNamespace("tree", quietly=TRUE) # for regression and classification trees
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
# source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")        # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions

# ---- load-data -------------------------------------------------------------
ds <-  MASS::Boston

set.seed(1)
train_rows <- sample(1:nrow(ds), nrow(ds)/2)  

# ---- inspect-data -------------------------------------------------------------
ds %>% dplyr::glimpse()


ds_train <- ds[ train_rows, ]
ds_test  <- ds[-train_rows, ]

# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------

# ----- basic-model -------------------------------------------------------------
tree_boston <- tree::tree(medv ~ ., ds_train)
summary(tree_boston)
plot(tree_boston)
text(tree_boston, pretty = 2)

# diagnose where to prune
cv_boston <- tree::cv.tree(object = tree_boston)
cv_boston
plot(cv_boston$size, cv_boston$dev, type = "b")

# now prune
prune_boston <- tree::prune.tree(
  tree = tree_boston

  ,best = 5
  )
plot(prune_boston)
text(prune_boston, pretty = 0)


# predict
yhat <- stats::predict(tree_boston, newdata = ds_test )
plot(yhat, ds_test$medv)
abline(0,1)
mean( (yhat - ds_test$medv)^2)

### Bagging

set.seed(1)
bag_boston <- randomForest::randomForest(
  medv ~ .
  ,data = ds_train
  ,mtry = 13 # predictors considered at each split
  ,importance = TRUE
)
bag_boston
# evaluate performance
yhat_bag <- stats::predict(bag_boston, newdata = ds_test)
plot(yhat_bag, ds_test$medv)
mean( (yhat_bag - ds_test$medv)^2 )

# add ntree
set.seed(1)
bag_boston <- randomForest::randomForest(
  medv ~ .
  ,data = ds_train
  ,mtry = 13 # predictors considered at each split
  ,importance = TRUE
  ,ntree = 25
)
bag_boston
# evaluate performance
yhat_bag <- stats::predict(bag_boston, newdata = ds_test)
plot(yhat_bag, ds_test$medv)
mean( (yhat_bag - ds_test$medv)^2 )



### Random Forest
rf_boston <- randomForest::randomForest(
  formula = medv ~ .
  ,data = ds_train
  ,mtry = 6
  ,importance = TRUE
)
yhat_rf <- stats::predict(rf_boston, newdata = ds_test)
mean( (yhat_rf - ds_test$medv)^2 )

randomForest::importance(rf_boston) 
randomForest::varImpPlot(rf_boston)

# Sonata form report structure
# ---- dev-a-0 ---------------------------------
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

