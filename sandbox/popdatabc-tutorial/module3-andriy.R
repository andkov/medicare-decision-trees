library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)

# AK : I will break the original scripts into chunks interrupted by my comments and adjustments

# ---- original-1 -----------------------------
# the original starts here 
df <- read.csv("./sandbox/popdatabc-tutorial/readmissions.csv.gz")

# ---- comment-1 ------------------------------
# first interruption to study the data set

df %>% pryr::object_size()
df %>% glimpse() # too many variables to comfortably glimpse at
dim(df) # holy cow, 5095+ variables
names(df)
# subset variables into groups for easier management
var_stem <-  c(
  "day_30_readmit"  ,"admission_type"  ,"transfers"      
  ,"los"            ,"hosp"            ,"sex"            
  ,"age"            ,"day_30_readmits" ,"dow"            
  ,"month"          ,"admit.diag.mdc"  ,"id_dat" 
)   

# looks like the rest of variables have three unique prefexes:
# procedure, diagnosis, drug
var_nonstem    <- setdiff(names(df), var_stem)
(var_procedure <- grep("^procedure.\\d+$", var_nonstem, value = TRUE))
(var_diagnosis <- grep("^diagnosis.\\d+$", var_nonstem, value = TRUE))
(var_drug      <- grep("^drug.\\d+$", var_nonstem, value = TRUE))
# check that we have accounted for all the variables

(var_count_total     <- df %>% names() %>% length()) # 5095
(var_count_stem      <- var_stem       %>% length()) # 12
(var_count_procedure <- var_procedure  %>% length()) # 757
(var_count_diagnosis <- var_diagnosis  %>% length()) #3566
(var_count_drug      <- var_drug       %>% length()) #760
# should be equal to 5096
var_count_stem + var_count_procedure + var_count_diagnosis + var_count_drug


# explore with marginals to understand the scales of the variables
df_small <- df %>% dplyr::sample_frac(.05)
df_small %>% TabularManifest::histogram_discrete("day_30_readmit")
df_small %>% TabularManifest::histogram_discrete("admission_type")
df_small %>% TabularManifest::histogram_discrete("transfers")
df_small %>% TabularManifest::histogram_continuous("los") # length of stay
df_small %>% TabularManifest::histogram_discrete("hosp") # 20 unique values
df_small %>% TabularManifest::histogram_discrete("sex")
df_small %>% TabularManifest::histogram_continuous("age")
df_small %>% TabularManifest::histogram_discrete("dow") # day of the week
df_small %>% TabularManifest::histogram_discrete("month")
df_small %>% TabularManifest::histogram_discrete("admit.diag.mdc")
# df_small %>% TabularManifest::histogram_continuous("id_dat")# factor, breaks

# explore with a basic model
model1 <- glm(
  day_30_readmit ~ . 
  # ,family = binomial(link="logit")
  , data = df_small %>% select_(.dots = setdiff(var_stem,c("id_dat", "hosp"))) 
) 
source('./scripts/modeling/model-basic.R')
model1 %>% basic_model_info()
model1 %>% make_result_table()
# end of first comment/interruption

# ---- original-2 -----------------------------
# the original script continues

df$day_30_readmit <- factor(df$day_30_readmit)
df$id_dat <- NULL # Remove so that the formula is easier.

# A first model on the first 99 variables.
set.seed(1)
model <- randomForest(day_30_readmit ~ ., data = df[1:100], ntree = 500, do.trace = TRUE)

# Doesn't look like the best
model
# What is the vote percentage?
predictions <- model$votes[ ,"TRUE"]
mean(predictions) # Averaging across discharges, 6% of trees vote for readmission.

# ---- comment-2 ------------------ 
gg_e <- ggRandomForests::gg_error(model)
plot(gg_e)

# gg_md <- ggRandomForests::gg_minimal_depth(model) 
# gg_minimal_depth is not yet support for randomForest objects
# plot(gg_md)

# ---- original-3 -----------------------------
library(ROCR)
predictions <- model$votes[ ,"TRUE"]
pred <- ROCR::prediction(predictions, df$day_30_readmit)
perf_AUC <- ROCR::performance(pred, "auc") 
perf_AUC@y.values[[1]]

perf_ROC <- performance(pred, "tpr", "fpr")
plot(perf_ROC)

# Let's try getting using the Pearson correlation coefficient.

# Separate the "other variables" from the binary variables.
first.proc <- match("procedure.1", colnames(df))
other.vars <- df[1:(first.proc - 1)]
binary.vars <- df[first.proc:ncol(df)]

correlations <- cor(as.numeric(df$day_30_readmit), binary.vars)
# Why the error?
# Are any empty?

table(colSums(binary.vars) != 0)
# 611 are always empty!
# (Does it matter? Shouldn't random forest take care of this?)

# Let's remove those to decrease computational burden.
binary.vars <- binary.vars[colSums(binary.vars) != 0]
correlations <- cor(as.numeric(df$day_30_readmit), binary.vars)

# Let's take a look at the most correlated.
tail(sort(abs(correlations)))

# Use the top 100
top.100.binary.vars <- binary.vars[,tail(order(abs(correlations)), 100)]

# Which variables were selected?
tokens <- strsplit(names(top.100.binary.vars), "\\.")
sapply(tokens, `[[`, 1) # What is this magic? sapply(tokens, function(x) x[[1]])
table(sapply(tokens, `[[`, 1))

# Make a new data.frame.
new.df <- cbind(other.vars, top.100.binary.vars)

# Run a new model using the top 100
set.seed(1)
model2 <- randomForest(day_30_readmit ~ ., data = new.df, ntree = 200, do.trace = TRUE)

# Make a little function that plots the ROC and returns the AUC.
plot.roc <- function(model) {
  predictions <- model$votes[ ,"TRUE"]
  pred <- prediction(predictions, df$day_30_readmit)
  perf_AUC <- performance(pred, "auc") 
  perf_ROC <- performance(pred, "tpr", "fpr")
  plot(perf_ROC)
  perf_AUC@y.values[[1]]
}

plot.roc(model2)

# That is much better!
# How would growing more trees help?
set.seed(1)
model3 <- randomForest(day_30_readmit ~ ., data = new.df, ntree = 500, do.trace = TRUE)

plot.roc(model2) # 0.656
plot.roc(model3) # 0.666

# Virtually meaningless.
# What about adding more variables?

top.300.binary.vars <- binary.vars[,tail(order(abs(correlations)), 300)]
new.df.300 <- cbind(other.vars, top.300.binary.vars)
set.seed(1)
model4 <- randomForest(day_30_readmit ~ ., data = new.df.300, ntree = 200, do.trace = TRUE)

# A slight increase
plot.roc(model4) # 0.675

# We haven't seen any trees in the forest!
getTree(model4, k = 1, labelVar = TRUE)
# Not easy to "see". 

# So then which variables are important?
varImpPlot(model4)


