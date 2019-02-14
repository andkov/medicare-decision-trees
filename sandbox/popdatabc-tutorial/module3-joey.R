library(randomForest)

df <- read.csv("./data/readmissions.csv.gz")
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

library(ROCR)
predictions <- model$votes[ ,"TRUE"]

pred <- prediction(predictions, df$day_30_readmit)

perf_AUC <- performance(pred, "auc") 
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


