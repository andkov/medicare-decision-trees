rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

library(ggRandomForests)
library(randomForestSRC)
library(randomForest)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

data(Boston, package="MASS")
Boston$chas <- as.logical(Boston$chas)

Boston %>% glimpse()

rfsrc_Boston <- randomForest::randomForest(medv~., data=Boston)
# rfsrc_Boston <- randomForestSRC::rfsrc(medv~., data=Boston)
# data(rfsrc_Boston)
print(rfsrc_Boston)
gg_e <- gg_error(rfsrc_Boston)
plot(gg_e)

plot(gg_rfsrc(rfsrc_Boston), alpha=.5)+
   coord_cartesian(ylim=c(5,49))

# plot(gg_vimp(rfsrc_Boston), lbls=st.labs)
plot(gg_vimp(rfsrc_Boston))

varsel_Boston <- var.select(rfsrc_Boston)
gg_md <- gg_minimal_depth(varsel_Boston)
plot(gg_md)

plot.gg_minimal_vimp(gg_md)
plot(gg_minimal_vimp(varsel_Boston))
