`./sandbox/popdatabc-tutorial/` Directory
=========

This folder contains support files for the tutorial administered by PopDataBC on 2019-02-14.


During the session, we will predict 30-day hospital readmission using discharge data for 10,000 patients discharged from hospitals in Montreal. 


1.  This session will require the use of two different packages: `ROCR` and `randomForest`. Please install these packages if you wish to follow along. 

2.  Suggested pre-reading: Chapter 9 and 15 of Elements of Statistical Learning. Online text can be found here:  https://web.stanford.edu/~hastie/Papers/ESLII.pdf
 
3.  R code and Readmission data (files and related details attached)
- [./sandbox/module3.R][module3.R]
 
Note: The data file provided has been compressed in the gz format. It is about 100 MB uncompressed. If you want to just “take a look at it” before you begin, we recommend using R because it will 
automatically uncompress it.
 
Just run the line noted below and you’ll automatically load the entire data frame.
 df <- read.csv("readmissions.csv.gz")
 

