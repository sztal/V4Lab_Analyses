##########################################################
### Scaling of the KNOWELDGE item set in the CZ sample ###
##########################################################

### The scaling procedure that is presented here uses the Mokken Scale technique, which is an non-parametric IRT technique that allows prodcution of high-quality ordinal scales. Mokken scales are useful because they posses two very important properties: monotonous homogeneity and double monotonicity. Monotonous homogeneity means that in a proper Mokken scale it is assured that an intenisty of a respondent's trait (in this case it would be economic/financial knowledge) is positively correlated with a probability of he or she giving the correct anwer to a question. It ensures correct ordinal measurement of a trait in which the sum of the correct answers is a sufficient estimator of the trait.
### Double monotonicity means that for every person the item ordering by difficulty is invariant. In other words it means that the ordering of the items from the easiest to the most difficult stays the same regardless of a respondent in question. As a consequence this property means that the fraction of incorrect answers to an item is a sufficient estimator of its difficulty level.

### The main aim of this script is to test whether this two assumption hold in the case of the PL sample and what subset(s) yield the best scale(s). All the tests conducted here will be based on the inspection of four statistics:
### #ac: theoretical maximal number of the possible violations of the model
### #vi: number of the actual violations in the data
### #zsig : number of violations that are considered statistically significant
### #crit: special summary statistic that combines the former three; values above 40 implies moderate violations of the model assumptions; values above 80 implies strong violation

### The aim of the first part of the analysis is to detect and remove poorly scaling items. Then it will be possible to use inductive techniques to find items that form the best scale or scales.

### Load data
load(normalizePath("./Data/MainData/dat_CZ1.RData"))
data <- dat_cz1
### Restrict to the respondents with imputed missing values
### Restrict the dataset to KNOWLEDGE variables exclusively
data <- data[data$knowIV == 0, grep("^k[0-9]+", names(data), perl = TRUE)]

### Load additional packages and helper functions
source(normalizePath("./R_scripts//data_processing/processingTools.R"))
source(normalizePath("./R_scripts//Visualization/Themes//LatticeThemes.R"))
library(RColorBrewer)
library(lattice)
library(latticeExtra)
library(reshape2)
library(poLCA)
library(scatterplot3d)
library(MASS)
library(mokken)
library(knitr)
library(psych)
library(survival)
library(doBy)
### Load V4 color theme
V4bgw <- V4themes("standard_bgw")

### Inductive search for the best scales in the set of items (genetic algorithm approach)
scale = aisp(data, search="ga")
### The best 4-items scale (k10, k26, k29)
### There is no Mokken Scale in the Czech sample
### Even the best one does not scale; and it is completely different than the Polish one

### Therefore we will also try to scale the KNOWLEDGE items set using the Classical Test Theory approach


######################################
### CLASSICAL TEST THEORY APPROACH ###
######################################

### Load data
load(normalizePath("./Data/MainData/dat_CZ1.RData"))
data <- dat_cz1
### Restrict to the respondents with imputed missing values
### Restrict the dataset to KNOWLEDGE variables exclusively
data <- data[data$knowIV == 0, grep("^k[0-9]+", names(data), perl = TRUE)]

#### Correlation matrix for the entire set of items
round(cor(data), 3)

### Check reliability
alpha(data)
### Drop items that are negatively correlated with the scale
### (k1, k10, k19, k20, k22)
data <- data[, grep("^k(1|10|19|20|22)$", names(data), perl=TRUE, invert=TRUE)]
### Check reliability
alpha(data)
### Drop the worst items (k5, k14, k28)
data <- data[, grep("^k(5|14|28)", names(data), perl=TRUE, invert=TRUE)]
### Check reliability
alpha(data)

dataCCT <- data
### The distribution of the CCT scale scores
CCTscale <- apply(dataCCT, 1, sum)
histogram(CCTscale, par.settings=V4bgw, xlab="CCT Scale Score", ylab="Percent of Total",
          xlim=c(-1,26))
summary(CCTscale)
shapiro.test(CCTscale) ### The distribution is not strictly normal
### Nicely symmetric distribution

### The scale is too bad to be used.

### Summing up: the Czech KNOWLEDGE data can not be formally scaled

### Clean the workspace 
### (optional: uncomment to remove all objects from RStudio working memory)
# rm(list = ls())

### !!! <--- END OF SCRIPT ---> !!! ###

### Session info
# sessionInfo()
#
# R version 3.2.0 (2015-04-16)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 14.04.2 LTS
# 
# locale:
#       [1] LC_CTYPE=pl_PL.UTF-8       LC_NUMERIC=C               LC_TIME=pl_PL.UTF-8       
# [4] LC_COLLATE=pl_PL.UTF-8     LC_MONETARY=pl_PL.UTF-8    LC_MESSAGES=pl_PL.UTF-8   
# [7] LC_PAPER=pl_PL.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
# [10] LC_TELEPHONE=C             LC_MEASUREMENT=pl_PL.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
#       [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#       [1] reshape2_1.4.1      latticeExtra_0.6-26 lattice_0.20-31     RColorBrewer_1.1-2 
# 
# loaded via a namespace (and not attached):
#       [1] plyr_1.8.1    tools_3.2.0   Rcpp_0.11.5   grid_3.2.0    stringr_0.6.2