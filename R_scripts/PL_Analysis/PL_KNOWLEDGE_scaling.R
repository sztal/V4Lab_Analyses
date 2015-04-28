##########################################################
### Scaling of the KNOWELDGE item set in the PL sample ###
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
load(normalizePath("./Data/MainData/dat_PL1.RData"))
data <- dat_pl1
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
### The best 4-items scale (k8, k21, k22, k30)
data4 <- data[, c("k8", "k21", "k22", "k30")]
### Check the monotonous homogeneity assumption
mono4 <- check.monotonicity(data4)
summary(mono4)
### Check the double monotonicity assumption (using the restscores method)
rest4 <- check.restscore(data4) ### conforms well to the assumption
### Check the double monotonicity assumption (using the pmatrix method)
pmat4 <- check.pmatrix(data4) ### again conforms well
### Check global scaling properties
globalH4 <- coefH(data4)
rel4 <- check.reliability(data4)
### The global scaling coefficients are rather poor and the scale may be considered at most acceptable (global scaling coefficient H > 0.30)

### 4-items scale scores
know4 <- apply(data4, 1, sum)
### Distribution of the 4-items scale scores
histogram(know4, par.settings=V4bgw, xlab="4-items Scale Scores", ylab="Percent of Total")
### The distribution is strongly left-skewed what implies that the scale does not discriminate between respondents with high economic/financial knowledge, but do discriminate between those of the poor knowledge.

### The analysis proves that this data does not conform to the Mokken model
### Only the 4-items scale has barely satisfactory global scaling properties and allows some basic discrimination between respondents in regard to the economic/financial knowledge

### Therefore we will also try to scale the KNOWLEDGE items set using the Classical Test Theory approach


######################################
### CLASSICAL TEST THEORY APPROACH ###
######################################

### Load data
load(normalizePath("./Data/MainData/dat_PL1.RData"))
data <- dat_pl1
### Restrict to the respondents with imputed missing values
### Restrict the dataset to KNOWLEDGE variables exclusively
data <- data[data$knowIV == 0, grep("^k[0-9]+", names(data), perl = TRUE)]

#### Correlation matrix for the entire set of items
round(cor(data), 3)

### Check reliability
alpha(data)
### Drop items that are negatively correlated with the scale
### (k5, k11, k20, k24)
data <- data[, grep("5|11|20|24", names(data), perl=TRUE, invert=TRUE)]
### Check reliability
alpha(data)
### Drop the worst item (k14)
data <- data[, grep("k14", names(data), perl=TRUE, invert=TRUE)]
### Check reliability
alpha(data)
### The reliability of the scale is barely sastisfactory and rather could not be lifted by removing more items. It appears that this is the best one can get with this data.
### Save this dataset as a new object
dataCCT <- data
### The distribution of the CCT scale scores
CCTscale <- apply(dataCCT, 1, sum)
histogram(CCTscale, par.settings=V4bgw, xlab="CCT Scale Score", ylab="Percent of Total",
          xlim=c(-1,26))
summary(CCTscale)
shapiro.test(CCTscale) ### The distribution is not strictly normal
### Nicely symmetric distribution

###############################################
### BASIC CHECK OF THE THEORETICAL VALIDITY ###
###############################################

### The test of the theoretical validity of both scales will be conducted as follows:
### Since the scales are supposed to measure economic/financial knowledge, then students enrolled in economic and/or bussiness programmes should score higher on average.
### Moreover their scores should also get better with the time spent in a programme as they get better educated in the field

### Load data
load(normalizePath("./Data/MainData/dat_PL1.RData"))
data <- dat_pl1
### Load 4-items Mokken scale dataset and CCT scale dataset
### Restrict the dataset to KNOWLEDGE variables exclusively
dataCCT <- data[data$knowIV == 0, grep("^k[0-9]+|edu.*3|yea.*uni", names(data), perl = TRUE)]
dataCCT <- dataCCT[, grep("5|11|20|24|14", names(dataCCT), perl=TRUE, invert=TRUE)]
dataCCT$CCT <- apply(dataCCT[,grep("edu.*3|yea.*uni", names(dataCCT), perl=TRUE, invert=TRUE)],
                     1, sum)
data4 <- data[data$knowIV==0, grep("^k(8|21|22|30)|edu.*3|yea.*uni", names(data), perl=TRUE)]
data4$Mokken4 <- apply(data4[, grep("^k[0-9]+", names(data4), perl=TRUE)], 1, sum)

############################
### 4-items Mokken scale ###
############################
### Scores distributions among eduprog groups: EBMF, SSHA and STEM
tapply(data4$Mokken4, data4$eduprog3, summary)
bwplot(Mokken4 ~ eduprog3, data=data4, par.settings=V4bgw,
       ylab = "4-items Mokken Scale Score")
### Formal test of the differences
### We use dummy coding that compares the SSHA and STEM groups to the EBMF group
summary(lm(Mokken4 ~ eduprog3, data=data4))
### Both differences are highly significant and the type of education explains about 14.5% of the variance

### The change of knowledge over the years of education in the 4-items Mokken Scale
### Save the year_at_uni variables as an integer variables
### First we collapse MA_1 and MA_2 to one group becuase of the to few respondents in the MA_2
data4$year_at_uni <- recodeVar(data4$year_at_uni, src="MA_2", tgt="MA_1")
data4$uniyear <- as.numeric(data4$year_at_uni)
### Plot of the linear trends in the groups
xyplot(Mokken4 ~ uniyear | eduprog3, data=data4, par.settings=V4bgw,
       scales=list(x=list(labels=as.integer(0:4), tick.number=4)),
       xlab="Study Year", ylab="4-items Mokken Scale",
       panel=function(x, y, ...) {
       panel.xyplot(x, y, ...)
       panel.lmline(x, y, ...)
       })
### Regression models in the groups
lmEBMF <- lm(Mokken4 ~ uniyear, data=data4, subset=data4$eduprog3=="EBMF")
summary(lmEBMF) # No effect of the Study Year in the EBMF group
lmSSHA <- lm(Mokken4 ~ uniyear, data=data4, subset=data4$eduprog3=="SSHA")
summary(lmSSHA) # No effect of the Study Year in the SSHA group
lmSTEM <- lm(Mokken4 ~ uniyear, data=data4, subset=data4$eduprog3=="STEM")
summary(lmSTEM) # No effect of the Study Year in the STEM group

### So the analysis confirm the basic fact the EBMF group scores higher than the others. However, in none of them (even in the EBMF) do we observe the effect of the Study Year.
### It is most likely that this lack of the effect of the study year is due to the lack of discriminating power of the scale amongst respondents with higher knowledge.
### But still we observe the basic confirmation of the theoretical validity of the 4-items Mokken Scale

##############################################
### Classical Test Theory Scale (22 items) ###
##############################################
### Scores distributions among eduprog groups: EBMF, SSHA and STEM
tapply(dataCCT$CCT, data4$eduprog3, summary)
bwplot(CCT ~ eduprog3, data=dataCCT, par.settings=V4bgw,
       ylab = "22-items CCT Scale")
### Formal test of the differences
### We use dummy coding that compares the SSHA and STEM groups to the EBMF group
summary(lm(CCT ~ eduprog3, data=dataCCT))
### Both differences are highly significant and the type of education explains about 24.3% of the variance

### The change of knowledge over the years of education in the 22-items CCT Scale
### Save the year_at_uni variables as an integer variables
### First we collapse MA_1 and MA_2 to one group becuase of the to few respondents in the MA_2
dataCCT$year_at_uni <- recodeVar(dataCCT$year_at_uni, src="MA_2", tgt="MA_1")
dataCCT$uniyear <- as.numeric(dataCCT$year_at_uni)
### Plot of the linear trends in the groups
xyplot(CCT ~ uniyear | eduprog3, data=dataCCT, par.settings=V4bgw,
       scales=list(x=list(labels=as.integer(0:4), tick.number=4)),
       xlab="Study Year", ylab="22-items CCT Score",
       panel=function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.lmline(x, y, ...)
       })
### Regression models in the groups
lmEBMF <- lm(CTT ~ uniyear, data=dataCTT, subset=data4$eduprog3=="EBMF")
summary(lmEBMF) # No effect of the Study Year in the EBMF group
lmSSHA <- lm(CTT ~ uniyear, data=dataCTT, subset=data4$eduprog3=="SSHA")
summary(lmSSHA) # No effect of the Study Year in the SSHA group
lmSTEM <- lm(CTT ~ uniyear, data=dataCTT, subset=data4$eduprog3=="STEM")
summary(lmSTEM) # No effect of the Study Year in the STEM group

### So the analysis confirm the basic fact the EBMF group scores higher than the others. However, in none of them (even in the EBMF) do we observe the effect of the Study Year.
### It is most likely that this lack of the effect of the study year is due to the lack of discriminating power of the scale amongst respondents with higher knowledge.
### But still we observe the basic confirmation of the theoretical validity of the 22-items Classical Test Theory Scale

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