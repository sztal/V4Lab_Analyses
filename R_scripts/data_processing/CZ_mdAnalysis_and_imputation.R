#######################################################################################
### This script deals with the problem of missing values in CZ sample in two steps: ###
###   - detect respondents with two many missing answers and remove them            ###
###   - impute remaining missing values using MICE technique                        ###
### (sociodemographic variables are not addressed in this analysis)                 ###
#######################################################################################

### The general idea here is the following:
### only missing data in KNOWLEDGE questions, OPINION questions and SOCECO questions (the lesferism-etatism axis and the matrix of social-economic beliefs) are addressed.
### First respondents who have more tha 25% of missing data in KNOWLEDGE and/or OPINION questions and/or 3 NAs in SOCECO questions are detected. Then indicator variables are created to show which respondents have >25% of NAs in KNOWELDGE and which of them have >25% in OPINIONS and which of them have 3 NAs in SOCECO questions.
### Next the MICE imputation technique is used to produce 10 probable values for each missing data point. From these ten values one is chosen - as the mean rounded to the nearest integer or as the mode if the variable is non-numerical - and inserted in the place of the missing data point. However this procedure us put in motion if and only if a respondent does not belong to the set of respondents with to many NAs in this particular set of item (KNOWLEDGE, OPINION or SOCECO). Those who do belong to this set are left with NAs and in general will not be considered in analyses addresing this set of items.
### This procedure of the imputed value selection and respondents selection is implemented in a custom function (Function 6) is the processingTools.R script.


### Load data
load(normalizePath("./Data/MainData/CZ_selected.RData"))
### Save under more convenient name
data <- data_cz_select
### set ids to rownames
data <- data.frame(data[, -1], row.names = data[, 1])

#################################################################
### Detection of the respondents with too many missing values ###
#################################################################
### Load helper data processing functions
source(normalizePath("./R_scripts//data_processing/processingTools.R"))

### dataset with only KNOWLEDGE questions
KNOW <- data[, grep("^k[0-9]+", names(data), perl = TRUE)]
### dataset with only OPINION question
OPINION <- data[, grep("^o[0-9]+", names(data), perl = TRUE)]
### dataset with only social/economic viewpoint questions
SOCEKO <- data[, grep("MAT|lesefer", names(data), ignore.case = TRUE, perl = TRUE)]

####################################
### NAs in the KNOWLEDGE dataset ###
####################################
knowNAs <- apply(KNOW, 1, howManyNAs, frac = TRUE)
### Get ids of respondents that have more than 25% of NAs in KNOWLEDGE questions
### These respondents should not have their values imptuted
### and should not be considered in the analysis of KNOWLEDGE data
know25NA <- rownames(data[which(knowNAs > .25), ])

##################################
### NAs in the OPINION dataset ###
##################################
opinionNAs <- apply(OPINION, 1, howManyNAs, frac = TRUE)
### Get ids of respondents that have more than 25% of NAs in OPINION questions
### These respondents should not have their values imptuted
### and should not be considered in the analysis of OPINION data
opinion25NA <- rownames(data[which(opinionNAs > .25), ])

#################################################################
### NAs in the questions about social and economic standpoint ###
#################################################################
socecoNAs <- apply(SOCEKO, 1, howManyNAs, frac = FALSE)
### Get ids of respondents that have only NAs in soceco questions
### These respondents should not have their values imptuted
soceco3NA <- rownames(data[which(socecoNAs == 3), ])

##################################
### Create indicator veriables ###
##################################
### Indicator variables show 
### which respondents should be excluded from analyses of KNOWLEDGE and OPINION questions
knowIV <- vector(mode = "integer", length = dim(data)[1])
knowIV[which(rownames(data) %in% know25NA)] = 1
opinionIV <- vector(mode = "integer", length = dim(data)[1])
opinionIV[which(rownames(data) %in% opinion25NA)] = 1
data$knowIV <- knowIV
data$opinionIV <- opinionIV

######################################################
### Imputation with the MICE technique             ###
### (Multivariate Imputation by Chained Equations) ###
######################################################
### Imputation using MICE algorithm (Multivariate Imputation by Chained Equations)
### load the mice package and the required packages
library(Rcpp)
library(lattice)
library(mice)

### Prepare a proper data.frame object for imputation
### (it should be limited only to the variables of interest)
dataMICE <- data[, grep("^[ok][0-9]+|mat|lesefer", names(data), perl = TRUE)]

### Select imputation techniques for variables
imptech <- c(rep("polyreg", 23), rep("logreg", 29), rep("pmm", 3))
### Generator imputation object
IMP <- mice(dataMICE, m = 10, method = imptech, seed = 1001)
### Get helper functions for deriving imputed datasets from the MIDS objects of the mice package
source(normalizePath("./R_scripts//data_processing/processingTools.R"))

### Derive imputed dataset for KNOWLEDGE variables
KNOWimp <- IMP$imp[grep("^k[0-9]+", names(IMP$imp), perl = TRUE)]
### set the statistics for determining final imputed values
stats <- rep("mode", length(KNOWimp))
KNOWimpvals <- getDataFromMIDS(KNOWimp, data, stats = stats, rnd = TRUE, exclude_obs = know25NA, diagnostics = FALSE)
data[, grep("^k[0-9]+", names(data), perl = TRUE)] <- KNOWimpvals

### Derive imputed dataset for OPINION variables
OPINIONimp <- IMP$imp[grep("^o[0-9]*", names(IMP$imp), perl = TRUE)]
### set the statistics for determining final imputed values
stats <- rep("mode", length(OPINIONimp))
OPINIONimpvals <- getDataFromMIDS(OPINIONimp, data, stats = stats, rnd = TRUE, exclude_obs = opinion25NA, diagnostics = FALSE)
data[, grep("^o[0-9]+", names(data), perl = TRUE)] <- OPINIONimpvals

### Derive imputed dataset for SOCECO variables
SOCECOimp <- IMP$imp[grep("lesefer|^mat", names(IMP$imp), perl = TRUE)]
### set the statistics for detemining final imputed values
stats <- rep("mean", length(SOCECOimp))
SOCECOimpvals <- getDataFromMIDS(SOCECOimp, data, stats = stats, rnd = TRUE, exclude_obs = soceco3NA, diagnostics = FALSE)
data[, grep("lesefer|^mat", names(data), perl = TRUE)] <- SOCECOimpvals

#########################
### Save imputed data ###
#########################

### Rename the dataset object properly
dat_cz1 <- data
### Save recoded dataset to a .txt file
### field seprator is set "\t"
write.table(dat_cz1, sep = "\t", row.names = TRUE,
            file = normalizePath("./Data/MainData/dat_CZ1.txt"))
### Save recoded dataset to an R data object
save(dat_cz1, file = normalizePath("./Data/MainData/dat_CZ1.RData"))

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
#       [1] mice_2.22       lattice_0.20-31 Rcpp_0.11.5    
# 
# loaded via a namespace (and not attached):
#       [1] MASS_7.3-39         tools_3.2.0         nnet_7.3-9          rpart_4.1-9         grid_3.2.0         
#[6] randomForest_4.6-10