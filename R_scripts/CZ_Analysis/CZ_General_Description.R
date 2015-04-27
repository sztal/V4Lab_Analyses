############################################################
### General Statistical Description of the Polish Sample ###
############################################################

### Load data
load(normalizePath("./Data/MainData/dat_CZ1.RData"))
data <- dat_cz1

### load additional packages for data analysis and visualization
library(RColorBrewer)
library(lattice)
library(latticeExtra)
library(reshape2)

### Descriptive stats for socdemo variables and social-economic standpoint
summary(data[, grep("lesefer|mat", names(data), perl = TRUE)])

### Load the themes generating function
source(normalizePath("./R_scripts/Visualization/Themes/LatticeThemes.R"))
### Set the standard b-g-w colortheme for lattice charts
V4bgw <- V4themes("standard")
### Histogram of the distribution of the leseferism-etatism axis
leahist <- histogram(~ leseferism.etatism, data = data, par.settings = V4bgw, auto.key = TRUE,
                     xlab = "Leseferism - Etatism", ylab = "Percent of Total",
                     endpoints = c(0, 10))
### It looks rather symmetric althought not very normal (bimodality is rather just noise)

### SOCECO matrix distribution and plot
### Joint distribution over the two dimensions 
ecodim <- factor(data$mat_econimic)
#ecodim <- factor(ecodim, levels(ecodim)[4:1])
socdim <- factor(data$mat_social)
#socdim <- factor(socdim, levels(socdim)[4:1])
socecomatdist <- table(socdim, ecodim, useNA="ifany")[5:1, ]
matplot <- levelplot(table(data$mat_econimic, data$mat_social), col.regions = gray(100:0/100),
                     xlab = "Social issues", ylab = "Economic issues")

### Distributions of parents' education and the joint distribution
### Melt both variables into one long variable with categories
edudata <- cbind(data[, grep("father|mother", names(data), perl = TRUE)],  id = rownames(data))
edudata <- melt(edudata, id = "id")
edudata$value <- factor(edudata$value, levels = levels(data$father_edu), ordered = TRUE)
edudata <- data.frame(table(edudata$value, edudata$variable))
names(edudata) <- c("education", "parent", "freq")
educhart <- barchart(freq ~ education | parent, data = edudata,
                     par.settings = V4bgw, ylab = "Frequency",
                     scales = list(x = list(labels = c("< Primary", "Primary", "Vocational",
                                                       "High School", "Higher Educations", "PHD+"))),
                     strip = strip.custom(factor.levels = c("Father Education",
                                                            "Mother Education")))

### Joint distribution of parents' education
fedu <- factor(data$father_edu)
medu <- factor(data$mother_edu)
pedudist <- table(fedu, medu, useNA="ifany")[6:1, ]
### Matrix plot
edumatplot <- levelplot(table(data$father_edu, data$mother_edu), col.regions = gray(100:0/100),
                        xlab = "Mother Education", ylab = "Father Education",
                        scales=list(x=list(labels=c("< Primary", "Primary", "Vocational",
                                                    "High School", "Higher Educations", "PHD+"),
                                           rot=90),
                                    y=list(labels=c("< Primary", "Primary", "Vocational",
                                                    "High School", "Higher Educations", "PHD+"))))

### Year at university distribution
uniyeardist <- table(data$year_at_uni, useNA="ifany")
uniyearplot <- barchart(uniyeardist, par.settings = V4bgw, xlab = "Frequency",
                        scales=list(y=list(labels=c("BA 1st", "BA 2nd", "BA 3rd",
                                                    "MA 1st", "MA 2nd"))))
### Work experience distribution
wexpdist <- table(data$work_experience, useNA="ifany")
wexpchart <- barchart(wexpdist, par.settings = V4bgw, xlab = "Frequency",
                      scales=list(y=list(labels=c("No experience", "Up to 1 year",
                                                  "1 year or more"))))

### hometown size distribution
hsizedist <- table(data$hometown_size, useNA="ifany")
hsizechart <- barchart(hsizedist, par.settings = V4bgw, xlab = "Frequency")

### Gender distribution
genderdist <- table(data$gender, useNA="ifany")
genderchart <- barchart(genderdist, par.settings = V4bgw, xlab = "Frequency",
                        scales=list(y=list(labels=c("Female", "Male"))))

### Age distribution
summary(data$age)
agedist <- histogram(~ age, data = data, par.settings = V4bgw,
                     xlab = "Age", ylab = "Percent of Total")

### Study programme distribution (4 groups classification)
edu4dist <- table(data$eduprog4, useNA="ifany")
edu4chart <- barchart(edu4dist, par.settings = V4bgw, xlab = "Frequency")

### Study programme distribution (3 groups classification)
edu3dist <- table(data$eduprog3, useNA="ifany")
edu3chart <- barchart(edu3dist, par.settings = V4bgw, xlab = "Frequency")

### NAs distribution across the variables
### Load helper functions for finding numbers of NA in a dataset
source(normalizePath("./R_scripts//data_processing/processingTools.R"))
dataNA <- data[, grep("^[ok][0-9]+", names(data), perl = TRUE, invert = TRUE)]
NAs <- apply(dataNA, 2, howManyNAs, frac = TRUE)
NAchart <- barchart(NAs, par.settings = V4bgw, xlab = "Percent of NAs")

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