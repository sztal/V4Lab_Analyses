#########################################################################
### Analysis of distributions of the KNOWLEDGE items in the CZ sample ###
#########################################################################

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

### Items difficulty levels
diff <- data.frame(apply(data, 2, mean, na.rm=TRUE))
diff <- cbind(rownames(diff), diff)
names(diff) <- c("Item", "Difficulty")
diff[, 2] <- 1 - diff[, 2] ### change 'easiness' to 'difficulty'
### Load V4 color theme
V4bgw <- V4themes("standard_bgw")
### Summarize with a dotplot
### Load helper functions for visualization
source(normalizePath("./R_scripts//Visualization//PlottingFunctions.R"))
diffplot <- plotBinary(data, ci=TRUE, reverse=TRUE, vline=TRUE, theme=V4bgw)
diffplot <- update(diffplot, xlab = "Item Difficulty")

### Analysis of the KNOWLEDGE raw scores
### We define raw score to be the simple sum of the correct answers to the KNOWLEDGE questions
rawscores <- apply(data, 1, sum)
summary(rawscores)
### Graphical distributions of the rawscores (histogram)
rawhist <- histogram(rawscores, xlab="Raw Score", ylab="Percent of Total", par.settings=V4bgw)
### The histogram looks not very symmetric and normalish
### a little less potential for scaling than in the PL data?
### Normality of the distribution of the raw scores
shapiro.test(rawscores) ### the distribution is not normal

### Is the raw scores mean greater than 15?
t.test(rawscores, alternative="greater", mu=15) # yes it is!

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
#       [1] plyr_1.8.1      htmltools_0.2.6 tools_3.2.0     yaml_2.1.13     Rcpp_0.11.5     rmarkdown_0.5.1
# [7] grid_3.2.0      knitr_1.10      digest_0.6.8    stringr_0.6.2  