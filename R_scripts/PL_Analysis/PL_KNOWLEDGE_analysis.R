#####################################################################
### Analysis of distributions of the KNOWLEDGE items in PL sample ###
#####################################################################

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