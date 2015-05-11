############################################################
### CLUSTER ANALYSIS: KNOWLEDGE AND PREFERENCES/OPINIONS ###
############################################################

### The aim of this script is to uncover natural clusters of respondents in regard to their economic and financial knowledge and opinions in terms of liberalism and interventionism. Then the classification scheme developed by means of cluster analysis will be interpreted in terms of general social-political characteristics of Poland and Czech Republic.

### Load data
load(normalizePath("./Data/MainData/finalDataExtended.RData"))
data <- finalDataExtended

### Load packages
library(RColorBrewer)
library(lattice)
library(latticeExtra)
library(survival)
library(car)
library(doBy)
library(effsize)
library(heplots)
library(car)
library(psych)
library(sandwich)
library(mvtnorm)
library(TH.data)
library(multcomp)
library(mvnormtest)
library(MASS)
library(candisc)
library(biotools)
library(vcd)
library(smacof)
library(NbClust)
### Load plotting functions
source(normalizePath("./R_scripts/Visualization/PlottingFunctions.R"))
### Load processing tools
source(normalizePath("./R_scripts//data_processing/processingTools.R"))
### Load V4 color theme
source(normalizePath("./R_scripts//Visualization/Themes/LatticeThemes.R"))
V4bgw <- V4themes("standard_bgw")

### Data limited to the variables of interest
data.clust <- data[, c("leseferism.etatism", "mat_econimic", "mat_social",
                       "libsoc", "ngknow", "ngfinance")]

### Since we expect both countries to have different cluster structures the analysis will be split into PL and CZ part.
data.clust.pl <- data.clust[data$country=="PL", ]
data.clust.cz <- data.clust[data$country=="CZ", ]

### We use NbClust algorithm to determine the best number of clusters in the dataset.
##################
### PL sample ####
##################
set.seed(101) # set seed for the pseudorandom numbers generator
clust.pl <- NbClust(data.clust.pl, min.nc=2, max.nc=10, method="kmeans",
                    index="all", distance="euclidean")
### Optimal number of clusters is 2 (12 methods indicate it as the best solution)
### The second best is 3 clusters (with 4 votes for it)
### Clusters are of similar sizes; respondents with NAs get no assignment
table(clust.pl$Best.partition)

### Prepare variable storing information about the PL clustering
PLclust <- vector(mode="numeric", length=nrow(data))
PLclust[] <- NA
names(PLclust) <- rownames(data)
PLclust[names(clust.pl$Best.partition)] <- clust.pl$Best.partition[names(
      clust.pl$Best.partition)]

### add PL cluster assignments to the main dataset
data$PLclust <- PLclust

### Analysis of differences and interpretation
table(data$PLclust, data$eduprog3)
assocstats(table(data$PLclust, data$eduprog3))
### there is strong association between cluster and EBMF and cluster 2 and SSHA
table(data$PLclust, data$phighedu) # no difference!!!
assocstats(table(data$PLclust, data$phighedu))
table(data$PLclust, data$PL_hsize) # no difference!!!
assocstats(table(data$PLclust, data$PL_hsize))

### Check means of variabels used for clustering in clusters
summaryBy(libsoc+ngknow+ngfinance+leseferism.etatism+mat_econimic+mat_social ~ PLclust,
          data=data, FUN=mean, aa.rm=TRUE)[1:2, ]

### Check which differences are significant
t.test(libsoc ~ PLclust, data=data) # significant!
cohen.d(libsoc ~ PLclust, data=data) # small effect
t.test(ngknow ~ PLclust, data=data) # significant!
cohen.d(ngknow ~ PLclust, data=data) # large effect
t.test(ngfinance ~ PLclust, data=data) # significant!
cohen.d(ngfinance ~ PLclust, data=data) # large effect
t.test(leseferism.etatism ~ PLclust, data=data) # significant!
cohen.d(leseferism.etatism ~ PLclust, data=data) # weak effect
t.test(mat_econimic ~ PLclust, data=data) # not significant
cohen.d(mat_econimic ~ PLclust, data=data) # no effect
t.test(mat_social ~ PLclust, data=data) # not significant
cohen.d(mat_social ~ PLclust, data=data) # no effect

### It seems that in terms of attitudes there is only little difference between the clusters (and in terms of social-custom opinion there is none). However in terms of knowledge (both general economic and strictly financial) there is very strong difference.

### So we call the clusters:
### 1 - Experienced
### 2 - Disinterested

### Turn the cluster variable into a factor
PLclust <- factor(ifelse(PLclust == 1, "Experienced", "Disinterested"))
data$PLclust <- PLclust

### Now we will try to determine how well separated the clusters are 
### For this purpose we conduct linear disciminant analysis (to se how well the clusters may be separated in terms of the variables that were used) and then multivariate analysis of variance to see how much of the variance of the variables can be explained by the cluster assignment

### Prepare dataset for the LDA and MANOVA
data.pl <- data[!is.na(data$PLclust), c("libsoc", "ngknow", "ngfinance",
                                        "leseferism.etatism", "mat_econimic",
                                        "mat_social", "PLclust")]

### Box M test of variance-covariance matrices homogeneity (LDA assumption)
boxM(data.pl[, c("libsoc", "ngknow", "ngfinance", "leseferism.etatism",
                 "mat_econimic", "mat_social")], data.pl$PLclust)
### The matrices are homogeneous!

# MANOVA - as a basis for LDA
manova.pl <- manova(as.matrix(data.pl[, 1:6]) ~ data.pl$PLclust)
summary(manova.pl) # significant effect of the cluster assignment
etasq(manova.pl) # almost 70% of the variance is retained!
# LDA
lda.pl <- candisc(manova.pl) # All 2 canonical functions are significant
summary(lda.pl)
# cross-validation
cv.pl <- lda(PLclust ~ ., data=data.pl, prior=rep(1/2, 2), CV=TRUE)$class 
mean(cv.pl == data.pl$PLclust) # almost 98% of cross-validated accuracy

### Correlations between the variables and the canonicla scores
data.pl$PL_canscore <- lda.pl$scores[, 2]
lowerCor(data.pl[, c(1:6, 8)])
corr.test(data.pl[, c(1:6, 8)])

### Clearly financial/economical knowledge is the main dimension of the difference. But the analysis show again that there is persistent megative relationship between interventionist attitudes and financial and economic knowledge.