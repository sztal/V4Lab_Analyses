#########################################################
### Scaling of the OPINION items set in the CZ sample ###
#########################################################

### This script presents the procedure of the scaling the OPINION items set in the CZ sample. We use three-step approach based (description TO BE FINISHED)

### Load data
load(normalizePath("./Data/MainData/dat_CZ1.RData"))
data <- dat_cz1
### Restrict to the respondents with imputed missing values
### Restrict the dataset to KNOWLEDGE variables exclusively
data <- data[data$opinionIV == 0, grep("^o[0-9]+", names(data), perl = TRUE)]

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
library(survival)
library(doBy)
library(ca)
### Load V4 color theme
V4bgw <- V4themes("standard_bgw")

### We start with multiple correspondence analysis of the data in order to determine what are the main dimensions and to find items that poorly fit these dimensions and therefore should be excluded
fmca1 <- mjca(data, nd=NA, lambda="adjusted")
### The first axis is dominated by th difference between those respondents who have opinions and those who do not. It is only the second axis that corresponds to the liberalism-socialism divide. But again, the 'Agree' and 'Disagree' answers forms a strip going through the origin, what implies strong unidimensional character of the data (provided we neglect the 'Don't Know' answers for a moment)
### Therefore now we well exclude those items that do not fit well to the second dimension and are dominated by the 'Don't Know' answers in the first one (squarred correlations of 'Don't Know' with the first dimension is high while correlation of the others with the second dimension is low)
### Using this criterion we exclude : o2, o5, o14, o15, o16, o17

### Data trimmed to the chosen questions exclusively
data.lim1 <- data[, grep("^o(2|5|14|15|16|17)$", names(data), perl=TRUE, invert=TRUE)]
### Another MCA
fmca2 <- mjca(data.lim1, nd=NA, lambda="adjusted")
### Now the first dimension starts to be dominated by the liberalism-socialism axis; all 'Don't Know' answers are separated and lie in the upper left corner of plot

### Based on this analysis it is possible to tell which question and answers correspond with sympathy for liberalism and which with sympathy for socialism.
### Now it is possible to construct two unidimensional cumulative scales of liberalism and socialism. In these scales appropriate answers in questions ('Agrees' and 'Disagrees') are mapped to 1's and inapproriate (answers opposite to the correct ones and 'Don't Knows) to 0.
### So it is possible to scale them using IRT methods for cumulative scales such as the Mokken Scale

### Create new scales of liberalism and socialism
### First we derive datasets with answers mapped to the scales' answers
map <- read.table(normalizePath("./Data/Maps/LibSoc_map.csv"),
                  sep=";", row.names=1, header=TRUE)
libdat <- mapOPINION(data.lim1, map, country="CZ", verbose=TRUE, liberal=TRUE)
socdat <- mapOPINION(data.lim1, map, country="CZ", verbose=TRUE, liberal=FALSE)

### Now we can scale both datasets

### The scaling procedure that is presented here uses the Mokken Scale technique, which is an non-parametric IRT technique that allows prodcution of high-quality ordinal scales. Mokken scales are useful because they posses two very important properties: monotonous homogeneity and double monotonicity. Monotonous homogeneity means that in a proper Mokken scale it is assured that an intenisty of a respondent's trait (in this case it would be economic/financial knowledge) is positively correlated with a probability of he or she giving the correct anwer to a question. It ensures correct ordinal measurement of a trait in which the sum of the correct answers is a sufficient estimator of the trait.
### Double monotonicity means that for every person the item ordering by difficulty is invariant. In other words it means that the ordering of the items from the easiest to the most difficult stays the same regardless of a respondent in question. As a consequence this property means that the fraction of incorrect answers to an item is a sufficient estimator of its difficulty level.

### The main aim of this script is to test whether this two assumption hold in the case of the PL sample and what subset(s) yield the best scale(s). All the tests conducted here will be based on the inspection of four statistics:
### #ac: theoretical maximal number of the possible violations of the model
### #vi: number of the actual violations in the data
### #zsig : number of violations that are considered statistically significant
### #crit: special summary statistic that combines the former three; values above 40 implies moderate violations of the model assumptions; values above 80 implies strong violation

########################
### Liberalism scale ###
########################
### First we scale liberalism dataset using the Mokken Scale technique
### We use an inductive procedure based on a genetic algorithm to find a subset(s) of items that form the best scale(s)
libscales <- aisp(libdat, search="ga")
### Most of the items form a strong, core scale, others are residual
libvars <- rownames(libscales)[libscales == 1]
### Create new dataset with scale item exclusively
ldat <- libdat[, libvars]
### The selected scale is quite different from the PL scale...

### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### No significant violations
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### No significant violations
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### There are significant violations
### First we remove l6 (it does not belong to the PL liberalism subscale and has the lowest Hi value)

### Remove item l6
ldat <- ldat[, grep("l6", names(ldat), invert=TRUE, fixed=TRUE)]
### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### Significant violations in l12
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### No significant violations
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### Some significant violations
### Now remove l12, since it does not belong to the PL scale and has the lowest Hi value
### (the aim is to get scales as similar as possible to get a good joint scale in the end)

### remove l12
ldat <- ldat[, grep("l12", names(ldat), invert=TRUE, fixed=TRUE)]
### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### No significant violations
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### No significant violations
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### Significant violations in l8 and l20
### We remove l8 for the same reasons as before

### Remove l8
ldat <- ldat[, grep("l8", names(ldat), invert=TRUE, fixed=TRUE)]
### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### No significant violations
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### Some quite strong violations in l7 and l11
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### No violations

### Now we remove no items, since pmatrix give good result and all the items form a subset of the PL items set. Anyway the final shape of the scale will be uncovered in the joint analysis

### So both monotonous homogeneity and double monotonicity assumption holds; however the items' scaling coefficients are not too great (but all are acceptable)

### Check global scaling properties
libH <- coefH(ldat)
### Acceptable to good global scaling coefficient!
### 95% CI for H: 0.35-0.53
librel <- check.reliability(ldat)
### Cronbach's alpha of 0,62; but rather poor reliability

### Distribution of the scale scores
lib <- apply(ldat, 1, sum)
histogram(~lib, par.settings=V4bgw, xlab="Liberalism Scale Score", ylab="Percent of Total")
### Right skewed (people of low and very high liberalism may be hard to distinguish)

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
ldat <- cbind(ldat, dat_cz1[dat_cz1$opinionIV==0, grep("les.*etat|mat_", 
                                                       names(dat_cz1), perl=TRUE)])
ldat$lib <- lib
round(cor(ldat[, grep("les.*eta|mat_|lib", names(ldat), perl=TRUE)], use="pairwise"), 3)
### Correlations with the leseferism-etatism axis and the socdemo matrix are congruent with the theoretical expectations; basic level of theoretical validity is confirmed

#######################
### Socialism scale ###
#######################
### Now we scale socialism dataset using the Mokken Scale technique
### We use an inductive procedure based on a genetic algorithm to find a subset(s) of items that form the best scale(s)
socscales <- aisp(socdat, search="ga")
### Most of the items form a strong, core scale, others are residual
### (perhaps) there is a second subscale, but we will ignore it for a moment
socvars <- rownames(socscales)[socscales == 1]
### Create new dataset with scale item exclusively
sdat <- socdat[, socvars]

### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
socmono <- check.monotonicity(sdat) ### no significant violations
### Check the double monotonicity assumption (using the restscores method)
socrest <- check.restscore(sdat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the pmatrix method)
socpmat <- check.pmatrix(sdat) ### Fits well the Mokken model!

### Scale is weak; we exclude the worst scaling item (s3)

### Remove s3
sdat <- sdat[, grep("s3", names(sdat), invert=TRUE, fixed=TRUE)]
### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
socmono <- check.monotonicity(sdat) ### no significant violations
### Check the double monotonicity assumption (using the restscores method)
socrest <- check.restscore(sdat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the pmatrix method)
socpmat <- check.pmatrix(sdat) ### Fits well the Mokken model!

### Check global scaling properties
socH <- coefH(sdat)
### Poor to moderate scaling; 95% CI for H: 0.27-0.45
socrel <- check.reliability(sdat)
### Cronbach's alpha of 0,54; Very poor

### Distribution of the scale scores
soc <- apply(sdat, 1, sum)
histogram(~soc, par.settings=V4bgw, xlab="Socialism Scale Score", ylab="Percent of Total")
### Slightly right skewed; may be hard to discriminate between respondents with strong socialist attitudes

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
sdat <- cbind(sdat, dat_cz1[dat_cz1$opinionIV==0, grep("les.*etat|mat_", 
                                                       names(dat_cz1), perl=TRUE)])
sdat$soc <- soc
round(cor(sdat[, grep("les.*eta|mat_|soc", names(sdat), perl=TRUE)], use="pairwise"), 3)
### Correlations are congruent with expectations; basic theoretical validity is confirmed

################################################################
### COMBINED UNIDIMENSIONAL UNFOLDING SCALING OF BOTH SCALES ###
################################################################

### We check goodness-of-fit in tegard to the unfolding model visually using MCA
sortnames <- c(names(sort(apply(ldat[, grep("^l[0-9]+", names(ldat), perl=TRUE)], 2, mean))), rev(names(sort(apply(sdat[, grep("^s[0-9]+", names(sdat), perl=TRUE)], 2, mean)))))
ufdat <- cbind(ldat[, grep("^l[0-9]+", names(ldat), perl=TRUE)],
               sdat[, grep("^s[0-9]+", names(sdat), perl=TRUE)])
ufdat <- ufdat[, sortnames]
ufmca <- mjca(ufdat, nd=NA, lambda="adjust")

### Compute the distribution of unfolding model violations per respondend
uferrors <- apply(ufdat, 1, unfoldingErrors)
histogram(~uferrors, par.settings=V4bgw, xlab="Unfolding Errors per Person",
          ylab="Percent of Total")

### Numerical simulation of violations under the assumption of independency between the variables
vardiffs <- apply(ufdat, 2, mean)
simerrors <- numericalUnfoldingErrors(vardiffs, nobs=dim(ufdat)[1], 
                                      n=1000, seed=166)

### Distribution of the simulated errors
summary(simerrors)
histogram(~simerrors, par.settings=V4bgw, xlab="Simulated Unfolding Errors",
          ylab="Percent of Total")
### Normality test
shapiro.test(simerrors) ### It is normal
### 95% Confidence interval for the mean
quantile(simerrors, c(.05, .95))
### Z-statistic for the empirical number of violations in the data
zstat <- (sum(uferrors) - mean(simerrors)) / sd(simerrors)
### P-value
pnorm(zstat) ### Extremely significant

### Scallability coefficients for items and the scale
ufCoefs <- unfoldingH(ufdat)
### Quite good scallability; all items are acceptable; most are good
### Now we compute standard errors of these coefficients using bootstrap
set.seed=750 # set seed for the pseudorandom numbers genererator (for bootstrap)
ufSE <- unfoldingSE(ufdat, n=100)
### 95% CI for the H is: [0.35-0.50]
### Check of the ordinal specific objectivity (formal unfolding property)
### We use the conditional adjacency matrix technique
CondMat <- ufConditionalMat(ufdat)
### Inspection of the Conditional Matrix suggests that the scale is close to a perfectly unfolding in a strict sense; it means that it ensures the ordinal specific objectivity properties

### For the end we compute the reliability of our unfolding scale. We use the fact that it is absed on two Mokken scales glued together back-tob-back, so the relability of this scale may be computed as weighted (by the numbers of items in each Mokken scale) average of reliability coefficients of the two scales.
libRel <- check.reliability(ufdat[, grep("l", names(ufdat), perl=TRUE)])$alpha
socRel <- check.reliability(ufdat[, grep("s", names(ufdat), perl=TRUE)])$alpha
ufRel <- (libRel + socRel) / 2
### Alpha is about 0.58; poor

### We conclude that data conforms to the unidimensional unfolding model and have the property of the ordinal specific objectivity; however it is liekly to be burdened with high measurement error


##########################################################
### PREPARE THE DATASET OBJECT WITH THE OPINION SCALES ###
##########################################################

### Compute the unfolding scores using the Van Schuur counting method
data.backup <- data
data <- dat_cz1
libsoc <- vector(mode="numeric", length=dim(data)[1])
libsoc[data$opinionIV == 0] <- apply(ufdat, 1, unfoldingScore) - 8
### We shift the scale to the midpoint (a person that gave positive answers only to the easiest items from both subscales)
libsoc[data$opinionIV == 1] <- NA
data$libsoc <- libsoc

### We compute the score using also the Van der Brug approach
wlibsoc <- vector(mode="numeric", length=dim(data)[1])
wlibsoc[data$opinionIV == 0] <- apply(ufdat, 1, unfoldingScore, TRUE) - 4.5
wlibsoc[data$opinionIV == 1] <- NA
data$wlibsoc <- wlibsoc
### Again the scale is shifted to the midpoint (defined in the same manner as before)

### Check the distributions of both variables
histogram(~libsoc + wlibsoc, data=data, par.settings=V4bgw,
          xlab="Liberalism-Socialism", ylab="Percent of Total",
          strip=strip.custom(factor.levels=c("Van Schuur score", "Van der Brug score")),
          layout=c(1, 2))
### Numerical summary
summary(data[, c("libsoc", "wlibsoc")])

### Correlation between the two scales
cor(data[, c("libsoc", "wlibsoc")], use="pair") ### almost the same

### Save the subscale (liberalism and socialism)
liberalism <- vector(mode="numeric", length=dim(data)[1])
liberalism[data$opinionIV == 0] <- ldat$lib
liberalism[data$opinionIV == 1] <- NA
data$liberalism <- liberalism

socialism <- vector(mode="numeric", length=dim(data)[1])
socialism[data$opinionIV == 0] <- sdat$soc
socialism[data$opinionIV == 1] <- NA
data$socialism <- socialism

#####################
### SAVE THE DATA ###
#####################

dat_cz2 <- data
### save as .txt file
### field separator is set to "\t"
write.table(dat_cz2, sep="\t", row.names=TRUE,
            file=normalizePath("./Data/MainData/dat_CZ2.txt"))
### Save as an R data object
save(dat_cz2, file=normalizePath("./Data/MainData/dat_CZ2.RData"))

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
#       [1] psych_1.5.4          mice_2.22            Rcpp_0.11.5          ca_0.58             
# [5] doBy_4.5-13          survival_2.38-1      knitr_1.10           mokken_2.7.7        
# [9] poLCA_1.4.1          MASS_7.3-39          scatterplot3d_0.3-35 reshape2_1.4.1      
# [13] latticeExtra_0.6-26  lattice_0.20-31      RColorBrewer_1.1-2  
# 
# loaded via a namespace (and not attached):
#       [1] splines_3.2.0       mnormt_1.5-2        stringr_0.6.2       plyr_1.8.1         
# [5] tools_3.2.0         nnet_7.3-9          parallel_3.2.0      grid_3.2.0         
# [9] randomForest_4.6-10 Matrix_1.2-0        rpart_4.1-9