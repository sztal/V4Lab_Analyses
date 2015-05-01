#########################################################
### Scaling of the OPINION items set in the PL sample ###
#########################################################

### This script presents the procedure of the scaling the OPINION items set in the PL sample. We use three-step approach based (description TO BE FINISHED)

### Load data
load(normalizePath("./Data/MainData/dat_PL1.RData"))
data <- dat_pl1
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
### The first axis clearly corresponds to liberalism-socialism dimension. The second one is harder to interpret and it is likely that it is mostly residual and does not have any specific interpretaion. We see that almost all 'Don't Know' answers cluster together in the lower right corner and the 'Agree' and 'Disagree' answers form a strip going slightly diagonal through the origin. It suggest a strong unidimensional (bipolar) relationship between the items (excluding the 'Don't Know' answers).
### Therefore now we well exclude those items that do not fit well to the first dimension and are dominated by the 'Don't Know' answers (quality of 'Don't Know' is high while quality of the others is lower)
### Using this criterion we exclude : o2, o5, o15, o16, o17.

### Data trimmed to the chosen questions exclusively
data.lim1 <- data[, grep("^o(2|5|15|16|17)$", names(data), perl=TRUE, invert=TRUE)]
### Another MCA
fmca2 <- mjca(data.lim1, nd=NA, lambda="adjusted")

### Based on this analysis it is possible to tell which question and answers correspond with sympathy for liberalism and which with sympathy for socialism.
### Now it is possible to construct two unidimensional cumulative scales of liberalism and socialism. In these scales appropriate answers in questions ('Agrees' and 'Disagrees') are mapped to 1's and inapproriate (answers opposite to the correct ones and 'Don't Knows) to 0.
### So it is possible to scale them using IRT methods for cumulative scales such as the Mokken Scale

### Create new scales of liberalism and socialism
### First we derive datasets with answers mapped to the scales' answers
map <- read.table(normalizePath("./Data/Maps/PL_to_liberal_map.csv"),
                  sep=";", row.names=1, header=TRUE)
libdat <- mapOPINION(data.lim1, map, country="PL", verbose=TRUE, liberal=TRUE)
socdat <- mapOPINION(data.lim1, map, country="PL", verbose=TRUE, liberal=FALSE)

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

### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### only minor violations; none are significant
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### Perhaps l22 should be removed... (due to significant violations)

### New dataset without item l22
ldat <- ldat[, grep("l22", names(ldat), fixed=TRUE, invert=TRUE)]

### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### there are some violations, but none are significant
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### only minor and insignificant violations
### But l12 has very low itemwise scallability coefficient; should be removed

### New dataset without item l12
ldat <- ldat[, grep("l12", names(ldat), fixed=TRUE, invert=TRUE)]

### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### only minor and insignificant violations
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### fits well the Mokken model!
### All items have satisfctory itemwise scallability coefficients

### Check global scaling properties
libH <- coefH(ldat)
### Good scaling; 95% CI for H: 0.38-0.52
librel <- check.reliability(ldat)
### Cronbach's alpha of 0,73; Not ideal but satisfactory

### Distribution of the scale scores
lib <- apply(ldat, 1, sum)
histogram(~lib, par.settings=V4bgw, xlab="Liberalism Scale Score", ylab="Percent of Total")
### Not exactly symmetric (people of very low liberalism may be hard to distinguish)
### In all other respects seems fine

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
ldat <- cbind(ldat, dat_pl1[dat_pl1$opinionIV==0, grep("les.*etat|mat_", 
                                                    names(dat_pl1), perl=TRUE)])
ldat$lib <- lib
round(cor(ldat[, grep("les.*eta|mat_|lib", names(ldat), perl=TRUE)], use="pairwise"), 3)
## There is quite strong correlation with the liberalism-etatism axis and some correlations with the economic dimension of the soceco matrix. Therefore the scale can be considered theoretically valid at the basic level.

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
socmono <- check.monotonicity(sdat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the restscores method)
socrest <- check.restscore(sdat) ### only minor violations; none are significant
### Check the double monotonicity assumption (using the pmatrix method)
socpmat <- check.pmatrix(sdat) ### Fits well the Mokken model!

### Check global scaling properties
socH <- coefH(sdat)
### Moderate scaling; 95% CI for H: 0.32-0.46
socrel <- check.reliability(sdat)
### Cronbach's alpha of 0,69; Rather poor but still acceptable

### Distribution of the scale scores
soc <- apply(sdat, 1, sum)
histogram(~soc, par.settings=V4bgw, xlab="Socialism Scale Score", ylab="Percent of Total")
### Not exactly symmetric, but should discriminate quite well
### In all other respects seems fine

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
sdat <- cbind(sdat, dat_pl1[dat_pl1$opinionIV==0, grep("les.*etat|mat_", 
                                                       names(dat_pl1), perl=TRUE)])
sdat$soc <- soc
round(cor(sdat[, grep("les.*eta|mat_|soc", names(sdat), perl=TRUE)], use="pairwise"), 3)
### There is quite strong correlation with the liberalism-etatism axis and some correlations with the economic dimension of the soceco matrix. Therefore the scale can be considered theoretically valid at the basic level.


### Check the items orderings of both scales (in regard to difficulty levels)
libdiff <- sort(apply(ldat[, grep("^l[0-9]+", names(ldat), perl=TRUE)], 2, mean))
socdiff <- sort(apply(sdat[, grep("^s[0-9]+", names(sdat), perl=TRUE)], 2, mean))

### the orderings are almost perfect reversed reflections of each other; it implies that the scales may be combined to create a joint proximity unfolding scale.
### But first we need to assure that the scores of both scales are not functionally dependent
round(cor(sdat$soc, ldat$lib), 3) ### they are not!

### Additionaly we check the distribution of the lib - soc
prox <- lib - soc
histogram(~prox, par.settings=V4bgw,
          xlab="Liberalism - Socialism Scores", ylab="Percent of Total")

#################################################################
### COMBINED UNIDIMENSIONAL UNFOLDING SCALING OF  BOTH SCALES ###
#################################################################

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
### Very good scallability; However item H_15 (s19) does not scale (H < 0.3)
### So it should be removed
ufdat <- ufdat[, grep("s19", names(ufdat), invert=TRUE, fixed=TRUE)]
### Scallability coefficients for items and the scale
ufCoefs <- unfoldingH(ufdat) ### Now coefficients are very good
### Now we compute standard errors of these coefficients using bootstrap
ufSE <- unfoldingSE(ufdat, n=100)
### 95% CI for the H is: [0.42-0.54]
### Check of the ordinal specific objectivity (formal unfolding property)
### We use the conditional adjacency matrix technique
CondMat <- ufConditionalMat(ufdat)
### Inspection of the Conditional Matrix suggests that the scale is not perfectly unfolding in a strict sense; it has properties of two Mokken Scales glued together; this makes sense since this is how it was constructed; Nevertheless we will use unfolding scores for a more conscise analysis (one variable instead of two), but it should be kept in mind that the scale still need to be corrected and it does not ensure unidimensional bipolarity of socialism and liberalism; it can not be ruled out that these are separate dimensions; there is still more work to be done and more data (with differently formulated items) to be gathered

### For the end we compute the reliability of our quasi-unfolding scal. We use the fact that it is in fact two Mokken scales glued together back-tob-back, so the relability of this scale may be computed as weighted (by the numbers of items in each Mokken scale) average of reliability coefficients of the two scales.

libRel <- check.reliability(ufdat[, grep("l", names(ufdat), perl=TRUE)])$alpha
socRel <- check.reliability(ufdat[, grep("s", names(ufdat), perl=TRUE)])$alpha
ufRel <- (8*libRel + 6*socRel) / 14
### So the Cronbach's Alpha is about 0.72. Not very much, but enough to consider a scale satisfactory

### We conclude that the data conforms to the unidimensional in terms of scaling but it does not possess some of the formal properties of an unidimensional unfolding scale
