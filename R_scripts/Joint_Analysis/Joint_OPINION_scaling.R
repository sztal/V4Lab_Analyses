############################################################
### Scaling of the OPINION items set in the joint sample ###
############################################################

### This script presents the procedure of the scaling the OPINION items set in the joint (PL and CZ) sample. We use three-step approach based (description TO BE FINISHED)

### Load data
load(normalizePath("./Data/MainData/dat_PL1.RData"))
load(normalizePath("./Data/MainData/dat_CZ1.RData"))
datpl <- dat_pl1[dat_pl1$opinionIV == 0, grep("^o[0-9]+", names(dat_pl1), perl = TRUE)]
datcz <- dat_cz1[dat_cz1$opinionIV == 0, grep("^o[0-9]+", names(dat_cz1), perl = TRUE)]

### Make a jont dataset
fulldat <- rbind(dat_pl1, dat_cz1)
country <- vector(mode="character", length=nrow(fulldat))
country[1:nrow(dat_pl1)] <- "PL"
country[(nrow(dat_pl1)+1):nrow(fulldat)] <- "CZ"
fulldat$country <- factor(country)
data <- rbind(datpl, datcz)

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
library(psych)
### Load V4 color theme
V4bgw <- V4themes("standard_bgw")

### We start with multiple correspondence analysis to ensure proper interpretation of the OPINION questions answers
fmca1 <- mjca(data, nd=NA, lambda="adjusted")
### We see that 'Don't Know' answers are cleary separated and clustered in the upper right corner and that 'Agree' and 'Disagree' form a strip going through the origin, what implies significant unidimensionality of the data (provided we do not pay attention to the 'Don't Knows'). The strip seems to correspond to liberalism-socialism dimension
### We use the solution plot to determine which answers ('Agrees' and 'Disagrees') correspond to which pole (i.e. liberalism or socialism).
### The we will be able to recode this data and create two datasets of binary items that may be then scaled using Mokken procedure to obtain two cumulative subscales: liberalism subscale and socialism subscale; they will be subsequently composed to make a single unidimensional unfolding prerferential (proximity) scale, which will measure respondents' positions alongside the liberalism-socialism axis.
### But first we remove item o15, because it is hard to determine its meaning in regard to the subscales of liberalism/socialism; the same applies to o17 and o23.

### Data trimmed to the chosen questions exclusively
data.lim1 <- data[, grep("^o(15|17|23)$", names(data), perl=TRUE, invert=TRUE)]

### Create new scales of liberalism and socialism
### First we derive datasets with answers mapped to the scales' answers
map <- read.table(normalizePath("./Data/Maps/Joint_OPINION_map.csv"),
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

### We will also aim at constructing subscales that consist of the same number of items and in which the items do not overlap. Lack of overlapping is important for the subsequent unfolding scaling, because if one item appears in both subscales it will appear twice in the unfolding scale as two separate binary items that will be structurally correlated (not perfectly due to the 'Don't Know' answer, but substantially) and this is not desired in unidimensional unfolding scales.

########################
### Liberalism scale ###
########################
### First we scale liberalism dataset using the Mokken Scale technique
### We use an inductive procedure based on a genetic algorithm to find a subset(s) of items that form the best scale(s)
set.seed(1)
libscales <- aisp(libdat, search="ga", lowerbound=.35)
### Most of the items form a strong, core scale, others are residual
libvars <- rownames(libscales)[libscales == 1]
### Create new dataset with scale item exclusively
ldat <- libdat[, libvars]

#######################
### Socialism scale ###
#######################
### Now we scale socialism dataset using the Mokken Scale technique
### We use an inductive procedure based on a genetic algorithm to find a subset(s) of items that form the best scale(s)
set.seed(77)
socscales <- aisp(socdat, search="ga", lowerbound=.35)
### Most of the items form a strong, core scale, others are residual
### (perhaps) there is a second subscale, but we will ignore it for a moment
socvars <- rownames(socscales)[socscales == 1]
### Create new dataset with scale item exclusively
sdat <- socdat[, socvars]

#############################################
### SCALING OF THE LIBERALISM SCALE ITEMS ###
#############################################
### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
libmono <- check.monotonicity(ldat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### No violations

### Check global scaling properties
libH <- coefH(ldat)
### acceptable to good scaling; 95% CI for H: 0.30-0.46
librel <- check.reliability(ldat)
### Cronbach's alpha of 0,48; very poor but the homogeneity coefficients are good

### Distribution of the scale scores
lib <- apply(ldat, 1, sum)
histogram(~lib, par.settings=V4bgw, xlab="Liberalism Scale Score", 
          ylab="Percent of Total", breaks=c(-1:4))
### Not exactly symmetric (people of low liberalism may be hard to distinguish)

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
ldat <- cbind(ldat, fulldat[fulldat$opinionIV==0, grep("les.*etat|mat_", 
                                                       names(fulldat), perl=TRUE)])
ldat$lib <- lib
round(cor(ldat[, grep("les.*eta|mat_|lib", names(ldat), perl=TRUE)], use="pairwise"), 3)
## There is quite strong correlation with the liberalism-etatism axis and some correlations with the economic dimension of the soceco matrix. Therefore the scale can be considered theoretically valid at the basic level.

############################################
### SCALING OF THE SOCIALISM SCALE ITEMS ###
############################################
### Check the scaling properties of the scale
### Check the monotonous homogeneity assumption
socmono <- check.monotonicity(sdat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the restscores method)
socrest <- check.restscore(sdat) ### no significant violations
### Check the double monotonicity assumption (using the pmatrix method)
socpmat <- check.pmatrix(sdat) ### Fits well the Mokken model!

### Check global scaling properties
socH <- coefH(sdat)
### good to very good scaling; 95% CI for H: 0.36-0.50
socrel <- check.reliability(sdat)
### Cronbach's alpha of 0,57; poor, but again the homogeneity is good

### Distribution of the scale scores
soc <- apply(sdat, 1, sum)
histogram(~soc, par.settings=V4bgw, xlab="Socialism Scale Score",
          ylab="Percent of Total", breaks=-1:4)
### Not exactly symmetric, but quite

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
sdat <- cbind(sdat, fulldat[fulldat$opinionIV==0, grep("les.*etat|mat_", 
                                                       names(fulldat), perl=TRUE)])
sdat$soc <- soc
round(cor(sdat[, grep("les.*eta|mat_|soc", names(sdat), perl=TRUE)], use="pairwise"), 3)
## There is quite strong correlation with the liberalism-etatism axis and some correlations with the economic dimension of the soceco matrix. Therefore the scale can be considered theoretically valid at the basic level.

### Check the items orderings of both scales (in regard to difficulty levels)
libdiff <- sort(apply(ldat[, grep("^l[0-9]+", names(ldat), perl=TRUE)], 2, mean))
socdiff <- sort(apply(sdat[, grep("^s[0-9]+", names(sdat), perl=TRUE)], 2, mean))

### Check the correlation between the two scales
round(cor(sdat$soc, ldat$lib), 3)
### good; correlation is negative, but not too strong

### Additionaly we check the distribution of the lib - soc
prox <- lib - soc
histogram(~prox, par.settings=V4bgw,
          xlab="Liberalism - Socialism Scores", ylab="Percent of Total",
          breaks=-5:4)

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
                                      n=250, seed=166)

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
ufCoefs <- unfoldingH(ufdat) ### all are good
### Standard errors of the coefficients (based on bootstrap)
ufSE <- unfoldingSE(ufdat, n=50)
### 95% CI for the H: [0.33 - 0.47]; good

### Check of the ordinal specific objectivity (formal unfolding property)
### We use the conditional adjacency matrix technique
CondMat <- ufConditionalMat(ufdat)
### Inspection of the Conditional Matrix suggests that the scale is not perfectly unfolding in a strict sense; it is rather two Mokken scales glued together back-to-back. It still may be considered a proximity scale, but not in a strict sense of the ordinal specific objectivity model

# Reliability
alpha(ufdat)
### So the Cronbach's Alpha is about 0.60 [0.55 - 0.66]; acceptable

### We conclude that the data conforms to the unidimensional in terms of scaling but it does not possess some of the formal properties of an unidimensional unfolding scale

### Check the distribution of the scale scores (we use standard Van Schuur method of scale scores computing)
libsoc <- vector(mode="numeric", length=nrow(fulldat))
libsoc[] <- NA
names(libsoc) <- rownames(fulldat)
libsoc[rownames(ufdat)] <- apply(ufdat, 1, unfoldingScore)
fulldat$libsoc <- libsoc
histogram(~libsoc, data=fulldat, par.settings=V4bgw,
          xlab="Liberalism-Socialism score", ylab="Percent of Total",
          breaks=0:15)
### Numerical summary
summary(fulldat$libsoc)

### Now the scale has to be validiated in both PL and CZ sample

#####################
### PL VALIDATION ###
#####################

ufdatpl <- ufdat[1:nrow(datpl), ]
### Scallability coefficients for items and the scale
ufCoefspl <- unfoldingH(ufdatpl) ### all are good or very good

### Check of the ordinal specific objectivity (formal unfolding property)
### We use the conditional adjacency matrix technique
CondMatpl <- ufConditionalMat(ufdatpl)
### Inspection of the Conditional Matrix suggests that the scale is not perfectly unfolding in a strict sense; it is rather two Mokken scales glued together back-to-back. It still may be considered a proximity scale, but not in a strict sense of the ordinal specific objectivity model

# Reliability
alpha(ufdatpl) # alpha of 0.67; acceptable

#####################
### CZ VALIDATION ###
#####################

ufdatcz <- ufdat[(nrow(datpl)+1):nrow(ufdat), ]
### Scallability coefficients for items and the scale
ufCoefscz <- unfoldingH(ufdatcz) ### the scale is poor in the CZ sample

### Check of the ordinal specific objectivity (formal unfolding property)
### We use the conditional adjacency matrix technique
CondMatcz <- ufConditionalMat(ufdatcz)
### Inspection of the Conditional Matrix suggests that the scale is not perfectly unfolding in a strict sense; it is rather two Mokken scales glued together back-to-back. It still may be considered a proximity scale, but not in a strict sense of the ordinal specific objectivity model

# Reliability
alpha(ufdatcz) # alpha of 0.54; poor