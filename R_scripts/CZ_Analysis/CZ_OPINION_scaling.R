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
map <- read.table(normalizePath("./Data/Maps/PL_to_liberal_map.csv"),
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
libmono <- check.monotonicity(ldat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the restscores method)
librest <- check.restscore(ldat) ### Fits well the Mokken model!
### Check the double monotonicity assumption (using the pmatrix method)
libpmat <- check.pmatrix(ldat) ### Fits well the Mokken model!

### So both monotonous homogeneity and double monotonicity assumption holds; however the items' scaling coefficients are not too great (but all are acceptable)

### Check global scaling properties
libH <- coefH(ldat)
### Acceptable scaling (but on on the edge of being poor); 
### 95% CI for H: 0.26-0.42
librel <- check.reliability(ldat)
### Cronbach's alpha of 0,60; Poor reliability

### Distribution of the scale scores
lib <- apply(ldat, 1, sum)
histogram(~lib, par.settings=V4bgw, xlab="Liberalism Scale Score", ylab="Percent of Total")
### Not exactly, but quite symmetric (people of high liberalism may be hard to distinguish)
### In all other respects seems fine

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
ldat <- cbind(ldat, dat_cz1[dat_cz1$opinionIV==0, grep("les.*etat|mat_", 
                                                       names(dat_cz1), perl=TRUE)])
ldat$lib <- lib
round(cor(ldat[, grep("les.*eta|mat_|lib", names(ldat), perl=TRUE)], use="pairwise"), 3)

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

### Check global scaling properties
socH <- coefH(sdat)
### Moderate scaling; 95% CI for H: 0.32-0.46
socrel <- check.reliability(sdat)
### Cronbach's alpha of 0,69; Rather poor but still acceptable

### Distribution of the scale scores
soc <- apply(sdat, 1, sum)
histogram(~soc, par.settings=V4bgw, xlab="Socialism Scale Score", ylab="Percent of Total")
### Not exactly symmetric,may not discriminate between weakly socialist respondents

### Basic check of the thoretical validity
### Get the data for the liberalism-etatism axis and the soceco matrix
sdat <- cbind(sdat, dat_cz1[dat_cz1$opinionIV==0, grep("les.*etat|mat_", 
                                                       names(dat_cz1), perl=TRUE)])
sdat$soc <- soc
round(cor(sdat[, grep("les.*eta|mat_|soc", names(sdat), perl=TRUE)], use="pairwise"), 3)

### Clearly there are enormous problems with the Czech sample. Probably they are a result of poor cultural adaptation of the questionnaire. The two main problems are following:
###   - Only poor and short scales may be constructed
###   - The scales' scores correlate inversly with the control questions; liberalism score is postiviely correlated with the estatism pole of the liberalism-etatism axis and the 