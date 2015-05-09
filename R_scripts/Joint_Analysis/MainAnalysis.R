#####################
### MAIN ANALYSIS ###
#####################

### This script conducts the main analysis; it contains of between-groups comparisons (PL vs. CZ; types of education etc.)

### Load data
load(normalizePath("./Data//MainData/finalData.RData"))
data <- fulldat

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
### Load plotting functions
source(normalizePath("./R_scripts/Visualization/PlottingFunctions.R"))
### Load processing tools
source(normalizePath("./R_scripts//data_processing/processingTools.R"))
### Load V4 color theme
source(normalizePath("./R_scripts//Visualization/Themes/LatticeThemes.R"))
V4bgw <- V4themes("standard_bgw")

#########################################
### Description of the main variables ###
#########################################

summary(data[, c("libsoc", "ngknow", "knowraw")])
lapply(data[, c("libsoc", "ngknow", "knowraw")], quantile, na.rm=TRUE,
       probs=seq(0, 1, .1))

### By country
lapply(data[, c("libsoc", "ngknow", "knowraw")],
       function(x) tapply(x, data$country, summary))
lapply(data[, c("libsoc", "ngknow", "knowraw")],
       function(x) tapply(x, data$country, quantile, na.rm=TRUE, probs=seq(0, 1, .1)))

### By education type
lapply(data[, c("libsoc", "ngknow", "knowraw")],
       function(x) tapply(x, data$eduprog3, summary))
lapply(data[, c("libsoc", "ngknow", "knowraw")],
       function(x) tapply(x, data$eduprog3, quantile, na.rm=TRUE, probs=seq(0, 1, .1)))

### By education type and country
lapply(data[, c("libsoc", "ngknow", "knowraw")],
       function(x) tapply(x, interaction(data$eduprog3, data$country), summary))
lapply(data[, c("libsoc", "ngknow", "knowraw")],
       function(x) tapply(x, interaction(data$eduprog3, data$country), quantile,
                          na.rm=TRUE, probs=seq(0, 1, .1)))

###########################################
### Analysis of the KNOWLEDGE questions ###
###########################################

kdata <- data[, grep("^k[0-9]+|country|eduprog3", names(data), perl=TRUE)]
kdat <- kdata[, grep("^k[0-9]+", names(kdata), perl=TRUE)]
### Joint sample
### Items difficulties with 95% Agresti-Coul confidence intervals
diffPlot <- plotBinary(kdat, ci=TRUE, reverse=TRUE, vline=TRUE, alpha=.05)
diffPlot <- update(diffPlot, xlab="Item Difficulty", par.settings=V4bgw)
### Numerical summary
diffOrd <- round(sort(sapply(kdat, 
                             function(x) 1 - mean(x, na.rm=TRUE)), decreasing=TRUE), 3)
JointSubsets <- list(tricky=names(diffOrd)[1:7], medium=names(diffOrd)[8:10],
                     easy=names(diffOrd)[11:29])
JointFrame <- dlevelFrame(kdat)

### Education type
### EBMF
EBMFplot <- plotBinary(kdat[kdata$eduprog3=="EBMF", ], ci=TRUE, reverse=TRUE, vline=TRUE)
EBMFplot <- update(EBMFplot, xlab="Item Difficulty", par.settings=V4bgw)
EBMFord <- round(sort(sapply(kdat[kdata$eduprog3=="EBMF", ], 
                             function(x) 1 - mean(x, na.rm=TRUE)), decreasing=TRUE), 3)

### SSHA
SSHAplot <- plotBinary(kdat[kdata$eduprog3=="SSHA", ], ci=TRUE, reverse=TRUE, vline=TRUE)
SSHAplot <- update(SSHAplot, xlab="Item Difficulty", par.settings=V4bgw)
SSHAord <- round(sort(sapply(kdat[kdata$eduprog3=="SSHA", ], 
                             function(x) 1 - mean(x, na.rm=TRUE)), decreasing=TRUE), 3)

### STEM
STEMplot <- plotBinary(kdat[kdata$eduprog3=="STEM", ], ci=TRUE, reverse=TRUE, vline=TRUE)
STEMplot <- update(STEMplot, xlab="Item Difficulty", par.settings=V4bgw)
STEMord <- round(sort(sapply(kdat[kdata$eduprog3=="STEM", ], 
                             function(x) 1 - mean(x, na.rm=TRUE)), decreasing=TRUE), 3)

###############
### COUNTRY ###
###############
### PL
PLplot <- plotBinary(kdat[kdata$country=="PL", ], ci=TRUE, reverse=TRUE, vline=TRUE)
PLplot <- update(PLplot, xlab="Item Difficulty", par.settings=V4bgw)
PLord <- round(sort(sapply(kdat[kdata$country=="PL", ], 
                             function(x) 1 - mean(x, na.rm=TRUE)), decreasing=TRUE), 3)
PLSubsets <- list(tricky=names(PLord)[1:5], medium=names(PLord)[6:11],
                     easy=names(PLord)[12:29])
PLFrame <- dlevelFrame(kdat[kdata$country=="PL", ])

### CZ
CZplot <- plotBinary(kdat[kdata$country=="CZ", ], ci=TRUE, reverse=TRUE, vline=TRUE)
CZplot <- update(CZplot, xlab="Item Difficulty", par.settings=V4bgw)
CZord <- round(sort(sapply(kdat[kdata$country=="CZ", ], 
                           function(x) 1 - mean(x, na.rm=TRUE)), decreasing=TRUE), 3)
CZSubsets <- list(tricky=names(CZord)[1:7], medium=names(CZord)[8:10],
                  easy=names(CZord)[11:29])
CZFrame <- dlevelFrame(kdat[kdata$country=="CZ", ])


### Basic comaprisons of PL and CZ samples
########################################
### PL vs. CZ : Liberalism-Socialism ###
########################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ country, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
useOuterStrips(histogram(~ libsoc+liberalism+socialism+rawlib+rawsoc | country, data=data, 
                        par.settings=V4bgw, layout=c(2,5), 
                        xlab="Liberalism-Socialism scale", ylab="Percent of Total", 
                        breaks=-7:19, par.strip.text=list(cex=.8)),
              strip.left=strip.custom(factor.levels = c("Liberalism-Socialism",
                                                        "Liberalism", "Socialism",
                                                        "Liberalism Raw Score",
                                                        "Socialism Raw Score")))
### Very similar distributions
### T-tests
lapply(data[, c("libsoc", "liberalism", "socialism", "rawlib", "rawsoc")],
       function(x) t.test(x ~ data$country, var.equal=FALSE))
lapply(data[, c("libsoc", "liberalism", "socialism", "rawlib", "rawsoc")],
       function(x) cohen.d(x ~ data$country, pooled=TRUE))
### difference for socialism is medium; for lib-soc small
bwplot(libsoc ~ country, data=data, par.settings=V4bgw, ylab="Liberalism-Socialism")

#################################################################
### PL vs. CZ : self-description economic and social opinions ###
#################################################################
summaryBy(leseferism.etatism + mat_econimic + mat_social ~ country, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
useOuterStrips(histogram(~ leseferism.etatism + mat_econimic + mat_social | country, 
                         data=data, par.settings=V4bgw, layout=c(2,3), 
                         xlab="Response Value", ylab="Percent of Total", 
                         breaks=0:10),
               strip.left=strip.custom(factor.levels = c("Leseferism-Etatism Axis",
                                                         "Matrix: Economic Dimension",
                                                         "Matrix: Social Dimension")))

### T-tests
lapply(data[, c("leseferism.etatism", "mat_econimic", "mat_social")],
       function(x) t.test(x ~ data$country, var.equal=FALSE))
lapply(data[, c("leseferism.etatism", "mat_econimic", "mat_social")],
       function(x) cohen.d(x ~ data$country, pooled=TRUE))
### CZ is more conservative is the social dimension, but the magnitude of difference is small

##################################################
### PL vs. CZ : KNOWLEDGE scores (non-guessed) ###
##################################################
summaryBy(ngknow + knowraw ~ country, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
useOuterStrips(histogram(~ ngknow + knowraw | country, data=data, 
                         par.settings=V4bgw, layout=c(2,2), 
                         xlab="Knowledge Score", ylab="Percent of Total", 
                         breaks=-14:26),
               strip.left=strip.custom(factor.levels = c("Non-guessed Knowledge Score",
                                                         "Raw Knowledge Score")))
### T-tests
lapply(data[, c("ngknow", "knowraw")],
       function(x) t.test(x ~ data$country, var.equal=FALSE))
lapply(data[, c("ngknow", "knowraw")],
       function(x) cohen.d(x ~ data$country, pooled=TRUE))
### No difference
bwplot(ngknow ~ country, data=data, par.settings=V4bgw,
       ylab="Non-guessed knowledge score")

### Basic comparisons of types of education
##############################################
### Education types : Liberalism-Socialism ###
##############################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ eduprog3, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:3, ]
useOuterStrips(histogram(~libsoc+liberalism+socialism+rawlib+rawsoc | eduprog3, data=data, 
                         par.settings=V4bgw, layout=c(3,5), 
                         xlab="Liberalism-Socialism scale", ylab="Percent of Total", 
                         breaks=-7:19, par.strip.text=list(cex=.7)),
               strip.left=strip.custom(factor.levels = c("Liberalism-Socialism",
                                                         "Liberalism", "Socialism",
                                                         "Liberalism Raw Score",
                                                         "Socialism Raw Score")))
### Anovas
lapply(data[, c("libsoc", "liberalism", "socialism", "rawlib", "rawsoc")],
       function(x) summary(lm(x ~ data$eduprog3)))
bwplot(libsoc ~ eduprog3, data=data, par.settings=V4bgw, ylab="Liberalism-Socialism")

##########################################
### Education types : Knowledge scores ###
##########################################
summaryBy(ngknow + knowraw ~ eduprog3, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:3, ]
useOuterStrips(histogram(~ ngknow + knowraw | eduprog3, data=data, 
                         par.settings=V4bgw, layout=c(2,3), 
                         xlab="Knowledge Score", ylab="Percent of Total", 
                         breaks=-14:26),
               strip.left=strip.custom(factor.levels = c("Non-guessed Knowledge Score",
                                                         "Raw Knowledge Score")))
### Anovas
lapply(data[, c("ngknow", "knowraw")],
       function(x) summary(lm(x ~ data$eduprog3)))
bwplot(ngknow ~ eduprog3, data=data, par.settings=V4bgw,
       ylab="Non-guessed knowledge score")

###################################################
### Parental education : Liberalism - Socialism ###
###################################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ peduord, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:7, ]
useOuterStrips(histogram(~libsoc+liberalism+socialism+rawlib+rawsoc | peduord, data=data, 
                         par.settings=V4bgw, layout=c(7,5), 
                         xlab="Liberalism-Socialism scale", ylab="Percent of Total", 
                         breaks=-7:19, par.strip.text=list(cex=.7),
                         strip=strip.custom(factor.levels = c(
                               "I", "II", "III", "IV", "V", "VI"))),
               strip.left=strip.custom(factor.levels = c("Liberalism-Socialism",
                                                         "Liberalism", "Socialism",
                                                         "Liberalism Raw Score",
                                                         "Socialism Raw Score")))
### Anovas
lapply(data[, c("libsoc", "liberalism", "socialism", "rawlib", "rawsoc")],
       function(x) summary(lm(x ~ data$peduord)))
### Boxplots for the libsoc scale
bwplot(libsoc ~ peduord, data=data, par.settings=V4bgw, ylab="Liberalism-Socialism",
       xlab="Ordinal Indicator of Joint Parental Education", horizontal=FALSE,
       panel=function(...) {
             panel.bwplot(...)
             panel.abline(a=.70271, b=-.09216, lty=3, lwd=2, col="gray11")
             panel.text(x=1.5, y=-.1, labels="y = 0.70 - 0.09x", col="red3")
             })
### Clearly there is no relationship between socialism-liberalism and joint parental education
lowerCor(data[, c("libsoc", "leseferism.etatism",
                  "mat_econimic", "mat_social", "peduord")])
corr.test(data[, c("libsoc", "leseferism.etatism",
                   "mat_econimic", "mat_social", "peduord")])

### By country
### Distributions
tapply(data$libsoc, interaction(data$peduord, data$country), summary)
bwplot(libsoc ~ peduord | country, data=data, par.settings=V4bgw, ylab="Liberalism-Socialism",
       xlab="Ordinal Indicator of Joint Parental Education", horizontal=FALSE,
       scales=list(x=list(labels=as.character(0:6))))
### Clearly interventionist attitudes are more or less constant amongst Czech student, but fall with parental education amongst Polish students

### Interaction model (peduord * country)
summary(lm(libsoc ~ peduord * country, data=data))
### There is slight linear tren in the PL sample
### the higher joint parental education, the stronger liberalism

#################################################
### Father education : Liberalism - Socialism ###
#################################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ father_edu, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:4, ]
useOuterStrips(histogram(~libsoc+liberalism+socialism+rawlib+rawsoc | father_edu,
                         data=data, par.settings=V4bgw, layout=c(7,5), 
                         xlab="Liberalism-Socialism scale", ylab="Percent of Total", 
                         breaks=-7:19, par.strip.text=list(cex=.7),
                         strip=strip.custom(factor.levels = c(
                               "I", "II", "III", "IV", "V", "VI"))),
               strip.left=strip.custom(factor.levels = c("Liberalism-Socialism",
                                                         "Liberalism", "Socialism",
                                                         "Liberalism Raw Score",
                                                         "Socialism Raw Score")))
### Anovas
lapply(data[, c("libsoc", "liberalism", "socialism", "rawlib", "rawsoc")],
       function(x) summary(lm(x ~ data$father_edu)))
### Boxplots for the libsoc scale
bwplot(libsoc ~ father_edu, data=data, par.settings=V4bgw, ylab="Liberalism-Socialism",
       xlab="Father Education", horizontal=FALSE)

#################################################
### Mother education : Liberalism - Socialism ###
#################################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ mother_edu, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:4, ]
useOuterStrips(histogram(~libsoc+liberalism+socialism+rawlib+rawsoc | mother_edu,
                         data=data, par.settings=V4bgw, layout=c(7,5), 
                         xlab="Liberalism-Socialism scale", ylab="Percent of Total", 
                         breaks=-7:19, par.strip.text=list(cex=.7),
                         strip=strip.custom(factor.levels = c(
                               "I", "II", "III", "IV", "V", "VI"))),
               strip.left=strip.custom(factor.levels = c("Liberalism-Socialism",
                                                         "Liberalism", "Socialism",
                                                         "Liberalism Raw Score",
                                                         "Socialism Raw Score")))
### Anovas
lapply(data[, c("libsoc", "liberalism", "socialism", "rawlib", "rawsoc")],
       function(x) summary(lm(x ~ data$mother_edu)))
### Boxplots for the libsoc scale
bwplot(libsoc ~ mother_edu, data=data, par.settings=V4bgw, ylab="Liberalism-Socialism",
       xlab="Mother Education", horizontal=FALSE)

#############################################
### Parental education : KNOWLEDGE scores ###
#############################################
summaryBy(ngknow + knowraw ~ peduord, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:7, ]
useOuterStrips(histogram(~ ngknow + knowraw | peduord, data=data, 
                         par.settings=V4bgw, layout=c(2,3), 
                         xlab="Knowledge Score", ylab="Percent of Total", 
                         breaks=-14:26),
               strip.left=strip.custom(factor.levels = c("Non-guessed Knowledge Score",
                                                         "Raw Knowledge Score")))
### Anovas
lapply(data[, c("ngknow", "knowraw")],
       function(x) summary(lm(x ~ data$peduord)))
lapply(data[, c("ngknow", "knowraw")],
       function(x) summary(lm(x ~ as.factor(data$peduord))))
bwplot(ngknow ~ peduord, data=data, ylab="Non-guessed KNOWLEDGE Scores",
       xlab="Ordinal Joint Parental Education Indicator", horizontal=FALSE,
       par.settings=V4bgw)

### By country
### Distributions
tapply(data$ngknow, interaction(data$peduord, data$country), summary)
bwplot(ngknow ~ peduord | country, data=data, par.settings=V4bgw, ylab="Non-guessed KNOWLEDGE scores",
       xlab="Ordinal Indicator of Joint Parental Education", horizontal=FALSE,
       scales=list(x=list(labels=as.character(0:6))))
### Very low knowledge in the highest group in Poland is probably due to the sampling error, since this group is very small (n=5)
### Therefore we will test models including the highest group and not

### Interaction model with OIJPE <= 6 (peduord * country)
summary(lm(ngknow ~ peduord * country, data=data))
### no association
### Interaction model with OIJPE <= 5 (peduord * country)
### no association either
summary(lm(ngknow ~ peduord * country, data=data, subset=data$peduord <= 5))

tapply(data$peduord, data$country, table)

###########################################
### Father education : KNOWLEDGE scores ###
###########################################
summaryBy(ngknow + knowraw ~ father_edu, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:4, ]
useOuterStrips(histogram(~ ngknow + knowraw | father_edu, data=data, 
                         par.settings=V4bgw, layout=c(2,3), 
                         xlab="Knowledge Score", ylab="Percent of Total", 
                         breaks=-14:26),
               strip.left=strip.custom(factor.levels = c("Non-guessed Knowledge Score",
                                                         "Raw Knowledge Score")))
### Anovas
lapply(data[, c("ngknow", "knowraw")],
       function(x) summary(lm(x ~ data$father_edu)))
bwplot(ngknow ~ father_edu, data=data, ylab="Non-guessed KNOWLEDGE Scores",
       xlab="Father Education", horizontal=FALSE,
       par.settings=V4bgw)

###########################################
### Mother education : KNOWLEDGE scores ###
###########################################
summaryBy(ngknow + knowraw ~ mother_edu, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:4, ]
useOuterStrips(histogram(~ ngknow + knowraw | mother_edu, data=data, 
                         par.settings=V4bgw, layout=c(2,3), 
                         xlab="Knowledge Score", ylab="Percent of Total", 
                         breaks=-14:26),
               strip.left=strip.custom(factor.levels = c("Non-guessed Knowledge Score",
                                                         "Raw Knowledge Score")))
### Anovas
lapply(data[, c("ngknow", "knowraw")],
       function(x) summary(lm(x ~ data$mother_edu)))
bwplot(ngknow ~ mother_edu, data=data, ylab="Non-guessed KNOWLEDGE Scores",
       xlab="Mother Education", horizontal=FALSE,
       par.settings=V4bgw)

#################################################
### Liberalism-Socialism vs. KNOWLEDGE scores ###
#################################################
lowerCor(data[, grep("libsoc|les.*et|mat_|ngknow", names(data), perl=TRUE)])
knowlibsoclm <- lm(ngknow ~ libsoc, data=data, subset=knowIV==0)
xyplot(ngknow ~ libsoc, data=data, par.settings=V4bgw, xlab="Liberalism-Socialism",
       ylab="Non-guessed KNOWLEDGE score", panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.15, pos=3)
       })

### Controlling for country
lowerCor(data[data$country=="PL",
             grep("libsoc|les.*et|mat_|ngknow", names(data), perl=TRUE)])
lowerCor(data[data$country=="CZ",
              grep("libsoc|les.*et|mat_|ngknow", names(data), perl=TRUE)])
summary(lm(ngknow ~ libsoc*country, data=data, subset=knowIV==0))
summary(lm(ngknow ~ libsoc, data=data, subset=knowIV==0 & country=="PL"))
summary(lm(ngknow ~ libsoc, data=data, subset=knowIV==0 & country=="CZ"))
xyplot(ngknow ~ libsoc | country, data=data, par.settings=V4bgw,
       xlab="Liberalism-Socialism", ylab="Non-guessed KNOWLEDGE score",
       layout=c(1,2), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.15, pos=3)
       })
### In both coutnry high socialist attitudes are associated with lower KNOWLEDGE scores; however the effect is stronger in the Polish sample (10% of variance in PL vs. 4% in CZ)

### Controlling for type of education
lowerCor(data[data$eduprog3=="EBMF",
              grep("libsoc|les.*et|mat_|ngknow", names(data), perl=TRUE)])
lowerCor(data[data$eduprog3=="SSHA",
              grep("libsoc|les.*et|mat_|ngknow", names(data), perl=TRUE)])
lowerCor(data[data$eduprog3=="STEM",
              grep("libsoc|les.*et|mat_|ngknow", names(data), perl=TRUE)])
summary(lm(ngknow ~ libsoc*eduprog3, data=data, subset=knowIV==0))
summary(lm(ngknow ~ libsoc, data=data, subset=knowIV==0 & eduprog3=="EBMF"))
summary(lm(ngknow ~ libsoc, data=data, subset=knowIV==0 & eduprog3=="SSHA"))
summary(lm(ngknow ~ libsoc, data=data, subset=knowIV==0 & eduprog3=="STEM"))
xyplot(ngknow ~ libsoc | eduprog3, data=data, par.settings=V4bgw,
       xlab="Liberalism-Socialism", ylab="Non-guessed KNOWLEDGE score",
       layout=c(1,3), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.15, pos=3)
       })

################################################
### Liberalism-Socialism vs. WORK EXPERIENCE ###
################################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ work_experience, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:3, ]
useOuterStrips(histogram(~libsoc+liberalism+socialism+rawlib+rawsoc | work_experience,
                         data=data, par.settings=V4bgw, layout=c(7,5), 
                         xlab="Liberalism-Socialism scale", ylab="Percent of Total", 
                         breaks=-7:19, par.strip.text=list(cex=.7),
                         strip=strip.custom(factor.levels = c(
                               "I", "II", "III", "IV", "V", "VI"))),
               strip.left=strip.custom(factor.levels = c("Liberalism-Socialism",
                                                         "Liberalism", "Socialism",
                                                         "Liberalism Raw Score",
                                                         "Socialism Raw Score")))
### Anovas
data$wexp <- factor(data$work_experience, ordered=FALSE)
lapply(data[, c("libsoc", "liberalism", "socialism", "rawlib", "rawsoc")],
       function(x) summary(lm(x ~ data$wexp)))
### Boxplots for the libsoc scale
bwplot(libsoc ~ work_experience, data=data, par.settings=V4bgw,
       ylab="Liberalism-Socialism", xlab="Work Experience", horizontal=FALSE)
### Test of linear constrasts:
### I (wexp1) : no experience vs. up to 1 year and 1 year+ toghether
### II (wexp2) : up to 1 year vs. 1year+
### Intercept in the model corresponds to the average of cell means
mat <- matrix(c(1/3,1/3,1/3,
                1,-1/2,-1/2,
                0,1,-1), 3, 3)
mymat <- solve(t(mat))
contrasts(data$wexp) <- mymat[, 2:3]
summary(lm(libsoc ~ wexp, data=data))
### Respondents with work experience tend to be more liberal

### Controlling for country
bwplot(libsoc ~ work_experience | country, data=data, par.settings=V4bgw,
       ylab="Liberalism-Socialism", xlab="Work Experience", horizontal=FALSE,
       layout=c(2, 1))
anova(lm(libsoc ~ wexp * country, data=data)) # No interaction effect

############################################
### KNOWLEDGE SCORES vs. WORK EXPERIENCE ###
############################################
summaryBy(ngknow + knowraw ~ wexp, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)[1:3, ]
### Anovas
lapply(data[, c("ngknow", "knowraw")],
       function(x) summary(lm(x ~ data$wexp)))
### Test of linear constrasts:
### I (wexp1) : no experience vs. up to 1 year and 1 year+ toghether
### II (wexp2) : up to 1 year vs. 1year+
### Intercept in the model corresponds to the average of cell means
mat <- matrix(c(1/3,1/3,1/3,
                1,-1/2,-1/2,
                0,1,-1), 3, 3)
mymat <- solve(t(mat))
contrasts(data$wexp) <- mymat[, 2:3]
summary(lm(ngknow ~ wexp, data=data))
contrasts(data$wexp) <- contr.treatment(3)
### We se that the first contrast is significant, while the second is not
### Therefore it may be concluded that respondens with work experience have higher economic/financial knowledge
bwplot(ngknow ~ work_experience, data=data, ylab="Non-guessed KNOWLEDGE Scores",
       xlab="Work Experience", horizontal=FALSE,
       par.settings=V4bgw)

### Controling for country
bwplot(ngknow ~ work_experience | country, data=data, par.settings=V4bgw,
       ylab="Non-guessed KNOWLEDGE Scores", xlab="Work Experience", horizontal=FALSE,
       layout=c(2, 1))
anova(lm(ngknow ~ wexp * country, data=data)) # Interaction very close to significance

### Liberalism-Socialism and knowledge scores and study year
### First we create numerical variable indicating study year
data$uniyear <- as.numeric(data$year_at_uni)

### Correlations
corr.test(data[, c("libsoc", "ngknow", "uniyear")])
xyplot(ngknow ~ uniyear, data=data, par.settings=V4bgw,
       xlab="Study year", ylab="Non-guessed KNOWLEDGE score",
       layout=c(1,1), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.85, pos=3)
       })
summary(lm(ngknow ~ uniyear, data=data))
### Relationship is significant

### Controling for type of education
### Interaction effect
anova(lm(ngknow ~ uniyear*eduprog3, data=data)) # no significant interaction
xyplot(ngknow ~ uniyear | eduprog3, data=data, par.settings=V4bgw,
       xlab="Study year", ylab="Non-guessed KNOWLEDGE score",
       layout=c(1,3), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.85, pos=3)
       })

### Controling for type of education and country
anova(lm(ngknow ~ uniyear*eduprog3*country, data=data))
### Now we have an interaction between uniyear, country and type of education
xyplot(ngknow ~ uniyear | country + eduprog3, data=data, par.settings=V4bgw,
       xlab="Study year", ylab="Non-guessed KNOWLEDGE score",
       layout=c(2,3), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.70, pos=3)
       })

### Models in the subgroups
for(edu in levels(data$eduprog3)) {
      for(cntry in levels(data$country)) {
            cat(sprintf("######\nGROUP : %s in %s\n######\n", edu, cntry))
            print(summary(lm(ngknow ~ uniyear, data=data,
                       subset = data$eduprog3==edu & data$country==cntry)))
      }
}

##############################################
### Liberalism-socialism and hometown size ###
##############################################
### There are different variable levels in the Czech and Polish sample, so we already split the analysis by country
### By country
L <- tapply(data$libsoc, interaction(data$hometown_size, data$country), summary)
L[as.logical(Map(Negate(is.null), L))]

### Create hometown size variables for countries
### CZ
CZ_hsize <- data$hometown_size
CZ_hsize[data$country != "CZ"] <- NA
CZ_hsize <- droplevels(CZ_hsize)
### PL
PL_hsize <- data$hometown_size
PL_hsize[data$country != "PL"] <- NA
PL_hsize <- droplevels(PL_hsize)
### Add new variables to the dataset
data$PL_hsize <- PL_hsize
data$CZ_hsize <- CZ_hsize

### rather similar distributions
### PL
bwplot(libsoc ~ hometown_size, data=data[data$country=="PL", ], par.settings=V4bgw,
       ylab="Liberalism-interventionism score")
### CZ
bwplot(libsoc ~ hometown_size, data=data[data$country=="CZ", ], par.settings=V4bgw,
       ylab="Liberalism-interventionism score")
### no apparent and systematic differences; rather only noise

### Formal tests within countries
### PL
anova(lm(libsoc ~ PL_hsize, data=data, subset=data$country=="PL"))
### CZ
anova(lm(libsoc ~ CZ_hsize, data=data, subset=data$country=="CZ"))
### nothing happens here

#######################################################
### Knowledge scores and hometown size (by country) ###
#######################################################
L <- lapply(data[, c("ngknow", "knowraw")],
            function(x) tapply(x, interaction(data$hometown_size, data$country), 
                               summary))
cat("######\nNon-guessed scores\n######\n")
L[[1]][as.logical(Map(Negate(is.null), L[[1]]))]
cat("######\nRaw scores\n######\n")
L[[2]][as.logical(Map(Negate(is.null), L[[2]]))]

### Plots
### PL
bwplot(ngknow ~ hometown_size, data=data[data$country=="PL", ], par.settings=V4bgw,
       ylab="Liberalism-interventionism score")
### CZ
bwplot(ngknow ~ hometown_size, data=data[data$country=="CZ", ], par.settings=V4bgw,
       ylab="Liberalism-interventionism score")
### no apparent and systematic differences

### Formal tests within countries
### PL
anova(lm(ngknow ~ PL_hsize, data=data, subset=data$country=="PL"))
### CZ
anova(lm(ngknow ~ CZ_hsize, data=data, subset=data$country=="CZ"))
### nothing going on here

############################################################
### Analsysis of the strictly financial knowledge scores ###
############################################################
### Non-guessed financial knowledge scores are based on the following subset of the knowledge items with specifically financial content:

### - k1, k2, k6, k8, k12, k13, k14, k18, k22, k25, k28, k30.

finitems <- paste("k", c(1,2,6,8,12,13,14,18,22,25,28,30), sep="")
ngfinance <- apply(kdat[, finitems], 1, nonguessedKnowScore)
histogram(~ngfinance, par.settings=V4bgw, xlab="Percent of Total",
          ylab="Non-guessed financial knowledge scores")
summary(ngfinance)
data$ngfinance <- ngfinance

## By country
tapply(data$ngfinance, data$country, summary) # similar, but not the same
t.test(ngfinance ~ country, data=data, var.equal=FALSE) # no difference

## By type of education
tapply(data$ngfinance, data$eduprog3, summary)
summary(lm(ngfinance ~ eduprog3, data=data))
### We see that EBMF has highest scores and STEM is in the middle
bwplot(ngfinance ~ eduprog3, data=data, par.settings=V4bgw,
       ylab="Non-guessed financial knowledge scores")

## By country and type of education
### Helper categorical joint indicator of eduprog and country
data$edu3country <- interaction(data$eduprog3, data$country)
tapply(data$ngfinance, data$edu3country, summary)
anova(lm(ngfinance ~ country * eduprog3, data=data))
### There is significatn interaction
summary(lm(ngfinance ~ country * eduprog3, data=data))
bwplot(ngfinance ~ edu3country, data=data, par.settings=V4bgw,
       ylab="Non-guessed financial knowledge scores")

### Financial knowledge and liberalism-socialism
corr.test(data[, c("ngfinance", "libsoc")]) ### significant
### The higher socialist attitudes are the lower financial knowledge is
### Regression model of this association
summary(lm(ngknow ~ libsoc, data=data)) # 7% of the variance is retained

### By education type
### interaction effect
anova(lm(ngfinance ~ libsoc * eduprog3, data=data)) # not significant

### By country
### interaction effect
anova(lm(ngfinance ~ libsoc * country, data=data)) # notsignificant

### By education type and country
### 3-way interaction effect
anova(lm(ngfinance ~ libsoc * country * eduprog3, data=data))
### no 3-way and no 2-way interactions with liberalism-socialism

### Financial knowledge scores and study year
corr.test(data[, c("ngfinance", "uniyear")]) # significant relationship
xyplot(ngfinance ~ uniyear, data=data, par.settings=V4bgw,
       xlab="Study year", ylab="Non-guessed KNOWLEDGE score",
       layout=c(1,1), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.85, pos=3)
       })
summary(lm(ngfinance ~ uniyear, data=data))

### Controling for country and education type
xyplot(ngfinance ~ uniyear | country + eduprog3, data=data, par.settings=V4bgw,
       xlab="Study year", ylab="Non-guessed KNOWLEDGE score",
       layout=c(2,3), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="black",
                           digits=2, offset=1, at=.70, pos=3)
       })
anova(lm(ngfinance ~ uniyear * country * eduprog3, data=data))
### There are significant interaction between country and eduprog and 3-way interaction of this two factors and study year

### Models in the subgroups
for(edu in levels(data$eduprog3)) {
      for(cntry in levels(data$country)) {
            cat(sprintf("######\nGROUP : %s in %s\n######\n", edu, cntry))
            print(summary(lm(ngfinance ~ uniyear, data=data,
                             subset = data$eduprog3==edu & data$country==cntry)))
      }
}
### No group has truly significant effect

### FINANCIAL KNOWLEDGE AND HOMWETOWN SIZE ###
L <- tapply(data$ngfinance, interaction(data$hometown_size, data$country), summary)
L[as.logical(Map(Negate(is.null), L))]

### Plots
### PL
bwplot(ngfinance ~ hometown_size, data=data[data$country=="PL", ], par.settings=V4bgw,
       ylab="Liberalism-interventionism score")
### CZ
bwplot(ngfinance ~ hometown_size, data=data[data$country=="CZ", ], par.settings=V4bgw,
       ylab="Liberalism-interventionism score")
### no apparent and systematic differences

### Formal tests within countries
### PL
anova(lm(ngfinance ~ PL_hsize, data=data, subset=data$country=="PL"))
### CZ
anova(lm(ngfinance ~ CZ_hsize, data=data, subset=data$country=="CZ"))
### nothing going on here

###########################################################################
### Self-descriptory variables and parental education and hometown size ###
###########################################################################

### Leseferism-etatism axis and soceco matrix vs. OIJPE
lapply(data[, c("leseferism.etatism", "mat_econimic", "mat_social")],
       function(x) tapply(x, data$peduord, summary))
### Leseferism-etatism
bwplot(leseferism.etatism ~ peduord, data=data, par.settings=V4bgw,
       ylab="Leseferism-etatism axis", horizontal=FALSE,
       scales=list(x=list(labels=as.character(0:6))))
### Formal test
summary(lm(leseferism.etatism ~ peduord, data=data)) # nothing
### Interaction with country
summary(lm(leseferism.etatism ~ peduord * country, data=data)) # nothing

### Economic dimension of the matrix
bwplot(mat_econimic ~ peduord, data=data, par.settings=V4bgw,
       ylab="Matrix: economic", horizontal=FALSE,
       scales=list(x=list(labels=as.character(0:6))))
### Formal test
summary(lm(mat_econimic ~ peduord, data=data)) # nothing
### Interaction with country
summary(lm(mat_econimic ~ peduord * country, data=data)) # nothing

### Social dimension of the matrix
bwplot(mat_social ~ peduord, data=data, par.settings=V4bgw,
       ylab="Matrix: economic", horizontal=FALSE,
       scales=list(x=list(labels=as.character(0:6))))
### Formal test
summary(lm(mat_social ~ peduord, data=data)) # nothing
### Interaction with country
summary(lm(mat_social ~ peduord * country, data=data)) # nothing

### Social dimension of the matrix vs. hometown size
### PL
tapply(data[data$country=="PL", "mat_social"], data[data$country=="PL", "PL_hsize"],
       summary)
bwplot(mat_social ~ PL_hsize, data=data[data$country=="PL", ], par.settings=V4bgw,
       ylab="Matrix: social")
summary(lm(mat_social ~ hometown_size, data=data, subset=data$country=="PL"))
### linear contrast is significant in Poland; the bigger hometown is the less conservative a respondent is

### CZ
tapply(data[data$country=="CZ", "mat_social"], data[data$country=="CZ", "CZ_hsize"],
       summary)
bwplot(mat_social ~ CZ_hsize, data=data[data$country=="CZ", ], par.settings=V4bgw,
       ylab="Matrix: social")
summary(lm(mat_social ~ hometown_size, data=data, subset=data$country=="CZ"))
### no association in the Czech sample

############################################################################
### Correlations between liberalism-interventionism and self-description ###
############################################################################

### Joint sample
corr.test(data[, c("libsoc", "leseferism.etatism", "mat_econimic", "mat_social")])
### Yep, it works as it should

### PL sample
corr.test(data[data$country == "PL",
               c("libsoc", "leseferism.etatism", "mat_econimic", "mat_social")])
### Yep, it works as it should

### CZ sample
corr.test(data[data$country == "CZ",
               c("libsoc", "leseferism.etatism", "mat_econimic", "mat_social")])

#######################################
### SAVE THE EXTENDED FINAL DATASET ###
#######################################

finalDataExtended <- data[, -which(names(data) == "wexp")]
write.table(finalDataExtended, sep="\t", row.names=TRUE,
            file=normalizePath("./Data/MainData/finalDataExtended.txt"))
save(finalDataExtended, file=normalizePath("./Data/MainData/finalDataExtended.RData"))

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
#       [1] multcomp_1.4-0      TH.data_1.0-6       mvtnorm_1.0-2       sandwich_2.3-3     
# [5] psych_1.5.4         heplots_1.0-15      car_2.0-25          effsize_0.5.4      
# [9] latticeExtra_0.6-26 RColorBrewer_1.1-2  lattice_0.20-31     doBy_4.5-13        
# [13] survival_2.38-1    
# 
# loaded via a namespace (and not attached):
#       [1] Rcpp_0.11.5      splines_3.2.0    MASS_7.3-39      mnormt_1.5-2     minqa_1.2.4     
# [6] tools_3.2.0      nnet_7.3-9       pbkrtest_0.4-2   parallel_3.2.0   grid_3.2.0      
# [11] nlme_3.1-120     mgcv_1.8-6       quantreg_5.11    lme4_1.1-7       Matrix_1.2-0    
# [16] nloptr_1.0.4     codetools_0.2-11 SparseM_1.6      zoo_1.7-12 