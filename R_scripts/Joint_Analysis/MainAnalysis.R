#####################
### MAIN ANALYSIS ###
#####################

### This script conducts the main analysis; it contains of between-groups comparisons (PL vs. CZ; types of education etc.)

### Load data
load(normalizePath("./Data//MainData/finalData.RData"))
source(normalizePath("./R_scripts//Visualization/Themes/LatticeThemes.R"))
data <- fulldat

### Load packages
library(lattice)
library(latticeExtra)
library(doBy)
library(effsize)
library(heplots)
library(car)
library(psych)
library(sandwich)
library(mvtnorm)
library(TH.data)
library(multcomp)
### Load V4 color theme
V4bgw <- V4themes("standard_bgw")

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




### Basic comparisons of types of education
##############################################
### Education types : Liberalism-Socialism ###
##############################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ eduprog3, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
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

##########################################
### Education types : Knowledge scores ###
##########################################
summaryBy(ngknow + knowraw ~ eduprog3, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
useOuterStrips(histogram(~ ngknow + knowraw | eduprog3, data=data, 
                         par.settings=V4bgw, layout=c(2,3), 
                         xlab="Knowledge Score", ylab="Percent of Total", 
                         breaks=-14:26),
               strip.left=strip.custom(factor.levels = c("Non-guessed Knowledge Score",
                                                         "Raw Knowledge Score")))
### Anovas
lapply(data[, c("ngknow", "knowraw")],
       function(x) summary(lm(x ~ data$eduprog3)))

###################################################
### Parental education : Liberalism - Socialism ###
###################################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ peduord, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
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

#################################################
### Father education : Liberalism - Socialism ###
#################################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ father_edu, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
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
          FUN=c(mean, sd), na.rm=TRUE)
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
          FUN=c(mean, sd), na.rm=TRUE)
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

###########################################
### Father education : KNOWLEDGE scores ###
###########################################
summaryBy(ngknow + knowraw ~ father_edu, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
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
          FUN=c(mean, sd), na.rm=TRUE)
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

################################################
### Liberalism-Socialism vs. WORK EXPERIENCE ###
################################################
summaryBy(libsoc + liberalism + socialism + rawlib + rawsoc ~ work_experience, data=data, 
          FUN=c(mean, sd), na.rm=TRUE)
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
          FUN=c(mean, sd), na.rm=TRUE)
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
contrasts(data$work_experience) <- contr.treatment(3)
### We se that the first contrast is significant, while the second is not
### Therefore it may be concluded that respondens with work experience have higher economic/financial knowledge
bwplot(ngknow ~ work_experience, data=data, ylab="Non-guessed KNOWLEDGE Scores",
       xlab="Work Experience", horizontal=FALSE,
       par.settings=V4bgw)

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