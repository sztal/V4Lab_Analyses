### Load data
load(normalizePath("./Data/MainData//finalDataExtended.RData"))
data <- finalDataExtended
data.pl <- data[data$country=="PL", ]
data.cz <- data[data$country=="CZ", ]

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


######################
### Czech students ###
######################
### Is country effect significant, after controlling for study types?
Anova(lm(libsoc ~ eduprog3 * country, data=data), type=3) ### it seems it is
### T-test by edutypes
t.all <-  t.test(libsoc ~ country, data=data)
t.ebmf <- t.test(libsoc ~ country, data=data, subset=eduprog3=="EBMF")
t.ssha <- t.test(libsoc ~ country, data=data, subset=eduprog3=="SSHA")
t.stem <- t.test(libsoc ~ country, data=data, subset=eduprog3=="STEM")

### Weighted difference between PL and CZ
### It is weighted by the joint numer of respondents in each group (EBMF, SSHA and STEM)

### Group differences
dFrame <- summaryBy(libsoc ~ eduprog3 + country, data=data, FUN=mean, na.rm=TRUE)[1:6,]
diff.ebmf <- dFrame[1, 3] - dFrame[2, 3]
diff.ssha <- dFrame[3, 3] - dFrame[4, 3]
diff.stem <- dFrame[5, 3] - dFrame[6, 3]
### Difference vector
diffs <- c(diff.ebmf, diff.ssha, diff.stem)

### Difference variances
dv.ebmf <- ((t.ebmf$conf.int[2] - diff.ebmf) / qt(.975, t.ebmf$parameter))^2
dv.ssha <- ((t.ssha$conf.int[2] - diff.ssha) / qt(.975, t.ssha$parameter))^2
dv.stem <- ((t.stem$conf.int[2] - diff.stem) / qt(.975, t.stem$parameter))^2
### Difference variance vector
dvars <- c(dv.ebmf, dv.ssha, dv.stem)

### Group weights
w.all <- sum(!is.na(data$libsoc[!is.na(data$eduprog3)]))
w.ebmf <- sum(data$eduprog3=="EBMF" & !is.na(data$libsoc), na.rm=TRUE)
w.ssha <- sum(data$eduprog3=="SSHA" & !is.na(data$libsoc), na.rm=TRUE)
w.stem <- sum(data$eduprog3=="STEM" & !is.na(data$libsoc), na.rm=TRUE)

### Weights vector
weights <- c(w.ebmf, w.ssha, w.stem)

### Weighted difference
wdiff <- sum(diffs*(weights / w.all))

### Pooled variance of the weighted difference
pdvar <- sum( ((weights-1)*dvars) / (w.all - 1) )
pdsd <- sqrt(pdvar)
### 95% normal CI for the weighted difference
wdCI95 <- wdiff + c(-1,1)*pdsd*qt(.975, df=t.all$parameter)

######################################
### Associations between variables ###
######################################

### correlations between the L.-I. scale and non-guessed knowledge scores (general and financial)
## PL
corr.test(data.pl[, c("libsoc", "ngknow", "ngfinance")])
## CZ
corr.test(data.cz[, c("libsoc", "ngknow", "ngfinance")])
### Significane of difference of the correlation coefficients in countries
### Test for the L.I. - ngknow correlation
r.test(r12=cor(data.pl[, c("libsoc", "ngknow")], use="pair")[1,2],
       r34=cor(data.cz[, c("libsoc", "ngknow")], use="pair")[1,2],
       n=sum(!is.na(data.pl$libsoc)[!is.na(data.pl$ngknow)]),
       n2=sum(!is.na(data.cz$libsoc)[!is.na(data.cz$ngknow)]))
### Test for the L.I. - ngknow correlation
r.test(r12=cor(data.pl[, c("libsoc", "ngfinance")], use="pair")[1,2],
       r34=cor(data.cz[, c("libsoc", "ngfinance")], use="pair")[1,2],
       n=sum(!is.na(data.pl$libsoc)[!is.na(data.pl$ngfinance)]),
       n2=sum(!is.na(data.cz$libsoc)[!is.na(data.cz$ngfinance)]))

### By education type
### EBMF: PL
corr.test(data.pl[data.pl$eduprog3=="EBMF", c("libsoc", "ngknow", "ngfinance")])
### EBMF: CZ
corr.test(data.cz[data.cz$eduprog3=="EBMF", c("libsoc", "ngknow", "ngfinance")])
### SSHA: PL
corr.test(data.pl[data.pl$eduprog3=="SSHA", c("libsoc", "ngknow", "ngfinance")])
### SSHA: CZ
corr.test(data.cz[data.cz$eduprog3=="SSHA", c("libsoc", "ngknow", "ngfinance")])
### STEM: PL
corr.test(data.pl[data.pl$eduprog3=="STEM", c("libsoc", "ngknow", "ngfinance")])
### STEM: CZ
corr.test(data.cz[data.cz$eduprog3=="STEM", c("libsoc", "ngknow", "ngfinance")])


### Knowledge and study year
### Linear regression per edutype * country
ch1 <- xyplot(ngknow ~ uniyear | country + eduprog3, data=data, par.settings=V4bgw,
       xlab="Study year", ylab="Non-guessed KNOWLEDGE score",
       layout=c(2,3), panel=function(x, y, ...) {
             panel.xyplot(x, y, ..., grid=TRUE)
             panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="red3", cex=0.8,
                           digits=2, offset=1, at=.75, pos=3, adj=c(0,1))
       })

### Check differences between the second and the third years in the EBMF groups in PL and CZ
### means and sds
summaryBy(ngknow ~ country + eduprog3 + year_at_uni, data=data,
          FUN=c(mean, sd, function(x, na.rm) sum(!is.na(x))), na.rm=TRUE)
### Difference between II and III year in CZ
t.test(data.cz[data.cz$year_at_uni=="BA_2", "ngknow"], data.cz[data.cz$year_at_uni=="BA_3", "ngknow"])
### Difference between II and III year in PL
t.test(data.pl[data.pl$year_at_uni=="BA_2", "ngknow"], data.pl[data.pl$year_at_uni=="BA_3", "ngknow"])



### Joint parental higher education vs. knowledge and the L.-I. scale

### KNOWLEDGE ###
### No interaction between paretnal education and country
Anova(lm(ngknow ~ country * phighedu, data = data), type=3) ### no significant effects

### L.-I. scale ###
Anova(lm(libsoc ~ country * phighedu, data = data), type=3)
### significant interaction
### means and sds in groups
summaryBy(libsoc ~ country + phighedu, data=data,
          FUN=c(mean, sd, function(x, na.rm) sum(!is.na(x))), na.rm=TRUE)

### In the Czech sample we compare other groups to HE:none
### In the Polish sample we compare otehr groups to HE:both

### CZ
summary(lm(libsoc ~ relevel(factor(phighedu, ordered=FALSE), ref=1), data=data.cz))
### no significant differences

### PL
summary(lm(libsoc ~ relevel(factor(phighedu, ordered=FALSE), ref=3), data=data.pl))
### both groups are significantly different


### Knowledge and the L.-I. scale vs. work experience

### Knowledge ###
Anova(lm(ngknow ~ work_experience * country, data=data), type=3)
### interaction effect is almost significants

### PL
anova(lm(ngknow ~ work_experience, data=data.pl)) ### there is significant effect
pairwise.t.test(data.pl$ngknow, data.pl$work_experience)
### but no comparisons (with the Holme's correction) are significant what is in line with the lack of significant effects in the full interaction model for both countries

### CZ
anova(lm(ngknow ~ work_experience, data=data.cz)) ### not significant
### Again this agrees with the outcomes for the full interaction model for both countries.


### L.-I. scale ###
Anova(lm(libsoc ~ work_experience * country, data=data), type=3)
### no interaction effect

### So we test using the joint sample
Anova(lm(libsoc ~ work_experience, data=data), typ=3)
### There is an effect
pairwise.t.test(data$libsoc, data$work_experience)

### Means and sds
summaryBy(libsoc ~ work_experience, data=data,
          FUN=c(mean, sd, function(x, na.rm) sum(!is.na(x))), na.rm=TRUE)