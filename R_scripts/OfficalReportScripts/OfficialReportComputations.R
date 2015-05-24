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

### Mean difference (we assume the groups in the population have similar sizes)
mdiff <- mean(diffs)
### Mean difference variance (of course the assumptions stays unchanged)
mdvar <- mean(dvars)
mdsd <- sqrt(mdvar) # mean standard error of the difference

### 95% normal CI for the weighted difference
mdCI95 <- mdiff + c(-1,1)*mdsd*1.96

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

#####################################
### Item difficulties differences ###
#####################################

kdata.cz <- data.cz[, grep("^k[0-9]+", names(data.cz), perl=TRUE)]
kdata.pl <- data.pl[, grep("^k[0-9]+", names(data.pl), perl=TRUE)]

# Difficulties in countries
diffs.cz <- dlevelFrame(kdata.cz)
diffs.pl <- dlevelFrame(kdata.pl)

# Difference significance (PL - CZ)
# data.frame indicating significant differences + difference 95% confidence intervals
diffFrame <- data.frame(sig=rep(0, ncol(kdata.pl)),
                        lo=rep(0, ncol(kdata.pl)),
                        diff=rep(0, ncol(kdata.pl)),
                        up=rep(0, ncol(kdata.pl)),
                        PL=rep(0, ncol(kdata.pl)),
                        CZ=rep(0, ncol(kdata.pl)))
rownames(diffFrame) <- names(kdata.pl)
for(item in names(kdata.pl)) {
      counts <- c(sum(kdata.pl[, item], na.rm=TRUE),
                  sum(kdata.cz[, item], na.rm=TRUE))
      trials <- c(sum(!is.na(kdata.pl[, item])),
                  sum(!is.na(kdata.cz[, item])))
      fails <- trials - counts
      diffs <- fails / trials
      test <- prop.test(fails, trials)
      cat(sprintf("### Item: %s", item))
      print(test)
      if(test$p.value <= 0.05) diffFrame[item, "sig"] <- 1
      diffFrame[item, "lo"] <- test$conf.int[1]
      diffFrame[item, "diff"] <- diffs[1] - diffs[2]
      diffFrame[item, "up"] <- test$conf.int[2]
      diffFrame[item, "PL"] <- diffs[1]
      diffFrame[item, "CZ"] <- diffs[2]
}

# Joint dotplot
# Joint diff frame
diffs.joint <- 1 - cbind(diffs.pl, diffs.cz)[c(1,3,2,4,6,5)]
names(diffs.joint) <- c("PL.diff", "PL.lo", "PL.up", "CZ.diff", "CZ.lo", "CZ.up")
diffs.joint <- cbind(item=rownames(diffs.joint), diffs.joint)
# Plot
dotplot(reorder(item, PL.diff) ~ PL.diff, data=diffs.joint,
        par.settings=list(superpose.symbol=list(pch=list(1, 17),
                                                cex=1.5,
                                                col="black")),
        xlab="Item difficulty",
        lo.PL=diffs.joint$PL.lo, up.PL=diffs.joint$PL.up,
        lo.CZ=diffs.joint$CZ.lo, up.CZ=diffs.joint$CZ.up,
        panel=function(x, y, lo.PL, up.PL, lo.CZ, up.CZ, subscripts, ...) {
              panel.dotplot(x, y, col="black", pch="O", cex=1.2, ...)
              #panel.arrows(x0=lo.PL, y0=y,
              #             x1=up.PL, y1=y, code=3,
              #             angle=90, length=0.05, col="gray3")
              panel.dotplot(diffs.joint$CZ.diff[subscripts], y,
                            col="black", pch=17, cex=1.2, ...)
              #panel.arrows(x0=lo.CZ, y0=y,
              #             x1=up.CZ, y1=y, code=3,
              #             angle=90, length=0.05, col="indianred3")
        }, subscripts=TRUE, auto.key=list(text=c("PL", "CZ"), rows=2, cex=1.2,
                                          space="right"))