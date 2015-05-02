#######################################################################
### KNOWLEDGE NON-GUESSED SCORES + ORDINAL JOINT PARENTAL EDUCATION ###
#######################################################################

### This script adds last variables: KNOWLEDGE raw scores and npn-guessed scores and ordinal scale of joint parental education; then it generates the final dataset

### Load data
load(normalizePath("./Data//MainData/fulldat.RData"))

### Backup
dat.backup <- fulldat

### Load procesing tools
source(normalizePath("./R_scripts//data_processing/processingTools.R"))
library(doBy)

### Make KNOWLEDGE scores 
### Raw scores
knowraw <- vector(mode="numeric", length=nrow(fulldat))
knowraw[] <- NA
names(knowraw) <- rownames(fulldat)
nam <- rownames(fulldat[fulldat$knowIV == 0, ])
knowraw[nam] <- apply(fulldat[fulldat$knowIV == 0,
                              grep("^k[0-9]+$", names(fulldat), perl=TRUE)], 1, sum)
fulldat$knowraw <- knowraw

### Make KNOWLEDGE non-guessed scores
ngknow <- vector(mode="numeric", length=nrow(fulldat))
ngknow[] <- NA
names(ngknow) <- rownames(fulldat)
ngknow[nam] <- apply(fulldat[fulldat$knowIV == 0,
                             grep("^k[0-9]+$", names(fulldat), perl=TRUE)],
                     1, nonguessedKnowScore)
fulldat$ngknow <- ngknow

### Ordinal scale of joint parental education
### First we recode education to get rid of very small groups
fulldat$father_edu <- recodeVar(fulldat$father_edu,
                                src=c("<primary", "primary", "vocational"),
                                tgt=rep("<=vocational", 3))
fulldat$father_edu<- factor(fulldat$father_edu, 
                                     levels(fulldat$father_edu)[c(4,2,1,3)])
fulldat$father_edu <- recodeVar(fulldat$father_edu,
                                src=c("PHD+", "higher_edu"),
                                tgt=rep("higher_edu+", 2))
fulldat$father_edu<- factor(fulldat$father_edu, 
                            levels(fulldat$father_edu)[c(3,2,1)], ordered=TRUE)

fulldat$mother_edu <- recodeVar(fulldat$mother_edu,
                                src=c("<primary", "primary", "vocational"),
                                tgt=rep("<=vocational", 3))
fulldat$mother_edu<- factor(fulldat$mother_edu, 
                            levels(fulldat$mother_edu)[c(4,2,1,3)])
fulldat$mother_edu <- recodeVar(fulldat$mother_edu,
                                src=c("PHD+", "higher_edu"),
                                tgt=rep("higher_edu+", 2))
fulldat$mother_edu<- factor(fulldat$mother_edu, 
                            levels(fulldat$mother_edu)[c(3,2,1)], ordered=TRUE)

### Make ordinal joint parental education indicator
fint <- as.numeric(fulldat$father_edu) - 1
mint <- as.numeric(fulldat$mother_edu) - 1
peduord <- fint + mint
fulldat$peduord <- peduord

#################
### SAVE DATA ###
#################
write.table(fulldat, sep="\t", row.names=TRUE,
            file=normalizePath("./Data//MainData/finalData.txt"))
save(fulldat, file=normalizePath("./Data//MainData/finalData.RData"))

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
#       [1] psych_1.5.4          ca_0.58              doBy_4.5-13          survival_2.38-1     
# [5] knitr_1.10           mokken_2.7.7         poLCA_1.4.1          MASS_7.3-39         
# [9] scatterplot3d_0.3-35 reshape2_1.4.1       latticeExtra_0.6-26  lattice_0.20-31     
# [13] RColorBrewer_1.1-2  
# 
# loaded via a namespace (and not attached):
#       [1] Rcpp_0.11.5     splines_3.2.0   mnormt_1.5-2    stringr_0.6.2   plyr_1.8.1     
# [6] tools_3.2.0     parallel_3.2.0  grid_3.2.0      htmltools_0.2.6 yaml_2.1.13    
# [11] digest_0.6.8    Matrix_1.2-0    rmarkdown_0.5.1