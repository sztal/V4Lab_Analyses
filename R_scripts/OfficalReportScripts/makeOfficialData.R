#############################
### Make Official Dataset ###
#############################

### This short script transform finalDataExtended dataset into dataOfficial dataset.
### dataOfficial file is the official dataset tailored for the third party users.

### In order to obtain it, we get rid of some superfluous variables
### and assign more appropriate names to some others

### Load data
load(normalizePath("./Data//MainData/finalDataExtended.RData"))
data <- finalDataExtended

### Generate dataset
fdat <- data[, c("ngknow", "ngfinance")]
fdat$L.I.scale <- data$libsoc
fdat$typeofedu <- data$eduprog3
fdat$country <- data$country
fdat$phighedu <- data$phighedu
fdat$father_edu <- data$father_edu
fdat$mother_edu <- data$mother_edu
fdat$gender <- data$gender
fdat$age <- data$age
fdat$study_year <- data$year_at_uni
fdat$study_year_numeric <- data$uniyear
fdat$work_experience <- data$work_experience
fdat$PL_home_size <- data$PL_hsize
fdat$CZ_home_size <- data$CZ_hsize
fdat$typeofedu.country <- data$edu3country
fdat <- cbind(fdat, data[, grep("^(o|k)[0-9]+", names(data), perl=TRUE)])
### Add more informative rownames
rownames(fdat)[fdat$country == "PL"] <- paste("pl", 1:length(fdat$country[fdat$country=="PL"]), sep="")
rownames(fdat)[fdat$country == "CZ"] <- paste("cz", 1:length(fdat$country[fdat$country=="CZ"]), sep="")

### Save dataset ###
officialData <- fdat

# write .txt file
write.table(officialData, sep="\t", row.names=TRUE,
            file=normalizePath("./Data/OfficialData/officialData.txt"))
# write .RData file
save(officialData, file=normalizePath("./Data/OfficialData/officialData.RData"))

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
# loaded via a namespace (and not attached):
#       [1] tools_3.2.0