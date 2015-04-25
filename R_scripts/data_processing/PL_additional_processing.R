#################################################################
### Additional processing of the Polish dataset; it contains: ###
###   - classification of respondents' study programmes       ###
###   - removal of respondents according to age/edu criteria  ###
#################################################################

### Load data
load(normalizePath("./Data/MainData/PL_main_correct.RData"))
### Save it under a more easy-to-type name
data <- data_pl_correct

### Classification of the respondents' study programms
### They are grouped into four categories:
###   - social sciences and humanities (SSH)
###   - art programmes (Art)
###   - economics, bussiness and management studies, and finance and actuarial science (EBMF)
###   - STEM programmes (science, engineering, technology and math)
###
### Classification is conducted as follows:
###
### SSH     <---- Euro_studies (20 respondents)
###         <---- Linguistics (5 respondents)
###         <---- Leisure studies (1 respondent)
###         <---- Turkology (1 respondent)
###         <---- Psychology (37 respondents)
###
### Art     <---- Art (22 respondents)
###
### EBMF    <---- Bussiness_no_spec (5 respondents)
###         <---- Econometrics (5 respondents)
###         <---- Finance (78 respondents)
###         <---- International bussiness (1 respondents)
###         <---- Management (12 respondents)
###
### STEM    <---- Bioengineering (32 respondents)
###         <---- Energetics (69 respondents)
###         <---- Math/CS (23 respondents)
###
### Classification code
eduprog4 <- as.character(data$uni_programme)
eduprog4[grep("Euro.*stu|Lingui|Leisur|Turko|Psycho", eduprog4, perl = TRUE)] <- "SSH"
eduprog4[grep("Bussiness|Econome|Finance|Inter.*buss|Manag", eduprog4, perl = TRUE)] <- "EBMF"
eduprog4[grep("Bioeng|Energe|Math", eduprog4, perl = TRUE)] <- "STEM"
### Save the result as a new variable in the main dataset
data$eduprog4 <- factor(eduprog4)

### Additionally another classification has been prepared, in which Art and SSH are added together to make one group (SSHA)
eduprog3 <- as.character(data$eduprog4)
eduprog3[grep("^SSH$|^Art$", eduprog3, perl = TRUE)] <- "SSHA"
### Save the result as a new variable in the main dataset
data$eduprog3 <- factor(eduprog3)

################################
### Selection of respondents ###
################################

### Since the research is focused on the typical population of university students two selection criteria has been adopted:
###   - respondents have to be 18 to 30 years old
###   - respondents have to be enrolled in a university BA or MA programme (or equivalent)

### Check the first criterion
which(data$age > 30)
### three respondents have to be excluded
data <- data[-which(data$age > 30), ]

### Check the second criterion
which(data$year_at_uni == "PHD")
### one respondent have to be excluded
data <- data[-which(data$year_at_uni == "PHD"), ]

#####################
### Save new data ###
#####################

### Rename the new dataset
data_pl_select <- data
### Save as a .txt file
### field separator is set to "\t"
write.table(data_pl_select, sep = "\t", row.names = FALSE,
            file = normalizePath("./Data/MainData/PL_selected.txt"))
### Save as an R data object
save(data_pl_select, file = normalizePath("./Data/MainData/PL_selected.RData"))

### Clean the workspace 
### (optional: uncomment to remove all objects from RStudio working memory)
# rm(list = ls())

### !!! <--- END OF SCRIPT ---> !!! ###