#################################################################
### Additional processing of the Czecg dataset; it contains:  ###
###   - classification of respondents' study programmes       ###
###   - removal of respondents according to age/edu criteria  ###
#################################################################

### Load data
load(normalizePath("./Data/MainData/CZ_main_correct.RData"))
### Save it under a more easy-to-type name
data <- data_cz_correct

### Classification of the respondents' study programms
### They are grouped into four categories:
###   - social sciences and humanities (SSH)
###   - art programmes (Art)
###   - economics, bussiness and management studies, and finance and actuarial science (EBMF)
###   - STEM programmes (science, engineering, technology and math)
###
### Classification is conducted as follows:
###
### SSH     <---- Administration (2 respondents)
###         <---- Demography (11 respondents)
###         <---- Diplomacy (3 respondents)
###         <---- Internationl Relations (26 respondents)
###         <---- Journalism (3 respondent)
###         <---- Law (6 respondents)
###         <---- Political Science (5 respondnts)
###         <---- PR (23 respondents)
###         <---- Religion Studies (1 respondents)
###         <---- Sociology (34 respondent)
###
### Art     <---- Art (13 respondents)
###
### EBMF    <---- Bussiness (7 respondents)
###         <---- Econometrics (1 respondent)
###         <---- Economy (59 respondents)
###         <---- Finance/Actuarial Science (27 respondents)
###         <---- Management (36 respondents)
###
### STEM    <---- Math/CS (62 respondents)
###
### Classification code
eduprog4 <- as.character(data$uni_programme)
eduprog4[grep("Math.*CS", eduprog4, perl = TRUE)] <- "STEM"
eduprog4[grep("Bussi|Econom|Fina*.Actu|Manag", eduprog4, perl = TRUE)] <- "EBMF"
eduprog4[grep("STEM|Art|EBMF", eduprog4, perl = TRUE, invert = TRUE)] <- "SSH"
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
### Nothing to remove

#####################
### Save new data ###
#####################

### Rename the new dataset
data_cz_select <- data
### Save as a .txt file
### field separator is set to "\t"
write.table(data_cz_select, sep = "\t", row.names = FALSE,
            file = normalizePath("./Data/MainData/CZ_selected.txt"))
### Save as an R data object
save(data_cz_select, file = normalizePath("./Data/MainData/CZ_selected.RData"))

### Clean the workspace 
### (optional: uncomment to remove all objects from RStudio working memory)
# rm(list = ls())

### !!! <--- END OF SCRIPT ---> !!! ###