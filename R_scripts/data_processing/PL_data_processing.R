### It is recommended to open and use this script with UTF-8 encoding

###################################
### PL_main dataset preparation ###
###################################

### This script transforms Polish rawdata to a more tidy and usable format
### that is also free of non_ASCII characters.
### Load data
data <- read.csv(normalizePath("./Data/RawData/PL_rawdata.csv"), sep=";")
data.backup <- data # backup object

### Convert all KNOWLEDGE questions to binary variables
for(col in grep("^w[0-9]+", enc2utf8(names(data)), perl = TRUE)) {
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col])
      data[, col][data[, col] == 2] <- 0 ### recode 'Not true' to 0 and 'True' remains 1
}
### Convert all OPINION question to properly labelled factors
for(col in grep("^p[0-9]+", enc2utf8(names(data)), perl = TRUE)) {
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
      data[, col] <- as.character(data[, col]) 
      data[, col][data[, col] == "1"] <- "Agree"
      data[, col][data[, col] == "2"] <- "Disagree"
      data[, col][data[, col] == "3"] <- "DontKnow"
      data[, col] <- factor(data[, col],
                            levels = c("Agree", "Disagree", "DontKnow"))
}
### Set string variables to character class
for(col in grep("kierunek|id|rok", enc2utf8(names(data)), perl = TRUE)) {
      data[, col] <- as.character(data[, col])
}
### Convert 3 questions concerning economical/political beliefs to numerical values
for(col in grep("państwo|macierz", enc2utf8(names(data)), perl = TRUE)) {
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col])
}

### Convert parents' education to properly labelled factor
for(col in grep("wykszt", enc2utf8(names(data)), perl = TRUE)) {
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
      data[, col] <- as.character(data[, col])
      data[, col][data[, col] == "1"] <- "<primary"
      data[, col][data[, col] == "2"] <- "primary"
      data[, col][data[, col] == "3"] <- "vocational"
      data[, col][data[, col] == "4"] <- "high_school"
      data[, col][data[, col] == "5"] <- "higher_edu"
      data[, col][data[, col] == "6"] <- "PHD+"
      data[, col] <- factor(data[, col], ordered = TRUE,
                            levels = c("<primary", "primary",
                                       "vocational", "high_school",
                                       "higher_edu", "PHD+"))
}

### Convert year at university to a properly labelled factor
col <- grep("rok_stu", enc2utf8(names(data)), perl = TRUE)
data[, col] <- as.character(data[, col])
data[, col][data[, col] == "1"] <- "BA_1"
data[, col][data[, col] == "2"] <- "BA_2"
data[, col][data[, col] == "3"] <- "BA_3"
data[, col][data[, col] == "3/2"] <- "BA_3"
data[, col][data[, col] == "4"] <- "MA_1"
data[, col][data[, col] == "5"] <- "MA_2"
data[, col][data[, col] == ""] <- NA
data[, col] <- factor(data[, col], ordered = TRUE,
                      labels = c("BA_1", "BA_2", "BA_3", "MA_1", "MA_2", "PHD"))

### Unify the coding of study programs and convert to properly labelled factor
sp <- enc2utf8(as.character(data$kierunek_studiów)) ### save it as a separate variable for convenience
sp[sp == ""] <- NA
sp[grep("energ?|enrg", sp, ignore.case = TRUE, perl = TRUE)] <- "Energetics"
sp[grep("euro", sp, ignore.case = TRUE, perl = TRUE)] <- "Euro_studies"
sp[grep("^f(r| |$|ir)", sp, ignore.case = TRUE, perl = TRUE)] <- "Finance"
sp[grep("inform|matema", sp, ignore.case = TRUE, perl = TRUE)] <- "Math/CS"
sp[grep("in[zż].*bio", sp, ignore.case = TRUE, perl = TRUE)] <- "Bioengineering"
sp[grep("lingw.*sto", sp, ignore.case = TRUE, perl = TRUE)] <- "Linguistics"
sp[grep("malarstw|asp", sp, ignore.case = TRUE, perl = TRUE)] <- "Art"
sp[grep("^msg", sp, ignore.case = TRUE, perl = TRUE)] <- "International_bussiness"
sp[grep("psycho", sp, ignore.case = TRUE, perl = TRUE)] <- "Psychology"
sp[grep("miesi", sp, ignore.case = TRUE, perl = TRUE)] <- "Econometrics"
sp[grep("sgh", sp, ignore.case = TRUE, perl = TRUE)] <- "Bussiness_no_spec"
sp[grep("turyst", sp, ignore.case = TRUE, perl = TRUE)] <- "Leisure_studies"
sp[grep("turkol", sp, ignore.case = TRUE, perl = TRUE)] <- "Turkology"
sp[grep("zarz[aą]?(dz)?", sp, ignore.case = TRUE, perl = TRUE)] <- "Management"
col <- grep("kierunek", names(data), perl = TRUE)
data[, col] <- factor(sp, levels = enc2utf8(names(table(sp))))

### Convert work experience to properly labelled factor
col <- grep("^praca$", enc2utf8(names(data)), perl = TRUE)
data[, col] <- enc2utf8(as.character(data[, col]))
data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
data[, col] <- enc2utf8(as.character(data[, col]))
data[, col][data[, col] == "1"] <- "No_experience"
data[, col][data[, col] == "2"] <- "Up_to_1_year"
data[, col][data[, col] == "3"] <- "1_year+"
data[, col] <- factor(data[, col], ordered = TRUE,
                      levels = c("No_experience", "Up_to_1_year", "1_year+"))

### Convert hometown size to properly labelled factor
col <- grep("miejsc_po", enc2utf8(names(data)), perl = TRUE)
data[, col] <- enc2utf8(as.character(data[, col]))
data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
data[, col] <- enc2utf8(as.character(data[, col]))
data[, col][data[, col] == "1"] <- "<10k"
data[, col][data[, col] == "2"] <- "<10k-100k"
data[, col][data[, col] == "3"] <- "100k-500k"
data[, col][data[, col] == "4"] <- ">500k"
data[, col] = factor(data[, col], ordered = TRUE,
                     levels = c("<10k", "<10k-100k", "100k-500k", ">500k"))

### Unify coding of the birth date
col <- grep("rok_uro", enc2utf8(names(data)), perl = TRUE)
bdat <- enc2utf8(as.character(data[, col])) ### save it as a separate variable for convenience
bdat <- as.numeric(bdat) ### to get rid of non-numerical variables
bdat <- enc2utf8(as.character(bdat))
for(i in 1:length(bdat)) {
      if(is.na(bdat[i])) next
      else if(nchar(bdat[i]) < 2) bdat[i] <- NA
      else if(nchar(bdat[i]) == 2) bdat[i] <- paste("19", bdat[i], sep = "")
      else if(nchar(bdat[i]) == 3) bdat[i] <- NA
      else if(nchar(bdat[i]) == 4) next
      else bdat[i] <- NA
}
data[, col] <- as.numeric(bdat)

### Convert gender to properly labelled factor
col <- grep("płeć", enc2utf8(names(data)), perl = TRUE)
data[, col] <- enc2utf8(as.character(data[, col]))
data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
data[, col] <- enc2utf8(as.character(data[, col]))
data[, col] <- ifelse(data[, col] == "1", "female", "male")
data[, col] <- factor(data[, col], levels = c("female", "male"))

### Assign english names to variables
### Change p1-p23 into o1-o23 (since they are OPINION items)
substr(names(data)[grep("^p[0-9]+", enc2utf8(names(data)), perl = TRUE)], 1, 1) <- "o"
### Change w1-w30 into k1-k30 (since they are KNOWLEDGE items)
substr(names(data)[grep("^w[0-9]+", enc2utf8(names(data)), perl = TRUE)], 1, 1) <- "k"
### Give english name to leseferism-etatism axis
names(data)[grep("państwo", enc2utf8(names(data)), perl = TRUE)] <- "leseferism-etatism"
### Give english names to the economic and social dimensions of the opinion matrix
names(data)[grep("macie.*gospo", enc2utf8(names(data)), perl = TRUE)] <- "mat_econimic"
names(data)[grep("macie.*spo", enc2utf8(names(data)), perl = TRUE)] <- "mat_social"
### Give english names to parental education
names(data)[grep("wykszt_oj", enc2utf8(names(data)), perl = TRUE)] <- "father_edu"
names(data)[grep("wykszt_ma", enc2utf8(names(data)), perl = TRUE)] <- "mother_edu"
### Give english name to year at the university
names(data)[grep("rok_stu", enc2utf8(names(data)), perl = TRUE)] <- "year_at_uni"
### Give english name to studies programme
names(data)[grep("kierunek", enc2utf8(names(data)), perl = TRUE)] <- "uni_programme"
### Give english name to working experience
names(data)[grep("praca", enc2utf8(names(data)), perl = TRUE)] <- "work_experience"
### Give english name to size of the hometown
names(data)[grep("miejsc_poc", enc2utf8(names(data)), perl = TRUE)] <- "hometown_size"
### Give english name to birthdate
names(data)[grep("rok_uro", enc2utf8(names(data)), perl = TRUE)] <- "birthdate"
### Give english name to gender
names(data)[grep("płeć", enc2utf8(names(data)), perl = TRUE)] <- "gender"

### Compute age
data$age <- 2015 - data$birthdate

### Remove question k9 (since it has no truly correct answer)
data <- data[, -grep("k9", enc2utf8(names(data)), perl = TRUE)]

############################
### Save PL_main dataset ###
############################

### Save recoded data to a file
### Rename the dataset object properly
data_pl <- data
### field separator is set to "\t"
write.table(data_pl, file = normalizePath("./Data/RawData/PL_main.txt"), 
            sep = "\t", row.names = FALSE)
### Save recoded data to an R data object
save(data_pl, file = normalizePath("./Data/RawData/PL_main.RData"))

########################################
### END OF BASIC DATA PRE-PROCESSING ###
########################################

### Recode KNOWLEDGE items to reflect correct and incorrect answers
### load a script that maps answers to the correct answers
source(normalizePath("./R_scripts/data_processing/processingTools.R"))

### Load the map of the correct answers
map <- read.csv(normalizePath("./Data/Maps/CorrectAnswersMap_KNOWLEDGE.csv"), row.names = 1)

### Map respondents' answers to the correct answers
data_correct <- data
knowledge_vars <- grep("^k[0-9]+", names(data_correct), perl = TRUE)
data_correct[,knowledge_vars] <- mapKnowledgeToCorrect(data_correct[,knowledge_vars], map, "PL")

####################################
### Save PL_main_correct dataset ###
####################################

### Rename the dataset object properly
data_pl_correct <- data_correct
### Save recoded dataset to a .txt file
### field seprator is set "\t"
write.table(data_pl_correct, 
            file = normalizePath("./Data/MainData/PL_main_correct.txt"),
            sep = "\t", row.names = FALSE)
### Save recoded dataset to an R data object
save(data_pl_correct, file = normalizePath("./Data/MainData/PL_main_correct.RData"))

### Clean the workspace 
### (optional: uncomment to remove all objects from RStudio working memory)
# rm(list = ls())

### !!! <--- END OF SCRIPT ---> !!! ###