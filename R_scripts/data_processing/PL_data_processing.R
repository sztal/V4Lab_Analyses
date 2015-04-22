### This script transforms polish rawdata to a more tidy and usable format
### Load data
data <- read.csv("Data/PL_rawdata.csv", sep=";")
data.backup <- data # backup object

### Convert all KNOWLEDGE questions to binary variables
for(col in grep("^w[0-9]+", names(data))) {
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col])
      data[, col][data[, col] == 2] <- 0 ### recode 'Not true' to 0 and 'True' remains 1
}
### Convert all OPINION question to properly labelled factors
for(col in grep("^p[0-9]+", names(data))) {
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
      data[, col] <- as.character(data[, col]) 
      data[, col][data[, col] == "1"] <- "Agree"
      data[, col][data[, col] == "2"] <- "Disagree"
      data[, col][data[, col] == "3"] <- "DontKnow"
      data[, col] <- factor(data[, col],
                            levels = c("Agree", "Disagree", "DontKnow"))
}
for(col in grep("kierunek|id|rok", names(data))) {
      data[, col] <- as.character(data[, col])
}
### Convert 3 questions concerning economical/political beliefs to numerical values
for(col in grep("państwo|macierz", names(data))) {
      data[, col] <- as.character(data[, col])
      data[, col] <- as.numeric(data[, col])
}

### Convert parents' education to properly labelled factor
for(col in grep("wykszt", names(data))) {
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
col <- grep("rok_stu", names(data))
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
sp <- as.character(data$kierunek_studiów) ### save it as a separate variable for convenience
sp[sp == ""] <- NA
sp[grep("energ?|enrg", sp, ignore.case = TRUE)] <- "Energetics"
sp[grep("euro", sp, ignore.case = TRUE)] <- "Euro_studies"
sp[grep("^f(r| |$|ir)", sp, ignore.case = TRUE)] <- "Finance"
sp[grep("inform|matema", sp, ignore.case = TRUE)] <- "Math/CS"
sp[grep("in[zż].*bio", sp, ignore.case = TRUE)] <- "Bioengineering"
sp[grep("lingw.*sto", sp, ignore.case = TRUE)] <- "Linguistics"
sp[grep("malarstw|asp", sp, ignore.case = TRUE)] <- "Art"
sp[grep("^msg", sp, ignore.case = TRUE)] <- "International_bussiness"
sp[grep("psycho", sp, ignore.case = TRUE)] <- "Psychology"
sp[grep("miesi", sp, ignore.case = TRUE)] <- "Econometrics"
sp[grep("sgh", sp, ignore.case = TRUE)] <- "Bussiness_no_spec"
sp[grep("turyst", sp, ignore.case = TRUE)] <- "Leisure_studies"
sp[grep("turkol", sp, ignore.case = TRUE)] <- "Turkology"
sp[grep("zarz[aą]?(dz)?", sp, ignore.case = TRUE)] <- "Management"
col <- grep("kierunek", names(data))
data[, col] <- factor(sp, levels = names(table(sp)))

### Convert work experience to properly labelled factor
col <- grep("^praca$", names(data))
data[, col] <- as.character(data[, col])
data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
data[, col] <- as.character(data[, col])
data[, col][data[, col] == "1"] <- "No_experience"
data[, col][data[, col] == "2"] <- "Up_to_1_year"
data[, col][data[, col] == "3"] <- "1_year+"
data[, col] <- factor(data[, col], ordered = TRUE,
                      levels = c("No_experience", "Up_to_1_year", "1_year+"))

### Convert hometown size to properly labelled factor
col <- grep("miejsc_po", names(data))
data[, col] <- as.character(data[, col])
data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
data[, col] <- as.character(data[, col])
data[, col][data[, col] == "1"] <- "<10k"
data[, col][data[, col] == "2"] <- "<10k-100k"
data[, col][data[, col] == "3"] <- "100k-500k"
data[, col][data[, col] == "4"] <- ">500k"
data[, col] = factor(data[, col], ordered = TRUE,
                     levels = c("<10k", "<10k-100k", "100k-500k", ">500k"))

### Unify coding of the birth date
col <- grep("rok_uro", names(data))
bdat <- as.character(data[, col]) ### save it as a separate variable for convenience
bdat <- as.numeric(bdat) ### to get rid of non-numerical variables
bdat <- as.character(bdat)
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
col <- grep("płeć", names(data))
data[, col] <- as.character(data[, col])
data[, col] <- as.numeric(data[, col]) ### to get rid of non-numerical values
data[, col] <- as.character(data[, col])
data[, col] <- ifelse(data[, col] == "1", "female", "male")
data[, col] <- factor(data[, col], levels = c("female", "male"))

### Assign english names to variables
### Change p1-p23 into o1-o23 (since they are OPINION items)
substr(names(data)[grep("^p[0-9]+", names(data))], 1, 1) <- "o"
### Change w1-w30 into k1-k30 (since they are KNOWLEDGE items)
substr(names(data)[grep("^w[0-9]+", names(data))], 1, 1) <- "k"
### Give english name to leseferism-etatism axis
names(data)[grep("państwo", names(data))] <- "leseferism-etatism"
### Give english names to the economic and social dimensions of the opinion matrix
names(data)[grep("macie.*gospo", names(data))] <- "mat_econimic"
names(data)[grep("macie.*spo", names(data))] <- "mat_social"
### Give english names to parental education
names(data)[grep("wykszt_oj", names(data))] <- "father_edu"
names(data)[grep("wykszt_ma", names(data))] <- "mother_edu"
### Give english name to year at the university
names(data)[grep("rok_stu", names(data))] <- "year_at_uni"
### Give english name to studies programme
names(data)[grep("kierunek", names(data))] <- "uni_programme"
### Give english name to working experience
names(data)[grep("praca", names(data))] <- "work_experience"
### Give english name to size of the hometown
names(data)[grep("miejsc_poc", names(data))] <- "hometown_size"
### Give english name to birthdate
names(data)[grep("rok_uro", names(data))] <- "birthdate"
### Give english name to gender
names(data)[grep("płeć", names(data))] <- "gender"

### Compute age
data$age <- 2015 - data$birthdate

########################################
### END OF BASIC DATA PRE-PROCESSING ###
########################################

### Recode KNOWLEDGE items to reflect correct and incorrect answers
