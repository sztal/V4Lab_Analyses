### This script transforms polish rawdata to a more tidy and usable format
### Load data
data <- read.csv("Data/PL_rawdata.csv", sep=";")
data.backup <- data # backup object

### Transform numeric data into numeric variables
### (all non-numeric entries are equivalent to missing data)
### For more information about the datasets and variables see README file
for(var in names(data)[grep("(p|w)[0-9]+", names(data))]) transform(data, var = as.numeric(var))