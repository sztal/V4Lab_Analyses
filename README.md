# V4Lab_Analyses
This repository stores all data and analytic scripts of the V4 Lab project.

The aim of the project is to measure of economical and financial knowledge of Czech and Polish students as well as assess their opinions in terms of economic liberalism-interventionism. Furthermore, we also try to shed some light on the associations between the knowledge and the opinions as well as other potentialy important sociodemographical variables (such as parents' education and type of education).

More information can be found on the project's website: [http://v4lab.eu/](http://v4lab.eu/).

## Directories description
This section presents brief description of all directories that are used throughout the analysis in order to keep the analysis structured and legible.

* **Data** : Here the datasets are stored. 
	* *RawData* : Here the raw data is stored. The raw comes in two flavours - true raw and quasi-raw. True raw datasets (CZ_rawdata.csv and PL_rawdata.csv) contain the raw data, that is exactly what was transferred from the paper questionnaires to the digital .csv sheets. In general this means that the raw files contain the raw input from the respondents with only one exeption - the data is anonimized. Quasi-raw data (CZ_main and PL_main) are the datasets that are translated to English (so they do not contain any non-ASCII characterts) and are properly recoded. The quasi-raw data is stored in two formats: .txt and .RData. Those who work in R are recommended to use .RData files since they retain the proper structure of all variables (factor variables remain factors, ordered factors remain ordered factors etc.).
	* *MainData* : Here the processed datasets are stored. All datasets here are represented as .txt (with \t as a field separator) and .RData files. Again, R users are recommended to use .RData files. More info about the files here is to be found in the Map of Files (below).
	* *Maps* : this directory stores helper files that are used to map respondents' answers to correct answers in order to properly recode KNOWLEDGE questions and also files that map items (from KNOWLEDGE and OPINION sets) to their semantic content. 

* **R_scripts** : This directory stores all R scripts that were used for the purpose of data processing and analysis.
	* *data_processing* : here all scripts used to process the data are stored. Data processing includes: producing recoded data from the raw data, removing outlying respondents and missing data imputation. Moreover, scripts with custom data processing functions are stored here as well.
	* *PL_Analysis* : Here all scripts used in the analysis of the Polish sample are stored. 

## Map of Files
In this sections brief descriptions of all files (both datasets and scripts) are presented altogether with a map of relations between datasets and scripts. In other words the map shows which datasets are being processed to yield which datasets, and also which scripts are used in order to get this result.

Step-by-step procedure to transform raw data into the final recoded dataset with all complex indicators (PL sample):

	1. PL_rawdata.csv ---> PL_main.txt/.RData --> PL_main_correct.txt/.RData (script : PL_data_processing.R) 
	2. PL_main_correct ---> PL_selected.txt/.RData (script : PL_additional_processing.R)
	3. PL_selected.txt ---> dat_PL1.txt/.RData (script : PL_mdAnalysis_and_imputation.R)

Step-by-step procedure to transfrom raw data into the final recoded dataset with all complex indicator (CZ sample):

	1. CZ_rawdata.csv ---> CZ_main.txt/.RData --> CZ_main_correct.txt/.RData (script : CZ_data_processing.R) 
	2. CZ_main_correct ---> CZ_selected.txt/.RData (script : CZ_additional_processing.R)
	3. CZ_selected.txt ---> dat_CZ1.txt/.RData (script : CZ_mdAnalysis_and_imputation.R)

### Files description
* **Data/RawData**
	* *CZ_rawdata.csv* : Czech sample anonimized raw data. Many non-ASCII characters. Processing scripts are coded to deal with that and keep and enforce UTF-8 coding but be cautious and remeber about it anyway, while processing these datasets. More info about variables and coding in the the Codebook_raw.pdf.
	* *PL_rawdata.csv* : Polish sample anonimized raw data. Similar to the Czech file.
	* *CZ_main.txt/.RData* : Recoded datasets. Everything is translated to English and all non-ASCII characters are eradicated. Moreover aswer codes are simplified and some new derived variables created. However it is mostly just the raw dataset translated to English and with proper structure imposed on the variables in R (factors etc.).
	* *CZ_main.txt/.RData* : The same but for the Polish sample.

* **Data/MainData**
	* *CZ_main_correct.txt/.RData* : The same as CZ_main.txt/.RData, but now variables from the set of the KNOWLEDGE items do not reflect actual respondents' answers but their correctness (this is where the map of the correct answers was used).
	* *PL_main_correct.txt/.RData* : The same but for the Polish sample.
	* *CZ_selected.txt/.RData* : Classification of study programmes is introduced and outlying respondents are removed. More info in the associated processing script (see the Map of Files).
	* *PL_selected.txt/.RData* : The same but for the Polish sample.
	* *dat_CZ1.txt/.RData* : This dataset has some (but not all) missing data imputed. This is the first file of the type 'dat'. 'dat' files are preprocessed enough to be good to work with.
	* *dat_PL1.txt/.RData* : The same but for the Polish sample.

* **R_scripts/data_processing**
	* *CZ_data_processing.R* : This script produces CZ_main.txt/.RData from CZ_rawdata.csv (see the Map of Files). It is densely commented to provide stepby-step explanation of all decisions that had to be taken during this stage of data processing.
	* *PL_data_processing* : The same but for the Polish sample.
	* *CZ_additional_processing* : This script produces CZ_selected.txt/.RData from CZ_main.txt/.RDatai (see the Map of Files). It introduces classification of respondents' study programmes (detailed description in the script comments) and removes outlying observations.
	* *PL_additional_processing* : The same but for the Polish sample.
	* *CZ_mdAnalysis_and_imputation.R* : This script produces dat_CZ1.txt/.RData from CZ_selected.txt/.RData (see the Map of Files). It imputes missing data and detects respondents with too many of them to be relaibly imputed. More detailed info to be found in the script comments.
	* *PL_mdAnalysis_and_imputation.R* : The same but for the Polish sample.
	* *processingTools.R* : This is a helper file that stores various custom functions that was written to facilitate data preparation and processing. It is frequently used by the data processing scripts.
