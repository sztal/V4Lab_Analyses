# V4 Lab Analyses
This repository stores all data and analytic scripts of the V4 Lab project.

The aim of the project is to measure of economical and financial knowledge of Czech and Polish students as well as assess their opinions in terms of economic liberalism-interventionism. Furthermore, we also try to shed some light on the associations between the knowledge and the opinions as well as other potentialy important sociodemographical variables (such as parents' education and type of education).

More information can be found on the project's website: [http://v4lab.eu/](http://v4lab.eu/).

Detailed description of the dataset and the variables' labelling and coding can be found in the Codebook (Data/OfficialData/Codebook.pdf).

## Some useful terminology
Through out the scripts, reports and other materials and documents in the repository, reader may find some not necessarily unambiguous notions and. Here we try to present and explain the most important and frequent of them:

	* *KNOWLEDGE items* : this one refers to a set of questions about knowledge of economic and financial problems and concepts. More information about them may be found in the Codebook.
	* *OPINION items* : a set of questions concernign opinions on various topics related to the role of state in the economic and social life. Used to construct the L.-I. scale (see the Codebook for more information).
	* *PL sample* : Polish part of the data gathered for the purpose of this project.
	* *CZ sample* : Czech part of the data gather for the purpose of this project.
	* *Joint sample* : the entire sample (so PL part + Cz part).
	* *L.-I. scale* : a unidimensional, bipolar, preferntial scale that is used to measure respondents' attitudes towards the role of the state in economy in terms of the econimic liberalism-interventionism. Again, more information on it can be found in the Codebook.
	* *Non-guessed KNOWLEDGE scores* : the main indicator of the economic-financial and strictly financial knowledge used throughout the project. Its values corresponds to the number of questions to which respondent had to know the correct answer in order to obtain the final number of correctly answered questions. It is based on an assumption that, since the items were dychotomic, then in the case a respondent does not know the correct answer, he or she tries to guess it with a 50% chance of success. More information on this indicator in the Codebook.
	* *Codebook* : a .pdf English document with descriptions of all variables and their possible values. Indispensable for those, who want to work with our data.

## Directories description
This section presents brief description of all directories that are used throughout the analysis in order to keep the analysis structured and legible.

* **Questionnaires** : Here the questionnaire (Polish and Czech version) are stored. Moreover it contains OPINION and KNOWLEDGE items English translations.

* **Data** : Here the datasets are stored. 
	* *RawData* : Here the raw data is stored. The raw comes in two flavours - true raw and quasi-raw. True raw datasets (CZ_rawdata.csv and PL_rawdata.csv) contain the raw data, that is exactly what was transferred from the paper questionnaires to the digital .csv sheets. In general this means that the raw files contain the raw input from the respondents with only one exeption - the data is anonimized. Quasi-raw data (CZ_main and PL_main) are the datasets that are translated to English (so they do not contain any non-ASCII characterts) and are properly recoded. The quasi-raw data is stored in two formats: .txt and .RData. Those who work in R are recommended to use .RData files since they retain the proper structure of all variables (factor variables remain factors, ordered factors remain ordered factors etc.).
	* *MainData* : Here the processed datasets are stored. All datasets here are represented as .txt (with \t as a field separator) and .RData files. Again, R users are recommended to use .RData files. More info about the files here is to be found in the Map of Files (below).
	* *Maps* : this directory stores helper files that are used to map respondents' answers to correct answers in order to properly recode KNOWLEDGE questions and also files that map items (from KNOWLEDGE and OPINION sets) to their semantic content. 
	* *OfficialData* : this directory stores the official dataset with proper naming and coding of variables. It also contains the script that generates it.

* **R_scripts** : This directory stores all R scripts that were used for the purpose of data processing and analysis.
	* *data_processing* : here all scripts used to process the data are stored. Data processing includes: producing recoded data from the raw data, removing outlying respondents and missing data imputation. Moreover, scripts with custom data processing functions are stored here as well. Especially all routines used for MUDFOLF type scaling of the set of items measuring opinions (in regard to economic liberlism-interventionism) are implemented here in the script: processingTools.R 
	* *Visualization* : Here we store scripts with implementations of useful helper functions for data visualization as well as code of our color scheme for lattice type plots.
	* *PL_Analysis* : Here all scripts used in the analysis of the Polish sample are stored.
	* *CZ_Analysis* : Here all scripts used in the analysis of the Czech sample are stored.
	* *Join_Analysis* : Here all scripts used in the analysis of the Join sample (PL + CZ) are stored. These are the most important scripts that presents actually important (i.e. not only technical) analyses. 
	* *OfficalReportScripts* : This folder stores a script that replicates all computations done in the official report of the V4 Lab Project.

* **Reports** : here we store .Rmd (R markdown) files with reports on the main parts of the data production/analytic procedures.

* **PDF_Reports** : here are .pdf reports based on .Rmd files. 

## Map of Files
In this sections brief descriptions of all files (both datasets and scripts) are presented altogether with a map of relations between datasets and scripts. In other words the map shows which datasets are being processed to yield which datasets, and also which scripts are used in order to get this result.

Step-by-step procedure to transform raw data into the final recoded dataset with all complex indicators (PL sample):

	1. PL_rawdata.csv ---> PL_main.txt/.RData ---> PL_main_correct.txt/.RData (script : PL_data_processing.R) 
	2. PL_main_correct ---> PL_selected.txt/.RData (script : PL_additional_processing.R)
	3. PL_selected.txt ---> dat_PL1.txt/.RData (script : PL_mdAnalysis_and_imputation.R)
	4. dat_PL1.txt/.RData ---> dat_PL2.txt/.RData (script : PL_OPINION_scaling.R; this data was not used in the end)

Step-by-step procedure to transfrom raw data into the final recoded dataset with all complex indicator (CZ sample):

	1. CZ_rawdata.csv ---> CZ_main.txt/.RData ---> CZ_main_correct.txt/.RData (script : CZ_data_processing.R) 
	2. CZ_main_correct ---> CZ_selected.txt/.RData (script : CZ_additional_processing.R)
	3. CZ_selected.txt ---> dat_CZ1.txt/.RData (script : CZ_mdAnalysis_and_imputation.R)
	4. dat_CZ1.txt/.RData ---> dat_CZ2.txt/.RData (script : CZ_OPINION_scaling.R; this data was not used in the end)

Step-by-step procedure to obtain the final working and final official datasets (Joint sample):

	1. dat_PL1.txt/.RData + dat_CZ1.txt/.RData ---> fulldat.txt/.RData (script : Joint_OPINION_scaling.R)
	2. fulldat.txt/.RData ---> finalData.txt/.RData (script : Joint_KNOWLEDGE_scores_and_peduord_indicators.R)
	3. finalData.txt/.RData ---> finalDataExtended.txt/.RData (script : MainAnalysis.R)
	4. finalDataExtended.txt/.RData ---> officialData.txt/.RData (script : makeOfficialData.R) 

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
	* *dat_CZ2.txt/.RData* : Dataset with OPINION scaling in the Czech sample. Not used in the actual final analysis.
	* *dat_PL2.txt/.RData* : The same but for the Polish sample.
	* *fulldat.txt/.RData* : This dataset consists of all main variables and also has the scale of opinions in regard to economic liberalism-interventionism. In general this scale is called the Liberalism-Interventionism scale (the L.-I. scale). Psychometric qualities of the scale are assessed in this script and validation in subsamples is conducted. The outcomes are not too great, but it seems that satisfactory level of theoretical validity is met.
	* *finalData.txt/.RData* : Here the non-guessed KNOWLEDGE scores (both economic-financial and strictly financial) are added alongside some syntetic indicators of paretnal education (they are not used; the joint parents' higher education indicator is added only in the finalDataExtended dataset).
	* *finalDataExtended.txt/.RData* : Here an indicator of joint higher education of parents' is added together with some minor changes in coding and naming of variables.

* **Data/OfficialData**
	* *officialData.txt/.RData* ---> this is the dataset with proper naming and coding of all important variables. Unimportant and superfluous variables have been removed from it. This dataset is recommended to use by external analysts.

* **Data/Maps**	
	* *CorrectAnswersMap_KNOWLEDGE.csv* : the file is a map that maps respondents' answers to the KNOWLEDGE items to correct items. It is used to compute respondents' raw scores (total number of correct answers).
	* *Joint_OPINION_map.csv* : the file maps anwers to the OPINION items to the characteristic answers of the liberal and interventionist poles of the L.-I. scale. It is used in the OPINION scaling procedure.
	* *L.I.scale_map.csv* : this is almost the same file, but divided by country (it does not make any difference in fact; characteristic answers are the same in both countries).
	* *PL_items_map_KNOWLEDGE.csv* : this file maps KNOWLEDGE items to their semantic contents in Polish.
	* *PL_items_map_OPINIONS.csv* : this file maps OPINION items to their semantic contents in Polish.

* **R_scripts/data_processing**
	* *CZ_data_processing.R* : This script produces CZ_main.txt/.RData from CZ_rawdata.csv (see the Map of Files). It is densely commented to provide stepby-step explanation of all decisions that had to be taken during this stage of data processing.
	* *PL_data_processing* : The same but for the Polish sample.
	* *CZ_additional_processing* : This script produces CZ_selected.txt/.RData from CZ_main.txt/.RDatai (see the Map of Files). It introduces classification of respondents' study programmes (detailed description in the script comments) and removes outlying observations.
	* *PL_additional_processing* : The same but for the Polish sample.
	* *CZ_mdAnalysis_and_imputation.R* : This script produces dat_CZ1.txt/.RData from CZ_selected.txt/.RData (see the Map of Files). It imputes missing data and detects respondents with too many of them to be relaibly imputed. More detailed info to be found in the script comments.
	* *PL_mdAnalysis_and_imputation.R* : The same but for the Polish sample.
	* *processingTools.R* : This is a helper file that stores various custom functions that was written to facilitate data preparation and processing. It is frequently used by the data processing scripts. Especially, routines for the MUDFOLD scaling of the L.-I. scale are implemented here.

* **R_scripts/Visualization**
	* *PlottingFunctions.R* : This script stores various functions for useful data manipulation for plotting in lattice.
	* *Themes/LatticeThemes.R* : Here the V4 Lab Project lattice colorscheme is defiened.

* **R_scripts/CZ_analysis**
	* *CZ_General_Description.R* : This script performs a general analysis of the sample structure in regard to the sociodemographic variablesi in the Czech sample.
	* *CZ_KNOWLEDGE_analysis.R* : This script performs analysis of the answers to the KNOWLEDGE items set in the Czech sample.
	* *CZ_KNOWLEDGE_scaling.R* : This script is an attempt to formally scale the KNOWLEDGE items set in the Czech sample. It was a complete failure, so the script is not really important.
	* *CZ_OPINION_scaling.R* : This script performs scaling procedures (MUDFOLD) on the OPINION items set in the Czech sample. The outcomes of this analysis were not used in the further work (the joint scaling matters).

* **R_scripts/PL_analysis**
	* *PL_General_Description.R* : This script performs a general analysis of the sample structure in regard to the sociodemographic variablesi in the Polish sample.
	* *PL_KNOWLEDGE_analysis.R* :  This script performs analysis of the answers to the KNOWLEDGE items set in the Polish sample.
	* *PL_KNOWLEDGE_scaling.R* : This script is an attempt to formally scale the KNOWLEDGE items set in the Polish sample. It was a complete failure, so the script is not really important.
	* *PL_OPINION_scaling.R* :  This script performs scaling procedures (MUDFOLD) on the OPINION items set in the Polish sample. The outcomes of this analysis were not used in the further work (the joint scaling matters).

* **R_scripts/Joint_Analysis**
	* *Joint_OPINION_scaling.R* : This script perforsm MUDFOLD scaling of the OPINION item set in the Joint sample. It consists of item selection, psychometric qualities assessment, validation in subsamples (PL and CZ) and theoretcial validity assessment.
	* *Joint_KNOWLEDGE_scores_and_peduord_indicators.R* : This script adds non-guessed KNOWLEDGE scores and some (finally not used) joint indicators of parents' education.
	* *MainAnalysis.R* : This script adds a joint indicator of higher education of parents and conducts extensive main analyses (that are summarized in a corresponding .pdf report).
	* *Clustering.R* : This script performs basic analysis of the clustering structures in the data. Nothing interesting has been found.

* **Reports/PDF_reports** (they contain the same files but in different formats: .Rmd and .pdf)
	* *CZ_descriptives.Rmd/.pdf* : This report covers the sociodemographic structure of the Czech sample.
	* *PL_descriptives.Rmd/.pdf* : This report covers the sociodemographic structure of the Polish sample.
	* *CZ_KNOWLEDGE_analysis.Rmd/.pdf* : This report covers analysis of the answers to KNOWLEDGE items in the Czech sample.
	* *PL_KNOWLEDGE_analysis.Rmd/.pdf* : This report covers analysis of the answers ot KNOWLEDGE items in the Polish sample.
	* *PL_KNOWLEDGE_scaling.Rmd/.pdf* : This report covers an attempt to formally scale the KNOLWEDGE item set in the Polish sample. It was a complete failure, so there is no corresponding report for the Czech sample.
	* *Joint_OPINION_scaling.Rmd/.pdf* : This report covers the procedure of MUDFOLD scaling of the OPINION items in the Joint sample. It covers items selection, assessment of the psycometric qualities of the scale, validation in the subsamples (PL + CZ) as well as assessment of theoretical validity of the scale.
	* *V4_Lab_Main_Report.Rmd/.pdf* : The report covers the extensive main analysis of the V4 Lab Project dataset. It loosely corresponds to the analyses in the official report, although may diverge from it in some important aspects. However it covers almost all the topic covered in the official report plus many others. However its aim was to provide preliminary knowledge about the patterns in data, so some analyses migh have been conducted in non optimal ways. Thus the analyses in the official report are more accurate.
