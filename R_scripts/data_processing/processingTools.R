####################################################
### Data processing tools for the V4 Lab project ###
####################################################

### This file stores various tools used for basic processing of the data from the V4Lab project

### !!! <--- Function 1 ---> !!! #### (START)
###################################################
### Map respondents' answers to correct answers ###
###################################################
mapKnowledgeToCorrect <- function(data, map, country, verbose = FALSE) {
      ### This function maps answers from the KNOWLEDGE item bank to correct answers
      ### 1st argument is a data.frame containing only the KNOWLEDGE items
      ### 2nd argument is a data.frame with the map of the correct answers
      ### The map should have 2 columns; 1st is the item; 2nd is the correct answer
      
      ### IMPORTANT! This function should be used only on data with no non-ASCII characters
      
      ### Check whether the input data is OK
      stopifnot(is.data.frame(data), is.data.frame(map),
                any(grepl("PL", names(map), perl = TRUE, ignore.case = TRUE)) | any(grepl("CZ", names(map), perl = TRUE, ignore.case = TRUE)),
                is.character(country))
      
      ### Get proper country code
      cntry <- names(map)[grep(country, names(map), perl = TRUE, ignore.case = TRUE)]
      
      ### Inner function that verifies correctness of one respondent's answers
      correct_respondent <- function(resvec, map, country) {
            ### Check wheter the input data is OK
            stopifnot(is.data.frame(resvec) | is.vector(resvec),
                      !is.null(names(resvec)),
                      all(is.element(names(resvec), rownames(map))),
                      is.character(country))
            ### Convert resvec to vector if necessary
            if(!is.vector(resvec)) {
                  resvec_n <- names(resvec)
                  resvec <- as.numeric(resvec)
                  names(resvec) <- resvec_n
            }
            ### Check respondent's answers
            correct_vec <- vector(mode = "numeric", length = length(resvec))
            names(correct_vec) <- names(resvec)
            for(item in names(resvec)) {
                  if(!(item %in% rownames(map))) {
                        stop(message = "items in data and items in map do not match")
                  }
                  if(verbose) print(sprintf("---Item : %s", item))
                  ans <- as.numeric(resvec[item])
                  if(verbose) print(sprintf("ans -> %d", as.integer(ans)))
                  corr_ans <- as.numeric(map[item, cntry])
                  if(verbose) print(sprintf("corr_ans -> %d", as.integer(corr_ans)))
                  if(corr_ans != 0 & corr_ans != 1) {
                        correct_vec[item] <- NA
                        if(verbose) print(sprintf("%s -< NA (improper question)", item))
                  }
                  else if(is.na(ans)) {
                        correct_vec[item] <- NA
                        if(verbose) print(sprintf("%s <- NA", item))
                  }
                  else if(ans == corr_ans) {
                        correct_vec[item] <- 1
                        if(verbose) print(sprintf("%s <- 1", item))
                  }
                  else {
                        correct_vec[item] <- 0
                        if(verbose) print(sprintf("%s <- 0", item))
                  }
            }
            return(correct_vec)
      }
      ### Map respondents' answers
      mapped_data <- data
      mapped_data[ , ] <- 0
      for(i in 1:dim(data)[1]) {
            if(verbose) print(sprintf("RESPONDENT No. %d", i))
            resp <- data[i, ]
            mapped_resp <- correct_respondent(resp, map, cntry)
            mapped_data[i, ] <- mapped_resp
      }
      return(mapped_data)
}
### !!! <--- Function 2 ---> !!! ### (END)