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
      ### 3rd arguments is a country code (PL or CZ)
      ### 4th argument is a flag that indicates wheter information about the execution of the function should be returned
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
### !!! <--- Function 1 ---> !!! ### (END)


### !!! <--- Function 2 ---> !!! ### (START)
###################################################
### Compute number/fraction of NA's in a vector ###
###################################################
howManyNAs <- function(vec, frac = FALSE) {
      ### function computes the number or fraction of NA's in the input vector
      ### it takes two arguments:
      ###   - vec : vector of input data (may be also a list or a single row of a data.frame)
      ###   - frac : this is a logical flag that indicates wheter a fraction should be returned
      ### Check the input data
      stopifnot(is.vector(vec) | is.list(vec) | (is.data.frame(vec) & dim(vec)[1] != 1),
                is.logical(frac))
      NAs <- sum(is.na(vec))
      if(frac) return(NAs / length(vec))
      else return(NAs)
}
### !!! <--- Function 2 ---> !!! ### (END)

### !!! <--- Function 3 ---> !!! ### (END)
###############################################
### Compute the dominant (mode) of a vector ###
###############################################
### It takes one argument:
### x - a vector of some type
dominant <- function(x) {
      stopifnot(is.vector(x) | (is.data.frame(x) & dim(x)[1] == 1))
      if(is.data.frame(x)) x <- unlist(x)
      Tab = table(x)
      mode = 0
      uni = 0
      if(is.numeric(x)) {
            uni = sort(unique(x))
            mode = which(Tab==max(Tab))[[1]]
            mode = uni[mode]
            return(mode)
      }
      else {
            mode = which(Tab==max(Tab))[[1]]
            return(names(Tab[mode]))
      }
}
### !!! <--- Function 3 ---> !!! ### (END)

### !!! <--- Function 4 ---> !!! ### (START)
###########################################################################
### Compute relative or absolute entropy of distribution/vector in bits ###
###########################################################################
### It takes 2 arguments:
### x - a vector ar a table object with a distribution
### rel - logical flag indicating wheter relative or absolute entropy should be compted
entropy <- function(x, rel = FALSE) {
      # x maybe of a class table or or a simple vector
      if(class(x) != "table") x = table(x)
      x = prop.table(x)
      k = length(x) # number of classes
      H = -x*log(x, 2)
      H[is.na(H)] = 0
      H = sum(H)
      if(rel) {if(k==1) return(0) else return(H / log(k, 2))}
      else return(H)
}
### !!! <--- Function 4 ---> !!! ### (END)

### !!! <--- Function 5 ---> !!! ### (START)
#################################
### Compute relative variance ###
#################################
### It takes 2 arguments
### - x : a numerical data vector
### - na.rm : flag indicating wheter NAs should be removed or not
relvar <- function(x, na.rm=FALSE) {
      L = max(x, na.rm=na.rm) - min(x, na.rm=na.rm)
      D = var(x, na.rm=na.rm)
      return(D / (L^2/4))
}

### !!! <--- Function 6 ---> !!! ### (START)
#######################################################################
### Derive imputed dataset from a list of multiply imputed datasets ###
#######################################################################
getDataFromMIDS <- function(impdata, data, stats = NULL, rnd = FALSE, exclude_obs = NULL, diagnostics = TRUE) {
      ### Function returns a data.frame with imputed values 
      ### It takes 6 arguments:
      ### - impdata : list of dataframes with multiply imputed values (as in the mids objects from mice package). Elements of the list should be named with variable names as in the mids objects. See mice package for more info.
      ### - data : a dataset that was used for imputation (it may have additional variables)
      ### - stats : a character vector indicating which statistic should be used to determine the final imputed value. Possible values (mean, median, mode)
      ### - rnd : logical flag that determines wheter value in stats should be rounded or not. If set to FALSe then they are not rounded; if set to integer then the values are rounded accordingly
      ### - obs : a vector of ids of observations (numerical or rownames) to indicate for which observations data should be imputed and for which NAs should be left
      ### - diagnostics : logical flag indicating wheter additional object with diagnostics informations should be returned or not
      ### Check the input data
      stopifnot(is.list(impdata),
                all(sapply(impdata, is.data.frame) + sapply(impdata, is.null)),
                is.data.frame(data),
                (is.character(stats) & length(stats) == length(impdata)) | is.null(stats),
                is.logical(rnd) | (is.integer(rnd) & length(rnd) == 1),
                is.null(exclude_obs) | is.vector(exclude_obs),
                is.logical(diagnostics),
                all(intersect(names(impdata), names(data)) == names(impdata)))
      ### Inner helper function that computes the final imputed values for a respondent
      imputeFinal <- function(vec, stat, rnd = FALSE, diagnostics) {
            ### vec - vector of imputation for a given observation
            ### stat - a length 1 character vector indicating the final imputation statistic
            ### rnd - as in the parent function
            ### diagnostics - as in the parent function
            ### Check the input data
            stopifnot(is.vector(vec) | (is.data.frame(vec) & dim(vec)[1] == 1),
                      is.character(stat) & length(stat) == 1,
                      stat %in% c("mean", "median", "mode"),
                      is.logical(rnd) | (is.integer(rnd) & length(rnd) == 1))
            impval = 0
            diag = 0
            diagtype = ""
            if(is.data.frame(vec)) vec <- unlist(vec)
            if(stat == "mean") {
                  if(is.logical(rnd)) {
                        if(rnd) impval = as.integer(round(mean(vec)))
                        else impval = mean(vec)
                  }
                  else impval = round(mean(vec), rnd)
                  if(diagnostics) {
                        diag = relvar(vec)
                        diagtype = "relative variance"
                  }
            }
            else if(stat == "median") {
                  if(is.logical(rnd)) {
                        if(rnd) impval = as.integer(round(median(vec)))
                        else impval = median(vec)
                  }
                  else impval = round(median(vec), rnd)
                  if(diagnostics) {
                        diag = entropy(vec, rel = TRUE)
                        diagtype = "relative entropy"
                  }
            }
            else {
                  impval = dominant(vec)
                  diag = entropy(vec, rel = TRUE)
                  diagtype = "relative entropy"
            }
            if(diagnostics & stat == "mean") {
                  return(list(value = impval, mean = diag, diagtype = diagtype))
            }
            else if(diagnostics & stat != "mean") {
                  return(list(value = impval, entropy = diag, diagtype = diagtype))
            }
            else return(impval)
      }
            
      ### Prepare data frame fro storing the output
      DF <- data[, which(names(data) %in% names(impdata))]
      DFdiag <- DF
      DFdiag[ , ] <- 0
      diagvector <- vector(mode = "character", length = dim(DF)[2])
      names(diagvector) <- names(DF)

      ### Prepare vector for iteration
      vars <- names(impdata[!is.null(impdata)])
      ### Set the stats vector if NULL
      if(is.null(stats)) {
            stats <- vector(mode = "character", length = dim(DF)[2])
            names(stats) <- names(DF)
            for(var in vars) {
                  if(is.factor(DF[, var]) | is.character(DF[, var])) stats[var] = "mode"
                  else stats[var] = "mean"
            }
      }
      ### Set the stats vector if not NULL
      else names(stats) <- vars
      ### Derive the imputed dataset
      for(var in vars) {
            cat(sprintf("var : %s\n", var))
            if(is.null(impdata[[var]])) next
            dat <- impdata[[var]]
            stat <- stats[var]
            observations <- rownames(dat)
            if(!is.null(exclude_obs)) observations <- setdiff(observations, exclude_obs)
            for(obs in observations) {
                  cat(sprintf("    obs : %s\n", obs))
                  vals <- imputeFinal(dat[obs, ], stat=stat, rnd=rnd, diagnostics=diagnostics)
                  impval = vals[[1]]
                  diag = 0
                  diagtype = ""
                  if(diagnostics) {
                        diag = vals[[2]]
                        diagtype = vals[[3]]
                  }
                  DF[obs, var] = impval
                  if(is.integer(impval)) cat(sprintf("    NA <-- %d\n", impval))
                  else if(is.numeric(impval)) cat(sprintf("    NA <-- %f\n", impval))
                  else cat(sprintf("    NA <-- %s\n", impval))
                  DFdiag[obs, var] = diag
                  diagvector[var] = diagtype
            }
      } 
      if(diagnostics) return(list(Imputed = DF, Diagnostics = DFdiag, DiagTypes = diagvector))
      else return(DF)
}
### !!! <--- Function 6 ---> !!! ### (END)