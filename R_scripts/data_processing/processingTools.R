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

### !!! <--- Function 7 ---> !!! ### (START)
##############################################################
### MAP OPINION ANSWERS TO LIBERALISM AND SOCIALISM SCALES ###
##############################################################
mapOPINION <- function(data, map, country, liberal, verbose=FALSE) {
      ### This function maps answers from the OPINION item bank to liberalism and socialism scales' answers
      ### 1st argument is a data.frame containing only the OPINION items
      ### 2nd argument is a data.frame with the map of the scales' answers
      ### 3rd arguments is a country code (PL or CZ)
      ### 4th arguments is the scale flag (the liberalism scale is returned if TRUE; FALSE returns the socialism scale)
      ### 5th argument is a flag that indicates wheter information about the execution of the function should be returned
      ### The map should have 4 columns: 1st is the answers for the liberalism scale (PL); 2nd is the answers for the socialism scale (PL); 3rd and 4th stores the scales' values for the CZ scales
      
      ### IMPORTANT! This function should be used only on data with no non-ASCII characters
      
      ### Check whether the input data is OK
      stopifnot(is.data.frame(data), is.data.frame(map), dim(map)[2] == 4,
                any(grepl("PL", names(map), perl = TRUE, ignore.case = TRUE)) | any(grepl("CZ", names(map), perl = TRUE, ignore.case = TRUE)),
                is.character(country) & length(country) == 1,
                is.logical(verbose) & length(verbose) == 1,
                is.logical(liberal) & length(liberal) == 1)
      
      ### Get proper country code
      cntry <- names(map)[grep(country, names(map), perl = TRUE, ignore.case = TRUE)]
      if(liberal) cntry <- grep("lib", cntry, perl=TRUE, ignore.case=TRUE, value=TRUE)
      else cntry <- grep("soc", cntry, perl=TRUE, ignore.case=TRUE, value=TRUE)
      
      ### Inner function that maps the answer of a respondent to scales
      mapRespondent <- function(resvec, map, country, liberal) {
            ### Check wheter the input data is OK
            stopifnot(is.data.frame(resvec) | is.vector(resvec),
                      !is.null(names(resvec)),
                      all(is.element(names(resvec), rownames(map))),
                      is.character(country) & length(country) == 1)
            ### Convert resvec to character vector if necessary
            if(!is.vector(resvec)) {
                  resvec_n <- names(resvec)
                  resvec <- as.character(lapply(resvec, as.character))
                  names(resvec) <- resvec_n
            }
            
            ### Check respondent's answers
            correct_vec <- vector(mode = "numeric", length = length(resvec))
            names(correct_vec) <- names(resvec)
            #print(resvec)
            for(item in names(resvec)) {
                  if(!(item %in% rownames(map))) {
                        stop(message = "items in data and items in map do not match")
                  }
                  if(is.na(map[item, cntry]) | is.null(map[item, cntry]) | map[item, cntry] == "") {
                        next
                  }
                  if(verbose) print(sprintf("---Item : %s", item))
                  ans <- as.character(resvec[item])
                  if(verbose) print(sprintf("ans -> %s", as.character(ans)))
                  corr_ans <- as.character(map[item, cntry])
                  if(verbose) print(sprintf("corr_ans -> %s", as.character(corr_ans)))
                  if(is.na(ans)) {
                        correct_vec[item] <- NA
                        if(verbose) print(sprintf("%s <- NA", item))
                  }
                  else if(as.character(ans) == as.character(corr_ans)) {
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
      #print(mapped_data)
      mapped_data[ , ] <- 0
      for(i in 1:dim(data)[1]) {
            if(verbose) print(sprintf("RESPONDENT No. %d", i))
            resp <- data[i, ]
            #print(resp)
            mapped_resp <- mapRespondent(resp, map, cntry, liberal)
            #print(mapped_resp)
            mapped_data[i, ] <- mapped_resp
      }
      if(liberal) substr(names(mapped_data), 1, 1) <- "l"
      else substr(names(mapped_data), 1, 1) <- "s"
      return(mapped_data)
}
### !!! <--- Function 7 ---> !!! ### (END)

### !!! <--- Function 8 ---> !!! ### (START)
################################
### COMPUTE UNFOLDING ERRORS ###
################################
unfoldingErrors <- function(vec) {
      ### This function computes the number of violations of the unfolding model in a binary numeric data vector
      ### It takes 1 argument
      ###   - vec: a numeric binary data vector

      vec <- as.numeric(vec)
      ### Check the input data
      stopifnot(is.numeric(vec),
                all(is.element(levels(as.factor(vec)), c("0", "1"))))
      
      m <- length(vec)
      ### Compute unfolding errors
      errors <- 0
      ones <- which(vec == 1)
      nones <- length(ones)
      if(nones >= 2) {
            for(i in 1:(nones-1)) {
                  for(j in (i+1):nones) {
                        left <- ones[i]
                        right <- ones[j]
                        if(right - left == 1) next
                        span <- vec[left:right]
                        errors <- errors + length(span[span==0])
                  }
            }
      }
      return(errors)
}
### !!! <--- Function 8 ---> !!! ### (END)

### !!! <--- Function 9 ---> !!! ### (START)
################################################################
### NUMERICALLY SIMULATE EXPECTED NUMBER OF UNFOLDING ERRORS ###
################################################################
numericalUnfoldingErrors <- function(vardiffs, nobs, n, seed) {
      ### This function performs a numerical experiment simulating the distribution of unfolding errors in a dataset under an assumption of independence of variables given a vector of difficulty levels of these variables
      ### It takes 4 arguments:
      ###   - vardiffs: a vector of variables' difficulty levels
      ###   - nobs: the number of observations in the simulated dataset
      ###   - n: the number of simulated datasets
      ###   - seed: seed for the pseudorandom numbers generator
      
      ### Check the input arguments
      stopifnot(is.numeric(vardiffs),
                is.numeric(nobs) & length(nobs) == 1,
                is.numeric(n) & length(n) == 1)
      
      ### Make varnames
      m <- length(vardiffs)
      varnames <- vector(mode="character", length=m)
      if(is.null(names(vardiffs))) {
            varnames <- paste(as.character(1:m), "var", sep="")
      }
      else varnames <- names(vardiffs)
      
      ### Set seed
      set.seed(seed)
      
      ### Inner helper function
      ### It generates simulated data for one observation
      makeSimRow <- function(vardiffs) {
            m <- length(vardiffs)
            simdata <- vector(mode="numeric", length=m)
            rnums <- runif(m, 0, 1)
            simdata[rnums <= vardiffs] <- 1
            return(simdata)
      }
            
      ### Conduct simulation
      simerrors <- vector(mode="numeric", length=n)
      for(iter in 1:n) {
            cat(sprintf("ITERATION: %d\n", as.integer(iter)))
            dat <- data.frame(matrix(0, nrow=nobs, ncol=m))
            names(dat) <- varnames
            for(row in 1:nobs) {
                  #cat(sprintf("\trow: %d\n", as.integer(row)))
                  dat[row, ] <- makeSimRow(vardiffs)
            }
            errors <- sum(apply(dat, 1, unfoldingErrors))
            simerrors[iter] <- errors
      }
      return(simerrors)
}
### !!! <--- Function 9 ---> !!! ###

### !!! <--- Function 10 ---> !!! ###
###############################################
### CHECK IF A VARIABLE IS NUMERICAL BINARY ###
###############################################
is.binary <- function(vec) {
      ### Takes one argument:
      ### vec: a vector of some type
      out <- is.numeric(vec) & all(is.element(levels(as.factor(vec)), c("0", "1")))
      return(out)
}
### !!! <--- Function 10 ---> !!! ###

### !!! <--- Function 11 ---> !!! ###
################################################
### COMPUTE TRIPLET H IN THE UNFOLDING MODEL ###
################################################
unfoldingHijk <- function(dat) {
      ### This function computes H correlation statistic for a triplet of items for the unfolding model
      ### It takes 2 arguments:
      ###   - dat: a data.frame with binary numerical variables
      ###   - vars: a numerical vector of length 3 indicating indexes of the variables  in the data.frame
      ### The function does not tolerate NAs
      
      ### Check the input data
      stopifnot(is.data.frame(dat) & all(apply(dat, 2, is.binary)),
                all(complete.cases(dat)))
      
      diffs <- apply(dat, 2, mean)
      n <- dim(dat)[1]
      ### Observed violations
      Eobs <- sum(apply(dat, 1, unfoldingErrors))
      cat(sprintf("E-obs --> %d\n", as.integer(Eobs)))
      ### Expected violations under the null model
      Eexp <- diffs[1]*(1-diffs[2])*diffs[3]*n
      cat(sprintf("E-exp --> %f\n", Eexp))
      Hijk <- 1 - (Eobs/Eexp)
      cat(sprintf("Hijk --> %f\n", Hijk))
      return(Hijk)
}
### !!! <--- Function 11 ---> !!! ###

### !!! <--- Function 12 ---> !!! ###
########################
### COMPUTE ALL Hijk ###
########################
allUnfoldingHijk <- function(dat) {
      ### This function computes all unfolding Hijk correlations in a dataset
      ### It takes 1 argument:
      ###   - dat: a data.frame with only binary numerical variables and no NAs
      ### CAUTION!!! The function assumes that the variables are in a proper unfolding order
      ### Check the input arguments
      stopifnot(is.data.frame(dat), all(apply(dat, 2, is.binary)),
                all(complete.cases(dat)))
      
      ### Compute the number of triplets
      m <- dim(dat)[2]
      triplets <- choose(m, 3)
                        
      ### Compute Hijk statistics
      stats <- vector(mode="numeric", length=triplets)
      statnames <- vector(mode="character", length=triplets)
      counter <- 0
      for(i in 1:(m-2)) {
            for(j in (i+2):m) {
                  for(k in (i+1):(j-1)) {
                        counter <- counter + 1
                        sname <- paste(c("H",i,j,k), collapse="_")
                        statnames[counter] <- sname
                        Hijk <- unfoldingHijk(dat[, c(i,k,j)])
                        stats[counter] <- Hijk
                  }
            }
      }
      names(stats) <- statnames
      return(stats)
}
### !!! <--- Function 12 ---> !!! ### (END)

### !!! <--- Function 13 ---> !!! ### (END)
#############################################
### COMPUTE UNFOLDING H and Hi STATISTICS ###
#############################################
unfoldingH <- function(dat) {
      ### Takes one argument:
      ###   - dat: a data.frame of binary variables and proper unfolding item ordering
      ### Check the input arguments
      stopifnot(is.data.frame(dat), all(apply(dat, 2, is.binary)),
                all(complete.cases(dat)))
      
      ### Compute H
      n <- dim(dat)[1]
      m <- dim(dat)[2]
      triplets <- choose(m, 3)
      His <- vector(mode="numeric", length=m)
      nHis <- vector(mode="character", length=m)
      Eexps <- vector(mode="numeric", length=triplets)
      Eobs <- vector(mode="numeric", length=triplets)
      nvec <- vector(mode="character", length=triplets)
      counter <- 0
      for(i in 1:(m-2)) {
            for(j in (i+2):m) {
                  for(k in (i+1):(j-1)) {
                        counter <- counter + 1
                        cdat <- dat[, c(i,k,j)]
                        diffs <- apply(cdat, 2, mean)
                        Eexp <- diffs[1]*(1-diffs[2])*diffs[3]*n
                        Eexps[counter] <- Eexp
                        Eob <- sum(apply(cdat, 1, unfoldingErrors))
                        Eobs[counter] <- Eob
                        name <- paste(c(i,k,j), collapse="_")
                        nvec[counter] <- name
                  }
            }
      }
      for(i in 1:m) {
            pattern <- paste("_?", paste(i, "_?", sep=""), sep="")
            ids <- grep(pattern, nvec, perl=TRUE)
            His[i] <- 1 - sum(Eobs[ids])/sum(Eexps[ids])
            name <- paste("H", i, sep="_")
            nHis[i] <- name
      }
      names(His) <- nHis
      H <- 1 - sum(Eobs)/sum(Eexps)
      L <- list(H, His)
      names(L) <- c("H", "Hi")
      return(L)
}
### !!! <--- Function 13 ---> !!! ###

### !!! <--- Function 14 ---> !!! ### 
#########################################################
### PERFORM EASIEST ITEMS TEST OF THE UNFOLDING MODEL ###
#########################################################
EasiestItemsTest <- function(dat, ids) {
      ### This function performs the easiest items test of the unidimensional unfolding scale models fit, as described by Van Schuur. First three subsets of respondents are found: 1) those with only 2 positives answers that are additionally alocated to two easiest items of the right pole of the unfolding scale; 2) those who allocated their 2 aswers to the two easiest items of the left pole of the scale; 3) and those who allocated their two answers to the easiest items of the both polar subscales of the unfolding scale. The it is tested whether positive answers in these groups are positively correlated. In a perect unfolding model positive answers should be independent in the first two groups and positively correlated in the third.
      ### The function takes 2 arguments:
      ###   - dat: a data.frame of binary variables in a proper unfolding model
      ###   - ids: numeric vecotr of lenght 4 with ids of the four variables of interest; alternativel it may be a character vector of length 4 with their labels.
      
      ### Check the input arguments
      stopifnot(is.data.frame(dat), all(apply(dat, 2, is.binary)),
                length(ids) == 4 & (is.character(ids) | is.numeric(ids)))
      
      ### Perform the test
      resp2 <- apply(dat, 1, sum) == 2
      dat <- dat[resp2, ids]
      n <- dim(dat)[1]
      if(n < 20) {
            cat(sprintf("N = %d\n", as.integer(n)))
            stop(message="Too few respondents; N < 20")
      }
      tableft <- table(dat[, 1:2])
      tabright <- table(dat[, 3:4])
      tabmiddle <- table(dat[, 2:3])
      ltest <- chisq.test(tableleft)
      rtest <- chisq.test(tableright)
      mtest <- chisq.test(tablemiddle)
      leftout <- print(ltest)
      rightout <- print(rtest)
      middleout <- print(mtest)
      L <- list(leftout, rightout, middleout)
      names(L) <- c("Mokken Left", "Mokken Right", "Unfolding Middle")
      return(L)
}
### !!! <--- Function 14 ---> !!! ### (END)

### !!! <--- Function 15 ---> !!! ### (START)
################################################################
### COMPUTE BOOTSTRAP STANDARD ERRORS FOR UNFOLDING Hi and H ###
################################################################
unfoldingSE <- function(dat, n=100) {
      ### The function comptues standard errors for unfolding Hi and H statistcs. Bootstrap is used due to the lack of analytical formula.
      ### It takes 2 arguments:
      ###   - dat: a data.frame of binary variables with a proper unfolding items ordering
      ###   - n: the number of simulated datasets in the bootstrap procedure
      
      ### Check the input arguments
      stopifnot(is.data.frame(dat), all(apply(dat, 2, is.binary)),
                is.numeric(n) & length(n) == 1)

      ### Compute standard errors
      m <- dim(dat)[2]
      nobs <- dim(dat)[1]
      His <- paste("H", 1:m, sep="_")
      Hvec <- vector(mode="numeric", length=n)
      Himat <- matrix(0, nrow=n, ncol=m)
      colnames(Himat) <- His
      
      ### Bootstrap loop
      for(i in 1:n) {
            cat(sprintf("ITERATION: %d\n", as.integer(i)))
            ids <- sample.int(nobs, size=nobs, replace=TRUE)
            bootdat <- dat[ids, ]
            Hstats <- unfoldingH(bootdat)
            Hvec[i] <- Hstats$H
            Himat[i, ] <- Hstats$Hi
      }
      
      Hse <- sd(Hvec)
      Hise <- apply(Himat, 2, sd)
      L <- list(Hse, Hise)
      names(L) <- c("H_se", "Hi_se")
      return(L)
}
### !!! <--- Function 15 ---> !!! ###

### !!! <--- Function 16 ---> !!! ###
###################################################################
### COMPUTE CONDITIONAL ADJACENCY MATRIX FOR AN UNFOLDING MODEL ###
###################################################################
ufConditionalMat <- function(dat, round=2) {
      ### The function takes 2 arguments:
      ###   - dat: a data.frame with binary variables and a proper unfolding items orderings
      ###   - round: a numerical vector of length 1 indicating how long decimal expansion in the output should be
      
      ### Check the input data
      stopifnot(is.data.frame(dat) & all(apply(dat, 2, is.binary)))
      
      ### Generate the condtitional adjacency matrix
      m <- dim(dat)[2]
      CondMat <- matrix(0, nrow=m, ncol=m)
      rownames(CondMat) <- names(dat)
      colnames(CondMat) <- paste("|", names(dat), sep="")
      for(i in 1:m) {
            for(j in 1:m) {
                  if(i == j) {
                        CondMat[i, j] <- NA
                        #cat(sprintf("%d == %d --> NA\n",
                        #            as.integer(i), as.integer(j)))
                  }
                  else {
                        cdat <- dat[dat[, j] == 1, i]
                        ctab <- table(cdat)
                        cprob <- 0
                        if(length(ctab) == 2) {
                              cprob <- prop.table(ctab)[2]
                        }
                        CondMat[i, j] <- cprob
                        #cat(sprintf("%d != %d --> %f\n", 
                        #            as.integer(i), as.integer(j), cprob))
                  }
            }
      }
      return(round(CondMat, round))
}
### !!! <--- Function 16 ---> !!! ### (END)

### !!! <--- Function 17 ---> !!! ### (START)
###############################################
### COMPUTE UNIDIMENSIONAL UNFOLDING SCORES ###
###############################################
unfoldingScore <- function(vec, weighted=FALSE) {
      ### This function computes unfolding scale scores using Van Schuur or Van der Bruug approach
      ### It takes 2 arguments:
      ###   - vec: a binary vector of data with proper unfolding ordering
      ###   - weighted: logical flag indicating whether weighted approach of Van der Bruug should be used. If false then standard approach of Van Schuur is used
      
      ### Check the input data
      stopifnot(is.numeric(vec) | is.data.frame(vec) & dim(vec)[1] == 1,
                is.binary(as.numeric(vec)),
                is.logical(weighted) & length(weighted) == 1)
      
      vec <- as.numeric(vec)
      ### Van Schuur approach
      if(!weighted) {
            score <- 0
            if(sum(vec) == 0) {
                  score <- NA
                  return(score)
            }
            ones <- which(vec == 1)
            firstOne <- ones[1]
            lastOne <- 0
            if(length(ones) == 1) {
                  leftzeros <- length(vec[1:firstOne]) - 1
                  score = score + 1 + leftzeros*2
            }
            else {
                  lastOne <- ones[length(ones)]
                  leftzeros <- length(vec[1:firstOne]) - 1
                  inzeros <- setdiff(firstOne:lastOne, ones)
                  score <- score + length(ones) + 2*leftzeros
                  for(zero in inzeros) {
                        if(mean(zero > ones) == .5) score <- score + 1
                        else if(mean(zero > ones) < .5) score <- score + 2
                  }
            }
      }
      else {
            score <- sum(vec*(1:length(vec))) / sum(vec)
      }
      return(score)
}