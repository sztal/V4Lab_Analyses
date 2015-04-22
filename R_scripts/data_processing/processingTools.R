### This file stores various tools used for basic processing of the data from the V4Lab project

mapKnowledgeToCorrect <- function(data, map) {
      ### This function maps answers from the KNOWLEDGE item bank to correct answers
      ### 1st argument is a data.frame containing only the KNOWLEDGE items
      ### 2nd argument is a data.frame with the map of the correct answers
      ### The map should have 2 columns; 1st is the item; 2nd is the correct answer
      
      ### Check whether the input data is OK
      stopifnot(is.data.frame(data))
      stopifnot(is.data.frame(map))
      stopifnot(dim(map)[2] == 2)
      stopifnot(dim(data)[2] == dim(map)[1])
      
      ### Map answers to correct answers
      