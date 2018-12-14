#' GetDataFromState
#' This function, given a state abbreviation and a provider taxonomy outputs a data fram
#' @param "state abbreviation", "taxonomy"
#' @return a data frame
#' @examples 
#' GetDataFromState("RI", "Primary Care")

GetDataFromState<-function(state, taxonomy) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  if(!require(rvest)){
    install.packages("rvest")
    library(rvest)
  }
  
  if(!require(XML)){
    install.packages("XML")
    library(XML)
  }
  
  if(!require(stringi)){
    install.packages("stringi")
    library(stringi)
  }
  
  if(!require(stringr)){
    install.packages("stringr")
    library(stringr)
  }
  zipcode_holder<-ZipsFromState(state)
  data<-NPIcode_taxonomy(zipcode_holder, taxonomy)
}