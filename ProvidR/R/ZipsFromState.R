#' ZipsFromState
#' Pulls out all the zipcodes from a state
library(rvest)
library(httr)
library(dplyr)
library(jsonlite)
library(XML)
library(stringr)
library(zipcode)
library(dplyr)
library(stringr)
library(ggplot2)
library(stringi)
library(roxygen2)
library(testthat)
library(repmis)



ZipsFromState<-function(state_name){
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}

