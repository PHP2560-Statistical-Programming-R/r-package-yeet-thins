#' GetDataFromState
#' This function, given a state abbreviation and a provider taxonomy outputs a data fram
#' @param "state abbreviation", "taxonomy"
#' @return a data frame
#' @examples GetDataFromState("RI", "Primary Care")
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
GetDataFromState<-function(state, taxonomy) {
  zipcode_holder<-ZipsFromState(state)
  data<-NPIcode_taxonomy(zipcode_holder, taxonomy)
}