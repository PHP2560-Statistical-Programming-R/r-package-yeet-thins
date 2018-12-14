#' ZipsFromState
#' Pulls out all the zipcodes from a state
#' This is a helper function that is used for other functions.
#' @param "State abbreviaton"
#' @return a list of zipcodes from the state
#' @examples 
#' ZipsFromState("CT")

ZipsFromState<-function(state_name){
  #install required packages
  if(!require(zipcode)){
    install.packages("zipcode")
    library(zipcode)
  }

  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  #process data
  data(zipcode)
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}
