#' ZipsFromState
#' Pulls out all the zipcodes from a state

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

ZipsFromState("RI")


