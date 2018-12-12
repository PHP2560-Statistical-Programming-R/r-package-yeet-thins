#' ZipsFromState
#' Pulls out all the zipcodes from a state

if(!require(zipcode)){
  install.packages("zipcode")
  library(zipcode)
}


ZipsFromState<-function(state_name){
  data(zipcode)
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}

