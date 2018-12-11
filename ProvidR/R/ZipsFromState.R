#' ZipsFromState
#' Pulls out all the zipcodes from a state
#' @param "State"
#' @return Zipcodes from the given state'



ZipsFromState<-function(state_name){
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}

document()
