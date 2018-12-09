#`ZipsFromState
#'
#'This function, given a state, provides you with all of the zip codes in that state.
#'@param state_name.
#'@return a vector containing all of the zipcodes in a given state
#'@example
#'ZipsFromState("RI")
#'ZipsFromState("CT")

ZipsFromState<-function(state_name){
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}
devtools::document("man")
