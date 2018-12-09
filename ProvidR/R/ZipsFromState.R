#`ZipsFromState
#'
#'This function, given a state, provides you with all of the zip codes in that state.
#'@param state_name.

ZipsFromState<-function(state_name){
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}
