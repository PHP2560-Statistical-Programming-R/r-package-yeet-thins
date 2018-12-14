#' GetDataFromState
#' This function, given a state abbreviation and a provider taxonomy outputs a data fram
#' @param "state abbreviation", "taxonomy"
#' @return a data frame
#' @examples GetDataFromState("RI", "Primary Care")

GetDataFromState<-function(state, taxonomy) {
  zipcode_holder<-ZipsFromState(state)
  data<-NPIcode_taxonomy(zipcode_holder, taxonomy)
}