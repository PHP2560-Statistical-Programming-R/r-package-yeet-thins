#'SummaryByCounty
#'This function gives you the number of providers in each county, the number of providers per 1,000 people, and the population of the county
#'@param a data from from provider in the state by county
#'@return a data frame
#' @examples 
#' SummaryByCounty("RI", "Cardiologist")

SummaryByCounty<-function(state, taxonomy){
  #install/load required packages
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  prov.dat <- ProviderInStateByCounty(state,taxonomy)
  #process data
  rows <- prov.dat %>%
    filter(Abbreviation==state) %>%
    group_by(CTYNAME, STNAME, POPESTIMATE2010, state.name) %>%
    count() %>%
    select(CTYNAME, POPESTIMATE2010, STNAME, state.name, n) %>%
    mutate(provider_density = n/POPESTIMATE2010*1000) %>%
    arrange(n)
  return(rows)
}
