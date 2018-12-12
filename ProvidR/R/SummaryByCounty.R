#'SummaryByCounty
#'This function gives you the numver of providers in each county, the number of providers per 1,000 people, and the population of the county
#'@param a data from from provider in the state by county
#'@return a data fram
#

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

SummaryByCounty<-function(df){
  rows<-df %>%
    group_by(CTYNAME, POPESTIMATE2010, Abbreviation, state.name) %>%
    count() %>%
    filter(Abbreviation==state.name) %>%
    select(CTYNAME, POPESTIMATE2010, state.name, n) %>%
    mutate(provider_density = n/POPESTIMATE2010*1000) %>%
    arrange(n)
  return(rows)
}