source("Libraries.R")
check_packages(c("rvest", "httr", "dplyr", "jsonlite", "XML", "stringr", "zipcode",
                 "ggplot2", "stringi", "roxygen2", "testthat"))

source("Libraries.R")
check_packages(c("rvest", "httr", "dplyr", "jsonlite", "XML", "stringr", "zipcode",
                 "ggplot2", "stringi", "roxygen2", "testthat"))

ZipsFromState<-function(state_name){
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}
