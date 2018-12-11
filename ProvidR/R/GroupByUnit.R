#groups by diferent units
source("Libraries.R")
check_packages(c("rvest", "httr", "dplyr", "jsonlite", "XML", "stringr", "zipcode",
                 "ggplot2", "stringi", "roxygen2", "testthat"))
GroupByUnit<-function(data, unit){
  if(unit=="zipcode"){
    countbyzip<-data%>%
      group_by(zipcode) %>%
      count() %>%
      arrange(n)

    return(countbyzip)
  } else if (unit==="county") {
    countbycounty<-data%>%
      group_by(county) %>%
      count() %>%
      arrange(n)

    return(countbycounty)
  } else if (unit=="state"){
    countbystate<-data%>%
      group_by(state) %>%
      count() %>%
      arrange(n)

    return(countbystate)
  } else if (unit=="city"){
    countbycity<-data%>%
      group_by(city) %>%
      count() %>%
      arrange(n)

    return(countbycity)
  } else if (unit=="type"){
    countbyprovider<-data%>%
      group_by(Primary_Taxonomy) %>%
      count() %>%
      arrange(desc(n))

    return(countbyprovider)
  }
}
