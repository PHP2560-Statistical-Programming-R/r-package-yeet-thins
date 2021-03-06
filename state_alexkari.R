library(rvest)
library(httr)
library(dplyr)
library(jsonlite)
library(XML)
library(stringr)
library(zipcode)
library(dplyr)
library(stringr)
library(ggplot2)
library(stringi)
library(roxygen2)
library(testthat)



#1. All zipcodes from the state
ZipsFromState<-function(state_name){
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}


#2. Uses zipcodes to pull NPI providers

ProvidersInStateByCounty<-function(state,taxonomy){
  state="RI"
  taxonomy="Mental Health"
  zips_used <- ZipsFromState(state)
  url1<- "https://npiregistry.cms.hhs.gov/registry/search-results-table?addressType=ANY&postal_code=" #setting the url to scrape from
  provider.data <- data.frame() #initializing an empty data frame
  skips <- seq(0,9999999,100) #create skips
  for (i in 1:length(zips_used)) { #iterating over all RI zip codes
    for (j in 1:length(skips)){ #also iterating over skils
      zip <- zips_used[i]
      skip <- skips[j]
      tax <- str_replace_all(taxonomy," ","+")
      url<-paste0(url1,zip,"&skip=",skip,"&taxonomy_description=",tax) #pasting the url, with the rhode island zip code and including the skips
      #text scrape to pull our places by zip code
      h <- read_html(url, timeout = 200)
      reps <- h %>% #setting up the repeating structure
        html_node("table") %>%
        html_table()
      if (any(is.na(reps[,1])) | all(is.na(reps[,1]))) { #if the page has no physicians, move to next zip code
        break}
      reps$zip <- zips_used[i]
      reps$state_abbrev <- state
      provider.data <- rbind(provider.data,reps) #binding together the provider data and reps data
    }
  }
  
  colnames(provider.data) <- c("NPI","Name","NPI_Type","Primary_Practice_Address","Phone","Primary_Taxonomy","zipcode")
  
  zip_link<-read.csv("zcta_county_rel_10.txt") %>%
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    rename(zipcode = ZCTA5) %>%
    mutate(zipcode = as.character(zipcode))
  zip_link$zipcode = stri_pad_left(zip_link$zipcode, 5, "0")
  
  NPI_join<-inner_join(provider.data, zip_link, by="zipcode")
  census<-read.csv("co-est2017-alldata.csv")
  
  NPI_to_census<-inner_join(NPI_join, census, by=c("STATE", "COUNTY"))
  
  #getting input into same format as STNAME
  state_abbrev <- read.csv("state_abbrev.txt")
  state_abbrev$STNAME <- state_abbrev$State
  NPI_to_census_abbrev <- left_join(NPI_to_census,state_abbrev,by="STNAME")
  
  #5. Return the summary measure
  rows<-NPI_to_census_abbrev %>%
    filter(Abbreviation == state) %>%
    group_by(STNAME, CTYNAME, POPESTIMATE2010) %>%
    count() %>%
    arrange(n)
  return(rows)
  
}

provider.data<-ProvidersInStateByCounty("RI", "Mental Health")


