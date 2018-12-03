##libraries needed
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
##Base function to be used for everything else

NPIcode_taxonomy<-function(zipcode,taxonomy){
  url1<- "https://npiregistry.cms.hhs.gov/registry/search-results-table?addressType=ANY&postal_code=" #setting the url to scrape from
  provider.data <- data.frame() #initializing an empty data frame
  skips <- seq(0,9999999,100) #create skips
  for (i in 1:length(zipcode)) { #iterating over all RI zip codes
    for (j in 1:length(skips)){ #also iterating over skils
      zip <- zipcode[i]
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
      reps$zip <- zipcode[i]
      provider.data <- rbind(provider.data,reps) #binding together the provider data and reps data
    }
  }
  colnames(provider.data) <- c("NPI","Name","NPI_Type","Primary_Practice_Address","Phone","Primary_Taxonomy","zipcode")
  
  provider.data$street <- noquote( str_extract(provider.data$Primary_Practice_Address,pattern="^[a-zA-Z0-9_. -]+?(?=\n)")) #street name and number
  provider.data$city <- noquote( str_extract(provider.data$Primary_Practice_Address,pattern="(?<=\t)[a-zA-Z_. -]+?(?=, )"))#city
  provider.data$statename<- noquote( str_extract(provider.data$Primary_Practice_Address,pattern="(?<=, )[A-Z]+(?=\\s)")) #state postal code (2 characters)
  provider.data<-mutate(provider.data, zipcode= as.character(zipcode))
  
  zip_link<-read.csv("zcta_county_rel_10.txt") %>%
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    rename(zipcode = ZCTA5) %>%
    mutate(zipcode = as.character(zipcode))
  zip_link$zipcode = stri_pad_left(zip_link$zipcode, 5, "0")
  
  NPI_join<-full_join(provider.data, zip_link, by="zipcode")
  census<-read.csv("co-est2017-alldata.csv")
  
  NPI_to_census<-full_join(NPI_join, census, by=c("STATE", "COUNTY"))
  return(NPI_to_census)
}