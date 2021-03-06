#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#loading libraries
library(shiny)
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

#setting up helper functions
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
      h <- read_html(url, timeout = 10000000)
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
  return(provider.data)
}

countbyzip <- function(data){data %>% #creating a count of practices by zip code
    group_by(zipcode) %>%
    count() %>%
    arrange(n)
}

ZipsFromState<-function(state_name){
  data(zipcode)
  zip_holder<-zipcode%>%
    filter(state==state_name)%>%
    select(zip)
  zip_state<-zip_holder[,1]
  return(zip_state)
}

#Data Functions
GetDataFromState<-function(state, taxonomy) {
  zipcode_holder<-ZipsFromState(state)
  data<-NPIcode_taxonomy(zipcode_holder, taxonomy)
}


#Processing Functions
LowProviderNumberZip<-function(data){
  NPIcode_taxonomy()
  counts<-countbyzip(data)
  number_practices<-counts%>%select(n) %>% #selecting and grouping by frequency
    group_by(n) %>%
    count() %>%
    arrange(n) #arranging by frequency
  number_practices.low <- head(number_practices) #getting the first rows of the number_pratices and designating them as low numbers of practices
  plot1<-ggplot(number_practices.low, aes(x=n, y=nn))+
    geom_bar(stat="identity", fill="blue", position=position_dodge()) + labs(x = "Number of providers", y = "Number of zip codes")+
    theme_minimal() + ggtitle("Zip codes with low numbers of providers")
  plot(plot1)
}

HighProviderNumberZip<-function(data){
  counts<-countbyzip(data)
  number_practices<-counts%>%select(n) %>% #selecting and grouping by frequency
    group_by(n) %>%
    count() %>%
    arrange(desc(n)) #arranging by frequency
  number_practices.high <- head(number_practices) #getting the first rows of the number_pratices and desingnating them as low numbers of practices
  plot1<-ggplot(number_practices.high, aes(x=n, y=nn))+
    geom_bar(stat="identity", fill = "blue")+
    theme_minimal()+
    labs(x = "Number of providers", y = "Number of zip codes") +
    ggtitle("Zip codes with high numbers of providers")
  plot(plot1)
}

BottomFiveZipcodes<-function(data){
  counts_holder<-data%>%
    group_by(zipcode) %>%
    count() %>%
    arrange(n)
  counts<-head(counts_holder)
  plot1<-ggplot(counts, aes(x=zipcode, y=n))+
    geom_bar(stat="identity", fill = "blue") + labs(x = "Zipcode", y = "Number of providers")+
    theme_minimal()+
    coord_flip() +
    ggtitle("Bottom five zip codes by provider number")
  plot(plot1)
}

TopFiveZipcodes<-function(data){
  counts_holder<-data%>%
    group_by(zipcode) %>%
    count() %>%
    arrange(desc(n))
  counts<-head(counts_holder)
  plot1<-ggplot(counts, aes(x=zipcode, y=n))+
    geom_bar(stat="identity", fill = "blue") + labs(x = "Zipcode", y = "Number of providers")+
    theme_minimal()+
    coord_flip() +
    ggtitle("Top five zip codes by provider number")
  plot(plot1)
}

TopTaxonomiesZip<-function(data){
  provider_grouping<-data %>% #creating a count of practices by providers
    group_by(Primary_Taxonomy) %>%
    count() %>%
    arrange(desc(n))
  top5providers<-head(provider_grouping)
  providers_bar<-ggplot(top5providers, aes(x=Primary_Taxonomy, y=n))+
    geom_bar(stat="identity", fill = "blue")+
    labs(x = "Provider Type", y = "Number of Providers")+
    theme_minimal() +
    coord_flip()
  plot(providers_bar)
}

BottomTaxonomiesZip<-function(data){
  provider_grouping<-data %>% #creating a count of practices by providers
    group_by(Primary_Taxonomy) %>%
    count() %>%
    arrange(n)
  bottom5providers<-head(provider_grouping)
  providers_bar<-ggplot(bottom5providers, aes(x=Primary_Taxonomy, y=n))+
    geom_bar(stat="identity", fill = "blue")+
    coord_flip()+
    theme_minimal()+
    labs(x = "Provider Type", y = "Number of Providers")
  plot(providers_bar)
}

ProviderInStateByCounty<-function(state,taxonomy){
  #install/load required packages
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }

  if(!require(rvest)){
    install.packages("rvest")
    library(rvest)
  }

  if(!require(XML)){
    install.packages("XML")
    library(XML)
  }

  if(!require(stringi)){
    install.packages("stringi")
    library(stringi)
  }

  if(!require(stringr)){
    install.packages("stringr")
    library(stringr)
  }

  #pull data
  zips_used <- ZipsFromState(state)
  load("ProvidR/Data/zcta_county_rel_10.Rda")
  load("ProvidR/Data/co_est2017.Rda")
  zip_link = zcta_county_rel_10
  census = co_est2017_alldata
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
      h <- read_html(url, timeout = 500)
      reps <- h %>% #setting up the repeating structure
        html_node("table") %>%
        html_table()
      if (any(is.na(reps[,1])) | all(is.na(reps[,1]))) { #if the page has no physicians, move to next zip code
        break}
      reps$zip <- zips_used[i]
      provider.data <- rbind(provider.data,reps) #binding together the provider data and reps data
    }
  }

  colnames(provider.data) <- c("NPI","Name","NPI_Type","Primary_Practice_Address","Phone","Primary_Taxonomy","zipcode")

  provider.data$state.name <- noquote(str_extract(provider.data$Primary_Practice_Address,pattern="(?<=, )[A-Z]+(?=\\s)")) #state postal code (2 characters)

  provider.data <- filter(provider.data,state.name==state)

  zip_link<- zip_link %>%
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    rename(zipcode = ZCTA5) %>%
    mutate(zipcode = as.character(zipcode))
  zip_link$zipcode = stri_pad_left(zip_link$zipcode, 5, "0")

  NPI_join<-inner_join(provider.data, zip_link, by="zipcode")

  NPI_join$STATE<-as.character(NPI_join$STATE)

  NPI_to_census<-inner_join(NPI_join, census, by=c("STATE", "COUNTY"))
  NPI_to_census$state.name.long <- as.character(NPI_to_census$STNAME)

  load("ProvidR/Data/state_abbrev.Rda")

  state_abbrev$state.name.long <- as.character(state_abbrev$State)

  NPI_states <- left_join(NPI_to_census, state_abbrev, by=c("state.name.long"))

  return(NPI_states)

}

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

BottomFiveProviderDensities<-function(state,taxonomy){
  data <- reactive(SummaryByCounty(state,taxonomy))
  counts_holder<-data%>%
    group_by(CTYNAME) %>%
    count() %>%
    arrange(n)
  counts<-head(counts_holder)
  plot<-ggplot(counts, aes(x=CTYNAME, y=provider_density))+
    geom_bar(stat="identity", fill = "blue") + labs(x = "County", y = "Number of providers")+
    theme_minimal()+
    coord_flip()
  print(plot + ggtitle("Bottom five counties by provider number"))
  return(plot)
}


#SHINY APP#


#Defining the UI
ui <- fluidPage(

  # Application title
  titlePanel("ProvidR: Visualizing Providers of a Given Type in a County or State"),

  # Sidebar with a slider input for number of bins d
  sidebarLayout(
    sidebarPanel(
      helpText("Please input a state abbreviation, a taxonomy, and a graphing option. ProvidR will return a visualization."),
      textInput("state", label = h3("State Abbreviation"), value = "RI"),

      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),

      textInput("Taxonomy", label = h3("Taxonomy"), value = "Mental Health"),

      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),

      radioButtons("Graph", label = h3("Graphing Option"),
                   choices = list("Zip codes with highest provider coverage" = 1, "Zip codes with lowest provider coverage" = 2, "Number of zip codes with high provider coverage" = 3, "Number of zip codes with low provider coverage"= 4, "Most common taxonomies in state" = 5, "Least common taxonomies in state"=6, "Counties with lowest provider density"=7),
                   selected = 1),

      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      actionButton("do", "Get Visualization")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
    )
  )
)



#SERVER LOGIC
server <- function(input, output) {
  observeEvent(input$do, {
    stateinput<-reactive({as.string(input$state)})
    taxonomyinput<-reactive({as.string(input$Taxonomy)})
    serv_data<-GetDataFromState(stateinput(), taxonomyinput())


    output$Plot<-renderPlot({
      if(input$Graph==1){
        plotted<-TopFiveZipcodes(serv_data)
      } else if(input$Graph==2){
        plotted<-BottomFiveZipcodes(serv_data)
      } else if(input$Graph==3){
        plotted<-HighProviderNumberZip(serv_data)
      } else if(input$Graph==4){
        plotted<-LowProviderNumberZip(serv_data)
      } else if(input$Graph==5){
        plotted<-TopTaxonomiesZip(serv_data)
      } else if(input$Graph==6){
        plotted<-BottomTaxonomiesZip(serv_data)
      } else if(input$Graph==7){
        plotted<-BottomFiveProviderDensities(stateinput, taxonomyinput)
      }
      output$plot<-renderPlot({
        reactivePlot(plotted)
      })
    })
  }
  )
}

  # Run the application
  shinyApp(ui = ui, server = server)

