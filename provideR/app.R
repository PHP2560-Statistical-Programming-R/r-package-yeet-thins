#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
    for (j in 1:length(skips)) { #also iterating over skils
      zip <- zipcode[i] #storing zipcodes in zip
      skip <- skips[j] 
      tax <- str_replace_all(taxonomy," ","+")
      url<-paste0(url1,zip,"&skip=",skip,"&taxonomy_description=",tax) #pasting the url, with the rhode island zip code and including the skips
      #text scrape to pull our places by zip code
      h <- read_html(url, timeout = 10000000) #reading the url as html and storing it in h with timeout set
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
  return(provider.data) #returning a dataset that shows all of the provider
}

countbyzip <- function(data){data %>% #creating a count of practices by zip code
    group_by(zipcode) %>% #group by zip code
    count() %>% #creating a count of the groups within the zipcode
    arrange(n)%>% #arranging by n
    ungroup()
}

ZipsFromState<-function(state_name){ #creating a function that will take the Zips from the state
  data(zipcode) #reading data from zipcode
  zip_holder<-zipcode%>% #assigning zipcode to a zip_holder
    filter(state==state_name)%>% #filtering zipcodes by state name
    select(zip) #selecting a zipcode
  zip_state<-zip_holder[,1] #returning rows from the zip_holder
  return(zip_state)
}

#Data Functions
GetDataFromState<-function(state, taxonomy) { #creating a function that inputs state and taxonomy and outputs the NPI code 
  zipcode_holder<-ZipsFromState(state)
  data<-NPIcode_taxonomy(zipcode_holder, taxonomy)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ProvidR: Visualizing Providers of a Given Type in a State"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("ProvidR is an app created by Alexander Adia, Jackie Goldman, Kari Kusler, and Meghan Peterson. To use it, please specify a state and taxonomy and click Get Data. After a few minutes, ProvidR will tell you that it is ready to produce visualizations for you."),
      textInput("state", label = h3("State Abbreviation"), value = "RI"),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      
      textInput("taxonomy", label = h3("Taxonomy"), value = "Mental Health"),
      
      actionButton("data", "Get Data"),
      #graphing options
      radioButtons("graph", label = h3("Graphing Option"),
                   choices = list("Zip codes with high provider coverage" = 1, "Zip codes with low provider coverage" = 2, "Top 5 zip codes with high provider coverage" = 3, "Top 5 zip codes with low provider coverage"= 4, "Most common taxonomies in state" = 5, "Least common taxonomies in state"=6),
                   selected = 1),
      #action button
      actionButton("do", "Get Visualization")
    ),
    
    # Show a plot 
    mainPanel(h3("Results"),
              textOutput("text"),
              plotOutput("plot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  observeEvent(input$data, {
    dataframe<<-GetDataFromState(input$state, input$taxonomy)
    if(!is.null(dataframe)){
      output$text<-renderText("ProvidR is ready for visualizations!")
    }
  }
               )
  
  observeEvent(input$do, {

    if(input$graph==1){
      counts<-countbyzip(dataframe)
      number_practices<-counts%>%select(n) %>% #selecting and grouping by frequency
        group_by(n) %>%
        count() %>%
        arrange(desc(n)) #arranging by frequency
      number_practices.high <- head(number_practices) #getting the first rows of the number_pratices and desingnating them as low numbers of practices
      plot1<-ggplot(number_practices.high, aes(x=n, y=nn))+
        geom_bar(stat="identity", fill = "blue")+
        theme_minimal()+
        labs(x = "Number of providers", y = "Number of zip codes", title="Zip codes with high numbers of providers")
      
      
      output$plot <- renderPlot({print(plot1)})
    } else if(input$graph==2){
      counts<-countbyzip(dataframe)
      number_practices<-counts%>%select(n) %>% #selecting and grouping by frequency
        group_by(n) %>%
        count() %>%
        arrange(n) #arranging by frequency
      number_practices.low <- head(number_practices) #getting the first rows of the number_pratices and desingnating them as low numbers of practices
      
      plot2<-ggplot(number_practices.low, aes(x=n, y=nn))+
        geom_bar(stat="identity", fill="blue", position=position_dodge()) + labs(x = "Number of providers", y = "Number of zip codes", title="Zip codes with low numbers of providers")+
        theme_minimal()
      output$plot <- renderPlot({print(plot2)})
    } else if(input$graph==3){
      counts_holder<-dataframe%>%
        group_by(zipcode) %>% 
        count() %>%
        arrange(desc(n)) 
      counts<-head(counts_holder)
      plot3<-ggplot(counts, aes(x=zipcode, y=n))+
        geom_bar(stat="identity", fill = "blue") + labs(x = "Zipcode", y = "Number of providers", title= "Top five zip codes by provider number")+
        theme_minimal()+ 
        coord_flip()
      output$plot <- renderPlot({print(plot3)})
    } else if(input$graph==4){
      counts_holder<-dataframe%>%
        group_by(zipcode) %>% 
        count() %>%
        arrange(n) 
      counts<-head(counts_holder)
      
      plot4<-ggplot(counts, aes(x=zipcode, y=n))+
        geom_bar(stat="identity", fill = "blue") + labs(x = "Zipcode", y = "Number of providers", title="Bottom five zip codes by provider number")+
        theme_minimal()+
        coord_flip()
      output$plot <- renderPlot({print(plot4)})
    } else if(input$graph==5){
      provider_grouping<-dataframe %>% #creating a count of practices by providers
        group_by(Primary_Taxonomy) %>% 
        count() %>%
        arrange(desc(n))
      top5providers<-head(provider_grouping)
      
      plot5<-ggplot(top5providers, aes(x=Primary_Taxonomy, y=n))+
        geom_bar(stat="identity", fill = "blue")+
        labs(x = "Provider Type", y = "Number of Providers", title="Top Provider Type in the Taxonomy")+
        theme_minimal() + 
        coord_flip()
      output$plot <- renderPlot({print(plot5)})
    } else if(input$graph==6){
      provider_grouping<-dataframe %>% #creating a count of practices by providers
        group_by(Primary_Taxonomy) %>% 
        count() %>%
        arrange(n)
      bottom5providers<-head(provider_grouping)
      
      
      plot6<- ggplot(bottom5providers, aes(x=Primary_Taxonomy, y=n))+
        geom_bar(stat="identity", fill = "blue")+
        coord_flip()+
        theme_minimal()+
        labs(x = "Provider Type", y = "Number of Providers", title="Least Common Provider Type in the Taxonomy")
      output$plot <- renderPlot({print(plot6)})
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)