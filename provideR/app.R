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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Identify Providers in your County or State"),
  
  # Sidebar with a slider input for number of bins d
  sidebarLayout(
    sidebarPanel(
      textInput("state", label = h3("State Abbreviation"), value = "RI"),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),  
      
      textInput("Taxonomy", label = h3("Taxonomy"), value = "Mental Health"),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))), 
      
      radioButtons("Graph", label = h3("Radio buttons"),
                  choices = list("Zip codes with highest provider coverage" = 1, "Zip codes with lowest provider coverage" = 2, "Number of zip codes with low provider coverage" = 3, "Number of zip codes with high provider coverage"= 4, "Least common taxonomies in state" = 5, "Most common taxonomies in state"=6), 
                  selected = 1),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

