#'TopFiveZipcodes
#'This function, given a data frame created with GetDataFromState, creates a vizualtation of the five zipcodes with the most providers
#'@param the data frame create by GetDataFromState
#'@return a graph of the five zipcodes with the most providers
#'@examples
#'Data<-GetDataFromState("FL", "addiction")
#'TopFiveZipcodes(Data)

TopFiveZipcodes<-function(data){
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  counts_holder<-data%>%
    group_by(zipcode) %>% 
    count() %>%
    arrange(desc(n)) 
  counts<-head(counts_holder)
  plot<-ggplot(counts, aes(x=zipcode, y=n))+
    geom_bar(stat="identity", fill = "blue") + labs(x = "Zipcode", y = "Number of providers")+
    theme_minimal()+ 
    coord_flip()
  print(plot + ggtitle("Top five zip codes by provider number"))
  return(plot)
}