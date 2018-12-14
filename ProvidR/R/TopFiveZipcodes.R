#'TopFiveZipcodes
#'This function, given a data frame created with GetDataFromState, creates a vizualtation of the five zipcodes with the most providers
#'@param the data frame create by GetDataFromState
#'@return a graph of the five zipcodes with the most providers
#'@examples
#'Data<-GetDataFromState("FL", "Addiction")
#'TopFiveZipcodes(Data)

TopFiveZipcodes<-function(data){
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