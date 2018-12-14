#' LowProviderNumberZip
#' This function outputs a visualization of the zip codes with the lowest number of providers in a state (with at least one provider minimum in the zip code)
#' @param The data frame created by the function GetDataFromState
#' @return a bar graph of the zip codes with the lowest number of providers in a state.

LowProviderNumberZip<-function(data){
  counts<-countbyzip(data)
  number_practices<-counts%>%select(n) %>% #selecting and grouping by frequency
    group_by(n) %>%
    count() %>%
    arrange(n) #arranging by frequency
  number_practices.low <- head(number_practices) #getting the first rows of the number_pratices and desingnating them as low numbers of practices
  plot<-ggplot(number_practices.low, aes(x=n, y=nn))+
    geom_bar(stat="identity", fill="blue", position=position_dodge()) + labs(x = "Number of providers", y = "Number of zip codes")+
    theme_minimal()
  print(plot + ggtitle("Zip codes with low numbers of providers"))
}