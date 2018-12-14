#'HighProviderNumberZip
#'This outputs a graph of the zip codes with the highest number of providers in a state
#'@param the data frame created with GetDataFromState
#'@return a bar graph the the zip codes with the highest number of providers in a taxonomy
#'@examples 
#'Data<-GetDataFromState("RI", "Mental Health")
#' HighProviderNumberZip(Data)

HighProviderNumberZip<-function(data){  
  #load required packages
  if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
   counts<-countbyzip(data)
  number_practices<-counts%>%select(n) %>% #selecting and grouping by frequency
    group_by(n) %>%
    count() %>%
    arrange(desc(n)) #arranging by frequency
  number_practices.high <- head(number_practices) #getting the first rows of the number_pratices and desingnating them as low numbers of practices
  plot<-ggplot(number_practices.high, aes(x=n, y=nn))+
    geom_bar(stat="identity", fill = "blue")+
    theme_minimal()+
    labs(x = "Number of providers", y = "Number of zip codes")
  print(plot + ggtitle("Zip codes with high numbers of providers"))
}