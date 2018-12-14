#'BottomTaxonomiesZip
#'This function, given a data frame, returns the least frequent provider taxonomies in a zipcode
#'@param the data frame created with GetDataFromStates
#'@return A visualization of the 5 least frequent provider taxonomies in a zipcode
#'@examples
#'Data<-GetDataFromState("MI", "Primary Care")
#'BottomTaxonomiesZip(Data)

BottomTaxonomiesZip<-function(data){
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
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
  print(providers_bar + ggtitle("Taxonomies with fewest number of providers"))
}

