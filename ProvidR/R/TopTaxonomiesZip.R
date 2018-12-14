#'TopTaxonomiesZip
#'Given a data set, this function returns the most common provider taxonomies in a zipcode
#'@param a data frame created with GetDataFromState
#'@return a visualization of the most common provider taxonomies
#'@examples
#'Data<-GetDataFromState("WI", "Nutrition")
#'TopTaxonomiesZip(Data)

TopTaxonomiesZip<-function(data){
  #loading required packages
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
    arrange(desc(n))
  top5providers<-head(provider_grouping)
  providers_bar<-ggplot(top5providers, aes(x=Primary_Taxonomy, y=n))+
    geom_bar(stat="identity", fill = "blue")+
    labs(x = "Provider Type", y = "Number of Providers")+
    theme_minimal() + 
    coord_flip()
  print(providers_bar + ggtitle("Number providers by taxonomy"))
}    
} 