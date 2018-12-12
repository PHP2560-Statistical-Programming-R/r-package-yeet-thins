#this still needs to be functionified but it works
#checking if packages are installed; installing them if not
maps<-function(state,taxonomy){
  #install/load required packages
  if(!require(raster)){
    install.packages("raster")
    library(raster)
  }
  
  if(!require(dplyr){
    install.packages("dplyr")
    library(dplyr)
  }
  
  if(!require(ggmap){
    install.packages("ggmap")
    library(ggmap)
  }
  
  us<-getData('GADM', country='USA', level=2)  #Get the County Shapefile for the US
  
  unique(us$NAME_1)
  
  ri<-subset(us,NAME_1=="Rhode Island")

  #test2 is county-level summary data from ProviderInStateByCounty
  #get rid of "County" as that is not in the geospacial data
  test2$NAME_2 <- sub(pattern=" County", replacement="", test2$CTYNAME) 
  ri2 <- merge(ri, test2, by.x='NAME_2', by.y='NAME_2')

  #making colors
  p <- colorRampPalette(c("white", "red"))(128)
  palette(p)
  prov <- ri2$provider_density
  cols <- (prov - min(prov))/diff(range(prov))*127+1
  #the final product
  plot(ri2, col=cols)
}


