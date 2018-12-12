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
  #plot(ri)
  #head(ri)
  test2$NAME_2 <- sub(pattern=" County", replacement="", test2$CTYNAME)
  #left_join(ri,test2,by=NAME_2) #doesn't work for spacial data

  head(ri)
  
  plot(ri)
  points(test2$provider_density,col="red",pch=16)
  
  
  ri2 <- merge(ri, test2, by.x='NAME_2', by.y='NAME_2')
  names(ri2)
  head(ri2)
  
}


