DensityMapByCounty<-function(df,state) {
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
  us.shape<-getData('GADM', country='USA', level=2)  #Get the County (level 2) Shapefile for the US
  state.shape<-subset(us.shape,NAME_1==state)
  
  #test2 is county-level summary data from ProviderInStateByCounty
  #get rid of "County" as that is not in the geospacial data
  df$NAME_2 <- sub(pattern=" County", replacement="", df$CTYNAME) 
  state.map <- merge(state.shape, test2, by.x='NAME_2', by.y='NAME_2')
  #making colors (white to red)
  p <- colorRampPalette(c("white", "red"))(128)
  palette(p)
  prov <- state.map$provider_density
  cols <- (prov - min(prov))/diff(range(prov))*127+1
  #the final product
  plot(state.map, col=cols)
}







