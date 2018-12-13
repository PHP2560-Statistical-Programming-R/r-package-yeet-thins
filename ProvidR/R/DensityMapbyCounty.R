#' DensityMapByCounty
#' Maps providers per 1000 people by county

DensityMapByCounty<-function(dat,state) {
  #install/load required packages
  if(!require(raster)){
    install.packages("raster")
    library(raster)
  }
  if(!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(ggmap)) {
    install.packages("ggmap")
    library(ggmap)
  }
  df <- dat
  us.shape<-getData('GADM', country='USA', level=2)  #Get the County (level 2) Shapefile for the US
  state.shape<-subset(us.shape,NAME_1==state)
  df$NAME_2 <- sub(pattern=" County", replacement="", df$CTYNAME) 
  state.map <- merge(state.shape, df, by.x='NAME_2', by.y='NAME_2')
  #making colors (white to red)
  p <- colorRampPalette(c("white", "red"))(128)
  palette(p)
  prov <- state.map$provider_density
  cols <- (prov - min(prov))/diff(range(prov))*127+1
  #the final product
  plot(state.map, col=cols)
}







