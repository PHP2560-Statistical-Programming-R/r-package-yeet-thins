#' DensityMapByCounty
#' Maps providers per 1000 people by county

DensityMapByCounty<-function(state,taxonomy) {
  df.sum <- SummaryByCounty(state,taxonomy)

  #install/load required packages
  if(!require(raster)){
    install.packages("raster")
    library(raster)
  }

  if(!require(ggmap)) {
    install.packages("ggmap")
    library(ggmap)
  }

  state.name <- df.sum$STNAME

  us.shape<-getData('GADM', country='USA', level=2)  #Get the County (level 2) Shapefile for the US
  state.shape<-subset(us.shape,NAME_1==state.name[1])
  df.sum$NAME_2 <- sub(pattern=" County", replacement="", df$CTYNAME)
  state.map <- merge(state.shape, df.sum, by.x='NAME_2', by.y='NAME_2')
  #making colors (white to red)
  p <- colorRampPalette(c("white", "red"))(128)
  palette(p)
  prov <- state.map$provider_density
  cols <- (prov - min(prov))/diff(range(prov))*127+1
  #the final product
  plot(state.map, col=cols)
}
