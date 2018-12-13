#' DensityMapByCounty
#' Maps providers per 1000 people by county

DensityMapByCounty<-function(state,taxonomy) {
  dat.sum <- SummaryByCounty(state,taxonomy)

  #install/load required packages
  if(!require(raster)){
    install.packages("raster")
    library(raster)
  }

  if(!require(ggmap)) {
    install.packages("ggmap")
    library(ggmap)
  }

  state.name <- dat.sum$STNAME

  us.shape<-getData('GADM', country='USA', level=2)  #Get the County (level 2) Shapefile for the US
  state.shape<-subset(us.shape,NAME_1==state.name[1])
  dat.sum$NAME_2 <- sub(pattern=" County", replacement="", dat.sum$CTYNAME)
  state.map <- merge(state.shape, df.sum, by.x='NAME_2', by.y='NAME_2')
  #unloading raster because it masks select
  detach("package:raster", unload=TRUE)
  #making colors (white to red)
  p <- colorRampPalette(c("white", "red"))(128)
  palette(p)
  prov <- state.map$provider_density
  cols <- (prov - min(prov))/diff(range(prov))*127+1
  #the final product
  return(plot(state.map, col=cols))
}
