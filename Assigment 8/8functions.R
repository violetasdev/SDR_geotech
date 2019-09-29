ndvi<- function(starobj,band_dim=3){
  #Create new class for our function
  ndviClass <- setClass("ndvi", slots=c("ndvi","stars"))

  #Calculate NDVI
  per_band=split(starobj, band_dim)
  ndvi_calculated <- (per_band[4,] - per_band[3,]) / (per_band[4,] + per_band[3,])

  ndviObject<-ndviClass(ndvi=ndvi_calculated,stars=starobj)

  return(ndviObject)
}


plotndvi<-function(ndviObjectSel,colorsGrey=(grey((1:10)/10))){
  plot(ndviObjectSel@ndvi, col=colorsGrey, breaks=c(seq(-1,1,length.out = 11)),main="NDVI Plot")
}
