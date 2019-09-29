#1. write a function called ndvi that computes ndvi from a stars object with
#three dimensions (x, y, band), that takes as argument the band dimension
#number (with default value 3). This function shall return an object of
#class c("ndvi", "stars")

library(stars)
library (sp)


tif = system.file("tif/L7_ETMs.tif", package = "stars")
starobj = read_stars(tif)
band_dim=3

ndvi<- function(starobj,band_dim=3){
  #Create new class for our function 
  ndviClass <- setClass("ndvi", slots=c("ndvi","stars"))
  
  #Calculate NDVI
  per_band=split(starobj, band_dim)
  ndvi_calculated <- (per_band[4,] - per_band[3,]) / (per_band[4,] + per_band[3,])
  
  ndviObject<-ndviClass(ndvi=ndvi_calculated,stars=starobj)
  
  return(ndviObject)
}


#2. write a plot method for ndvi objects, which by default takes a color
#scheme of 10 grey values (grey((1:10)/10)) ranging from -1 to 1 (i.e.,
#overwrites col and breaks arguments of plot.stars), and then calls
#plot.stars with these values.

plotndvi<-function(ndviObjectSel,colorsGrey=(grey((1:10)/10))){
  plot(ndviObjectSel@ndvi, col=colorsGrey, breaks=c(seq(-1,1,length.out = 11)),main="NDVI Plot")
}

#3. write a package with the ndvi function and plot method that documents
#both, and contains an example to run it on the data of the cube exercise

arroz1=ndvi(starobj)
plotndvi(arroz1)


#4. check whether the package checks cleanly

#5. submit the source package (ending in .tar.gz) as your learnweb assignment, 
#also when there are errors/warnings/notes left.