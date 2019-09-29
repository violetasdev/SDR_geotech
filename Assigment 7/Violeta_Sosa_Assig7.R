#Two data sets are found here:
  
#http://pebesma.staff.ifgi.de/pm10/3_2a.txt
#https://github.com/r-spatial/stars/blob/master/inst/tif/L7_ETMs.tif
#Read the dataset 3_2a.txt in, and try to create the following time series plot:

## First plop time series
library(readr)
library(ggplot2)
datat=data.frame(read_csv("3_2a.txt"))

class(datat)

plot(type="l",main="Time Series Plotting", xlab="Time",ylab="pm10",strptime(datat$Time, format="%H:%M:%S"), datat$pm10)

ggplot(datat, aes(datat$Time,datat$pm10)) + geom_line() +labs(x = "Time",y="pm10", title="Time Series Plotting")


#Second plot
#For the following plot use the same dataset; you may want to use segments to color line 
#segments according to their pm10 value (where pm10 is linearly mapped into sf::sf.colors(10))

graphics.off()
(colores=rev(sf::sf.colors(10)))
(rangeCol=100*seq(0,max(datat$pm10),length.out=10))

plot(datat$Long,datat$Lat, col=colores,main="Segments Plot", xlab="Lon",ylab="Lat")

s <- seq(length(datat$pm10)-1)
s <- s[-length(s)]

segments(datat$Long[s], datat$Lat[s], datat$Long[s+2], datat$Lat[s+2],rep(colores, rangeCol), lwd = 5)

#Try to reproduce the following plot, where pixel values were mapped into grey((9:0)/10)
#for the first band of L7_ETMs.tif.

library(stars)
library(sf)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
colorsGrey=(grey((9:0)/10))
colorsGrey
plot(x[,,,1], col=colorsGrey, breaks=c(seq(47,255,length.out = 11)))


     