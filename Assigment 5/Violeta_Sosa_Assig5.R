## Exercises for the meeting of May 17, 2019

#Hand in: zip file with .Rmd and .html file, with answers to the exercises of the chapter: https://keen-swartz-3146c4.netlify.com/featureattributes.html as well as those in the "further exercises" html for today.

##Exercise 1
#Add a variable to the `nc` dataset by `nc$State = "North Carolina"`. Which value should you attach to this variable for the attribute-geometry relationship (agr)?
  
library(sf)
library(tidyverse)

nc <- system.file("gpkg/nc.gpkg", package="sf") %>%
  read_sf() %>%
  st_transform(32119)

nc2%>%st_agr()

nc
nc$State = "North Carolina"
nc
nc1 <- nc %>% select(NAME,State) %>%
  st_set_agr(c(State= "constant"))

nc1 %>%
  st_agr()

#We should attach a constant agr because this value is valid for all the geometries

##Exercise 2

#Create a new sf object from the geometry obtained by `st_union(nc)`, and assign `"North Carolina"` to the variable `State`. Which `agr` can you now assign to this attribute variable?
(state_geom=st_sf(st_union(nc)))
plot(state_geom)
state_geom$State="North Carolina"

st2<-state_geom%>%select(State)%>%
  st_set_agr(c(State="identity"))

st2%>%
  st_agr()

st2

#This variable State is now a identity variable because it represents the whole geometry

##Exercise 3

#Use `st_area` to add a variable with name `area` to `nc`. Compare the `area` and`AREA` variables in the `nc` dataset. What are the units of `AREA`? Are the two linearly related? If there are discrepancies, what could be the cause?

nc$area=st_area(nc)
nc%>%select(NAME, AREA,area)


##Exercise 4

#Is the `area` variable intensive or extensive? Is its agr equal to `constant`, `identity` or `aggregate`?
nc%>%select(NAME, AREA,area)%>%
  st_agr()

#A more correct solution
plot(AREA~area,nc)

#remove the CSR to get a proper projection 
st_crs(nc)=NA
summary(nc$AREA-nc$area2)
#By the summary we can notice there are small differences, 
#thus AREA does not have units and it is wrong to consider it as an area!


  
##Exercise 5
#Find the name of the county that contains POINT(-78.34046 35.017)
#Create the point

#how to create th point another way
st_as_sfc("POINT(-78.34046 35.017)")
#how to check the intersection in another way
nc[look,]
#sf has an inbuilt argument to check the intersection for the pass argument




look<-st_sfc(st_point(c(-78.34046,35.017)),crs=4326)
plot(st_geometry(st_transform(nc,4326)))
plot(look,col="red",pch = 19, add=TRUE)
#Query the intersection
container=(st_intersects(look,st_transform(nc,4326)))
states<-nc[unlist(container),]
states$NAME
#Ploting the result
plot(st_geometry(states),col="orange",main="States that intersects POINT(-78.34046 35.017)")


##Exercise 6
#Find the names of all counties with boundaries that touch county `Sampson`.

sampson= nc[nc$NAME == "Sampson", ]
sampson_to=nc[unlist(st_touches(sampson,nc)),]
#List of names
sampson_to$NAME
plot(st_geometry(sampson_to),col="orange",main="Counties with boundaries that touch county `Sampson`.")
plot(st_geometry(sampson), col="red",add=TRUE)


##Exercise 7
#List the names of all counties that are less than 50 km away from county `Sampson`.

#OJO CON LAS UNIDADES

sampson_close=nc[unlist(st_is_within_distance(sampson,nc,dist=50000)),]
sampson_close$NAME


#For the following exercises, we will work with naturalearth:

library(rnaturalearth)
e = ne_countries(returnclass = "sf")["pop_est"]

##Exercise 8
#Plot the variable pop_est. Which projection is used?
e
plot(e)  

###Projection used is WGS84

##Exercise 9
#Plot the variable converted to +proj=ortho, see here. What happened to Antarctica?
e2<-st_transform(e,st_crs("+proj=ortho"))
plot(e2)

###It dissapeared from the view, the crs origin is 0

##Exercise 10
#Plot the same data with the ortho projection, adding `lat_0=-90`, `lat_0=90`, `lon_0=90`, `lon_0=-90` for four different views. Try to explain what happens.
e2<-st_transform(e,st_crs("+proj=ortho +lat_0=-90 +lat_0=90 +lon_0=90 +lon_0=-90"))
plot(e2)

#the projection now has as origin a view that able us to see the antartica as a central point

##Exercise 11
#Create a polygon of the northern hemisphere. Use that to get the intersection with e, and plot that with `lat_0=90`.
#Get North Hemisphere
hnorth<-st_transform(e,st_crs("+proj=ortho +lat_0=90"))
plot(hnorth)
#Create a unique geometry
hnorthclean<-st_union(st_geometry(e[unlist((!is.na(st_dimension(hnorth)))),]))
plot(hnorthclean)
#Plot the new geometry from pole north view
hnorthcleanview<-st_transform(hnorthclean,st_crs("+proj=ortho +lat_0=-90"))
plot(hnorthcleanview)

#Intersection with e
(result<-st_intersects(hnorthclean,e,sparse=FALSE))
geom_in<-e[unlist(result),]

result2<-st_transform(geom_in,st_crs("+proj=ortho +lat_0=90 +lat_0=-90 +lon_0=-90 +lon_0=90"))
#Intersection plotting
plot(result2)


##Exercise 12
#For the land mass between latitudes 45 and 55 and longitudes 0 and 10, estimate the total population as well as the average population.

landmass<-st_crop(e, c(xmin=0, xmax=10, ymin=45, ymax=55))
plot(landmass)
(total_pop=sum(landmass$pop_est))
(avg_pop=mean(landmass$pop_est))

p1 <- rbind(c(0,45), c(10,45), c(10,55), c(0,55), c(0,45))

(pol <-st_sfc(st_polygon(list(p1)),crs=4326))
plot(e,reset=FALSE)
plot(pol,col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5), border = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)

aoi=st_contains(pol,e)
?st_contains
geom_in2<-e[unlist(aoi),]
plot(geom_in2)
plot(pol,col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5), border = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)




library(rnaturalearth)
library(sf)
e = ne_countries(returnclass = "sf")["pop_est"]

plot(st_transform(e,st_crs("+proj=ortho +lat_0=-90")),col = terrain.colors(50,alpha=0.5),main="Natural Earth Dataset - South Pole view")
plot(st_transform(e,st_crs("+proj=ortho +lat_0=90")),col = terrain.colors(50,alpha=0.5),main="Natural Earth Dataset - North Pole view")
plot(st_transform(e,st_crs("+proj=ortho +long_0=90")),col = terrain.colors(50,alpha=0.5),main="Natural Earth Dataset - Side Pole view")
plot(st_transform(e,st_crs("+proj=ortho +long_0=-90")),col = terrain.colors(50,alpha=0.5),main="Natural Earth Dataset - Side Pole view")


