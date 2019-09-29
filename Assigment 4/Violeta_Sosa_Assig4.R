library(sf)
library(tidyverse)
library(units)
library(lwgeom)

##Exercise 1
#Look for a shapefile; download it; import it with `sf::st_read`, and plot it. What does it show?
shapeName <- st_read('depto/depto.shp')
shapeName
plot(shapeName)

##Exercise 2
#Try plotting one of the attribute variables using a different color scheme

shapeName%>%select(AREA) %>%plot(graticule = TRUE, axes = TRUE)
shapeName%>%select(AREA) %>%plot(graticule = TRUE, axes = TRUE, col = heat.colors(10))
shapeName%>%select(DPTO) %>%plot(graticule = TRUE, axes = TRUE, col = topo.colors(50))

##Exercise 3
#What is the class of the object plotted? Plot only the geometry using `st geometry`
class(shapeName)
plot(st_geometry(shapeName))

##Exercise 4
#Which layers does the GeoPackage `http://www.geopackage.org/data/sample1_2.gpkg` have?
#Import them both in R, using `sf`. Call the object with feature geometries `x1`.

st_layers("sample1_2.gpkg")

dataset <- ("sample1_2.gpkg")

counties<-read_sf(dataset, "counties")
countiestbl<-read_sf(dataset, "countiestbl")

x1<-counties
x1
x1%>%select(AREA) %>%plot(graticule = TRUE, axes = TRUE)

##Exercise 5
#Create an `sf` object with a point geometry at latitude `36.21` and longitude `-81.19`
(sfc = st_sfc(st_point(c(-81.19,36.21)), crs = 4269))

##Exercise 6
#Query the features of x1 at this point: plot the resulting geometry, and show the attributes associated with this geometry.

geometry=st_intersects(sfc,x1)
geometry

plot(st_geometry(x1[unlist(geometry),]),expandBB = c(2,2,2,2), col = 'green')
x1[geometry,]

plot(st_geometry(x1), add=TRUE)
plot(sfc, expandBB = c(10, 10, 10, 10), col = "red", lwd = 5, add=TRUE)
x1[unlist(geometry),]


##Exercise 7
#For the nc dataset that ships with package sf, compute the area of county Columbus

(file <- system.file("gpkg/nc.gpkg", package="sf"))
(file %>% read_sf()  -> nc)
nc%>%select(NAME)

nc%>%select(NAME)%>%filter(NAME=="Columbus") %>%plot(graticule = TRUE, axes = TRUE, col=1, ADD=TRUE)
columbus = nc %>% filter(NAME == "Columbus")
st_area(columbus)


plot(st_geometry(x1[unlist(geometry),]),expandBB = c(2,2,2,2), col =rgb(red = 255, green = 140, blue = 0, alpha = 0.5), main="Resulting Geometries")


##Exercise 8
#Compute also the same area after transforming the geometry to EPSG 2264, and compare the value with that obtained from the unprojected data; express the difference in a percentage.

nc_newgeo<-nc
(nc_newgeo %>% st_transform(2264) -> nc_newgeo.2264)
nc_newgeo.2264%>%select(NAME)%>%filter(NAME=="Columbus") %>%plot(graticule = TRUE, axes = TRUE, col=1, ADD=TRUE)
columbus_new = nc_newgeo.2264 %>% filter(NAME == "Columbus")
(area4267=st_geod_area(columbus))
(area2264=(st_area(columbus_new) %>% set_units(m^2)))

((area2264/area4267)*100)

##Exercise 9
#Create a plot with the nc states outlines that has all states that are partly within 50 km from Columbus filled with color green.


columbusarea = nc[nc$NAME == "Columbus", ]
(selection= st_is_within_distance(columbusarea,nc, dist = 50))
plot(st_geometry(nc))
plot(st_geometry(nc[unlist(selection),]), add = TRUE, col = 'green')

?st_is_within_distance()
##OTHER STUGFFFF
nc%>%
  select(NAME)%>%
  filter(NAME==features_wi)%>%
  plot(graticule = TRUE, axes = TRUE, col=1, ADD=TRUE)


nc%>%select(NAME)%>%filter(features_wi)%>%plot()
?select
areall=nc[nc$NAME==features_wi,]

plot(st_geometry(areall), expandBB = c(2, 2, 2, 2), col = "green", lwd = 3)
plot(nc[0], add = TRUE)
plot(nc[0])

ColumbusName = nc[nc$NAME == "Columbus",]
nc[ColumbusName, op = st_within]
plot(nc[ColumbusName, op = st_touches])


##Exercise 10
#Create a plot with the nc states outlines that has all states that are entirely within 100 km from Columbus filled with color red.
(columbusarea = nc[nc$NAME == "Columbus", ])
plot(st_geometry(nc))

#Columbus transform for buffer distance calculation 
nc_tr = st_transform(columbusarea, 3857)
plot(st_transform(st_buffer(nc_tr, 100000),4267),col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5), border = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)
otro=st_transform(st_buffer(nc_tr, 100000),4267)
contain_true=st_contains(otro,nc)
plot(st_geometry(nc[unlist(contain_true),]), add = TRUE, col = 'red')

(read_sf("http://www.geopackage.org/data/sample1_2.gpkg", options="GPKG"))
?unlist()
?st_contains
