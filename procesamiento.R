library(raster)
library(rgdal)
library(sp)
library(ggplot)
library(lattice)

#Leer archivos raster#
setwd("~/Documents/F101992.v4 (2)")
r1992 <- readGDAL("F101992.v4b_web.stable_lights.avg_vis.tif")
r1992_r <- raster("F101992.v4b_web.stable_lights.avg_vis.tif")

#Regiones de Colombia
regiones1 <- getData("GADM", country="CO", level=1)
regiones2 <- getData("GADM", country="CO", level=2)
plot(regiones1, axes=T, asp=1)

#Ejemplos por ciudad (sólo para experimentar)
antioquia <- regiones1[regiones1@data$ID_1 == 2, ]
plot(antioquia, axes=T, asp=1)
san_andres <- regiones1[regiones1@data$ID_1 == 26 ,]
plot(san_andres, axes=T, asp=1)


my_ext <- extent(-76, -75, 10.2, 10.6)
r1992_ext <- crop(r1992_r, my_ext)
plot(r1992_ext)
plot(regiones1, add=T)
plot(regiones2, add=T)

