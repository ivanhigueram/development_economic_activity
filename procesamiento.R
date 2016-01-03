library(raster)
library(rgdal)
library(sp)
library(ggplot)
library(lattice)

#Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
r <- readGDAL("TIFF/F182012.v4c_web.stable_lights.avg_vis.tif")
colombia_regiones1 <- getData("GADM", country="CO", level=1)
colombia_regiones2 <- getData("GADM", country="CO", level=2)

plot(colombia_regiones2, axes=T)
colombia_ext <- extent(-79, -67, -4, 12.5)
r_colombia <- crop(r, colombia_ext)
plot(r_colombia, add=T)

plot(r_colombia, axes=T, col=terrain.colors(100))
plot(colombia_regiones1, add=T)


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

