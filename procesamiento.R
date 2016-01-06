library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(lattice)

#Get administrative GIS data
setwd("/Volumes/LaCie/NOAA2/TIFF/Datos")
colombia_departments <- getData("GADM", download=T, country="CO", level=1)
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015) ")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015) ") 

#Get political GIS data
landmines <- readOGR(dsn = "MAP&MUSE_GIS", layer = "eventos_shape")

#Project CRS to black and indigenous territories
reproject_layers <- list(black_territories, indigenous_territories)
layers_reprojected <- lapply(reproject_layers, spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
black_territories_reproject <- layers_reprojected[[1]]
indigenous_territories_reproject <- layers_reprojected[[2]]

#Select only departments over the pacific littoral (Chocó, Valle del Cauca, Cauca and Nariño)
pacific_littoral <- c(11, 13, 21, 30)
pacific_littoral_map <- colombia_departments[colombia_departments@data$ID_1 %in% pacific_littoral, ]

#Filter only communities in the pacific lottoral
communities_littoral <- sapply(layers_reprojected, crop, pacific_littoral_map)

#Map of communities in the Pacific littoral
opar <- par()
par(pin=c(1,2))
par(xaxs = "i", yaxs = "i")
plot(pacific_littoral_map,
     xlim=c(-80.5, -75.0),
     ylim=c(0, 9.0),
     axes=T)
plot(communities_littoral[[1]], add=T, col="coral")
plot(communities_littoral[[2]], add=T, col="coral4")

colombia <- get_map(location = c(-79.5, 1.0, -75.0, 6.5), zoom = 6)
ggmap(colombia)

plot(colombia)

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

