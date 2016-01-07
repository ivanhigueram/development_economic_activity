library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(lattice)
library(plyr)
library(dplyr)
library(stringr)

#Get administrative GIS data
setwd("/Volumes/LaCie/Datos")
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


#Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
setwd("~")
setwd("/Volumes/LaCie/NOAA2/TIFF/")

#Read rasters and group them into a stack (I used the crop function to cut the rasters to the same extent and do the same with projectRaster)
list_raster <- list.files()
rasters <- lapply(list_raster, raster)
rasters <- lapply(rasters, crop, pacific_littoral_map)
extent_raster <- extent(rasters[[1]])
rasters <- lapply(rasters, projectRaster, rasters[[1]])
rasters_stack <- stack(rasters)






