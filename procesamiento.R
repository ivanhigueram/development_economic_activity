library(raster)
library(rgdal)
library(rasterVis)
library(sp)
library(ggplot2)
library(rgdal)
library(maptools)
library(lattice)
library(plyr)
library(grid)
library(dplyr)
library(stringr)

#Get administrative GIS data
setwd("/Volumes/LaCie/Datos")
colombia_departments <- getData("GADM", download=T, country="CO", level=1)
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015) ")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015) ") 

#Get political GIS data
landmines <- readOGR(dsn = "MAP&MUSE_GIS", layer = "eventos_shape")

#Project CRS to black and indigenous territories (1: blak territories, 2: indigenous territories)
reproject_layers <- list(black_territories, indigenous_territories)
layers_reprojected <- lapply(reproject_layers, spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Select only departments over the pacific littoral (Chocó, Valle del Cauca, Cauca and Nariño)
pacific_littoral <- c(11, 13, 21, 30)
pacific_littoral_map <- colombia_departments[colombia_departments@data$ID_1 %in% pacific_littoral, ]

#Filter only communities in the pacific littoral
communities_littoral <- lapply(layers_reprojected, crop, pacific_littoral_map)

#Elevation data
download.file(     
url = "http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Global_tiles_GMTED/300darcsec/mea/W090/10S090W_20101117_gmted_mea300.tif" ,
destfile = "altura_mean_30arc.tif", mode="wb")
elevation_pacifico <- raster("altura_mean_30arc.tif")
elevation_pacifico <- crop(elevation_pacifico, pacific_littoral_map)

#Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
setwd("~")
setwd("/Volumes/LaCie/NOAA2/TIFF/")

#Read rasters and group them into a stack (I used the crop function to cut the rasters to the same extent)
list_raster <- list.files()
rasters <- lapply(list_raster, raster)
rasters_pacifico <- lapply(rasters, crop, pacific_littoral_map)
rasters_extent <- extent(rasters_pacifico[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
elevation_pacifico <- setExtent(elevation_pacifico, rasters_extent) #The same for elevation raster
rasters_pacifico <- lapply(rasters_pacifico, setExtent, rasters_extent)
stack_pacifico <- stack(rasters_pacifico) #Stack them!

#Extract elevation and light data for each pixel (1*1 km  grid approximately)
stack_pacifico_dataframe <- extract(stack_pacifico, seq_len(ncell(stack_pacifico)), df=TRUE)
elevation_dataframe <- extract(elevation_pacifico, seq_len(ncell(elevation_pacifico)), df=TRUE)
merge_rasters_dataframes <- merge(elevation_dataframe, stack_pacifico_dataframe, by="ID")

#Join black communities territories (create a frontier)
communities_littoral[[1]]$ID <- 1
black_territories_union <- unionSpatialPolygons(communities_littoral[[1]], communities_littoral[[1]]$ID)

#Now I extract the light info to the spatial polygons of the communities
rasters_indigenas <- lapply(rasters_pacifico, raster::extract, communities_littoral[[2]], fun = mean, na.rm= TRUE, df = TRUE)
rasters_communities <- lapply(rasters_pacifico, raster::extract,communities_littoral[[1]], fun = mean, na.rm= TRUE, df = TRUE)

#Set the results as data.frame
luces_promedio_indigenas <- data.frame(rasters_indigenas)
luces_promedio_negritudes <- data.frame(rasters_communities)


