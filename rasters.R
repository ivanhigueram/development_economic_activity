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


#Rasters processing: crop, mask, make calculations and extract raster data

#Elevation data
setwd("/Volumes/LaCie/Datos") 
download.file(     
  url = "http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Global_tiles_GMTED/300darcsec/mea/W090/10S090W_20101117_gmted_mea300.tif" ,
  destfile = "altura_mean_30arc.tif", mode="wb")
elevation <- raster("altura_mean_30arc.tif")

#Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
setwd("~")
setwd("/Volumes/LaCie/NOAA2/TIFF/")

#Read rasters and group them into a stack (I used the crop function to cut the rasters to the same extent)
list_raster <- list.files()
rasters <- lapply(list_raster, raster)
rasters_extent <- extent(rasters[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters <- lapply(rasters, setExtent, rasters_extent)
rasters_pacifico <- lapply(rasters, crop, pacific_littoral_map_muni)
stack_pacifico <- stack(rasters_pacifico) #Stack them!

#Once cropped, you can mask the rasters to include all the pixels within the Pacific littoral (if the centroid of the pixel is outside the litroral, its value is set to NA)
stack_pacifico_mask <- mask(stack_pacifico, pacific_littoral_map_muni)

#Rasters cropped by department
list_stack_pacifico_dpto <- list()
for(i in names(pacific_littoral_maps)){
  list_stack_pacifico_dpto[[i]] <- lapply(rasters_pacifico, crop, pacific_littoral_maps[[i]])
}

list_stack_pacifico_dpto <- lapply(list_stack_pacifico_dpto, stack)

#Rasters masked by department (soory for the loops)
list_stack_pacifico_dpto_mask <- list()
for(i in names(pacific_littoral_maps)){
  if(i == i){
    list_stack_pacifico_dpto_mask[[i]] <- lapply(list_stack_pacifico_dpto[i], mask, pacific_littoral_maps[[i]])
  }
}

#The same for elevation raster
rasters_extent_pacifico <- extent(stack_pacifico)
elevation_pacifico <- crop(elevation, rasters_extent_pacifico)
elevation_pacifico <- setExtent(elevation_pacifico, rasters_extent_pacifico)#The same for elevation raster
elevation_pacifico <- mask(elevation_pacifico, pacific_littoral_map_dpto)

#----------------------------------Distances-------------------------------------#
#Create a distance raster (all distances to the nearest point)
distance_raster <- distanceFromPoints(stack_pacifico_mask[[1]], black_communities_union_p)
distance_raster_mask <- mask(distance_raster, pacific_littoral_map_dpto)

# or (...)
distance_raster2 <- distance(black_communities_rl) #rather quicker than distance_raster and nearly equal results
distance_raster2_mask <- mask(distance_raster2, pacific_littoral_map_dpto)
distance_raster_p <- as(distance_raster2, "SpatialPixels")

#We have to borders here: one border goes up to the mountain range (east) and one border to the west. 

#Identify cells within the polygon
cell_black_communities <- cellFromPolygon(distance_raster, black_communities_union)
pixels_black_communities <- over(black_communities_union, distance_raster_p, returnList = T) 
pixels_black_communities <- unlist(pixels_black_communities)

#Select the "inside" distances
black_communities_distance_raster <- rasterFromCells(distance_raster, unlist(pixels_black_communities), values=TRUE)

#----------------------------------Extract------------------------------------------# 

#Extract elevation and light data for each pixel (1*1 km  grid approximately)
stack_pacifico_dataframe <- extract(stack_pacifico_mask, seq_len(ncell(stack_pacifico_mask)), df=TRUE)
elevation_dataframe <- extract(elevation_pacifico, seq_len(ncell(elevation_pacifico)), df=TRUE)
distance_dataframe <- extract(distance_raster_mask, seq_len(ncell(distance_raster_mask)), df=TRUE)
distance_dataframe2 <- extract(distance_raster2_mask, seq_len(ncell(distance_raster2_mask)), df=TRUE)
merge_rasters_dataframes <- merge(distance_dataframe, stack_pacifico_dataframe, by="ID")
merge_rasters_dataframes <- merge(merge_rasters_dataframes, elevation_dataframe, by="ID")
merge_rasters_dataframes <- merge(merge_rasters_dataframes, distance_dataframe2, by="ID")

#Eliminate all NA cells (remember we mask the raster previously)
merge_rasters_dataframes_clean <- complete.cases(merge_rasters_dataframes)
merge_rasters_dataframes <- merge_rasters_dataframes[merge_rasters_dataframes_clean, ]

#Get negative distances from cells inside the collective territories (community)
merge_rasters_dataframes$dist_p <- ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), -1, 1) * merge_rasters_dataframes$layer.x
merge_rasters_dataframes$dist_rl <- ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), -1, 1) * merge_rasters_dataframes$layer.y

#Average years with two rasters 

