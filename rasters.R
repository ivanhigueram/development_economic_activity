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
stack_pacifico_mask <- mask(stack_pacifico, pacific_littoral_map_dpto)

#Rasters cropped by department
list_stack_pacifico_dpto <- list()
for(i in names(pacific_littoral_maps)){
  list_stack_pacifico_dpto[[i]] <- lapply(rasters_pacifico, crop, pacific_littoral_maps[[i]])
}

list_stack_pacifico_dpto <- lapply(list_stack_pacifico_dpto, stack)

#Rasters masked by department (sorry for the loops)
list_stack_pacifico_dpto_mask <- list()
for(i in names(pacific_littoral_maps)){
  if(i == i){
    list_stack_pacifico_dpto_mask[[i]] <- lapply(list_stack_pacifico_dpto[i], mask, pacific_littoral_maps[[i]])
  }
}
rm(i)

#The same for elevation raster (and calculate slope and aspect)
rasters_extent_pacifico <- extent(stack_pacifico)
elevation_pacifico <- crop(elevation, rasters_extent_pacifico)
elevation_pacifico <- setExtent(elevation_pacifico, rasters_extent_pacifico)#The same for elevation raster
elevation_pacifico <- mask(elevation_pacifico, pacific_littoral_map_dpto)

#Slope and aspects
slope_pacifico <- terrain(elevation_pacifico, opt = "slope")
aspect_pacifico <- terrain(elevation_pacifico, opt = "aspect")
hills_pacifico <- hillShade(slope_pacifico, aspect_pacifico, angle = 40, 0)


#----------------------------------Distances-------------------------------------#
#Create a distance raster (all distances to the nearest point)
distance_raster <- distanceFromPoints(stack_pacifico_mask[[1]], black_communities_union_p)
distance_raster_mask <- mask(distance_raster, pacific_littoral_map_dpto)
names(distance_raster_mask) <- "dist_p"

# or select -create- a frontier (x11 works for OS Mac only, dev.on() will do for OS WIN)
x11()
plot(black_communities_union_l)
plot(pacific_littoral_map_dpto, border = "blue", add = T)
x <- crop(black_communities_union_l, drawPoly())
x <- spTransform(x, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
x <- as(x, "SpatialPoints")
distance_raster_p <- distanceFromPoints(stack_pacifico_mask[[1]], x)
distance_raster_p_mask <- mask(distance_raster_p, pacific_littoral_map_dpto)
names(distance_raster_p_mask) <- "dist_f"

#Identify cells within the polygon
cell_black_communities <- cellFromPolygon(distance_raster, black_communities_union)
pixels_black_communities <- over(black_communities_union, distance_raster_p, returnList = T) 
pixels_black_communities <- unlist(pixels_black_communities)

#Distances to capitals (Cali, B/ventura, Quibdó, Popayan, Pasto)
capital_cities_maps <- lapply(capital_cities_maps, as, "SpatialLines")
capital_cities_maps <- lapply(capital_cities_maps, as, "SpatialPoints")

#To border
capital_distance_raster <- list()
for(i in capital_cities){
  capital_distance_raster[[i]] <- distanceFromPoints(stack_pacifico_mask[[1]], capital_cities_maps[[i]])
}
capital_distance_raster_stack <- stack(capital_distance_raster)
capital_distance_raster_stack <- mask(capital_distance_raster_stack, pacific_littoral_map_dpto)

#To centroid
capital_distance_raster_centroid <- distanceFromPoints(stack_pacifico_mask[[1]], capital_cities_centroids)
capital_distance_raster_centroid_mask <- mask(capital_distance_raster_centroid, pacific_littoral_map_dpto)
names(capital_distance_raster_centroid_mask) <- "dist_capital"

#Identify cells from departments and municipalities (for fixed-effects in RDD) - using function from Amy Whitehead
pacific_littoral_map_muni@data$ID_ESPACIA <- as(pacific_littoral_map_muni@data$ID_ESPACIA, "numeric")
pacific_littoral_map_muni_r <- rasterize(pacific_littoral_map_muni, distance_raster_mask, 
                                         field = c(pacific_littoral_map_muni$ID_ESPACIA))

#----------------------------------Extract------------------------------------------# 

#Extract elevation and light data for each pixel (1*1 km  grid approximately)

raster_dataframes_list <- list(stack_pacifico_mask, elevation_pacifico, distance_raster_mask, capital_distance_raster_centroid_mask, pacific_littoral_map_muni_r, slope_pacifico, distance_raster_p_mask)
dataframes_extract <- lapply(raster_dataframes_list, raster::extract, seq_len(ncell(stack_pacifico_mask)), df=TRUE)

#Merge
merge_rasters_dataframes <- Reduce(function(...) merge(..., by="ID"), dataframes_extract) 

#Eliminate all NA cells (remember we mask the raster previously)
merge_rasters_dataframes_clean <- complete.cases(merge_rasters_dataframes)
merge_rasters_dataframes <- merge_rasters_dataframes[merge_rasters_dataframes_clean, ]

#Get negative distances from cells inside the collective territories (community)
merge_rasters_dataframes$dist_p <- ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), -1, 1) * merge_rasters_dataframes$dist_p
merge_rasters_dataframes$dist_f <- ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), -1, 1) * merge_rasters_dataframes$dist_f

#Average years with two rasters 
colnames(merge_rasters_dataframes)[which(names(merge_rasters_dataframes) == "layer.y")] <- "dist_capital"
colnames(merge_rasters_dataframes)[which(names(merge_rasters_dataframes) == "layer")] <- "municode"
colnames(merge_rasters_dataframes)[2:36] <- lapply(colnames(merge_rasters_dataframes)[2:36], str_sub, 4, 7)
merge_rasters_dataframes <- mutate(merge_rasters_dataframes,  )


#Export to Stata
require(foreign)
write.dta(merge_rasters_dataframes, "merge_rasters_dataframes.dta")

