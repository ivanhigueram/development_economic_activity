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

#The same for elevation raster
rasters_extent_pacifico <- extent(stack_pacifico)
elevation_pacifico <- crop(elevation, rasters_extent_pacifico)
elevation_pacifico <- setExtent(elevation_pacifico, rasters_extent_pacifico)#The same for elevation raster
elevation_pacifico <- mask(elevation_pacifico, pacific_littoral_map_dpto)

#Extract elevation and light data for each pixel (1*1 km  grid approximately)
stack_pacifico_dataframe <- extract(stack_pacifico_mask, seq_len(ncell(stack_pacifico_mask)), df=TRUE)
elevation_dataframe <- extract(elevation_pacifico, seq_len(ncell(elevation_pacifico)), df=TRUE)
distance_dataframe <- extract(distance_raster, seq_len(ncell(distance_raster)), df=TRUE)
merge_rasters_dat daframes <- merge(distance_dataframe, stack_pacifico_dataframe, by="ID")

#Plot light/distance
merge_rasters_dataframes$layer[merge_rasters_dataframes$ID %in% unlist(cell_black_communities)] <- -1* (merge_rasters_dataframes$layer)
attach(merge_rasters_dataframes)
smoothScatter(layer, F101992.v4b_web.stable_lights.avg_vis, nbin=50, bandwidth = 0.02)

#Average years with two rasters 
year_list <- lapply(list_raster, str_sub, 4, 7)
duplicated_years <- year_list[duplicated(year_list)]
duplicated_rasters <- list_raster[lapply(list_raster, str_sub, 4, 7) %in% duplicated_years]


