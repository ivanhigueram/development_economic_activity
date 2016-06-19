library(raster)
library(rgdal)
library(rasterVis)
library(sp)
library(ggplot2)
library(maptools)
library(lattice)
library(plyr)
library(grid)
library(dplyr)
library(stringr)
library(gtools)
#-------------------------------------------------Prepare data----------------------------------------------#

# 1. Rasters processing: crop, mask, make calculations and extract raster data

processing_rasters <- function(layer.list, ext, shape){
  layer.list %>%
    lapply(setExtent, ext) %>%
    lapply(crop, shape) %>%
    stack() %>% 
    mask(shape)
}

# Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
setwd("~")
setwd("/Volumes/LaCie/NOAA2/TIFF/")

list_raster <- list.files() %>%
  lapply(raster)
rasters_extent <- extent(list_raster[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters_lights <- processing_rasters(list_raster, rasters_extent, pacific_littoral_map_dpto)

# 2. Soil data from FAO
#Download data
#setwd("/Volumes/LaCie/Datos") 
#url <- "http://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/"
#files <- str_c("sq", c(1:7), ".asc")
#filenames_list <- as.list(files)

#l_ply(filenames_list, download,
#      baseurl = "http://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/",
#      folder = "soil_quality"
#)

#Process data
setwd("/Volumes/LaCie/Datos/soil_quality")
list_raster <- list.files()
rasters <- lapply(list_raster, raster)
rasters_extent <- extent(rasters[[1]])
rasters_soil <- processing_rasters(rasters, rasters_extent, pacific_littoral_map_dpto) %>%
  raster::resample(rasters_lights, method = "ngb")

# 3. Elevation data from USGS

#Download data
#download.file(     
#  url = "http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Global_tiles_GMTED/300darcsec/mea/W090/10S090W_20101117_gmted_mea300.tif" ,
#  destfile = "altura_mean_30arc.tif", mode="wb")

#Process data
setwd("/Volumes/LaCie/Datos")
rasters_extent <- extent(rasters_lights)
elevation <- raster("altura_mean_30arc.tif") %>%
  crop(pacific_littoral_map_dpto) %>%
  setExtent(rasters_extent) %>%
  mask(pacific_littoral_map_dpto)


# 4. NASA Population data (no download available) - DEPRECATED

#setwd("/Volumes/LaCie/Datos/col_gpwv3_pdens_ascii_25")
#list_population <- list.files()
#population_rasters <- lapply(list_population, raster, crs = "+proj=longlat +datum=WGS84")
#population_pacifico <- lapply(population_rasters, crop, pacific_littoral_map_muni)
#population_pacifico <- lapply(population_pacifico, setExtent, rasters_extent_pacifico)
#stack_population_pacifico <- stack(population_pacifico) 
#stack_population_pacifico <- mask(stack_population_pacifico, pacific_littoral_map_muni)
#stack_population_pacifico <- raster::resample(stack_population_pacifico, stack_pacifico_mask)


# 6. Slope and aspects
slope_pacifico <- terrain(elevation, opt = "slope")
aspect_pacifico <- terrain(elevation, opt = "aspect")
hills_pacifico <- hillShade(slope_pacifico, aspect_pacifico, angle = 40, 0)
roughness_pacifico <- terrain(elevation, opt = "roughness")
flowdir_pacifico <- terrain(elevation, opt = "flowdir")
names(hills_pacifico) <- "hill"

#--------------------------------------------------Distances------------------------------------------------#

# 0. These are the saved raster distance files (it takes so much time to calculate so I have this copy)
setwd("/Volumes/LaCie/Datos")
distance_raster <- raster("distance_raster_ant.grd")

# 1. Create a distance raster (all distances to the nearest point)
#distance_raster <- distanceFromPoints(rasters_lights[[1]], black_communities_union_p) %>%
# mask(pacific_littoral_map_dpto)

names(distance_raster) <- "dist_p"

# 2. Identify cells within the polygon
cell_black_communities <- cellFromPolygon(distance_raster, black_communities_union)


#------------------------------------------------Controls--------------------------------------------------#

# 0. Saved distance to coastline
setwd("/Volumes/LaCie/Datos")
distance_raster_coast_mask <- raster("distance_raster_coast.grd")

# 1. Create a distance raster to the pacific ocean (manually create a coastline)
x11()
plot(as(pacific_littoral_map_dpto, "SpatialLines"))
x <- crop(pacific_littoral_map_dpto, drawPoly())
x <- spTransform(x, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
x <- as(as(x, "SpatialLines"), "SpatialPoints")
distance_raster_coast <- distanceFromPoints(rasters_lights[[1]], x) %>%
  mask(pacific_littoral_map_dpto)
names(distance_raster_coast) <- "dist_coast"

# 2. Distances to capitals (Cali, B/ventura, Quibdó, Popayan, Pasto) [To centroid]
raster_distance_capital <- distanceFromPoints(rasters_lights[[1]], capital_cities_centroids) %>%
  mask(pacific_littoral_map_dpto)
names(raster_distance_capital) <- "dist_capital"

# 3. Distance to big colonial cities (Cali, Popayán y Pasto)
raster_distance_colonial <- distanceFromPoints(rasters_lights[[1]], colonial_cities_centroids) %>%
  mask(pacific_littoral_map_dpto)
names(raster_distance_colonial) <- "dist_colonial"

# 3. Identify cells from departments and municipalities (for fixed-effects in RDD)
#Departments
pacific_littoral_map_dpto@data$Group.1 <- droplevels(pacific_littoral_map_dpto@data$Group.1)
pacific_littoral_map_dpto_r <- rasterize(pacific_littoral_map_dpto, rasters_lights[[1]], 
                                         field = c(pacific_littoral_map_dpto$Group.1))

#Municipalities
pacific_littoral_map_muni@data$ID_ESPACIA <- droplevels(pacific_littoral_map_muni@data$ID_ESPACIA)
pacific_littoral_map_muni_r <- rasterize(pacific_littoral_map_muni, rasters_lights[[1]], 
                                         field = c(pacific_littoral_map_muni$ID_ESPACIA))

# 4. Identify resolution years of communitary terrotories and ID for resolution
black_communities_littoral <- communities_littoral[[1]]
black_communities_littoral_r <- rasterize(black_communities_littoral, rasters_lights[[1]], 
                                          field = black_communities_littoral@data$year)

black_communities_littoral_r2 <- rasterize(black_communities_littoral, rasters_lights[[1]], 
                                          field = black_communities_littoral@data$OBJECTID_1)

# 5. Rename layers

names(pacific_littoral_map_muni_r) <- "municode" 
names(pacific_littoral_map_dpto_r) <- "dptocode"
names(black_communities_littoral_r) <- "year_resolution"
names(black_communities_littoral_r2) <- "community_id"

# 6. Get coordinates for each pixel

coordinates <- rasters_lights %>% 
  coordinates() %>% 
  data.frame()

coordinates$ID <- row.names(coordinates)

#--------------------------------------------------------Extract------------------------------------------# 

# 1. Extract all data for each pixel (1*1 km  grid approximately)
raster_dataframes_list <- list(rasters_lights, 
                               elevation, 
                               distance_raster, 
                               raster_distance_capital,
                               raster_distance_colonial,
                               distance_raster_coast,
                               pacific_littoral_map_muni_r, 
                               pacific_littoral_map_dpto_r, 
                               slope_pacifico, aspect_pacifico, hills_pacifico, roughness_pacifico,
                               rasters_soil,
                               black_communities_littoral_r,
                               black_communities_littoral_r2
                               )
dataframes_extract <- lapply(raster_dataframes_list, raster::extract, seq_len(ncell(rasters_lights)), df=TRUE)
dataframes_extract[[length(dataframes_extract) + 1 ]] <- coordinates

# 2. Merge
merge_rasters_dataframes <- Reduce(function(...) merge(..., by="ID", all = T), dataframes_extract) 

# 3. Get negative distances from cells inside the collective territories (community)

merge_rasters_dataframes$dist_p <- ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), 1, - 1) * merge_rasters_dataframes$dist_p

# 4. Eliminate all NA cells (this are NA's from the mask process)
merge_rasters_dataframes_clean <- complete.cases(merge_rasters_dataframes[, 2:54])
merge_rasters_dataframes <- merge_rasters_dataframes[merge_rasters_dataframes_clean, ]

# 5. Average years with two rasters
names(merge_rasters_dataframes)[2:36] <- lapply(names(merge_rasters_dataframes)[2:36], str_sub, 4, 7)
duplicated_years <- names(merge_rasters_dataframes)[duplicated(names(merge_rasters_dataframes))]
duplicated_years2 <- str_c(duplicated_years, "1", sep = ".")
names(merge_rasters_dataframes)[2:36] <- make.names(names(merge_rasters_dataframes)[2:36], unique = T)
names(merge_rasters_dataframes)[2:36] <- str_sub(names(merge_rasters_dataframes)[2:36], 2)
merge_rasters_dataframes <- merge_rasters_dataframes[, order(names(merge_rasters_dataframes), decreasing  = F)]

for(i in duplicated_years){ 
  merge_rasters_dataframes[, i] <- rowMeans(merge_rasters_dataframes[, which(names(merge_rasters_dataframes) == i) : which(names(merge_rasters_dataframes) == str_c(i, 1, sep = "."))])
} 
merge_rasters_dataframes <- merge_rasters_dataframes[, -which(names(merge_rasters_dataframes) %in% duplicated_years2)]
names(merge_rasters_dataframes)[1:22] <- str_c("dm", names(merge_rasters_dataframes)[1:22])

# 6. Modify variables
merge_rasters_dataframes$dptocode <- as.factor(merge_rasters_dataframes$dptocode)
merge_rasters_dataframes$municode <- as.factor(merge_rasters_dataframes$municode)
merge_rasters_dataframes$community_id <- as.factor(merge_rasters_dataframes$community_id)
merge_rasters_dataframes$year_resolution <- as.factor(merge_rasters_dataframes$year_resolution)
soils <- c(36:42)
merge_rasters_dataframes[, soils] <- lapply(merge_rasters_dataframes[, soils], factor)

# 6.1. Create different trearments (Regular treatment)
merge_rasters_dataframes$treatment <- as.factor(ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), 1, 0))

# 6.2. Resolution year treatment
merge_rasters_dataframes$year_resolution <-  mapvalues(merge_rasters_dataframes$year_resolution, from = levels(merge_rasters_dataframes$year_resolution), to = levels(communities_littoral[[1]]@data$year))

for(level in unique(merge_rasters_dataframes$year_resolution)){
  merge_rasters_dataframes[paste("t", level, sep = "")] <- ifelse(merge_rasters_dataframes$year_resolution == level, 1, 0)
}

na.zero <- function (x){
  x[is.na(x)] <- 0
  return(x)
}
merge_rasters_dataframes$tNA <- NULL
merge_rasters_dataframes[, 47:64] <- lapply(merge_rasters_dataframes[, 47:64], na.zero)
merge_rasters_dataframes <- merge_rasters_dataframes[, mixedsort(colnames(merge_rasters_dataframes))]

# 6.3. Cumulative treatment
merge_rasters_dataframes[, 43:60] <- matrixStats::rowCummaxs(as.matrix(merge_rasters_dataframes[, 43:60]))
merge_rasters_dataframes[, 43:60] <- lapply(merge_rasters_dataframes[, 43:60], factor)

# 7. Reshape dataframe (wide to long)
#To balance the panel, we need to cover the years that were without treatment
merge_rasters_dataframes_long <- merge_rasters_dataframes
merge_rasters_dataframes_long$t1992 <- 0
merge_rasters_dataframes_long$t1993 <- 0
merge_rasters_dataframes_long$t1994 <- 0
merge_rasters_dataframes_long$t1995 <- 0
merge_rasters_dataframes_long$t2009 <- merge_rasters_dataframes_long$t2008
merge_rasters_dataframes_long$t2013 <- merge_rasters_dataframes_long$t2012
merge_rasters_dataframes_long <- merge_rasters_dataframes_long[, mixedsort(colnames(merge_rasters_dataframes_long))]
merge_rasters_dataframes_long[, 49:52] <- lapply(merge_rasters_dataframes_long[, 49:52], factor)
merge_rasters_dataframes_long$t2014 <- NULL
merge_rasters_dataframes_long$t2015 <- NULL

merge_rasters_dataframes_long <- reshape(merge_rasters_dataframes_long,
                                         varying = c(names(merge_rasters_dataframes_long)[8:29],
                                                     names(merge_rasters_dataframes_long)[43:64]),
                                         timevar = "year",
                                         idvar = "ID",
                                         direction = "long",
                                         sep = "")
attach(merge_rasters_dataframes_long)
merge_rasters_dataframes_long <- merge_rasters_dataframes_long[order(ID, year), ]
detach(merge_rasters_dataframes_long)


# 8. Export to Stata (benchmarking for estimations)
setwd("/Volumes/LaCie/Datos") 
require(foreign)
write.dta(merge_rasters_dataframes, "merge_rasters_dataframes_nuevo.dta")
write.dta(merge_rasters_dataframes_long, "merge_rasters_dataframes_long_nuevo.dta")

merge_rasters_dataframes <- read.dta("merge_rasters_dataframes_nuevo.dta")
merge_rasters_dataframes_long <- read.dta("merge_rasters_dataframes_long_nuevo.dta")
