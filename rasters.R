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
#-------------------------------------------------Prepare data----------------------------------------------#

# 1. Rasters processing: crop, mask, make calculations and extract raster data

#Soil data from FAO
setwd("/Volumes/LaCie/Datos") 
url <- "http://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/"
files <- str_c("sq", c(1:7), ".asc")
filenames_list <- as.list(files)

l_ply(filenames_list, download,
      baseurl = "http://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/",
      folder = "soil_quality"
)

# 2. Elevation data from USGS
download.file(     
  url = "http://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Global_tiles_GMTED/300darcsec/mea/W090/10S090W_20101117_gmted_mea300.tif" ,
  destfile = "altura_mean_30arc.tif", mode="wb")
elevation <- raster("altura_mean_30arc.tif")




# 3. Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
setwd("~")
setwd("/Volumes/LaCie/NOAA2/TIFF/")

#Read rasters and group them into a stack (I used the crop function to cut the rasters to the same extent)
list_raster <- list.files()
rasters <- lapply(list_raster, raster)
rasters_extent <- extent(rasters[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters <- lapply(rasters, setExtent, rasters_extent)
rasters_pacifico <- lapply(rasters, crop, pacific_littoral_map_muni)
stack_pacifico <- stack(rasters_pacifico) #Stack them!
stack_pacifico_mask <- mask(stack_pacifico, pacific_littoral_map_dpto)

# 4. NASA Population data (no download available)
rasters_extent_pacifico <- extent(stack_pacifico)
setwd("/Volumes/LaCie/Datos/col_gpwv3_pdens_ascii_25")
list_population <- list.files()
population_rasters <- lapply(list_population, raster, crs = "+proj=longlat +datum=WGS84")
population_pacifico <- lapply(population_rasters, crop, pacific_littoral_map_muni)
population_pacifico <- lapply(population_pacifico, setExtent, rasters_extent_pacifico)
stack_population_pacifico <- stack(population_pacifico) 
stack_population_pacifico <- mask(stack_population_pacifico, pacific_littoral_map_muni)
stack_population_pacifico <- resample(stack_population_pacifico, stack_pacifico_mask)


# 5. The same for elevation raster (and calculate slope and aspect)
elevation_pacifico <- crop(elevation, rasters_extent_pacifico)
elevation_pacifico <- setExtent(elevation_pacifico, rasters_extent_pacifico)#The same for elevation raster
elevation_pacifico <- mask(elevation_pacifico, pacific_littoral_map_dpto)

# 6. Slope and aspects
slope_pacifico <- terrain(elevation_pacifico, opt = "slope")
aspect_pacifico <- terrain(elevation_pacifico, opt = "aspect")
hills_pacifico <- hillShade(slope_pacifico, aspect_pacifico, angle = 40, 0)
names(hills_pacifico) <- "hill"
roughness_pacifico <- terrain(elevation_pacifico, opt = "roughness")
flowdir_pacifico <- terrain(elevation_pacifico, opt = "flowdir")

#--------------------------------------------------Distances------------------------------------------------#

# 0. These are the saved raster distance files
distance_raster <- raster("distance_raster.grd")
distance_raster_p_mask <- raster("distance_raster_frontera.grd")

# 1. Create a distance raster (all distances to the nearest point)
distance_raster <- distanceFromPoints(stack_pacifico_mask[[1]], black_communities_union_p)
distance_raster_mask <- mask(distance_raster, pacific_littoral_map_dpto)
names(distance_raster_mask) <- "dist_p"

# 2. Identify cells within the polygon
cell_black_communities <- cellFromPolygon(distance_raster_mask, black_communities_union)


#------------------------------------------------Controls--------------------------------------------------#

# 1. Create a distance raster to the pacific ocean
x11()
plot(as(pacific_littoral_map_dpto, "SpatialLines"))
x <- crop(pacific_littoral_map_dpto, drawPoly())
x <- spTransform(x, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
x <- as(as(x, "SpatialLines"), "SpatialPoints")
distance_raster_coast <- distanceFromPoints(stack_pacifico_mask[[1]], x)
distance_raster_coast_mask <- mask(distance_raster_coast, pacific_littoral_map_dpto)
names(distance_raster_coast_mask) <- "dist_coast"


# 2. Distances to capitals (Cali, B/ventura, Quibdó, Popayan, Pasto) [To centroid]
capital_distance_raster_centroid <- distanceFromPoints(stack_pacifico_mask[[1]], capital_cities_centroids)
capital_distance_raster_centroid_mask <- mask(capital_distance_raster_centroid, pacific_littoral_map_dpto)
names(capital_distance_raster_centroid_mask) <- "dist_capital"

# 3. Identify cells from departments and municipalities (for fixed-effects in RDD)
pacific_littoral_map_muni@data$ID_ESPACIA <- as(pacific_littoral_map_muni@data$ID_ESPACIA, "numeric")
pacific_littoral_map_muni_r <- rasterize(pacific_littoral_map_muni, distance_raster_p_mask, 
                                         field = c(pacific_littoral_map_muni$ID_ESPACIA))

pacific_littoral_map_dpto@data$Group.1 <- as(pacific_littoral_map_dpto@data$Group.1, "factor")
pacific_littoral_map_dpto_r <- rasterize(pacific_littoral_map_dpto, distance_raster_p_mask, 
                                         field = c(pacific_littoral_map_dpto$Group.1))


# 4. Identify resolution years of communitary terrotories
black_communities_littoral <- communities_littoral[[1]]
black_communities_littoral@data$year <- as(black_communities_littoral@data$year, "factor")
black_communities_littoral_r <- rasterize(black_communities_littoral, distance_raster_p_mask, 
                                          field = c(black_communities_littoral@data$year))

# 5. Rename layers

names(pacific_littoral_map_muni_r) <- "municode"
names(pacific_littoral_map_dpto_r) <- "dptocode"
names(black_communities_littoral_r) <- "year_resolution"

#--------------------------------------------------------Extract------------------------------------------# 

# 1. Extract all data for each pixel (1*1 km  grid approximately)
raster_dataframes_list <- list(stack_pacifico_mask, elevation_pacifico, distance_raster_mask, capital_distance_raster_centroid_mask, pacific_littoral_map_muni_r, pacific_littoral_map_dpto_r, slope_pacifico, aspect_pacifico, hills_pacifico, roughness_pacifico, distance_raster_p_mask, stack_population_pacifico, black_communities_littoral_r)
dataframes_extract <- lapply(raster_dataframes_list, raster::extract, seq_len(ncell(stack_pacifico_mask)), df=TRUE)

# 2. Merge
merge_rasters_dataframes <- Reduce(function(...) merge(..., by="ID", all = T), dataframes_extract) 

# 3. Eliminate all NA cells (this are NA's from the mask process)
merge_rasters_dataframes_clean <- complete.cases(merge_rasters_dataframes[, -53])
merge_rasters_dataframes <- merge_rasters_dataframes[merge_rasters_dataframes_clean, ]

# 4. Get negative distances from cells inside the collective territories (community)
merge_rasters_dataframes$dist_p <- ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), 1, -1) * merge_rasters_dataframes$dist_p

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
merge_rasters_dataframes$treatment <- as.factor(ifelse(merge_rasters_dataframes$ID %in% unlist(cell_black_communities), 1, 0))


# 7. Reshape dataframe (wide to long)
merge_rasters_dataframes_long <- reshape(merge_rasters_dataframes,
                                         varying = names(merge_rasters_dataframes)[1:22],
                                         timevar = "year",
                                         idvar = "ID",
                                         direction = "long",
                                         sep = "")
attach(merge_rasters_dataframes_long)
merge_rasters_dataframes_long <- merge_rasters_dataframes_long[order(ID, year), ]
detach(merge_rasters_dataframes_long)


# 8. Export to Stata (pure benchmarking for estimations)
setwd("/Volumes/LaCie/Datos") 
require(foreign)
write.dta(merge_rasters_dataframes, "merge_rasters_dataframes.dta")
write.dta(merge_rasters_dataframes_long, "merge_rasters_dataframes_long.dta")
