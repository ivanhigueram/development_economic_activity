library(raster)
library(rgdal)
library(rasterVis)
library(sp)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(lattice)
library(plyr)
library(grid)
library(dplyr)
library(stringr)


#SpatialPolgon processing: Get administrative shape files and more geographical info. 

#Get administrative GIS data
setwd("/Volumes/LaCie/Datos")
colombia_municipios <- readOGR(dsn = "Geografia", layer="Municipios")
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015) ")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015) ") 

#Project CRS of black and indigenous territories (1: blak territories, 2: indigenous territories) and municipalities
reproject_layers <- list(black_territories, indigenous_territories)
layers_reprojected <- lapply(reproject_layers, spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
colombia_municipios <- spTransform(colombia_municipios, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Aggregate municipalities to get departments (states)
colombia_municipios_df <- data.frame(colombia_municipios)
colombia_departamentos <- unionSpatialPolygons(colombia_municipios, colombia_municipios$NOM_DEPART)
colombia_municipios_agg <- aggregate(colombia_municipios_df[,2], list(colombia_municipios$NOM_DEPART), sum)
row.names(colombia_municipios_agg) <- as.character(colombia_municipios_agg$Group.1)
colombia_departamentos <- SpatialPolygonsDataFrame(colombia_departamentos, colombia_municipios_agg) #Shape file for departments (made with the join of municipalities. "Group.1" is the depatment code)

#Select only departments and municipalities over the pacific littoral (Chocó, Valle del Cauca, Cauca and Nariño)
pacific_littoral <- c("CAUCA", "CHOCÓ", "VALLE DEL CAUCA", "NARIÑO")
pacific_littoral_map_muni <- colombia_municipios[colombia_municipios@data$NOM_DEPART %in% pacific_littoral, ]
pacific_littoral_map_dpto <- colombia_departamentos[colombia_departamentos@data$Group.1 %in% pacific_littoral, ]
pacific_littoral_maps <- split(pacific_littoral_map_dpto, factor(pacific_littoral_map_dpto@data$Group.1))

#Filter only communities in the pacific littoral (using the extent of the shapefiles drop some polygons)
communities_littoral <- lapply(layers_reprojected, crop, extent(-80, -75, 0.4, 9))

#Filter communities by departments in the pacific littoral (1: black, 2: indigenous)
communities_littoral_dpto <- list()
for(i in names(pacific_littoral_maps)){
  communities_littoral_dpto[[i]] <- lapply(layers_reprojected, crop, pacific_littoral_maps[[i]])
}

#Get black communities by year
communities_littoral[[1]]@data$year <- str_extract(communities_littoral[[1]]@data$RESOLUCION, "[1-2][0, 9][0, 1, 9][0-9]")
communities_littoral[[1]]@data$year <- as.factor(communities_littoral[[1]]@data$year)
levels(communities_littoral[[1]]@data$year)[levels(communities_littoral[[1]]@data$year)==2919] <- "2012"

#Filter capital cities (for distance calculations)
capital_cities <- c("CALI", "BUENAVENTURA", "QUIBDÓ", "POPAYÁN", "PASTO")
capital_cities_map <- pacific_littoral_map_muni[pacific_littoral_map_muni@data$NOM_MUNICI %in% capital_cities, ]
capital_cities_maps <- split(capital_cities_map, factor(capital_cities_map@data$NOM_MUNICI))
capital_cities_centroids <- gCentroid(capital_cities_map, byid=T)

#Join black communities territories (remember we have exported another spatial join)
black_communities_union <- gUnaryUnion(communities_littoral[[1]]) #R Union - deprecated option 

#Clear holes from polygon (thanks Roger Bivand)
BCp <- slot(black_communities_union, "polygons") 
holes <- lapply(BCp, function(x) sapply(slot(x, "Polygons"), slot, "hole")) 
res <- lapply(1:length(BCp), function(i) slot(BCp[[i]], "Polygons")[!holes[[i]]]) 
IDs <- row.names(black_communities_union) 
black_communities_union <- SpatialPolygons(lapply(1:length(res), function(i) Polygons(res[[i]], ID=IDs[i])), proj4string=CRS(proj4string(black_communities_union))) 

#Transform union to lines and points 
black_communities_union_l <- as(black_communities_union, "SpatialLines")
black_communities_union_p <- as(black_communities_union_l, "SpatialPoints")

