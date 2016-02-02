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

#Filter only communities in the pacific littoral
communities_littoral <- lapply(layers_reprojected, crop, pacific_littoral_map_dpto)

#Filter communities by departments in the pacific littoral (1: black, 2: indigenous)
communities_littoral_dpto <- list()
for(i in names(pacific_littoral_maps)){
  communities_littoral_dpto[[i]] <- lapply(layers_reprojected, crop, pacific_littoral_maps[[i]])
}

#Get black communities by year
communities_littoral[[1]]@data$year <- str_extract(communities_littoral[[1]]@data$RESOLUCION, "[1-2][0, 9][0, 1, 9][0-9]")
communities_littoral[[1]]@data$year <- as.factor(communities_littoral[[1]]@data$year)
levels(commun ities_littoral[[1]]@data$year)[levels(communities_littoral[[1]]@data$year)==2919] <- "2012"

#Join black communities territories
black_communities_union <- gUnaryUnion(communities_littoral[[1]])
black_communities_union_l <- as(black_communities_union, "SpatialLines")
black_communities_union_p <- as(black_communities_union_l, "SpatialPoints")
black_communities_r <- rasterize(black_communities_union, stack_pacifico_mask[[1]])
black_communities_rl <- rasterize(black_communities_union_l, distance_raster)
                                                                                

