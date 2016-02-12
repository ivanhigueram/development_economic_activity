library(dismo)
library(rasterVis)

#Maps
setwd("~/Dropbox/BANREP/Pacifico/Primer_DTSER/Mapas_Graficos")

#Map of communities in the Pacific littoral
opar <- par()
par(pin=c(1,2))
par(xaxs = "i", yaxs = "i")
plot(pacific_littoral_map,
     xlim=c(-80.5, -75.0),
     ylim=c(0, 9.0),
     axes=T)
plot(communities_littoral[[1]], col="coral")
plot(pacific_littoral_map_dpto, add=T)

#Map of lights over the Pacific littoral (rasters is a list of the rasters for the all the years)
png("litoral_distancias.jpeg", width = 8.5, height = 11, units = 'in', res = 500)
plot(distance_raster_mask)
plot(black_communities_union, border="red", lwd=1, add=T)
dev.off()

#Plot elevation, aspect and hill 
plot(hills_pacifico, col = grey(0:100/100), legend = F, main = "Elevación litoral pacífico")
plot(elevation_pacifico, col = rainbow(25, alpha = 0.35), add = T)
plot(pacific_littoral_map_dpto, add = T)
plot(black_communities_union, add= T, border= "grey")

#Export to KML files
lapply(rasters_pacifico, KML)

#Plot distance raster
png("litoral_distancias.jpeg", width = 8.5, height = 11, units = 'in', res = 1000)
plot(distance_raster_mask, main="Distancias a los territorios comunitarios")
plot(black_communities_union, border="red", lwd=1, add=T)
plot(pacific_littoral_map_dpto, add=T)
dev.off()

