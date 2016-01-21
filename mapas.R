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
plot(communities_littoral[[1]], add=T, col="coral")
plot(communities_littoral[[2]], add=T, col="coral4")

#Map of lights over the Pacific littoral (rasters is a list of the rasters for the all the years)
littoral_lights2013 <- mask(rasters_pacifico[[35]], pacific_littoral_map)
png("litoral_comunidades_2013_municipios.jpeg", width = 8.5, height = 11, units = 'in', res = 1000)
plot(littoral_lights2013, col=grey.colors(100))
plot(communities_littoral[[1]], add=T,  border = "blue", lwd=1.5)
plot(communities_littoral[[2]], add=T,  border = "yellow", lwd=1.5)
plot(pacific_littoral_map, lwd=2, border= "red", lwd=1, add=T)
dev.off()

#Export to KML files
lapply(rasters_pacifico, KML)
