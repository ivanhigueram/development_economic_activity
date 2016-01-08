#Maps

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
litoral_lights92 <- mask(rasters_pacifico[[35]], pacific_littoral_map)
png("litoral_comunidades_2013.jpeg", width = 8.5, height = 11, units = 'in', res = 1000)
plot(litoral_lights92, col=grey.colors(100))
plot(pacific_littoral_map, add=T)
plot(communities_littoral[[1]], add=T, lwd=0.1)
dev.off()
