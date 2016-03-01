library(dismo)
library(rasterVis)
library(animation)
library(GISTools)

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

#Nighlights raster animation
saveGIF({
  ani.options(interval = 1, nmax = 35)
  for(i in c(1:35)){
    plot(stack_pacifico_mask[[i]], col=grey.colors(100))
    plot(pacific_littoral_map_dpto, add = T, main = "Luminosidad por año")
  }
}, movie.name = "light.gif", ani.width = 800, ani.height = 1000)

saveLatex({
  ani.options(interval = 1, nmax = 35)
  for(i in c(1:35)){
    plot(stack_pacifico_mask[[i]], col=grey.colors(100))
    plot(pacific_littoral_map_dpto, add = T, main = "Luminosidad por año")
    map.scale()
  }
}, nmax = 35, interval = 0.5, img.name = "dm_plot",
latex.filename = ifelse(interactive(), "dm_plot_year.tex"))


#Map of lights over the Pacific littoral (rasters is a list of the rasters for the all the years)
littoral_lights2013 <- mask(rasters_pacifico[[35]], pacific_littoral_map)
png("litoral_comunidades_2013_municipios.jpeg", width = 8.5, height = 11, units = 'in', res = 1000)
plot(littoral_lights2013, col=grey.colors(100))
plot(layers_reprojected[[1]], add=T,  boder="blue", lwd=)
plot(pacific_littoral_map, lwd=2, border= "red", lwd=1, add=T)
dev.off()


#Time line economic activity by lights (treatment vs. no-treatment)

rasters_year <- group_by(merge_rasters_dataframes_long, year, treatment)
rasters_year <- summarise(rasters_year, 
                              total_dm=sum(dm),
                              mean_dm = mean(dm)
                          
)
g1 <- ggplot(rasters_year, aes(x=year, y=mean_dm, colour=treatment)) + geom_line(size=0.8)
g1 <- g1 + scale_x_continuous(breaks=c(1992:2013))
g1 <- g1 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))
g1 <- g1 + labs(x = "Año", y = "Actividad económica (densidad luz)") 
g1 <- g1 + ggtitle("Serie actividad económica - tratamiento vs. no tratamiento")
g1 <- g1 + theme(plot.title=element_text(size=rel(1), lineheight=.9,
                                         face="plain", color="black"))
g1

