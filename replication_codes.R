


#Table 1 - Descriptive statistics by group - Not working
stats_table <- lapply(names(merge_rasters_dataframes_long), function(x) {
  merge_rasters_dataframes_long_bw %>%
  group_by(as.factor(treatment)) %>%
  summarize(mean = mean(x, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE),
            max = max(x),
            min = min(x))
})

#Another approach
summary_df_bw <- split(merge_rasters_dataframes_long_bw, as.factor(merge_rasters_dataframes_long_bw$treatment))
stargazer(summary_df_bw)

vars <- ("altura_mean_30arc", "roughness", "slope", "colds00ag", "dist_coast", "dist_capital", "dist_colonial")
stats_table <- group_by(merge_rasters_dataframes_long, treatment) %>%
  summarize()

#Table 2 - RD Sharp for 2013
attach(merge_rasters_dataframes)
rd2013 <- rdrobust(x = dist_p_km,
                   y = log(0.01 + dm2013),
                   vce = "hc1",
                   all =T)
rd2013_controles <- rdrobust(x = dist_p_km,
                             y = log(0.01 + dm2013),
                             covs = cbind(altura_mean_30arc, roughness, slope, factor(sq1), dist_coast, dist_colonial,
                                          dist_capital),
                             vce = "hc1",
                             all =T)
rd2013_controles_fx <- rdrobust(x = dist_p_km,
                                y = log(0.01 + dm2013),
                                covs = cbind(altura_mean_30arc, roughness, slope, factor(sq1), dist_coast, dist_colonial,
                                             dist_capital, factor(municode), factor(dptocode)),
                                cluster = ID + municode,
                                vce = "hc1",
                                all =T)
rd2013_controles_fx_nn <- rdrobust(x = dist_p_km,
                                   y = log(0.01 + dm2013),
                                   covs = cbind(altura_mean_30arc, roughness, slope, factor(sq1), dist_coast, dist_colonial,
                                                dist_capital, factor(municode), factor(dptocode)),
                                   vce = "nn", nnmatch = 8,
                                   all =T)

detach(merge_rasters_dataframes)


stargazer(t(rbind(t(rd2013$tabl3.str[3, ]), t(rd2013_controles$tabl3.str[3, ]), t(rd2013_controles_fx$tabl3.str[3, ]), 
                  t(rd2013_controles_fx_nn$tabl3.str[3, ]))))


# Table 3 - Panel for the bw suggested by RD
merge_rasters_dataframes_long$dist_p_km <- merge_rasters_dataframes_long$dist_p / 1000
merge_rasters_dataframes_long_bw <- filter(merge_rasters_dataframes_long, dist_p_km < rd2013_controles_fx$bws[1, 1], dist_p_km > -rd2013_controles_fx$bws[1, 1])
merge_rasters_dataframes_long_capital <- filter(merge_rasters_dataframes_long_bw, dist_capital > 48827.92)


reg <- felm(log(0.01 + dm) ~ as.factor(t) | ID + year | 0 | 0, data = merge_rasters_dataframes_long_bw)
reg_clust <- felm(log(0.01 + dm) ~ as.factor(t) | ID + year | 0 |  ID + year, data = merge_rasters_dataframes_long_bw)
reg_clust_capitals <- felm(log(0.01 + dm) ~ as.factor(t) | ID + year | 0 | ID + year, data = merge_rasters_dataframes_long_capital)


stargazer(reg_clust, reg_clust_capitals)


#---------------------------------------------MAPS AND GRAPHS----------------------------------------------#

#Map 1 - OpenStreetMap
pacifico_osm <- openmap(c(8.700, -79.420),c(0.648, -74.928), type="osm", minNumTiles = 15) %>%
  openproj(projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(pacifico_osm)
plot(black_communities_union, add = T)
raster::scalebar(100, type = "bar", below = "km", divs = 4, xy = click()) #scalebar


#Map 2. Raster lights
rasters_suma <- stackApply(rasters_lights, indices = rep(1, 35), fun = mean)
plot(rasters_suma, axes = F)
plot(capital_cities_centroids, cex = 0.5, pch = 16, add = T)
plot(colombia, add = T)
plot(black_communities_union, add = T, lty = 3, lwd = 0.5)
plot(pacific_littoral_map_dpto, add = T, lwd = 0.5)
raster::scalebar(200, type = "bar", below = "km", divs = 4, xy = click())


#Timeline economic activity by lights (treatment vs. no-treatment)
rasters_year <- group_by(merge_rasters_dataframes_long, year, treatment) %>%
  summarise(total_dm=sum(dm),
            mean_dm = mean(dm)
  )

g3 <- ggplot(rasters_year, aes(x=year, y=total_dm, colour=as.factor(treatment))) + geom_line(size=1)
g3 <- g3 + scale_x_continuous(breaks=c(1992:2013))
g3 <- g3 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size = 10),
                 axis.text.y = element_text(size = 10))
g3 <- g3 + labs(x = "Año", y = "Actividad económica (densidad luz)") 
g3 <- g3 + theme(
  axis.title.x = element_text(face="bold", size=10),
  axis.title.y = element_text(face = "bold", size = 10))
g3

#For the vecinity 
rasters_year_bw <- group_by(merge_rasters_dataframes_long_bw, year, treatment) %>%
  summarise(total_dm=sum(dm),
            mean_dm = mean(dm)
  )

g4 <- ggplot(rasters_year_bw, aes(x=year, y=total_dm, colour=as.factor(treatment))) + geom_line(size=1)
g4 <- g4 + scale_x_continuous(breaks=c(1992:2013)) + scale_colour_discrete(name = "Tratamiento")
g4 <- g4 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size = 10),
                 axis.text.y = element_text(size = 10))
g4 <- g4 + labs(x = "Año", y = "Actividad económica (densidad luz)") 
g4 <- g4 + theme(
  axis.title.x = element_text(face="bold", size=10),
  axis.title.y = element_text(face = "bold", size = 10), legend.position = "bottom")
g4
