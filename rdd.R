library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)


#Data frames by distance
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_f<10000, dist_f>-10000)
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_f<5000, dist_f>-5000)
merge_rasters_3km <- filter(merge_rasters_dataframes, dist_f<3000, dist_f>-3000)
merge_rasters_1km <- filter(merge_rasters_dataframes, dist_f<1000, dist_f>-1000)

#rdrobust pacakge

#Models 
#(IK and CT non-parametric estimators - Sharp RD)
rd_allyearsSRD <- lapply(merge_rasters_dataframes[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_dataframes$dist_f, c = 0, all = T))
rd_allyearsSRD_5k <- lapply(merge_rasters_5km[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_5km$dist_f, c = 0, all = T))



#(IK and CT non-parametric estimators - Fuzzy RD)
rd_allyearsFRD <- lapply(merge_rasters_1km[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_1km$dist_f, fuzzy = merge_rasters_1km$slope,c = 0, all = T))


#RD graph approach
#1 km 

rdplot <- rdplot(log(1 + merge_rasters_1km$`2013`), x =merge_rasters_1km$dist_f, c = 0,
                 x.label = "Distancia", y.label = "Actividad económica", y.lim = c(0, 0.2))


#Estimation of different BW
rdbwselect(y = merge_rasters_dataframes$`1992`,
                   x = merge_rasters_1km$dist_f , c=0, all=T)

#rdd package

rdestimate <- RDestimate(`1992`~ dist_f,
                         data = merge_rasters_1km, cutpoint = 0, verbose = T)

plot(rdestimate)


rdrobust <- rdrobust(y = merge_rasters_1km$`1992`,
                     x = merge_rasters_1km$dist_f , c=0, matches = 8, all=T)




(rdplot(merge_rasters_1km$F101992.v4b_web.stable_lights.avg_vis, 
        x =merge_rasters_1km$dist_f, c = 0))

