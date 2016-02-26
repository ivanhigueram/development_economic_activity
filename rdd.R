library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)
library(stargazer)
library(xtable)

#Variables <- cut-off (dist_f) and outcome (all the dm)
covariates <- c("altura_mean_30arc", "aspect", "slope","hill", "dist_capital")
covariates_df <- select(merge_rasters_300m, one_of(covariates))

#Data frames by distance
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_p<10000, dist_p>-10000) 
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_p<5000, dist_p>-5000) 
merge_rasters_3km <- filter(merge_rasters_dataframes, dist_p<3000, dist_p>-3000)
merge_rasters_2km <- filter(merge_rasters_dataframes, dist_p<2000, dist_p>-2000)
merge_rasters_1.5km <- filter(merge_rasters_dataframes, dist_p<1500, dist_p>-1500)
merge_rasters_1km <- filter(merge_rasters_dataframes, dist_p<1000, dist_p>-1000) 
merge_rasters_500m <- filter(merge_rasters_dataframes, dist_p<500, dist_p>-500)
merge_rasters_400m <- filter(merge_rasters_dataframes, dist_p<400, dist_p>-400)
merge_rasters_300m <- filter(merge_rasters_dataframes, dist_p<300, dist_p>-300)
merge_rasters_200m <- filter(merge_rasters_dataframes, dist_p<200, dist_p>-200)


#Bandwidth (2.5 km around cutoff value)          
#RDD Tools
discontinuity_data <- RDDdata(x = dist_p,
                              y = dmpooled,
                              data = merge_rasters_300m,
                              covar = covariates_df,
                              cutpoint = 0) 

reg_para <- RDDreg_lm(discontinuity_data)


#rdtools

reg_para <- RDDreg_lm(discontinuity_data, order = 1)
bw_ik <- RDDbw_IK(discontinuity_data)
reg_nonpara <- RDDreg_np(RDDobject = discontinuity_data, bw = bw_ik)

#rdrobust pacakge
rdrobust_bw <- list()
attach(merge_rasters_dataframes) 
rdrobust_bw <- mapply(rdrobust(x = dist_p, y = dm2013, fuzzy = altura_mean_30arc ,subset = dptocode == 13))

attach(merge_rasters_dataframes)

rdplot <- rdplot(dm1997, dist_p, c = 0,
                 x.label = "Distancia (metros)", y.label = "Actividad económica (dm)",
                 title = "Grafico de discontinuidad (1997)")

detach(merge_rasters_dataframes)

#Models 
#(IK and CT non-parametric estimators - Sharp RD)
rd_allyearsSRD <- lapply(merge_rasters_dataframes[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_dataframes$dist_f, c = 0, all = T))


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

rdestimate <- RDestimate(dm2013 ~ dist_p + altura_mean_30arc ,
                         data = merge_rasters_dataframes,
                         subset = merge_rasters_dataframes$dptocode == "13",
                         cutpoint = 0, verbose = T)

plot(rdestimate)


rdrobust <- rdrobust(y = merge_rasters_dataframes$dm2pooled,
                     x = merge_rasters_dataframes$dist_p , c=0, all=T)
