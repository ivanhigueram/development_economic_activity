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
merge_rasters_bw <- list()
for(i in c(100, 200, 300, 400, 500, 1000, 2500, 2000)){
  merge_rasters_bw[[str_c(i)]] <- filter(merge_rasters_dataframes, dist_p < i, dist_p > -i) 
}

#Bandwidth (2.5 km around cutoff value)          
#RDD Tools
discontinuity_data <- RDDdata(x = dist_p,
                              y = dm2013,
                              data = merge_rasters_dataframes,
                              cutpoint = 0) 

reg_para <- RDDreg_lm(discontinuity_data, order = 3)
reg_nonpara <- RDDreg_np(discontinuity_data)

#rdtools

reg_para <- RDDreg_lm(discontinuity_data, order = 1)
bw_ik <- RDDbw_IK(discontinuity_data)
reg_nonpara <- RDDreg_np(RDDobject = discontinuity_data, bw = bw_ik)

#rdrobust pacakge


dependent <- names(merge_rasters_dataframes)[1:22]
independent <- list("treatment", "poly(dist_p, 3)", "factor(dptocode)")


#500 m
parametric_year_controls_1 <- lapply(dependent, function(x){
  lm(as.formula(paste(x, paste(independent, collapse = " + "), sep = " ~ ")), data = merge_rasters_300m)
})



lapply(parametric_year_controls_1, function(x){
  coefficients(x)[3]})

dm_names <- list()
for(i in c(1:22)){
  dm_names[[i]] <- names(merge_rasters_dataframes)[[i]]
}

attach(merge_rasters_dataframes) 
rd_nopara_97 <- rdrobust(x = dist_p, y = dm1997, all = T)
detach(merge_rasters_dataframes)

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

rdestimate <- RDestimate(dm1992 ~ dist_p,
                         data = merge_rasters_dataframes,
                         cutpoint = 0, verbose = T)

plot(rdestimate)


rdrobust <- rdrobust(y = merge_rasters_dataframes$dm1992,
                     x = merge_rasters_dataframes$dist_p , c=0, all=T)
