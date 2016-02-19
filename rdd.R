library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)

#Variables <- cut-off (dist_f) and outcome (all the dm)
covariates_df <- select(merge_rasters_dataframes, -num_range("dm", 1992:2013))
covariates_df <- select(covariates_df, -matches("colds"))
covariates_df$municode <- as.factor(covariates_df$municode)

                        
#Data frames by distance
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_f<10000, dist_f>-10000) 
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_f<5000, dist_f>-5000) 
merge_rasters_3km <- filter(merge_rasters_dataframes, dist_f<3000, dist_f>-3000)
merge_rasters_1km <- filter(merge_rasters_dataframes, dist_f<1000, dist_f>-1000) 


#rdtools
reg_para_valle <- list()
for(i in names(merge_rasters_dataframes)[1:22]){
 reg_para_valle[[i]] <- RDDdata(x = dist_p, 
                                y = str_c("merge_rasters_dataframes", i, sep = "$"), 
                                data = subset(merge_rasters_dataframes, dptocode == "32"), 
                                cutpoint = 0)
}
reg_para_valle <- lapply(reg_nonpara_valle, RDDreg_lm, order = 3)

discontinuity_data <- RDDdata(x = dist_p, 
                              y = dm2013, 
                              z = select(subset(merge_rasters_dataframes, dptocode == "32"), c(treatment)), 
                              data = subset(merge_rasters_dataframes, dptocode == "32"), 
                              cutpoint = 0)
reg_para <- RDDreg_lm(discontinuity_data, order = 3)
bw_ik <- RDDbw_IK(discontinuity_data)
reg_nonpara <- RDDreg_np(RDDobject = discontinuity_data, bw = bw_ik)
                           
#rdrobust pacakge

attach(merge_rasters_dataframes)
rdrobust <- rdrobust( y = dm2013,
                      x = dist_f,
                      fuzzy = treatment,
                      subset = dptocode == 13)


rdplot <- rdplot(dm2013, dist_f, c = 0, subset = dptocode == 13,
                 x.label = "Distancia", y.label = "Actividad económica", 
                 x.lim = c(-2000, 2000), y.lim = c(0, 2),
                 lowerend = -2000 , upperend = 2000)

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

rdestimate <- RDestimate(dm2013 ~ dist_p + treatment,
                         data = merge_rasters_dataframes,
                         subset = dptocode == 13, cutpoint = 0, bw = 2000)

plot(rdestimate)


rdrobust <- rdrobust(y = merge_rasters_dataframes$dm2013,
                     x = merge_rasters_dataframes$dist_f , c=0, all=T)




(rdplot(merge_rasters_1km$F101992.v4b_web.stable_lights.avg_vis, 
        x =merge_rasters_1km$dist_f, c = 0))


lm <- lm(`1992` ~ dist_f, dist_f < 0, data = merge_rasters_1km)
