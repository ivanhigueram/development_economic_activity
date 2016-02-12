library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)


#Data frames by distance
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_f<10000, dist_f>-10000)
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_f<5000, dist_f>5000)
merge_rasters_3km <- filter(merge_rasters_dataframes, dist_f<3000, dist_f>-3000)
merge_rasters_1km <- filter(merge_rasters_dataframes, dist_f<1000, dist_f>-1000)


x <- merge_rasters_dataframes$dist_p
y <- merge_rasters_dataframes$F101992.v4b_web.stable_lights.avg_vis
covariates <- c(merge_rasters_dataframes$altura_mean_30arc, factor(merge_rasters_dataframes$municode))

#Define RDD data (RDDTools package) - Non-parametric model (data-driven)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_dataframes$F101992.v4b_web.stable_lights.avg_vis , x =merge_rasters_dataframes$dist_f,  cutpoint = 0)
reg_para <- RDDreg_lm(merge_rasters_dataframes_rdd, order = 4)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#10 km
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_p<10000, dist_p>-10000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_10km$F182013.v4c_web.stable_lights.avg_vis, x =merge_rasters_10km$dist_f, cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#5 km
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_f<5000, dist_f>-5000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_5km$F182013.v4c_web.stable_lights.avg_vis, x =merge_rasters_5km$dist_p, cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#1 km
merge_rasters_1km <- filter(merge_rasters_dataframes, dist_f<=1000, dist_f>=-1000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_1km$F101992.v4b_web.stable_lights.avg_vis, x =merge_rasters_1km$dist_f, cutpoint = 0)
reg_para <- RDDreg_lm(merge_rasters_dataframes_rdd, order = 5)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)


#rdd package - the difficult one

#1 km 

#rdrobust pacakge

#RD graph approach
#1 km 

(rdplot(merge_rasters_1km$F101992.v4b_web.stable_lights.avg_vis, 
        x =merge_rasters_1km$dist_f, c = 0))


#Estimation of different BW
rdbwselect(y = merge_rasters_1km$F182013.v4c_web.stable_lights.avg_vis,
                   x = merge_rasters_1km$dist_f , c=0, all=T)

#rdd package
rdestimate <- RDestimate(log(1 + F182013.v4c_web.stable_lights.avg_vis) ~ dist_p + slope,
                         data = merge_rasters_dataframes, cutpoint = 10)

plot(rdestimate)


rdrobust <- rdrobust(y = merge_rasters_10km$F142001.v4b_web.stable_lights.avg_vis,
                     x = merge_rasters_10km$dist_f , c=0, matches = 8, all=T)

rd_allyears <- lapply(merge_rasters_10km[2:36], function(x) rdrobust(y = x, x = merge_rasters_10km$dist_f, c = 0, bwselect = "IK"))





(rdplot(merge_rasters_1km$F101992.v4b_web.stable_lights.avg_vis, 
        x =merge_rasters_1km$dist_f, c = 0))

