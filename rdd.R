library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)

x <- merge_rasters_dataframes$dist_p
y <- merge_rasters_dataframes$F101992.v4b_web.stable_lights.avg_vis
controls <- c(merge_rasters_dataframes$altura_mean_30arc, factor(merge_rasters_dataframes$municode))

#Define RDD data (RDDTools package) - Non-parametric model (data-driven)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_dataframes$F101992.v4b_web.stable_lights.avg_vis, x =merge_rasters_dataframes$dist_p, cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#10 km
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_p<10000, dist_p>-10000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_10km$F101992.v4b_web.stable_lights.avg_vis, x =merge_rasters_10km$dist_p, covar = c(merge_rasters_10km$dist_capital) , cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)


#5 km
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_p<5000, dist_p>-5000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_5km$F101992.v4b_web.stable_lights.avg_vis, x =merge_rasters_5km$dist_p, covar = c(merge_rasters_5km$dist_capital, factor(merge_rasters_5km$municode), merge_rasters_5km$altura_mean_30arc), cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#1 km

merge_rasters_1km <- filter(merge_rasters_dataframes, dist_p<=1000, dist_p>=-1000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_1km$dist_capital, x =merge_rasters_1km$dist_p , cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)


#rdd package - the difficult one

#1 km 
rdestimate <- RDestimate(F101992.v4b_web.stable_lights.avg_vis ~ dist_p |  altura_mean_30arc + layer.y, cutpoint = 0, data=merge_rasters_1km)



#rdrobust pacakge

#RD graph approach
#1 km 

merge_rasters_20km <- filter(merge_rasters_dataframes, dist_p<=20000, dist_p>=-20000)
(rdplot(y = merge_rasters_20km$F182013.v4c_web.stable_lights.avg_vis, 
        x =merge_rasters_20km$dist_p, c = 0, binselect = "qsmvpr", scalel = 4, scaler = 10))

#RD estimation by bias corrected estimates
rdrobust <- rdrobust(y = merge_rasters_20km$F182013.v4c_web.stable_lights.avg_vis,
                     x = merge_rasters_20km$dist_p , c=0, all=T)

#Estimation of different BW
rdbwselect(y = merge_rasters_1km$F182013.v4c_web.stable_lights.avg_vis,
                   x = merge_rasters_1km$dist_p , c=0, all=T)



