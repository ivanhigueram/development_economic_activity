#Define RDD data (RDDTools package) - Non-parametric model (data-driven)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_dataframes$F101992.v4b_web.stable_lights.avg_vis, x =merge_rasters_dataframes$dist_p, cutpoint = 50000)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#10 km
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_p<10000, dist_p>-10000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_10km$F142000.v4b_web.stable_lights.avg_vis, x =merge_rasters_10km$dist_p, cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#5 km
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_p<5000, dist_p>-5000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_5km$F142000.v4b_web.stable_lights.avg_vis, x =merge_rasters_5km$dist_p, cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)

#1 km
merge_rasters_1km <- filter(merge_rasters_dataframes, dist_p<1000, dist_p>-1000)
merge_rasters_dataframes_rdd <- RDDdata(y = merge_rasters_1km$F101992.v4b_web.stable_lights.avg_vis, x =merge_rasters_1km$dist_p, cutpoint = 0)
bw_ik <- RDDbw_IK(merge_rasters_dataframes_rdd)
reg_nonpara <- RDDreg_np(RDDobject = merge_rasters_dataframes_rdd, bw = bw_ik)
print(reg_nonpara)
plot(reg_nonpara)


#Create an RD object (rdd package)







