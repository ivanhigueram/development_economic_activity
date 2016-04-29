library(KernSmooth)
bw <- dpill(merge_rasters_dataframes_bw["2500"]$dist_p, log(0.01 + merge_rasters_dataframes_bw["2500"]$dm2013))

local.lineal <- locpoly(merge_rasters_dataframes$dist_p, log(0.01 + merge_rasters_dataframes$dm2013), bandwidth =  1000, degree = 1)

plot(local.lineal$x, local.lineal$y, type = "l")
