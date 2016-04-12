library(KernSmooth)
bw <- dpill(merge_rasters_dataframes$dist_capital, log(1 + merge_rasters_dataframes$dm1997), divisor = 5000)
local.lineal <- locpoly(merge_rasters_dataframes$dist_capital, log(1 + merge_rasters_dataframes$dm1997), bandwidth = bw / 2, degree = 3)
plot(local.lineal$x, local.lineal$y, type = "l")
