
#Plot light/distance
merge_rasters_dataframes$layer[merge_rasters_dataframes$ID %in% unlist(cell_black_communities)] <- -1* (merge_rasters_dataframes$layer)
attach(merge_rasters_nNA)
smoothScatter(layer, dim_92, nbin=50, bandwidth = 0.02)


#Transform (Michalopoulos etal) - 92
merge_rasters_nNA <- filter(merge_rasters_dataframes, F101992.v4b_web.stable_lights.avg_vis != "NA")
merge_rasters_nNA <- mutate(merge_rasters_nNA, dim_13 = log(0.01 + F182013.v4c_web.stable_lights.avg_vis))
merge_rasters_nNA5 <- filter(merge_rasters_nNA, layer<5000, layer>-5000)
density <- RDestimate(dim_13 ~ layer)
