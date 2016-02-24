#RDD panel 
#Create lagged variable
library(DataCombine)
merge_rasters_dataframes_long <- slide(merge_rasters_dataframes_long
                                       , Var = "dm", GroupVar = "ID", slideBy = -1)

#Treatment variable within time and space
merge_rasters_dataframes_long$treatment_t <- as.numeric(ifelse(merge_rasters_dataframes_long$year > 1996, 1, 0))
merge_rasters_dataframes_long$treatment_tp <- with(merge_rasters_dataframes_long, as.numeric(treatment_t) * as.numeric(treatment))

#Select windows of treatment (and also reduce n)
merge_rasters_long_10km <- filter(merge_rasters_dataframes_long, dist_p<10000, dist_p>-10000) 
merge_rasters_long_5km <- filter(merge_rasters_dataframes_long, dist_p<5000, dist_p>-5000) 
merge_rasters_long_3km <- filter(merge_rasters_dataframes_long, dist_p<3000, dist_p>-3000)
merge_rasters_long_1km <- filter(merge_rasters_dataframes_long, dist_p<1000, dist_p>-1000) 
merge_rasters_long_500m <- filter(merge_rasters_dataframes_long, dist_p<500, dist_p>-500)

#Panel analysis
panel.set <- plm.data(merge_rasters_long_500m, index = c("ID", "year"))
plm <- plm(dm ~  treatment_tp * factor(dptocode) + dist_capital + altura_mean_30arc + aspect + slope + hill + factor(year), 
           data = panel.set, model = "within")
