library(plm)

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
merge_rasters_long_1500km <- filter(merge_rasters_dataframes_long, dist_p<1500, dist_p>-1500)
merge_rasters_long_1km <- filter(merge_rasters_dataframes_long, dist_p<1000, dist_p>-1000) 
merge_rasters_long_500m <- filter(merge_rasters_dataframes_long, dist_p<500, dist_p>-500)
merge_rasters_long_400m <- filter(merge_rasters_dataframes_long, dist_p<400, dist_p>-400)
merge_rasters_long_300m <- filter(merge_rasters_dataframes_long, dist_p<300, dist_p>-300)
merge_rasters_long_200m <- filter(merge_rasters_dataframes_long, dist_p<200, dist_p>-200)
merge_rasters_long_100m <- filter(merge_rasters_dataframes_long, dist_p<100, dist_p>-100)

#Panel analysis
panel.set <- plm.data(merge_rasters_long_100m, index = c("ID", "year"))
plm_100 <- plm(dm ~  treatment_tp  +  poly(dist_p, 1),
           data = panel.set, model = "within", effect = c("twoway"))
clustpanel100.se <- plm::vcovHC(plm_100, type = "HC1", cluster = c("group"))
clustpanel100 <- coeftest(parametric4_cc_t, robustclust4.se_t)[, 4]


panel.set <- plm.data(merge_rasters_long_200m, index = c("ID", "year"))
plm_200 <- plm(dm ~  treatment_tp * factor(dptocode) + poly(dist_p, 1) + factor(year), 
               data = panel.set, model = "within", effect = c("twoway"))

panel.set <- plm.data(merge_rasters_long_300m, index = c("ID", "year"))
plm_300 <- plm(dm ~  treatment_tp * factor(dptocode) + poly(dist_p, 1) + factor(year), 
               data = panel.set, model = "within")

panel.set <- plm.data(merge_rasters_long_400m, index = c("ID", "year"))
plm_400 <- plm(dm ~  treatment_tp * factor(dptocode) + poly(dist_p, 1) + factor(year), 
               data = panel.set, model = "within")

panel.set <- plm.data(merge_rasters_long_500m, index = c("ID", "year"))
plm_500 <- plm(dm ~  treatment_tp + poly(dist_p, 1), 
               data = panel.set, model = "within", effect = c("twoway"))


stargazer(plm_100, plm_200, plm_300, plm_400, plm_500, 
          title = "Estimación paramétrica panel (Ventanas entre 100 km y 500 km)",
          covariate.labels = c("LATE"),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p", "[:dm:]"), keep = c("treatment_tp"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles", "Efectos panel"),
          omit.stat = c("rsq", "adj.rsq", "ser"), keep.stat = c("n", "f", "aic"),df = F, notes.label = "Nota: ", notes.align = "c",
          initial.zero = F
)






subset = year %in% c(1996:2013)




