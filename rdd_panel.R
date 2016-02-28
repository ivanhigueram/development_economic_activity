library(plm)
#RDD panel 
#Create lagged variable
library(DataCombine)
merge_rasters_dataframes_long <- slide(merge_rasters_dataframes_long
                                       , Var = "dm", GroupVar = "ID", slideBy = -1)

#Treatment variable within time and space
merge_rasters_dataframes_long$treatment_t <- as.numeric(ifelse(merge_rasters_dataframes_long$year > 1996, 1, 0))
merge_rasters_dataframes_long$treatment_tp <- with(merge_rasters_dataframes_long, as.numeric(treatment_t) * as.numeric(treatment))

#Subset dataframe by year and windows of treatment (bw)
merge_rasters_long_bw <- list()
for(i in c(100, 200, 300, 400, 500, 1000, 2500, 2000)){
  merge_rasters_long_bw[[i]] <- filter(merge_rasters_dataframes_long, dist_p < i, dist_p > -i) 
}

merge_rasters_long_year <- list()
for(i in c(1992:2013)){
  merge_rasters_long_year[[i]] <- filter(merge_rasters_dataframes_long, year == i) 
}

merge_rasters_long_treatment <- filter(merge_rasters_dataframes_long, year > 1997)
merge_rasters_long_notreatment <- filter(merge_rasters_dataframes_long, year < 1997)

#Panel tables
#Table 4: Panel treatment with clusters

panel_1 <- lm(dm ~  treatment  + poly(dist_p, 1) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_treatment)
panelclust1 <- cluster.vcov(panel_1, merge_rasters_long_treatment$municode) 
panelclust1_coef <- coeftest(panel_1, panelclust2)

panel_2 <- lm(dm ~  treatment  + poly(dist_p, 2) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_treatment)
panelclust2 <- cluster.vcov(panel_2, merge_rasters_long_treatment$municode) 
panelclust2_coef <- coeftest(panel_2, panelclust2)

panel_3 <- lm(dm ~  treatment  + poly(dist_p, 3) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_treatment)
panelclust3 <- cluster.vcov(panel_3, merge_rasters_long_treatment$municode) 
panelclust3_coef <- coeftest(panel_3, panelclust3)

panel_4 <- lm(dm ~  treatment  + poly(dist_p, 4) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_treatment)
panelclust4 <- cluster.vcov(panel_4, merge_rasters_long_treatment$municode) 
panelclust4_coef <- coeftest(panel_4, panelclust4)




#Panel analysis (pooled all years) - no treatment
panel_bw <- lapply(merge_rasters_long_bw, function(x){
  lm(dm ~  treatment_tp  + poly(dist_p, 1) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
       factor(dptocode) + factor(municode) + factor(year), data = x)
})

panel_1 <- lm(dm ~  treatment_tp  + poly(dist_p, 2) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                 factor(dptocode) + factor(municode) + factor(year), 
              data = merge_rasters_dataframes_long, 
              subset = year > 1997)
           
robustclust1.se <- cluster.vcov(plm_100, merge_rasters_dataframes_long$municode) 
robustclust1 <- coeftest(plm_100, robustclust1.se) 


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


panel.set <- plm.data(merge_rasters_dataframes_long, index = c("ID", "year"))





subset = year %in% c(1996:2013)




