#RDD panel 
#Create lagged variable
library(DataCombine)
library(multiwayvcov)
library(plm)

#Treatment variable within time and space
merge_rasters_dataframes_long$treatment_t <- as.numeric(ifelse(merge_rasters_dataframes_long$year > 1996, 1, 0))
merge_rasters_dataframes_long$treatment_tp <- with(merge_rasters_dataframes_long, as.numeric(treatment_t) * as.numeric(treatment))

#Subset dataframe by year and windows of treatment (bw)
merge_rasters_long_bw <- list()
for(i in c(100, 200, 300, 400, 500, 1000, 2500, 2000, 5000, 6000, 10000)){
  merge_rasters_long_bw[[str_c(i)]] <- filter(merge_rasters_dataframes_long, dist_p < i, dist_p > -i) 
}

merge_rasters_long_year <- list()
for(i in c(1992:2013)){
  merge_rasters_long_year[[i]] <- filter(merge_rasters_dataframes_long, year == i) 
}

merge_rasters_long_treatment <- filter(merge_rasters_dataframes_long, year > 1997)
merge_rasters_long_notreatment <- filter(merge_rasters_dataframes_long, year < 1997)

#Panel tables
#Table 4: Panel treatment robust

panel_1 <- lm(log(1 + dm) ~  t  + poly(dist_p, 1) +  altura_mean_30arc + slope + roughness + hill + dist_capital + dist_colonial + dist_coast + colds95ag +colds00ag +
              factor(municode) + factor(year) + factor(sq1) + factor(sq7), data = merge_rasters_dataframes_long)
panelclust1 <- cluster.vcov(panel_1, merge_rasters_dataframes_long$ID)
panelrobust1 <- vcovHC(panel_1, "HC1")
panelclust1_coef <- coeftest(panel_1, panelclust1)
panelrobust1_coef <- coeftest(panel_1, panelrobust1)


panel_2 <- lm(dm ~  treatment  + poly(dist_p, 2) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_treatment)
panelclust2 <- cluster.vcov(panel_2, merge_rasters_long_treatment$municode) 
panelrobust2 <- vcovHC(panel_2, "HC1")
panelclust2_coef <- coeftest(panel_2, panelclust2)
panelrobust2_coef <- coeftest(panel_2, panelrobust2)[, 4]

panel_3 <- lm(dm ~  treatment  + poly(dist_p, 3) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_treatment)
panelclust3 <- cluster.vcov(panel_3, merge_rasters_long_treatment$municode) 
panelrobust3 <- vcovHC(panel_3, "HC1")
panelclust3_coef <- coeftest(panel_3, panelclust3)
panelrobust3_coef <- coeftest(panel_3, panelrobust3)[, 4]

panel_4 <- lm(dm ~  treatment  + poly(dist_p, 4) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_treatment)
panelclust4 <- cluster.vcov(panel_4, merge_rasters_long_treatment$municode) 
panelrobust4 <- vcovHC(panel_4, "HC1")
panelclust4_coef <- coeftest(panel_4, panelclust4)
panelrobust4_coef <- coeftest(panel_4, panelrobust4)[, 4]

stargazer(panel_1, panel_2, panel_3, panel_4, 
          title = "Estimación paramétrica panel (1997 - 2013)",
          dep.var.caption = "Densidad de luz en todo el litoral", dep.var.labels = "Tratamiento",
          column.labels = c("Primer grado", "Segundo grado", "Tercer grado", "Cuarto grado"),
          covariate.labels = c("Diferencia"),
          se = list(panelrobust1, panelrobust2, panelrobust3, panelrobust4),
          p = list(panelrobust1_coef, panelrobust2_coef, panelrobust3_coef, panelrobust4_coef),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p", "[:dm:]"), keep = c("treatment"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles", "Efectos panel"),
          omit.stat = c("rsq", "adj.rsq", "ser"), keep.stat = c("n", "f", "aic"),df = F, notes.label = "Nota: ", notes.align = "c",
          initial.zero = F
)


#Panel 5: Panel no treatment years
panel_1nt <- lm(dm ~  treatment  + poly(dist_p, 1) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_notreatment)
panelclust1 <- cluster.vcov(panel_1nt, merge_rasters_long_notreatment$municode) 
panelrobust1 <- vcovHC(panel_1nt, "HC1")
panelclust1_coef <- coeftest(panel_1nt, panelclust1)
panelrobust1_coef <- coeftest(panel_1nt, panelrobust1)[, 4]


panel_2nt <- lm(dm ~  treatment  + poly(dist_p, 2) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_notreatment)
panelclust2 <- cluster.vcov(panel_2nt, merge_rasters_long_notreatment$municode) 
panelrobust2 <- vcovHC(panel_2nt, "HC1")
panelclust2_coef <- coeftest(panel_2nt, panelclust2)
panelrobust2_coef <- coeftest(panel_2nt, panelrobust2)[, 4]

panel_3nt <- lm(dm ~  treatment  + poly(dist_p, 3) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_notreatment)
panelclust3 <- cluster.vcov(panel_3nt, merge_rasters_long_notreatment$municode) 
panelrobust3 <- vcovHC(panel_3nt, "HC1")
panelclust3_coef <- coeftest(panel_3nt, panelclust3)
panelrobust3_coef <- coeftest(panel_3nt, panelrobust3)[, 4]

panel_4nt <- lm(dm ~  treatment  + poly(dist_p, 4) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
                factor(dptocode) + factor(municode) + factor(year), data = merge_rasters_long_notreatment)
panelclust4 <- cluster.vcov(panel_4nt, merge_rasters_long_notreatment$municode) 
panelrobust4 <- vcovHC(panel_4nt, "HC1")
panelclust4_coef <- coeftest(panel_4nt, panelclust4)
panelrobust4_coef <- coeftest(panel_4nt, panelrobust4)[, 4]

stargazer(panel_1nt, panel_2nt, panel_3nt, panel_4nt, 
          title = "Estimación paramétrica panel (1992 - 1997)",
          dep.var.caption = "Densidad de luz en todo el litoral", dep.var.labels = " Años sin Tratamiento",
          column.labels = c("Primer grado", "Segundo grado", "Tercer grado", "Cuarto grado"),
          covariate.labels = c("Diferencia"),
          se = list(panelrobust1, panelrobust2, panelrobust3, panelrobust4),
          p = list(panelrobust1_coef, panelrobust2_coef, panelrobust3_coef, panelrobust4_coef),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p", "[:dm:]"), keep = c("treatment"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles", "Efectos panel"),
          omit.stat = c("rsq", "adj.rsq", "ser"), keep.stat = c("n", "f", "aic"),df = F, notes.label = "Nota: ", notes.align = "c",
          initial.zero = F
)


#Table 6: Panel using windows of treatment (bw = 100 m - 2000 m)

panel_bw <- lapply(merge_rasters_long_bw, function(dd)lm(dm ~  treatment_tp  + poly(dist_p, 1) +  altura_mean_30arc + aspect + slope + hill + dist_capital + 
       factor(dptocode) + factor(municode) + factor(year), data = dd))

panel_bw_robust <- lapply(panel_bw, vcovHC, "HC1")
panelrobust_bw_coef <- mapply(function(x, y){
  coeftest(x, y)
  }, x = panel_bw, y = panel_bw_robust)



stargazer(panel_bw[[2]], panel_bw[[5]], panel_bw[[7]], panel_bw[[9]], panel_bw[[11]],
          title = "Estimación paramétrica panel (1992 - 1997)",
          dep.var.caption = "Densidad de luz en todo el litoral", dep.var.labels = " Años sin Tratamiento",
          column.labels = c("bw = 200 m.", "bw = 500m.", "bw = 2.5 km.", "bw = 5 km.", "bw = 10 km."),
          covariate.labels = c("Diferencia"),
          se = list(panel_bw_robust[[1]], panel_bw_robust[[5]], panel_bw_robust[[7]], panel_bw_robust[[9]], panel_bw_robust[[11]]),
          p = list(panelrobust_bw_coef[[1]], panelrobust_bw_coef[[5]], panelrobust_bw_coef[[7]], panelrobust_bw_coef[[9]], panelrobust_bw_coef[[11]]),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p", "[:dm:]"), keep = c("treatment"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles", "Efectos panel"),
          omit.stat = c("rsq", "adj.rsq", "ser"), keep.stat = c("n", "f", "aic"),df = F, notes.label = "Nota: ", notes.align = "c",
          initial.zero = F
)

c(100, 200, 300, 400, 500, 1000, 2500, 2000, 5000, 6000, 10000)

panel.set <- plm.data(merge_rasters_dataframes_long, index = c("ID", "year"))




