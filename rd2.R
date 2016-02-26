

#Table 1: Treatment year (1997) with parametric estimators and controls  

#Non-bandwidth estimators - possible treatment years (97)
#First order polynomial
parametric1_t <- lm(dm1997 ~ treatment + dist_p , data = merge_rasters_dataframes)
parametric1_c_t <- lm(dm1997 ~ treatment + dist_p + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                       data = merge_rasters_dataframes)
parametric1_cc_t <- lm(dm1997 ~ treatment + dist_p + altura_mean_30arc + aspect + slope + hill + dist_capital 
                        + factor(dptocode) + factor(municode),
                        data = merge_rasters_dataframes)
robustclust1.se_t <- cluster.vcov(parametric1_cc_t, merge_rasters_dataframes$municode)
robust1clust_t <- coeftest(parametric1_cc_t, robustclust1.se_t)[, 4]


#Second order polynomial
parametric2_t <- lm(dm1997 ~ treatment + poly(dist_p, 2) , data = merge_rasters_dataframes)
parametric2_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 2) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                      data = merge_rasters_dataframes)
parametric2_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 2) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                       + factor(dptocode) + factor(municode),
                       data = merge_rasters_dataframes)
robustclust2.se_t <- cluster.vcov(parametric2_t, merge_rasters_dataframes$municode)
robust2clust_t <- coeftest(parametric2_t, robustclust2.se_t)[, 4]

#Third order polynomial
parametric3_t <- lm(dm1997 ~ treatment + poly(dist_p, 3) , data = merge_rasters_dataframes)
parametric3_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 3) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                      data = merge_rasters_dataframes)
parametric3_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 3) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                       + factor(dptocode) + factor(municode),
                       data = merge_rasters_dataframes)
robustclust3.se_t <- cluster.vcov(parametric3_t, merge_rasters_dataframes$municode)
robust3clust_t <- coeftest(parametric3_cc_t, robustclust3.se_t)[, 4]

#Fourth order polynomial
parametric4_t <- lm(dm1997 ~ treatment + poly(dist_p, 4) , data = merge_rasters_dataframes)
parametric4_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 4) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                      data = merge_rasters_dataframes)
parametric4_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 4) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                       + factor(dptocode) + factor(municode),
                       data = merge_rasters_dataframes)
robustclust4.se_t <- cluster.vcov(parametric4_cc_t, merge_rasters_dataframes$municode)
robust4clust_t <- coeftest(parametric4_cc_t, robustclust4.se_t)[, 4]

stargazer(parametric1_cc_t, parametric2_cc_t, parametric3_cc_t, parametric4_cc_t, 
          title = "Estimación paramétrica en año de tratamiento - 1997",
          dep.var.caption = "Densidad de luz", dep.var.labels = "Tratamiento",
          column.labels = c("Primer grado", "Segundo grado", "Tercer grado", "Cuarto grado"),
          covariate.labels = c("Diferencia"),
          se = list(robust1.se_t, robust2.se_t, robust3.se_t, robust4.se_t),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p"), keep = c("treatment"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles"),
          omit.stat = c("rsq"), df = F, notes.label = "Nota: ", notes.align = "c", 
          initial.zero = F)

#Table 2: Treatment year (1997) with parametric estimators and controls (with robust standard error by cluster)

stargazer(parametric1_cc_t, parametric2_cc_t, parametric3_cc_t, parametric4_cc_t, 
          title = "Estimación paramétrica en año de tratamiento corregida por clusters - 1997",
          dep.var.caption = "Densidad de luz en todo el litoral", dep.var.labels = "Tratamiento",
          column.labels = c("Primer grado", "Segundo grado", "Tercer grado", "Cuarto grado"),
          covariate.labels = c("Diferencia"),
          se = list(robustclust1.se_t, robustclust2.se_t, robustclust3.se_t, robustclust4.se_t),
          p = list(robust1clust_t, robust2clust_t, robust3clust_t, robust4clust_t),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p"), keep = c("treatment"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles"),
          omit.stat = c("rsq", "ser"), df = F, notes.label = "Nota: ", notes.align = "c", 
          initial.zero = F)

#Table 3: Treatment year (1997) with parametric estimators and controls - bw = 500 m, 1km and 2km only with first and second degree polynomials

parametric1km_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) , data = merge_rasters_1km)
parametric1km_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                      data = merge_rasters_1km)
parametric1km_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                       + factor(dptocode) + factor(municode),
                       data = merge_rasters_1km)
robustclust1km.se_t <- cluster.vcov(parametric1km_cc_t, merge_rasters_1km$municode)
robustclust1km_t <- coeftest(parametric1km_cc_t, robustclust1km.se_t)[, 4]


parametric500m_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) , data = merge_rasters_1km)
parametric500m_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                        data = merge_rasters_500m)
parametric500m_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                         + factor(dptocode) + factor(municode),
                         data = merge_rasters_500m)
robustclust500m.se_t <- cluster.vcov(parametric500m_cc_t, merge_rasters_500m$municode)
robustclust500m_t <- coeftest(parametric500m_cc_t, robustclust500m.se_t)[, 4]


parametric2km_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) , data = merge_rasters_2km)
parametric2km_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                        data = merge_rasters_2km)
parametric2km_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                         + factor(dptocode) + factor(municode),
                         data = merge_rasters_2km)
robustclust2km.se_t <- cluster.vcov(parametric2km_cc_t, merge_rasters_2km$municode)
robustclust2km_t <- coeftest(parametric2km_cc_t, robustclust2km.se_t)[, 4]


stargazer(parametric500m_cc_t, parametric1km_cc_t, parametric2km_cc_t,
          title = "Estimación paramétrica en año de tratamiento corregida por clusters - 1994",
          dep.var.caption = "Densidad de luz por ventanas (bw)", dep.var.labels = "Tratamiento",
          column.labels = c("bw = 500 m.", "bw = 1km.", "bw = 2 km."),
          covariate.labels = c("Diferencia"),
          se = list(robustclust1.se_t, robustclust2.se_t, robustclust3.se_t, robustclust4.se_t),
          p = list(robustclust500m_t, robustclust1km_t, robustclust2km_t),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p"), keep = c("treatment"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles"),
          omit.stat = c("rsq", "ser"), df = F, notes.label = "Nota: ", notes.align = "c", 
          initial.zero = F)

