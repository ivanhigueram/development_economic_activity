
#Table 1: Treatment year (1997) with parametric estimators and controls  

#Non-bandwidth estimators - possible treatment years (97)
#First order polynomial
parametric1_t <- lm(log(0.01 + dm1997) ~ factor(t1997) + dist_p , data = merge_rasters_dataframes)
parametric1_c_t <- lm(dm1997 ~ treatment + dist_p + altura_mean_30arc + aspect + slope + hill + dist_capital,  
                       data = merge_rasters_dataframes)
parametric1_cc_t <- lm(log(0.01 + dm1997) ~ as.factor(t1997) + dist_p + altura_mean_30arc + roughness + slope + colds95ag + dist_coast + dist_capital + dist_colonial
                        + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                        + factor(dptocode) + factor(municode),
                        data = merge_rasters_dataframes)
robustclust1.se_t <- cluster.vcov(parametric1_cc_t, ~ municode + dptocode + community_id)
robust1clust_t <- coeftest(parametric1_cc_t, robustclust1.se_t)


+ factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7)

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
parametric3_cc_t <- lm(log(0.01 + dm1997) ~ as.factor(t18) + poly(dist_p, 3) + altura_mean_30arc + aspect + slope + hill + dist_coast + dist_capital
                       + factor(dptocode) + factor(municode) + factor(sq1) + roughness  + colds95ag,
                       subset = 
                       data = merge_rasters_dataframes)
robustclusst3.se_t <- vcovHC(parametric3_cc_t, "HC1")
robust3clust_t <- coeftest(parametric3_cc_t, robustclusst3.se_t)

#Fourth order polynomial
parametric4_t <- lm(dm1997 ~ treatment + poly(dist_p, 4) , data = merge_rasters_dataframes)
parametric4_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 4) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                      data = merge_rasters_dataframes)
parametric4_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 4) + altura_mean_30arc + aspect + slope + hill + dist_capital + dist_coast
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

parametric1km_t <- lm(dm1997 ~ treatment + poly(dist_p, 2) , data = merge_rasters_bw[[6]])
parametric1km_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 2) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                      data = merge_rasters_bw[[6]])
parametric1km_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital
                       + factor(dptocode) + factor(municode),
                       data = merge_rasters_bw[[6]])
robustclust1km.se_t <- vcovHC(parametric1km_cc_t, "HC1")
robustclust1km_t <- coeftest(parametric1km_cc_t, robustclust1km.se_t)[, 4]


parametric500m_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) , data =  merge_rasters_bw[[5]])
parametric500m_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                        data =  merge_rasters_bw[[5]])
parametric500m_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                         + factor(dptocode) + factor(municode),
                         data =  merge_rasters_bw[[5]])
robustclust500m.se_t <- vcovHC(parametric500m_cc_t, "HC1")
robustclust500m_t <- coeftest(parametric500m_cc_t, robustclust500m.se_t)[, 4]


parametric2km_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) , data = merge_rasters_bw[[8]])
parametric2km_c_t <- lm(dm1997 ~ treatment + poly(dist_p, 1) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                        data = merge_rasters_bw[[8]])
parametric2km_cc_t <- lm(dm1997 ~ treatment + poly(dist_p, 2) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                         + factor(dptocode) + factor(municode),
                         data = merge_rasters_bw[[8]])
robustclust2km.se_t <- vcovHC(parametric2km_cc_t, "HC1")
robustclust2km_t <- coeftest(parametric2km_cc_t, robustclust2km.se_t)[, 4]


stargazer(parametric500m_cc_t, parametric1km_cc_t, parametric2km_cc_t,
          title = "Estimación paramétrica en año de tratamiento - 1997",
          dep.var.caption = "Densidad de luz por ventanas (bw)", dep.var.labels = "Tratamiento",
          column.labels = c("bw = 500 m.", "bw = 1km.", "bw = 2 km."),
          covariate.labels = c("Diferencia"),
          se = list(robustclust500m.se_t, robustclust1km.se_t, robustclust2km.se_t),
          p = list(robustclust500m_t, robustclust1km_t, robustclust2km_t),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p"), keep = c("treatment"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles"),
          omit.stat = c("rsq", "ser"), df = F, notes.label = "Nota: ", notes.align = "c", 
          initial.zero = F)





