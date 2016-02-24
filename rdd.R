library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)
library(stargazer)
library(xtable)

#Variables <- cut-off (dist_f) and outcome (all the dm)
covariates <- c("altura_mean_30arc", "aspect", "slope","hill", "dist_capital")
covariates_df <- select(merge_rasters_dataframes, one_of(covariates))


#Data frames by distance
merge_rasters_10km <- filter(merge_rasters_dataframes, dist_f<10000, dist_f>-10000) 
merge_rasters_5km <- filter(merge_rasters_dataframes, dist_f<5000, dist_f>-5000) 
merge_rasters_3km <- filter(merge_rasters_dataframes, dist_f<3000, dist_f>-3000)
merge_rasters_1km <- filter(merge_rasters_dataframes, dist_f<1000, dist_f>-1000) 

#Parametric estimation (different polynomials - using both lm() and RDDtool package)
#Formula
formula <- paste("dmpooled ~ treatment + dist_p")
formula2 <- paste("dmpooled ~ treatment + poly(dist_p, 2)")
formula3 <- paste("dmpooled ~ treatment + poly(dist_p, 3)")
formula4 <- paste("dmpooled ~ treatment + poly(dist_p, 4)")
controles <- paste(covariates, collapse = "+")
controles_fx <- paste("factor(dptocode)", "factor(municode)", sep  = "+")
controles_panel <- paste(names(merge_rasters_dataframes)[1:5], collapse = "+")


#Non-bandwidth estimators - treated years
#First order polynomial
parametric1 <- lm(as.formula(paste(formula)) , data = merge_rasters_dataframes)
parametric1_c <- lm(as.formula(paste(formula, controles, sep  = "+")), 
                  data = merge_rasters_dataframes)
parametric1_cc <- lm(as.formula(paste(formula, controles, controles_fx, sep = "+")),
                     data = merge_rasters_dataframes)
parametric1_ccc <- lm(as.formula(paste(formula, controles, controles_fx, controles_panel, sep = "+")),
                     data = merge_rasters_dataframes)
robust1 <- coeftest(parametric1_cc, vcov = vcovHC(parametric1_ccc, "HC1")) 
robust1.se <- vcovHC(parametric1_cc, type = "HC1") 
robust1.se_c <- vcovHC(parametric1_ccc, type = "HC1")

#Second order polynomial
parametric2 <- lm(as.formula(paste(formula2)) , data = merge_rasters_dataframes)
parametric2_c <- lm(as.formula(paste(formula2, controles, sep  = "+")), 
                    data = merge_rasters_dataframes)
parametric2_cc <- lm(as.formula(paste(formula2, controles, controles_fx, sep = "+")),
                     data = merge_rasters_dataframes)
parametric2_ccc <- lm(as.formula(paste(formula2, controles, controles_fx, controles_panel, sep = "+")),
                      data = merge_rasters_1km)
robust2 <- coeftest(parametric2_cc, vcov = vcovHC(parametric2_ccc, "HC1")) 
robust2.se <- vcovHC(parametric2_cc, type = "HC1") 
robust2.se_c <- vcovHC(parametric2_ccc, type = "HC1")

#Third order polynomial
parametric3 <- lm(as.formula(paste(formula3)) , data = merge_rasters_dataframes)
parametric3_c <- lm(as.formula(paste(formula3, controles, sep  = "+")), 
                    data = merge_rasters_dataframes)
parametric3_cc <- lm(as.formula(paste(formula3, controles, controles_fx, sep = "+")),
                     data = merge_rasters_dataframes)
parametric3_ccc <- lm(as.formula(paste(formula3, controles, controles_fx, controles_panel, sep = "+")),
                      data = merge_rasters_dataframes)
robust3 <- coeftest(parametric3_ccc, vcov = vcovHC(parametric3_ccc, "HC1")) 
robust3.se <- vcovHC(parametric3_cc, type = "HC1") 
robust3.se_c <- vcovHC(parametric3_ccc, type = "HC1")

#Table LaTeX output
stargazer(parametric1_cc, parametric1_ccc, parametric2_cc, parametric2_ccc, parametric3_cc, parametric3_ccc, 
          title = "Estimación paramétrica con años agregados de tratamiento (1996 - 2013)",
          dep.var.caption = "Años agregados (1996 - 2013)",  dep.var.labels.include = FALSE,
          column.labels = c("Primer grado", "Primer grado", "Segundo grado", "Segundo grado", "Tercer grado", "Tercer grado"),
          covariate.labels = c("ATE"),
          se = list(robust1.se, robust1.se_c, robust2.se, robust2.se_c, robust3.se, robust3.se_c),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p", "[:dm:]"), keep = c("treatment1"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles", "Efectos panel"),
          omit.stat = c("rsq", "adj.rsq", "ser"), keep.stat = c("n", "f", "aic"),df = F, notes.label = "Nota: ", notes.align = "c",
          initial.zero = F
)


#Non-bandwidth estimators - non treated years (92 - 96)
#First order polynomial
parametric1_nt <- lm(dmpooled92_96 ~ treatment + dist_p , data = merge_rasters_dataframes)
parametric1_c_nt <- lm(dmpooled92_96 ~ treatment + dist_p + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                    data = merge_rasters_dataframes)
parametric1_cc_nt <- lm(dmpooled92_96~ treatment + dist_p + altura_mean_30arc + aspect + slope + hill + dist_capital 
                     + factor(dptocode) + factor(municode),
                     data = merge_rasters_dataframes)
robust1_nt <- coeftest(parametric1_cc_nt, vcov = vcovHC(parametric1_cc_nt, "HC1"))
robust1.se_nt <- vcovHC(parametric1_cc_nt, type = "HC1")

#Second order polynomial
parametric2_nt <- lm(dmpooled92_96 ~ treatment + poly(dist_p, 2) , data = merge_rasters_dataframes)
parametric2_c_nt <- lm(dmpooled ~ treatment + poly(dist_p, 2) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                    data = merge_rasters_dataframes)
parametric2_cc_nt <- lm(dmpooled92_96 ~ treatment + poly(dist_p, 2) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                     + factor(dptocode) + factor(municode),
                     data = merge_rasters_dataframes)
robust2_nt <- coeftest(parametric2_cc_nt, vcov = vcovHC(parametric2_cc_nt, "HC1"))
robust2.se_nt <- vcovHC(parametric2_cc_nt, type = "HC1")

#Third order polynomial
parametric3_nt <- lm(dmpooled92_96 ~ treatment + poly(dist_p, 3) , data = merge_rasters_dataframes)
parametric3_c_nt <- lm(dmpooled92_96 ~ treatment + poly(dist_p, 3) + altura_mean_30arc + aspect + slope + hill + dist_capital, 
                    data = merge_rasters_dataframes)
parametric3_cc_nt <- lm(dmpooled92_96 ~ treatment + poly(dist_p, 3) + altura_mean_30arc + aspect + slope + hill + dist_capital 
                     + factor(dptocode) + factor(municode),
                     data = merge_rasters_dataframes)
robust3_nt <- coeftest(parametric3_cc, vcov = vcovHC(parametric2_cc_nt, "HC1"))
robust3.se_nt <- vcovHC(parametric3_cc_nt, type = "HC1")


stargazer(parametric1_cc_nt, parametric2_cc_nt, parametric3_cc_nt, 
          title = "Estimación paramétrica con años no tratados (1996 - 2013)",
          dep.var.caption = "Años agregados", dep.var.labels = "1992-1996",
          column.labels = c("Primer grado", "Segundo grado", "Tercer grado"),
          covariate.labels = c("Diferencia"),
          se = list(robust1.se_nt, robust2.se_nt, robust3.se_nt),
          omit = c("[a-z][:(:]", "[a-z][:(:]", "dist_p"), keep = c("treatment1"),
          omit.labels = c("Efectos fijos municipio", "Efectos fijos departamento", "Controles"),
          omit.stat = c("rsq"), df = F, notes.label = "Nota: ", notes.align = "c", 
          initial.zero = F)

#Bandwidth (2.5 km around cutoff value)          
#RDD Tools
discontinuity_data <- RDDdata(x = dist_p,
                              y = dmpooled,
                              data = merge_rasters_1km,
                              cutpoint = 0) 

reg_para <- RDDreg_lm(discontinuity_data)


#rdtools

reg_para <- RDDreg_lm(discontinuity_data, order = 1)
bw_ik <- RDDbw_IK(discontinuity_data)
reg_nonpara <- RDDreg_np(RDDobject = discontinuity_data, bw = bw_ik)

#rdrobust pacakge
rdrobust_bw <- list()
attach(merge_rasters_dataframes) 

rdrobust_bw <- mapply(rdrobust(x = dist_p, y = dmpooled,
                       h = 2000))
detach(merge_rasters_dataframes)


rdplot <- rdplot(dmpooled, dist_p, c = 0, subset = dptocode == 22,
                 x.label = "Distancia", y.label = "Actividad económica", 
                 x.lim = c(-2000, 2000), y.lim = c(0, 5),
                 lowerend = -2000 , upperend = 2000)

detach(merge_rasters_dataframes)

#Models 
#(IK and CT non-parametric estimators - Sharp RD)
rd_allyearsSRD <- lapply(merge_rasters_dataframes[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_dataframes$dist_f, c = 0, all = T))


#(IK and CT non-parametric estimators - Fuzzy RD)
rd_allyearsFRD <- lapply(merge_rasters_1km[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_1km$dist_f, fuzzy = merge_rasters_1km$slope,c = 0, all = T))


#RD graph approach
#1 km 

rdplot <- rdplot(log(1 + merge_rasters_1km$`2013`), x =merge_rasters_1km$dist_f, c = 0,
                 x.label = "Distancia", y.label = "Actividad económica", y.lim = c(0, 0.2))


#Estimation of different BW
rdbwselect(y = merge_rasters_dataframes$`1992`,
                   x = merge_rasters_1km$dist_f , c=0, all=T)

#rdd package

rdestimate <- RDestimate(dmpooled ~ dist_f + slope ,
                         data = merge_rasters_dataframes,
                         subset = merge_rasters_dataframes$dptocode == "32",
                         cutpoint = 0, verbose = T)

plot(rdestimate)


rdrobust <- rdrobust(y = merge_rasters_dataframes$dm2pooled,
                     x = merge_rasters_dataframes$dist_p , c=0, all=T)
