library(plm)
library(lmtest)
library(stringr)
library(multiwayvcov)
library(lmtest)

#Data frames by bw
merge_rasters_dataframes_bw <- list()
for(i in c(100, 200, 300, 400, 500, 1000, 2000, 2500, 3000, 5000, 10000)){
  merge_rasters_dataframes_bw[[str_c(i)]] <- filter(merge_rasters_dataframes, dist_p < i, dist_p > -i) 
}


#Exploratory graph (with raw data) -2013-
with(merge_rasters_dataframes, plot(y = log(0.01 + dm2013), x = dist_p_km, pch = 19, cex = 0.2, xlim = c(-20, 20))) 
left.lm <-  lm(log(0.01 + dm2013) ~ dist_p_km, merge_rasters_dataframes, subset = dist_p_km < 0)
right.lm <- lm(log(0.01 + dm2013) ~ dist_p_km, merge_rasters_dataframes, subset = dist_p_km >= 0)
left.x <- seq(-20, 0, 1)
right.x <- -left.x
lines(left.x, predict(left.lm, newd = data.frame(dist_p_km = left.x)), col = "red") 
lines(right.x, predict(right.lm, newd = data.frame(dist_p_km = right.x)), col = "red")


#Exploratory graph (with raw data) -1997-
with(merge_rasters_dataframes, plot(y = log(0.01 + dm1997), x = dist_p_km, pch = 19, cex = 0.2, xlim = c(-20, 20))) 
left.lm <-  lm(log(0.01 + dm1997) ~ dist_p_km, merge_rasters_dataframes, subset = dist_p_km < 0)
right.lm <- lm(log(0.01 + dm1997) ~ dist_p_km, merge_rasters_dataframes, subset = dist_p_km >= 0)
left.x <- seq(-20, 0, 1)
right.x <- -left.x
lines(left.x, predict(left.lm, newd = data.frame(dist_p_km = left.x)), col = "red") 
lines(right.x, predict(right.lm, newd = data.frame(dist_p_km = right.x)), col = "red")


#Exploratory graph (with raw data and local regression) - 2013 
g1 <- ggplot(merge_rasters_dataframes, aes(x = dist_p, y = dm2013))
g1 <- g1 + geom_point() + geom_smooth(method=lm, fullrange=TRUE)
g1


#First (and last try) to make a table for all years 

#Formulas to create a list of predictors. 
luces <- names(merge_rasters_dataframes)[19:35] 
tratamientos <- names(merge_rasters_dataframes)[49:65] 
controles <- c("altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial")
fijos <- c("factor(dptocode) + factor(municode)")
polinomios <- c(1:4)

formulas_fx <- paste(
  paste(paste("log(0.01 + " ,luces, ")" , sep = ""), tratamientos, sep = " ~ "),
  paste("poly(I(dist_p / 1000), ", polinomios, ")", sep = ""),
  paste(controles, fijos, sep = " + "))

formulas <- paste(
  paste(paste("log(0.01 + " ,luces, ")" , sep = ""), tratamientos, sep = " ~ "),
  paste("poly(I(dist_p / 1000), ", polinomios, ")", sep = ""),
  paste(controles), sep = " + ")

parametric_1km_control <- list()
parametric_1km_control_fx <- list() 


parametric_1km_control <- lapply(formulas,
  function(x) lm(as.formula(x), data = merge_rasters_dataframes_bw[["1000"]])
)



#Table 1: Final year (2013) with treatment for (2012) - parametric with clustered errors by pixel
# I decide to use "discontinuity samples" to the frontier / bw = 5000, 2500, 1000

#Bandwidth estimators - Final year (2013) 


#1 km 
parametric_1km_control <- list()
parametric_1km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_1km_control[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                           + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                           data = merge_rasters_dataframes_bw[["1000"]])
  parametric_1km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                              + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                              + factor(dptocode) + factor(municode),
                                              data = merge_rasters_dataframes_bw[["1000"]])
}
parametric_1km_SEcluster <- lapply(parametric_1km_control_fx, cluster.vcov, ~ municode)
parametric_1km_SErobust <- lapply(parametric_1km_control_fx, vcovHC, "HC1")
parametric_1km_control_fx_cluster <- mapply(coeftest, parametric_1km_control_fx , parametric_1km_SEcluster)
parametric_1km_control_fx_robust <- mapply(coeftest, parametric_1km_control_fx , parametric_1km_SErobust)


# 2 km 
parametric_2km_control <- list()
parametric_2km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_2km_control[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                           + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                           data = merge_rasters_dataframes_bw[["2000"]])
  parametric_2km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                              + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                              + factor(dptocode) + factor(municode),
                                              data = merge_rasters_dataframes_bw[["2000"]])
}
parametric_2km_SEcluster <- lapply(parametric_2km_control_fx, cluster.vcov, ~ municode)
parametric_2km_SErobust <- lapply(parametric_2km_control_fx, vcovHC, "HC1")
parametric_2km_control_fx_cluster <- mapply(coeftest, parametric_2km_control_fx , parametric_2km_SEcluster)
parametric_2km_control_fx_robust <- mapply(coeftest, parametric_2km_control_fx , parametric_2km_SErobust)


# 2.5 km 
parametric_2.5km_control <- list()
parametric_2.5km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_2.5km_control[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                           + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                           data = merge_rasters_dataframes_bw[["2500"]])
  parametric_2.5km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                              + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                              + factor(dptocode) + factor(municode),
                                              data = merge_rasters_dataframes_bw[["2500"]])
}
parametric_2.5km_SEcluster <- lapply(parametric_2.5km_control_fx, cluster.vcov, ~ municode)
parametric_2.5km_SErobust <- lapply(parametric_2.5km_control_fx, vcovHC, "HC1")
parametric_2.5km_control_fx_cluster <- mapply(coeftest, parametric_2.5km_control_fx , parametric_2.5km_SEcluster)
parametric_2.5km_control_fx_robust <- mapply(coeftest, parametric_2.5km_control_fx , parametric_2.5km_SErobust)


# 3 km 
parametric_3km_control <- list()
parametric_3km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_3km_control[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                           + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                           data = merge_rasters_dataframes_bw[["3000"]])
  parametric_3km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm2013) ~ as.factor(t2012) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                              + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                              + factor(dptocode) + factor(municode),
                                              data = merge_rasters_dataframes_bw[["3000"]])
}
parametric_3km_SEcluster <- lapply(parametric_3km_control_fx, cluster.vcov, ~ municode)
parametric_3km_SErobust <- lapply(parametric_3km_control_fx, vcovHC, "HC1")
parametric_3km_control_fx_cluster <- mapply(coeftest, parametric_3km_control_fx , parametric_3km_SEcluster)
parametric_3km_control_fx_robust <- mapply(coeftest, parametric_3km_control_fx , parametric_3km_SErobust)

#Extract coefficients from coeftest 

list_cluster <- list(parametric_1km_control_fx_cluster, parametric_2km_control_fx_cluster,
                     parametric_2.5km_control_fx_cluster, parametric_3km_control_fx_cluster)

list_lm <- list(parametric_1km_control_fx, parametric_2km_control_fx,
                parametric_2.5km_control_fx, parametric_3km_control_fx)


coef_cluster <- function(list){
    list %>%
    lapply("[", 2, -c(2, 4)) %>%  
    as.data.frame()
}

coef_lm <- function(list){  
   list %>%
   lapply(coeftest) %>%
   lapply("[", 2, -c(2, 4)) %>%
    as.data.frame()
}

coef_t_lm <- lapply(list_lm, coef_lm) %>%
  do.call(what = rbind)

coef_t_clus <- lapply(list_cluster, coef_cluster) %>%
  do.call(what = rbind)

# Table using stargazer 

stargazer(coef_t_lm, coef_t_clus, summary = F)


#Table 2: Initial year (1997) with treatment for (1996) - parametric with clustered errors by pixel
# I decide to use "discontinuity samples" to the frontier / bw = 5000, 2500, 1000

#Bandwidth estimators - Final year (1997) 


#1 km 
parametric_1km_control <- list()
parametric_1km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_1km_control[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                           + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                           data = merge_rasters_dataframes_bw[["1000"]])
  parametric_1km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                              + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                              + factor(dptocode) + factor(municode),
                                              data = merge_rasters_dataframes_bw[["1000"]])
}
parametric_1km_SEcluster <- lapply(parametric_1km_control_fx, cluster.vcov, ~ municode)
parametric_1km_SErobust <- lapply(parametric_1km_control_fx, vcovHC, "HC1")
parametric_1km_control_fx_cluster <- mapply(coeftest, parametric_1km_control_fx , parametric_1km_SEcluster)
parametric_1km_control_fx_robust <- mapply(coeftest, parametric_1km_control_fx , parametric_1km_SErobust)


# 2 km 
parametric_2km_control <- list()
parametric_2km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_2km_control[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                           + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                           data = merge_rasters_dataframes_bw[["2000"]])
  parametric_2km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                              + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                              + factor(dptocode) + factor(municode),
                                              data = merge_rasters_dataframes_bw[["2000"]])
}
parametric_2km_SEcluster <- lapply(parametric_2km_control_fx, cluster.vcov, ~ municode)
parametric_2km_SErobust <- lapply(parametric_2km_control_fx, vcovHC, "HC1")
parametric_2km_control_fx_cluster <- mapply(coeftest, parametric_2km_control_fx , parametric_2km_SEcluster)
parametric_2km_control_fx_robust <- mapply(coeftest, parametric_2km_control_fx , parametric_2km_SErobust)


# 2.5 km 
parametric_2.5km_control <- list()
parametric_2.5km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_2.5km_control[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                             + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                             data = merge_rasters_dataframes_bw[["2500"]])
  parametric_2.5km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                                + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                                + factor(dptocode) + factor(municode),
                                                data = merge_rasters_dataframes_bw[["2500"]])
}
parametric_2.5km_SEcluster <- lapply(parametric_2.5km_control_fx, cluster.vcov, ~ municode)
parametric_2.5km_SErobust <- lapply(parametric_2.5km_control_fx, vcovHC, "HC1")
parametric_2.5km_control_fx_cluster <- mapply(coeftest, parametric_2.5km_control_fx , parametric_2.5km_SEcluster)
parametric_2.5km_control_fx_robust <- mapply(coeftest, parametric_2.5km_control_fx , parametric_2.5km_SErobust)


# 3 km 
parametric_3km_control <- list()
parametric_3km_control_fx <- list()
for(i in c(1, 2, 3, 4)){
  parametric_3km_control[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                           + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7),
                                           data = merge_rasters_dataframes_bw[["3000"]])
  parametric_3km_control_fx[[str_c(i)]] <- lm(log(0.01 + dm1997) ~ as.factor(t1996) + poly(I(dist_p / 1000), i) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
                                              + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) 
                                              + factor(dptocode) + factor(municode),
                                              data = merge_rasters_dataframes_bw[["3000"]])
}
parametric_3km_SEcluster <- lapply(parametric_3km_control_fx, cluster.vcov, ~ municode)
parametric_3km_SErobust <- lapply(parametric_3km_control_fx, vcovHC, "HC1")
parametric_3km_control_fx_cluster <- mapply(coeftest, parametric_3km_control_fx , parametric_3km_SEcluster)
parametric_3km_control_fx_robust <- mapply(coeftest, parametric_3km_control_fx , parametric_3km_SErobust)

#Extract coefficients from coeftest 

list_cluster <- list(parametric_1km_control_fx_cluster, parametric_2km_control_fx_cluster,
                     parametric_2.5km_control_fx_cluster, parametric_3km_control_fx_cluster)

list_lm <- list(parametric_1km_control_fx, parametric_2km_control_fx,
                parametric_2.5km_control_fx, parametric_3km_control_fx)


coef_cluster <- function(list){
  list %>%
    lapply("[", 2, -c(2, 4)) %>%  
    as.data.frame()
}

coef_lm <- function(list){  
  list %>%
    lapply(coeftest) %>%
    lapply("[", 2, -c(2, 4)) %>%
    as.data.frame()
}

coef_t_lm <- lapply(list_lm, coef_lm) %>%
  do.call(what = rbind)

coef_t_clus <- lapply(list_cluster, coef_cluster) %>%
  do.call(what = rbind)

# Table using stargazer 

stargazer(coef_t_lm, coef_t_clus, summary = F, type = "text")



#Discontinuity graph of the models - 2013 (4th poly and 2.5 km)


# Create a data frame with ageYear column, interpolating across range
xmin <- min(merge_rasters_dataframes$dist_p_km)
xmax <- max(merge_rasters_dataframes$dist_p_km)
predicted <- data.frame(dist_p_km = seq(xmin, xmax, length.out=1000))
# Calculate predicted values of heightIn
predicted$dm2013 <- predict(lm(log(1 +dm2013) ~ poly(dist_p_km, 5), 
                                    data = merge_rasters_dataframes), predicted)

g1 <- ggplot(merge_rasters_dataframes, aes(x = dist_p_km, y = log(1 + dm2013))) +
  geom_point(colour="grey40") 
g1 <- g1 + geom_line(data = predicted, size = 0.5, color = "blue") 
g1

