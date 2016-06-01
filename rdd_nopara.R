library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)
library(stargazer)
library(xtable)
library(plyr)
library(dplyr)


#Tables by group
vars <- c("altura_mean_30arc", "roughness", "slope", "colds00ag", "dist_coast", "dist_capital", "dist_colonial")
stats_table <- merge_rasters_dataframes %>%
  group_by(treatment) %>%
  summarize()


#Data frames by distance
merge_rasters_bw <- list()
for(i in c(100, 200, 300, 400, 500, 1000, 2500, 2000)){
  merge_rasters_bw[[str_c(i)]] <- filter(merge_rasters_dataframes, dist_p < i, dist_p > -i) 
}

#Data frames by department
merge_rasters_dataframes_depto <- split(merge_rasters_dataframes, merge_rasters_dataframes$dptocode)


#RDD Tools
discontinuity_data <- RDDdata(x = dist_p_km,
                              y = log(0.01 + dm1997),
                              data = merge_rasters_dataframes,
                              cutpoint = 0) 

reg_para <- RDDreg_lm(discontinuity_data, order = 1, covariates = merge_rasters_dataframes[, 1:10])
reg_nonpara <- RDDreg_np(discontinuity_data)

reg_para <- RDDreg_lm(discontinuity_data, order = 1)
bw_ik <- RDDbw_IK(discontinuity_data)
reg_nonpara <- RDDreg_np(RDDobject = discontinuity_data, bw = bw_ik)
plotSensi(reg_nonpara, from = 1, to = 50, by = 5)
plotPlacebo(reg_nonpara)

#rdrobust pacakge

#Variables <- cut-off (dist_p) and outcome (all the dm)
dist <- merge_rasters_dataframes$dist_p_km
light <- cbind(merge_rasters_dataframes[14:35])

#Estimation for all years
rd_nonpara <- list()
for(i in c(1:22)){
  rd_nonpara[[str_c(i)]] <- rdrobust(x = dist, y = light[, i])
}


#Extract LATE's from list and create a table

rd_nonpara_table <- list() #Table of LATE and p-values
for(i in 1:length(rd_nonpara)){
  rd_nonpara_table[[i]] <- rd_nonpara[[i]]$tabl3.str[1, ]
}

rd_nonpara_table <- ldply(rd_nonpara_table) #Reshape and create a table of LATE's
rd_nonpara_table$year <- c(1992:2013)

for(i in 1:length(rd_nonpara_table)){
  rd_nonpara_table[, i] <- as.numeric(rd_nonpara_table[, i])
}

#Graph LATE for all years with IC's 
theme_set(theme_gray(base_size = 13))
g1 <- ggplot(rd_nonpara_table, aes(year)) +
  geom_line(aes(y = Coef), colour = "blue") +
  geom_ribbon(aes(ymin = rd_nonpara_table$`CI Lower`, ymax = rd_nonpara_table$`CI Upper`), alpha = 0.2)
g1 <- g1 + scale_x_continuous(breaks=c(1992:2013))
g1 <- g1 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size = 10),
                 axis.text.y = element_text(size = 10))
g1 <- g1 + labs(x="Año", y=expression(paste("Diferencia (LATE)")), title="Discontinuidad por año - Estimación no paramétrica \n(1992- 2013)") 
g1 <- g1 + theme(plot.title = element_text(size=20, face="bold", 
                                  margin = margin(10, 10, 10, 10)))
g1 <- g1 +  geom_vline(xintercept=1997, linetype = 2) 


png("LATE_local.png", width = 13, height = 9, units = 'in', res = 800)
g1
dev.off()

#rdlocrand package download (from Cattaneo's web-page)
url <- "http://www-personal.umich.edu/~cattaneo/software/rdlocrand/R/"
links <- getHTMLLinks(url)
filenames <- links[str_detect(links, ".R")]
filenames_list <- as.list(filenames)

l_ply(filenames_list, download,
      baseurl = "http://www-personal.umich.edu/~cattaneo/software/rdlocrand/R/",
      folder = "rdlocrand"
)

#Loading source commands of the package
setwd("rdlocrand")
source("rdwinselect.R")
source("rdrandinf.R")
source("rdsensitivity.R")
source("rdrbounds.R")

#Define a window of treatment
covariates <- cbind(merge_rasters_dataframes$slope, merge_rasters_dataframes$roughness)

window <- rdwinselect(dist, covariates)
window <- rdrandinf(light[, 2], dist ,statistic = "all", 
                 covariates = covariates, wmin = 10, wstep = 1, 
                 rdwreps=10000, p = 2)

rd_nonpara_locrand <- list()
for(i in c(1:22)){
  rd_nonpara_locrand[[str_c(i)]] <- rdrobust(x = dist, y = light[, i], h = 23, matches = 8) 
}

rd_nonpara_locrand_table <- list() #Table of LATE and p-values
for(i in 1:length(rd_nonpara_locrand)){
  rd_nonpara_locrand_table[[i]] <- rd_nonpara_locrand[[i]]$tabl3.str[1, ]
}

rd_nonpara_locrand_table <- ldply(rd_nonpara_locrand_table) #Reshape and create a table of LATE's
rownames(rd_nonpara_locrand_table) <- c(1992:2013)
rd_nonpara_locrand_table$year <- c(1992:2013)

for(i in 1:length(rd_nonpara_locrand_table)){
  rd_nonpara_locrand_table[, i] <- as.numeric(rd_nonpara_locrand_table[, i])
}

stargazer(rd_nonpara_locrand_table[,c(1:6)], summary = F,  
          title = "Estimación no paramétrica para todos los años con supuestos de aletoriedad",
          coef = rd_nonpara_locrand_table$`Coef `, se = rd_nonpara_locrand_table$`Std. Err.`,
          p = rd_nonpara_locrand_table$`P>|z|`)


#Graph LATE for all years with IC's for random selected windows
theme_set(theme_gray(base_size = 13))
g2 <- ggplot(rd_nonpara_locrand_table, aes(year)) +
  geom_line(aes(y = Coef), colour = "blue") +
  geom_ribbon(aes(ymin = rd_nonpara_locrand_table$`CI Lower` , ymax = rd_nonpara_locrand_table$`CI Upper`), alpha = 0.2)
g2 <- g2 + scale_x_continuous(breaks=c(1992:2013))
g2 <- g2 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size = 10, face = "bold"),
                 axis.text.y = element_text(size = 10, face = "bold"))
g2 <- g2 + labs(x="Año", y=expression(paste("Diferencia (LATE)")), title="Discontinuidad por año - Estimación no paramétrica\n(1992- 2013)") 
g2 <- g2 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 10, 10, 10)),
                 axis.title = element_text(face="bold"))
g2 <- g2 + geom_vline(xintercept=1997, linetype = 2) 

png("LATE_locrand.png", width = 13, height = 7, units = 'in', res = 800)
g2
dev.off()


g3 <- g2 + annotate("text", x = 1998, y = 0.1, label = "-0.5426") #Add label for 1998 LATE
png("LATE_locrand_98.png", width = 13, height = 7, units = 'in', res = 800)
g3
dev.off()


#RD graph approach


attach(merge_rasters_dataframes) 
png("rd_1997.png", width = 13, height = 9, units = 'in', res = 800)
rdplot_1997 <- rdplot(y = dm1997, x = dist_p, c = 0, p = 2, binselect = "esmv",
                 x.label = "Distancia", y.label = "Actividad económica", y.lim = c(0, 2), 
                 lowerend = -6000, upperend = 6000, col.lines = c(size = 10, color = "blue"),
                 title = "Discontinuidad en la actividad económica - 1997")
dev.off()
detach(merge_rasters_dataframes)


attach(merge_rasters_dataframes) 
png("rd_2013.png", width = 13, height = 9, units = 'in', res = 800)
rdplot_2013 <- rdplot(y = dm2013, x = dist_p, c = 0, p = 2, binselect = "esmv",
                      x.label = "Distancia", y.label = "Actividad económica", y.lim = c(0, 2), 
                      lowerend = -6000, upperend = 6000, col.lines = c(size = 10), 
                      title = "Discontinuidad en la actividad económica - 2013")
dev.off()
detach(merge_rasters_dataframes)

#rdd package
merge_rasters_dataframes$dist_p_km <- merge_rasters_dataframes$dist_p / 1000
tratamientos <- names(merge_rasters_dataframes)[49:65]
luces <- names(merge_rasters_dataframes)[19:35]
controles <- c("altura_mean_30arc + roughness + slope + colds00ag + dist_coast + sq1 + sq2 + sq3 + sq4 + sq5 + sq6 + sq7")
rd_nonpara_ik <- list()

formulas_sharp <- paste(paste(paste("log(0.01 + ", luces, ")", sep = ""), "dist_p_km", sep = " ~ "), controles, sep = " | ") 
formulas_fuzzy <- paste(paste(paste(paste("log(0.01 + ", luces, ")", sep = ""), "dist_p_km", sep = " ~ "),  tratamientos, sep = " + "), controles, sep = " | ") 

rd_nonpara_ik <- lapply(formulas_sharp, function(x) {
  RDestimate(as.formula(x),
             data = merge_rasters_dataframes_colonial,
             cluster = merge_rasters_dataframes_colonial$municode,
             cutpoint = 0,
             model = TRUE)
}) 

 
rd_nonpara_ik_fuzzy <- lapply(formulas_fuzzy, function(x) {
  RDestimate(as.formula(x),
             data = merge_rasters_dataframes_colonial,
             cluster = merge_rasters_dataframes_colonial$municode,
             cutpoint = 0,
             model = TRUE)
}) 



#Extract estimates and CI's.

rd_nonpara_ik_table <- lapply(rd_nonpara_ik, "[", c("est","p", "bw")) %>%
  lapply(ldply) %>%
  ldply() %>%
  mutate(year = c(rep(1997:2013, each = 3))) %>%
  setcolorder(c("year", "LATE", "Half-BW", "Double-BW", ".id")) %>%
  mutate( year = as.factor(year)) %>%
  select_(-.id)


stargazer(rd_nonpara_ik_table, summary = F)

rd_nonpara_ik_table <- lapply(rd_nonpara_ik, "[", c("est","p", "bw")) %>%
  lapply(ldply) %>%
  ldply() %>%
  mutate(year = c(rep(1997:2013, each = 3))) %>%
  setcolorder(c("year", "LATE", "Half-BW", "Double-BW", ".id")) %>%
  mutate( year = as.factor(year)) %>%
  reshape(direction = "wide", idvar = "year", timevar = ".id") 

rd_nonpara_ik_table_ci <- lapply(rd_nonpara_ik, "[", c("ci")) %>%
  lapply(ldply) %>%
  lapply("[", 2, ) %>%
  ldply() %>%
  mutate(year = c(rep(1997:2013, each = 1))) %>%
  reshape(direction = "wide", idvar = "year", timevar = ".id") 

names(rd_nonpara_ik_table_ci)[3] <- "superior"
names(rd_nonpara_ik_table_ci)[2] <- "inferior"

#Merge LATE's data
rd_nonpara_ik_table <- cbind(rd_nonpara_ik_table, rd_nonpara_ik_table_ci)


#Graph ggplot estimates LATE's
library(ggplot2)

g2 <- ggplot(rd_nonpara_ik_table, aes(x = as.numeric(as.character(year)), y = LATE.est)) +
  geom_ribbon(aes(ymin = rd_nonpara_ik_table$inferior  , ymax = rd_nonpara_ik_table$superior), alpha = 0.2)
g2 <- g2 + geom_line()  + theme_grey()
g2 <- g2 + scale_x_continuous(breaks=c(1992:2013)) + coord_fixed(ratio = 11)
g2 <- g2 + labs(x="Año", y=expression(paste("Diferencia (LATE)"))) 
g2 <- g2 + theme(axis.title.y = element_text(angle = 90, size = 12),
                 axis.title.x = element_text(angle = 0, size = 12))
g2 <- g2 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

g2 

png("~/Dropbox/BANREP/Pacifico/Primer_DTSER/Entregas/Imgs/DLATE_locrand.png", width = 13, height = 5, units = 'in', res = 800)
g2
dev.off()


#Graph discontinuities - 2013
par <- opar()
par(mfrow = c(3, 3))
plot(rd_nonpara_ik[[1]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[2]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[3]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[4]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[5]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[6]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[7]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[8]], 200 ,range = c(-5, 5), las = 1)
plot(rd_nonpara_ik[[9]], 200 ,range = c(-5, 5), las = 1)


#RD for pixels near cities
formulas_fx <- paste(paste(paste("log(0.01 + " ,luces, ")" , sep = ""), "dist_p_km", sep = " ~ "), controles, sep = " | ") 
merge_rasters_dataframes_cities <- subset(merge_rasters_dataframes, dist_capital < summary(dist_capital)[3]) 

rd_nonpara_ik_cities <- lapply(formulas_fx, function(x) {
  RDestimate(as.formula(x),
             data = merge_rasters_dataframes,
             cutpoint = 0,
             cluster = merge_rasters_dataframes_cities$municode,
             verbose = T)
}) 


#RD for non treatment years
merge_rasters_dataframes$dist_p_km <- merge_rasters_dataframes$dist_p / 1000
tratamientos <- names(merge_rasters_dataframes)[49:65]
luces_no_tratamiento <- names(merge_rasters_dataframes)[14:18]
controles <- c("altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial")
rd_nonpara_ik <- list()

formulas_fx_nt <- paste(paste(paste("log(0.01 + " ,luces_no_tratamiento, ")" , sep = ""), "dist_p_km", sep = " ~ "), controles, sep = " | ") 

rd_nonpara_ik_nt <- lapply(formulas_fx_nt, function(x) {
  RDestimate(as.formula(x),
             data = merge_rasters_dataframes,
             cluster = merge_rasters_dataframes$municode,
             cutpoint = 0,
             model = TRUE)
})

#Extract estimates and CI's.

rd_nonpara_ik_nt_table <- lapply(rd_nonpara_ik_nt, "[", c("est","p", "bw")) %>%
  lapply(ldply) %>%
  ldply() %>%
  mutate(year = c(rep(1992:1996, each = 3))) %>%
  setcolorder(c("year", "LATE", "Half-BW", "Double-BW", ".id")) %>%
  mutate( year = as.factor(year)) %>%
  dplyr::select(-.id)


stargazer(rd_nonpara_ik_nt_table, summary = F)

rd_nonpara_ik_nt_table <- lapply(rd_nonpara_ik_nt, "[", c("est","p", "bw")) %>%
  lapply(ldply) %>%
  ldply() %>%
  mutate(year = c(rep(1992:1996, each = 3))) %>%
  setcolorder(c("year", "LATE", "Half-BW", "Double-BW", ".id")) %>%
  mutate( year = as.factor(year)) %>%
  reshape(direction = "wide", idvar = "year", timevar = ".id") 

rd_nonpara_ik_nt_table_ci <- lapply(rd_nonpara_ik_nt, "[", c("ci")) %>%
  lapply(ldply) %>%
  lapply("[", 2, ) %>%
  ldply() %>%
  mutate(year = c(rep(1992:1996, each = 1))) %>%
  reshape(direction = "wide", idvar = "year", timevar = ".id") 

names(rd_nonpara_ik_nt_table_ci)[3] <- "superior"
names(rd_nonpara_ik_nt_table_ci)[2] <- "inferior"

#Merge LATE's data
rd_nonpara_ik_nt_table <- cbind(rd_nonpara_ik_nt_table, rd_nonpara_ik_nt_table_ci)


#Graph ggplot estimates LATE's
library(ggplot2)

g4 <- ggplot(rd_nonpara_ik_nt_table, aes(x = as.numeric(as.character(year)), y = LATE.est)) +
  geom_ribbon(aes(ymin = rd_nonpara_ik_nt_table$inferior  , ymax = rd_nonpara_ik_nt_table$superior), alpha = 0.2)
g4 <- g4 + geom_line()  + theme_grey()
g4 <- g4 + scale_x_continuous(breaks=c(1992:1996)) + coord_fixed(ratio = 11)
g4 <- g4 + labs(x="Año", y=expression(paste("Diferencia (LATE)"))) 
g4 <- g4 + theme(axis.title.y = element_text(angle = 90, size = 12),
                 axis.title.x = element_text(angle = 0, size = 12))
g4 <- g4 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

g4 

#Placebo with random cutpoints (panel)
discontinuity_data <- RDDdata(x = dist_p_km,
                              y = log(0.01 + dm),
                              data = merge_rasters_dataframes_long,
                              cutpoint = 0) 
reg_nonpara <- RDDreg_np(discontinuity_data)
placebo <- plotPlacebo(reg_nonpara, from = 0.1, to = 1, by = 0.1, same_bw = T, output = "ggplot")
placebo <- mutate(placebo, significativo = as.numeric(p_value > 0.05))


g3 <- ggplot(placebo, aes(x = cutpoint, y = LATE)) +
  geom_ribbon(aes(ymin = placebo$CI_low, ymax = placebo$CI_high), alpha = 0.2)
g3 <- g3 + geom_line(aes(colour = placebo$position)) + theme_grey()
g3 <- g3 + scale_x_continuous(breaks = seq(-80, 40, by = 10))
g3 <- g3 + coord_fixed(ratio = 20) + geom_hline(yintercept = 0)
g3 <- g3 + labs(x="Punto de discontinuidad", y=expression(paste("Diferencia (LATE)")))
g3 <- g3 + theme(axis.title.y = element_text(angle = 90, size = 12),
                 axis.title.x = element_text(angle = 0, size = 12), legend.position = "none")
g3 <- g3 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

g3


#Placebo with random cutpoints (2013)
discontinuity_data <- RDDdata(x = dist_p_km,
                              y = log(0.01 + dm2013),
                              data = merge_rasters_dataframes,
                              cutpoint = 0) 
reg_nonpara_2013 <- RDDreg_np(discontinuity_data)
placebo_2013 <- plotPlacebo(reg_nonpara_2013, from = 0.1, to = 1, by = 0.1, same_bw = T, output = "data")
placebo_2013 <- mutate(placebo_2013, significativo = as.numeric(p_value > 0.05))


g4 <- ggplot(placebo_2013, aes(x = cutpoint, y = LATE)) +
  geom_ribbon(aes(ymin = placebo_2013$CI_low, ymax = placebo_2013$CI_high), alpha = 0.2)
g4 <- g4 + geom_line(aes(colour = placebo_2013$position)) + theme_grey()
g4 <- g4 + scale_x_continuous(breaks = seq(-80, 40, by = 10))
g4 <- g4 + coord_fixed(ratio = 20) + geom_hline(yintercept = 0)
g4 <- g4 + labs(x="Punto de discontinuidad", y=expression(paste("Diferencia (LATE)")))
g4 <- g4 + theme(axis.title.y = element_text(angle = 90, size = 12),
                 axis.title.x = element_text(angle = 0, size = 12), legend.position = "none")
g4 <- g4 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

g4
